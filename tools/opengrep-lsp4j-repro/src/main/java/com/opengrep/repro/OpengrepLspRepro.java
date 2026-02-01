package com.opengrep.repro;

import com.google.gson.Gson;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import org.eclipse.lsp4j.ApplyWorkspaceEditParams;
import org.eclipse.lsp4j.ApplyWorkspaceEditResponse;
import org.eclipse.lsp4j.ClientCapabilities;
import org.eclipse.lsp4j.CreateFilesParams;
import org.eclipse.lsp4j.Diagnostic;
import org.eclipse.lsp4j.DiagnosticSeverity;
import org.eclipse.lsp4j.DidCloseTextDocumentParams;
import org.eclipse.lsp4j.DidOpenTextDocumentParams;
import org.eclipse.lsp4j.FileCreate;
import org.eclipse.lsp4j.InitializeParams;
import org.eclipse.lsp4j.InitializedParams;
import org.eclipse.lsp4j.MessageActionItem;
import org.eclipse.lsp4j.MessageParams;
import org.eclipse.lsp4j.MessageType;
import org.eclipse.lsp4j.PublishDiagnosticsParams;
import org.eclipse.lsp4j.ShowMessageRequestParams;
import org.eclipse.lsp4j.TextDocumentIdentifier;
import org.eclipse.lsp4j.TextDocumentItem;
import org.eclipse.lsp4j.WorkspaceFolder;
import org.eclipse.lsp4j.WorkDoneProgressCreateParams;
import org.eclipse.lsp4j.ProgressParams;
import org.eclipse.lsp4j.jsonrpc.Launcher;
import org.eclipse.lsp4j.jsonrpc.services.JsonNotification;
import org.eclipse.lsp4j.services.LanguageClient;
import org.eclipse.lsp4j.services.LanguageServer;
import org.eclipse.lsp4j.services.TextDocumentService;
import org.eclipse.lsp4j.services.WorkspaceService;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.Closeable;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.time.Duration;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Queue;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.LongAdder;

public final class OpengrepLspRepro {
    private static final int DEFAULT_THREADS = 4;
    private static final Duration DEFAULT_DIAGNOSTIC_TIMEOUT = Duration.ofSeconds(10);
    private static final int DEFAULT_MAX_FILES = 20;
    private static final Gson GSON = new Gson();

    public static void main(String[] args) throws Exception {
        Path workspace = Paths.get(envOrDefault("NOPCOMMERCE_PATH", "/tmp/nopCommerce")).toAbsolutePath();
        if (!Files.isDirectory(workspace)) {
            System.err.println("NOPCOMMERCE_PATH is not a directory: " + workspace);
            System.exit(2);
        }
        Path overlayDir = Files.createTempDirectory("opengrep-lsp-overlay-").toAbsolutePath();
        System.out.println("Overlay workspace: " + overlayDir);

        List<Path> files = listWorkspaceFiles(workspace);
        Path onlyList = resolveOnlyFilesList();
        if (onlyList != null) {
            files = filterByList(files, onlyList);
            System.out.println("Filtering to list: " + onlyList.toAbsolutePath());
            System.out.println("Files after list filter: " + files.size());
        }
        int maxFiles = resolveMaxFiles();
        List<Path> filesToProcess;
        if (maxFiles > 0 && files.size() > maxFiles) {
            filesToProcess = new ArrayList<>(files.subList(0, maxFiles));
        } else {
            filesToProcess = files;
        }
        final List<Path> finalFiles = filesToProcess;
        System.out.println("Found files: " + finalFiles.size());

        List<String> command = resolveLspCommand();
        System.out.println("Using LSP command: " + String.join(" ", command));

        ProcessBuilder builder = new ProcessBuilder(command);
        builder.directory(workspace.toFile());
        Process process = builder.start();

        String runTag = envOrDefault("RUN_TAG", "").trim();
        String suffix = runTag.isEmpty() ? "" : "-" + runTag;
        Path stderrLog = Paths.get("opengrep-lsp-stderr" + suffix + ".log").toAbsolutePath();
        Thread stderrThread = startStderrDrainer(process.getErrorStream(), stderrLog);

        boolean versionMatch = resolveVersionMatch(args);
        Stats stats = new Stats();
        DiagnosticsGate gate = new DiagnosticsGate(versionMatch);
        LineLogger telemetryLogger = new LineLogger(Paths.get("telemetry" + suffix + ".log").toAbsolutePath());
        LineLogger perFileLogger = buildPerFileLogger(suffix);

        OpenGrepClient client = new OpenGrepClient(stats, gate, telemetryLogger);
        Launcher<LanguageServer> launcher = Launcher.createLauncher(
                client,
                LanguageServer.class,
                process.getInputStream(),
                process.getOutputStream()
        );
        LanguageServer server = launcher.getRemoteProxy();
        client.attachServer(server);
        Future<Void> listening = launcher.startListening();

        initializeServer(server, workspace, overlayDir, client);

        TextDocumentService textService = server.getTextDocumentService();
        WorkspaceService workspaceService = server.getWorkspaceService();
        Duration diagnosticTimeout = resolveDiagnosticTimeout();
        AtomicInteger versionCounter = new AtomicInteger(1);
        int threads = resolveThreads();
        ExecutorService executor = Executors.newFixedThreadPool(threads);
        AtomicInteger nextIndex = new AtomicInteger(0);

        System.out.println("Version match enabled: " + versionMatch);
        long wallStart = System.nanoTime();
        for (int i = 0; i < threads; i++) {
            executor.submit(() -> {
                while (true) {
                    int idx = nextIndex.getAndIncrement();
                    if (idx >= finalFiles.size()) {
                        break;
                    }
                    Path file = finalFiles.get(idx);
                    processFile(file, workspace, overlayDir, textService, workspaceService, gate, stats, versionCounter, diagnosticTimeout, perFileLogger, versionMatch);
                }
            });
        }

        executor.shutdown();
        executor.awaitTermination(7, TimeUnit.DAYS);
        long wallElapsed = System.nanoTime() - wallStart;

        shutdownServer(server);

        telemetryLogger.close();
        closeQuietly(perFileLogger);
        closeQuietly(process.getOutputStream());
        closeQuietly(process.getInputStream());
        process.destroy();
        listening.cancel(true);

        stderrThread.join(TimeUnit.SECONDS.toMillis(3));

        emitStats(stats, wallElapsed, finalFiles.size(), stderrLog, suffix);
        writeFailures(stats.failures(), Paths.get("failures" + suffix + ".txt").toAbsolutePath());
        writeList(stats.errorDiagnosticUris(), Paths.get("diagnostics-errors" + suffix + ".txt").toAbsolutePath());
        writeList(stats.warningDiagnosticUris(), Paths.get("diagnostics-warnings" + suffix + ".txt").toAbsolutePath());
        writeList(stats.problemUris(), Paths.get("problem-files" + suffix + ".txt").toAbsolutePath());
        writeDiagnosticSummary(stats.diagnosticsSummary(), Paths.get("diagnostics-summary" + suffix + ".txt").toAbsolutePath());

        cleanupOverlay(overlayDir);

        if (!stats.failures().isEmpty()) {
            System.exit(1);
        }
    }

    private static void initializeServer(LanguageServer server, Path workspace, Path overlayDir, OpenGrepClient client) throws Exception {
        InitializeParams init = new InitializeParams();
        init.setProcessId((int) ProcessHandle.current().pid());
        init.setRootUri(workspace.toUri().toString());
        init.setCapabilities(new ClientCapabilities());
        init.setInitializationOptions(buildInitializationOptions());
        List<WorkspaceFolder> folders = new ArrayList<>();
        folders.add(new WorkspaceFolder(workspace.toUri().toString(), "nopCommerce"));
        folders.add(new WorkspaceFolder(overlayDir.toUri().toString(), overlayDir.getFileName().toString()));
        init.setWorkspaceFolders(folders);
        server.initialize(init).get(30, TimeUnit.SECONDS);
        server.initialized(new InitializedParams());
        awaitRulesRefreshed(client);
    }

    private static void awaitRulesRefreshed(OpenGrepClient client) {
        long timeout = parseLongEnv("RULES_REFRESH_TIMEOUT_SECS", 120);
        try {
            boolean ok = client.rulesRefreshed.await(timeout, TimeUnit.SECONDS);
            if (!ok) {
                System.err.println("Timed out waiting for semgrep/rulesRefreshed after " + timeout + "s");
            }
        } catch (InterruptedException ignored) {
            Thread.currentThread().interrupt();
        }
    }

    private static void shutdownServer(LanguageServer server) {
        try {
            server.shutdown().get(10, TimeUnit.SECONDS);
        } catch (Exception ignored) {
        }
        try {
            server.exit();
        } catch (Exception ignored) {
        }
    }

    private static void processFile(
            Path file,
            Path workspace,
            Path overlayDir,
            TextDocumentService textService,
            WorkspaceService workspaceService,
            DiagnosticsGate gate,
            Stats stats,
            AtomicInteger versionCounter,
            Duration diagnosticTimeout,
            LineLogger perFileLogger,
            boolean versionMatch
    ) {
        Path overlayPath = resolveOverlayPath(file, workspace, overlayDir);
        String uri = documentUriFor(overlayPath);
        stats.totalFiles.increment();

        int documentVersion = versionCounter.getAndIncrement();
        CompletableFuture<DiagnosticOutcome> future = gate.register(uri, documentVersion);
        long start = System.nanoTime();
        long fileSize = safeFileSize(file);

        boolean success = false;
        String status = "ok";
        int diagTotal = 0;
        int diagErrors = 0;
        int diagWarnings = 0;
        Integer actualVersion = null;
        try {
            copyToOverlay(file, overlayPath);
            sendDidCreate(workspaceService, overlayPath);
            TextDocumentItem item = new TextDocumentItem(
                    uri,
                    languageIdFor(file),
                    documentVersion,
                    ""
            );
            textService.didOpen(new DidOpenTextDocumentParams(item));

            DiagnosticOutcome outcome = future.get(diagnosticTimeout.toMillis(), TimeUnit.MILLISECONDS);
            actualVersion = outcome.actualVersion;
            if (outcome.status == OutcomeStatus.OK) {
                PublishDiagnosticsParams params = outcome.params;
                List<Diagnostic> diagnostics = params.getDiagnostics();
                diagTotal = diagnostics.size();
                for (Diagnostic diagnostic : diagnostics) {
                    DiagnosticSeverity severity = diagnostic.getSeverity();
                    if (severity == DiagnosticSeverity.Error) {
                        diagErrors++;
                    } else if (severity == DiagnosticSeverity.Warning) {
                        diagWarnings++;
                    }
                }
                stats.addDiagnostics(uri, diagnostics);
                success = true;
            } else {
                stats.failVersionMismatch(uri, documentVersion, actualVersion);
                status = "version-miss";
            }
        } catch (TimeoutException timeout) {
            stats.failTimeout(uri);
            status = "timeout";
        } catch (Exception ex) {
            stats.failException(uri, ex);
            status = "error";
        } finally {
            gate.unregister(uri);
            stats.addProcessingNanos(System.nanoTime() - start);
            textService.didClose(new DidCloseTextDocumentParams(new TextDocumentIdentifier(uri)));
            deleteOverlayFile(overlayPath);
        }

        if (success) {
            stats.successes.increment();
        } else {
            stats.failures.increment();
        }

        if (perFileLogger != null) {
            long elapsedMs = TimeUnit.NANOSECONDS.toMillis(System.nanoTime() - start);
            String line = String.format(
                    Locale.ROOT,
                    "%s\toverlay=%s\tstatus=%s\tms=%d\tbytes=%d\tdiagTotal=%d\tdiagErrors=%d\tdiagWarnings=%d\tversion=%d\tdiagVersion=%s",
                    file.toAbsolutePath().normalize(), overlayPath.toAbsolutePath().normalize(),
                    status, elapsedMs, fileSize, diagTotal, diagErrors, diagWarnings, documentVersion,
                    actualVersion == null ? "null" : actualVersion.toString()
            );
            perFileLogger.log(line);
        }
    }

    private static String languageIdFor(Path file) {
        String name = file.getFileName().toString();
        int idx = name.lastIndexOf('.');
        if (idx == -1 || idx == name.length() - 1) {
            return "plaintext";
        }
        return name.substring(idx + 1).toLowerCase(Locale.ROOT);
    }

    private static List<Path> listWorkspaceFiles(Path workspace) throws IOException {
        List<Path> files = new ArrayList<>();
        try (var stream = Files.walk(workspace)) {
            stream.filter(Files::isRegularFile)
                    .filter(path -> !isUnderGitDir(path))
                    .forEach(files::add);
        }
        return files;
    }

    private static boolean isUnderGitDir(Path path) {
        for (Path part : path) {
            if (part.toString().equals(".git")) {
                return true;
            }
        }
        return false;
    }

    private static Thread startStderrDrainer(InputStream stderr, Path logPath) throws IOException {
        BufferedWriter writer = Files.newBufferedWriter(logPath, StandardCharsets.UTF_8);
        Thread thread = new Thread(() -> {
            try (BufferedReader reader = new BufferedReader(new InputStreamReader(stderr, StandardCharsets.UTF_8))) {
                String line;
                while ((line = reader.readLine()) != null) {
                    synchronized (writer) {
                        writer.write(line);
                        writer.newLine();
                        writer.flush();
                    }
                }
            } catch (IOException ignored) {
            } finally {
                synchronized (writer) {
                    try {
                        writer.flush();
                    } catch (IOException ignored) {
                    }
                    try {
                        writer.close();
                    } catch (IOException ignored) {
                    }
                }
            }
        }, "opengrep-lsp-stderr");
        thread.setDaemon(true);
        thread.start();
        return thread;
    }

    private static void emitStats(Stats stats, long wallElapsedNanos, int totalFiles, Path stderrLog, String suffix) {
        long avgNanos = totalFiles == 0 ? 0L : stats.processingNanos.sum() / totalFiles;
        System.out.println("\n=== Stats ===");
        System.out.println("Files total: " + totalFiles);
        System.out.println("Failures (timeout + errors): " + stats.failures.sum());
        System.out.println("Timeout failures: " + stats.timeoutFailures.sum());
        System.out.println("Version mismatches: " + stats.versionMismatchFailures.sum());
        System.out.println("Total time: " + formatDuration(wallElapsedNanos));
        System.out.println("Avg time per file: " + formatDuration(avgNanos));
        System.out.println("Diagnostics errors: " + stats.errorDiagnostics.sum());
        System.out.println("Diagnostics warnings: " + stats.warningDiagnostics.sum());
        System.out.println("Total issue size: " + stats.totalDiagnostics.sum());
        System.out.println("Telemetry warnings: " + stats.telemetryWarnings.sum());
        System.out.println("Telemetry errors: " + stats.telemetryErrors.sum());
        System.out.println("stderr log: " + stderrLog);
        System.out.println("telemetry log: " + Paths.get("telemetry" + suffix + ".log").toAbsolutePath());
        System.out.println("failures list: " + Paths.get("failures" + suffix + ".txt").toAbsolutePath());
        System.out.println("diagnostic errors list: " + Paths.get("diagnostics-errors" + suffix + ".txt").toAbsolutePath());
        System.out.println("diagnostic warnings list: " + Paths.get("diagnostics-warnings" + suffix + ".txt").toAbsolutePath());
        System.out.println("problem files list: " + Paths.get("problem-files" + suffix + ".txt").toAbsolutePath());
        System.out.println("diagnostics summary: " + Paths.get("diagnostics-summary" + suffix + ".txt").toAbsolutePath());
        Path perFileLog = Paths.get("per-file" + suffix + ".log").toAbsolutePath();
        if (Files.exists(perFileLog)) {
            System.out.println("per-file log: " + perFileLog);
        }
    }

    private static String formatDuration(long nanos) {
        Duration duration = Duration.ofNanos(nanos);
        long seconds = duration.toSeconds();
        long millis = duration.minusSeconds(seconds).toMillis();
        return seconds + "s " + millis + "ms";
    }

    private static Map<String, Object> buildInitializationOptions() {
        Map<String, Object> scan = new ConcurrentHashMap<>();
        String configPath = System.getenv("OPENGREP_RULES_PATH");
        if (configPath != null && !configPath.isBlank()) {
            scan.put("configuration", List.of(configPath));
        }
        scan.put("maxTargetBytes", 0);
        scan.put("onlyGitDirty", false);
        scan.put("ci", false);
        Map<String, Object> metrics = new ConcurrentHashMap<>();
        metrics.put("enabled", false);
        metrics.put("isNewAppInstall", true);

        Map<String, Object> initOptions = new ConcurrentHashMap<>();
        initOptions.put("scan", scan);
        Boolean disableTargetCache = parseBooleanEnv("DISABLE_TARGET_CACHE");
        if (disableTargetCache != null) {
            initOptions.put("disableTargetCache", disableTargetCache);
        }
        Boolean scanOnMiss = parseScanOnMissEnv();
        if (scanOnMiss != null) {
            initOptions.put("scanOnMiss", scanOnMiss);
        }
        Boolean skipTaint = parseBooleanEnv("SKIP_TAINT");
        if (skipTaint != null) {
            initOptions.put("skipTaint", skipTaint);
        }
        initOptions.put("metrics", metrics);
        initOptions.put("doHover", false);
        return initOptions;
    }

    private static void writeFailures(Queue<String> failures, Path output) {
        try (BufferedWriter writer = Files.newBufferedWriter(output, StandardCharsets.UTF_8)) {
            for (String failure : failures) {
                writer.write(failure);
                writer.newLine();
            }
        } catch (IOException ignored) {
        }
    }

    private static void writeList(Iterable<String> items, Path output) {
        try (BufferedWriter writer = Files.newBufferedWriter(output, StandardCharsets.UTF_8)) {
            for (String item : items) {
                writer.write(item);
                writer.newLine();
            }
        } catch (IOException ignored) {
        }
    }

    private static void writeDiagnosticSummary(Map<String, DiagnosticSummary> summaryNotes, Path output) {
        try (BufferedWriter writer = Files.newBufferedWriter(output, StandardCharsets.UTF_8)) {
            for (Map.Entry<String, DiagnosticSummary> entry : summaryNotes.entrySet()) {
                DiagnosticSummary summary = entry.getValue();
                writer.write(entry.getKey());
                writer.write("\t");
                writer.write("warnings=" + summary.warnings.sum());
                writer.write("\t");
                writer.write("errors=" + summary.errors.sum());
                writer.write("\t");
                writer.write("total=" + summary.total.sum());
                writer.newLine();
            }
        } catch (IOException ignored) {
        }
    }

    private static List<String> splitCommand(String commandLine) {
        List<String> parts = new ArrayList<>();
        StringBuilder current = new StringBuilder();
        boolean inQuotes = false;
        for (int i = 0; i < commandLine.length(); i++) {
            char c = commandLine.charAt(i);
            if (c == '"') {
                inQuotes = !inQuotes;
                continue;
            }
            if (!inQuotes && Character.isWhitespace(c)) {
                if (current.length() > 0) {
                    parts.add(current.toString());
                    current.setLength(0);
                }
            } else {
                current.append(c);
            }
        }
        if (current.length() > 0) {
            parts.add(current.toString());
        }
        return parts;
    }

    private static String envOrDefault(String key, String defaultValue) {
        String value = System.getenv(key);
        return value == null || value.isBlank() ? defaultValue : value;
    }

    private static int parseIntEnv(String key) {
        String value = System.getenv(key);
        if (value == null || value.isBlank()) {
            return 0;
        }
        try {
            return Integer.parseInt(value.trim());
        } catch (NumberFormatException ignored) {
            return 0;
        }
    }

    private static long parseLongEnv(String key, long defaultValue) {
        String value = System.getenv(key);
        if (value == null || value.isBlank()) {
            return defaultValue;
        }
        try {
            return Long.parseLong(value.trim());
        } catch (NumberFormatException ignored) {
            return defaultValue;
        }
    }

    private static Boolean parseBooleanEnv(String key) {
        String value = System.getenv(key);
        if (value == null || value.isBlank()) {
            return null;
        }
        String normalized = value.trim().toLowerCase(Locale.ROOT);
        if (normalized.equals("1") || normalized.equals("true") || normalized.equals("yes")) {
            return true;
        }
        if (normalized.equals("0") || normalized.equals("false") || normalized.equals("no")) {
            return false;
        }
        return null;
    }

    private static boolean resolveVersionMatch(String[] args) {
        Boolean fromArgs = parseBooleanFlag(args, "--version-match");
        if (fromArgs != null) {
            return fromArgs;
        }
        Boolean fromEnv = parseBooleanEnv("VERSION_MATCH");
        return fromEnv != null && fromEnv;
    }

    private static Boolean parseBooleanFlag(String[] args, String flagName) {
        String prefix = flagName + "=";
        for (String arg : args) {
            if (arg.equals(flagName)) {
                return true;
            }
            if (arg.equals("--no-" + flagName.substring(2))) {
                return false;
            }
            if (arg.startsWith(prefix)) {
                String value = arg.substring(prefix.length());
                if (!value.isEmpty()) {
                    Boolean parsed = parseBooleanValue(value);
                    if (parsed != null) {
                        return parsed;
                    }
                }
            }
        }
        return null;
    }

    private static Boolean parseBooleanValue(String value) {
        String normalized = value.trim().toLowerCase(Locale.ROOT);
        if (normalized.equals("1") || normalized.equals("true") || normalized.equals("yes")) {
            return true;
        }
        if (normalized.equals("0") || normalized.equals("false") || normalized.equals("no")) {
            return false;
        }
        return null;
    }

    private static Boolean parseScanOnMissEnv() {
        String value = System.getenv("SCAN_ON_MISS");
        if (value == null || value.isBlank()) {
            return null;
        }
        String normalized = value.trim().toLowerCase(Locale.ROOT);
        if (normalized.equals("always") || normalized.equals("true") || normalized.equals("1")) {
            return true;
        }
        if (normalized.equals("skip") || normalized.equals("false") || normalized.equals("0")) {
            return false;
        }
        return null;
    }

    private static Duration resolveDiagnosticTimeout() {
        String value = System.getenv("DIAGNOSTIC_TIMEOUT_SECS");
        if (value == null || value.isBlank()) {
            return DEFAULT_DIAGNOSTIC_TIMEOUT;
        }
        try {
            long seconds = Long.parseLong(value.trim());
            if (seconds <= 0) {
                return DEFAULT_DIAGNOSTIC_TIMEOUT;
            }
            return Duration.ofSeconds(seconds);
        } catch (NumberFormatException ignored) {
            return DEFAULT_DIAGNOSTIC_TIMEOUT;
        }
    }

    private static int resolveThreads() {
        String value = System.getenv("THREADS");
        if (value == null || value.isBlank()) {
            return DEFAULT_THREADS;
        }
        try {
            int threads = Integer.parseInt(value.trim());
            return threads > 0 ? threads : DEFAULT_THREADS;
        } catch (NumberFormatException ignored) {
            return DEFAULT_THREADS;
        }
    }

    private static int resolveMaxFiles() {
        int fromEnv = parseIntEnv("MAX_FILES");
        if (fromEnv > 0) {
            return fromEnv;
        }
        return DEFAULT_MAX_FILES;
    }

    private static Path resolveOnlyFilesList() {
        String value = System.getenv("ONLY_FILES_LIST");
        if (value == null || value.isBlank()) {
            return null;
        }
        return Paths.get(value.trim());
    }

    private static List<Path> filterByList(List<Path> files, Path listPath) throws IOException {
        if (!Files.isRegularFile(listPath)) {
            System.err.println("ONLY_FILES_LIST is not a file: " + listPath.toAbsolutePath());
            return List.of();
        }
        List<String> lines = Files.readAllLines(listPath, StandardCharsets.UTF_8);
        var allowed = ConcurrentHashMap.<Path>newKeySet();
        for (String rawLine : lines) {
            String line = rawLine.trim();
            if (line.isEmpty() || line.startsWith("#")) {
                continue;
            }
            String uri = extractFileUri(line);
            if (uri != null) {
                try {
                    Path path = Paths.get(java.net.URI.create(uri)).toAbsolutePath().normalize();
                    allowed.add(path);
                } catch (Exception ignored) {
                }
                continue;
            }
            Path path = Paths.get(line);
            if (!path.isAbsolute()) {
                path = listPath.getParent() == null ? path : listPath.getParent().resolve(path);
            }
            allowed.add(path.toAbsolutePath().normalize());
        }
        if (allowed.isEmpty()) {
            return List.of();
        }
        List<Path> filtered = new ArrayList<>();
        for (Path file : files) {
            if (allowed.contains(file.toAbsolutePath().normalize())) {
                filtered.add(file);
            }
        }
        return filtered;
    }

    private static String extractFileUri(String line) {
        int idx = line.indexOf("file://");
        if (idx < 0) {
            return null;
        }
        String uri = line.substring(idx).trim();
        int end = uri.indexOf(' ');
        if (end > 0) {
            uri = uri.substring(0, end);
        }
        return uri;
    }

    private static String documentUriFor(Path file) {
        Path absolute = file.toAbsolutePath().normalize();
        return toStrictFileUri(absolute);
    }

    private static Path resolveOverlayPath(Path file, Path workspace, Path overlayRoot) {
        Path absoluteFile = file.toAbsolutePath().normalize();
        Path absoluteWorkspace = workspace.toAbsolutePath().normalize();
        Path relative;
        try {
            relative = absoluteWorkspace.relativize(absoluteFile);
        } catch (IllegalArgumentException ex) {
            relative = absoluteFile.getFileName();
        }
        return overlayRoot.resolve(relative).normalize();
    }

    private static void copyToOverlay(Path source, Path dest) throws IOException {
        Path parent = dest.getParent();
        if (parent != null) {
            Files.createDirectories(parent);
        }
        byte[] raw = Files.readAllBytes(source);
        byte[] normalized = normalizeLineEndingsAndBom(raw);
        Files.write(dest, normalized);
    }

    private static void deleteOverlayFile(Path path) {
        try {
            Files.deleteIfExists(path);
        } catch (IOException ignored) {
        }
    }

    private static void cleanupOverlay(Path overlayRoot) {
        try {
            if (!Files.exists(overlayRoot)) {
                return;
            }
            try (var walk = Files.walk(overlayRoot)) {
                walk.sorted((a, b) -> b.compareTo(a)).forEach(p -> {
                    try {
                        Files.deleteIfExists(p);
                    } catch (IOException ignored) {
                    }
                });
            }
        } catch (IOException ignored) {
        }
    }

    private static void sendDidCreate(WorkspaceService workspaceService, Path overlayPath) {
        try {
            String uri = documentUriFor(overlayPath);
            FileCreate fileCreate = new FileCreate(uri);
            workspaceService.didCreateFiles(new CreateFilesParams(List.of(fileCreate)));
        } catch (Exception ignored) {
        }
    }

    private static byte[] normalizeLineEndingsAndBom(byte[] raw) {
        int offset = 0;
        if (raw.length >= 3
                && (raw[0] & 0xFF) == 0xEF
                && (raw[1] & 0xFF) == 0xBB
                && (raw[2] & 0xFF) == 0xBF) {
            offset = 3;
        }
        byte[] tmp = new byte[raw.length - offset];
        System.arraycopy(raw, offset, tmp, 0, tmp.length);
        int len = tmp.length;
        int crlfPairs = 0;
        boolean hasCr = false;
        for (int i = 0; i < len; i++) {
            if (tmp[i] == '\r') {
                hasCr = true;
                if (i + 1 < len && tmp[i + 1] == '\n') {
                    crlfPairs++;
                    i++;
                }
            }
        }
        if (!hasCr) {
            return tmp;
        }
        byte[] out = new byte[len - crlfPairs];
        int j = 0;
        for (int i = 0; i < len; i++) {
            byte b = tmp[i];
            if (b == '\r') {
                if (i + 1 < len && tmp[i + 1] == '\n') {
                    out[j++] = '\n';
                    i++;
                } else {
                    out[j++] = '\n';
                }
            } else {
                out[j++] = b;
            }
        }
        return out;
    }

    private static String toStrictFileUri(Path path) {
        String pathStr = path.toString();
        if (File.separatorChar == '\\') {
            pathStr = pathStr.replace('\\', '/');
        }
        if (!pathStr.startsWith("/")) {
            pathStr = "/" + pathStr;
        }
        return "file://" + encodePath(pathStr);
    }

    private static String encodePath(String path) {
        byte[] bytes = path.getBytes(StandardCharsets.UTF_8);
        StringBuilder sb = new StringBuilder(bytes.length + 16);
        for (byte b : bytes) {
            int unsigned = b & 0xFF;
            char c = (char) unsigned;
            if (isUnreserved(c) || c == '/') {
                sb.append(c);
            } else {
                sb.append('%');
                sb.append(Character.toUpperCase(Character.forDigit((unsigned >> 4) & 0xF, 16)));
                sb.append(Character.toUpperCase(Character.forDigit(unsigned & 0xF, 16)));
            }
        }
        return sb.toString();
    }

    private static boolean isUnreserved(char c) {
        return (c >= 'A' && c <= 'Z')
                || (c >= 'a' && c <= 'z')
                || (c >= '0' && c <= '9')
                || c == '-' || c == '.' || c == '_' || c == '~';
    }

    private static List<String> resolveLspCommand() {
        String commandLine = System.getenv("OPENGREP_LSP_CMD");
        if (commandLine != null && !commandLine.isBlank()) {
            List<String> command = splitCommand(commandLine);
            if (command.isEmpty()) {
                System.err.println("OPENGREP_LSP_CMD is empty.");
                System.exit(2);
            }
            return normalizeLspCommand(command);
        }

        Optional<Path> binary = findBinaryUpwards("bin/opengrep-cli");
        if (binary.isPresent()) {
            List<String> command = new ArrayList<>();
            command.add(binary.get().toString());
            command.add("lsp");
            return command;
        }

        System.err.println("OPENGREP_LSP_CMD is not set and bin/opengrep-cli was not found.");
        System.exit(2);
        return List.of();
    }

    private static List<String> normalizeLspCommand(List<String> command) {
        if (command.isEmpty()) {
            return command;
        }
        String first = command.get(0);
        Path firstPath = Paths.get(first);
        String baseName = firstPath.getFileName().toString();
        if (baseName.equals("opengrep") && command.contains("lsp")) {
            Path candidate = firstPath.getParent() == null
                    ? Paths.get("opengrep-cli")
                    : firstPath.getParent().resolve("opengrep-cli");
            if (Files.isExecutable(candidate)) {
                List<String> updated = new ArrayList<>(command);
                updated.set(0, candidate.toString());
                return updated;
            }
        }
        return command;
    }

    private static Optional<Path> findBinaryUpwards(String relativePath) {
        Path current = Paths.get("").toAbsolutePath();
        for (int i = 0; i < 6; i++) {
            Path candidate = current.resolve(relativePath);
            if (Files.isExecutable(candidate)) {
                return Optional.of(candidate);
            }
            Path parent = current.getParent();
            if (parent == null) {
                break;
            }
            current = parent;
        }
        return Optional.empty();
    }

    private static void closeQuietly(Closeable closeable) {
        if (closeable == null) {
            return;
        }
        try {
            closeable.close();
        } catch (IOException ignored) {
        }
    }

    private static final class DiagnosticsGate {
        private final boolean versionMatch;
        private final ConcurrentHashMap<String, PendingDiagnostic> pendingByUri = new ConcurrentHashMap<>();

        DiagnosticsGate(boolean versionMatch) {
            this.versionMatch = versionMatch;
        }

        CompletableFuture<DiagnosticOutcome> register(String uri, int expectedVersion) {
            CompletableFuture<DiagnosticOutcome> future = new CompletableFuture<>();
            pendingByUri.put(uri, new PendingDiagnostic(expectedVersion, future));
            return future;
        }

        void resolve(PublishDiagnosticsParams params) {
            PendingDiagnostic pending = pendingByUri.get(params.getUri());
            if (pending == null || pending.future.isDone()) {
                return;
            }
            if (!versionMatch) {
                pending.future.complete(DiagnosticOutcome.ok(params));
                return;
            }
            Integer actual = params.getVersion();
            if (actual != null && actual == pending.expectedVersion) {
                pending.future.complete(DiagnosticOutcome.ok(params));
            } else {
                pending.future.complete(DiagnosticOutcome.versionMismatch(params, pending.expectedVersion, actual));
            }
        }

        void unregister(String uri) {
            pendingByUri.remove(uri);
        }
    }

    private static final class Stats {
        private final LongAdder totalDiagnostics = new LongAdder();
        private final LongAdder warningDiagnostics = new LongAdder();
        private final LongAdder errorDiagnostics = new LongAdder();
        private final LongAdder telemetryWarnings = new LongAdder();
        private final LongAdder telemetryErrors = new LongAdder();
        private final LongAdder processingNanos = new LongAdder();
        private final LongAdder failures = new LongAdder();
        private final LongAdder timeoutFailures = new LongAdder();
        private final LongAdder versionMismatchFailures = new LongAdder();
        private final LongAdder totalFiles = new LongAdder();
        private final LongAdder successes = new LongAdder();
        private final Queue<String> failuresList = new ConcurrentLinkedQueue<>();
        private final ConcurrentHashMap<String, DiagnosticSummary> diagnosticsByUri = new ConcurrentHashMap<>();
        private final ConcurrentHashMap.KeySetView<String, Boolean> errorDiagnosticUris = ConcurrentHashMap.newKeySet();
        private final ConcurrentHashMap.KeySetView<String, Boolean> warningDiagnosticUris = ConcurrentHashMap.newKeySet();
        private final ConcurrentHashMap.KeySetView<String, Boolean> timeoutUris = ConcurrentHashMap.newKeySet();
        private final ConcurrentHashMap.KeySetView<String, Boolean> versionMismatchUris = ConcurrentHashMap.newKeySet();

        void addDiagnostics(String uri, List<Diagnostic> diagnostics) {
            totalDiagnostics.add(diagnostics.size());
            int errorCount = 0;
            int warningCount = 0;
            for (Diagnostic diagnostic : diagnostics) {
                DiagnosticSeverity severity = diagnostic.getSeverity();
                if (severity == DiagnosticSeverity.Error) {
                    errorDiagnostics.increment();
                    errorCount++;
                } else if (severity == DiagnosticSeverity.Warning) {
                    warningDiagnostics.increment();
                    warningCount++;
                }
            }
            if (errorCount > 0) {
                errorDiagnosticUris.add(uri);
            }
            if (warningCount > 0) {
                warningDiagnosticUris.add(uri);
            }
            DiagnosticSummary summary = diagnosticsByUri.computeIfAbsent(uri, key -> new DiagnosticSummary());
            summary.total.add(diagnostics.size());
            summary.errors.add(errorCount);
            summary.warnings.add(warningCount);
        }

        void addProcessingNanos(long nanos) {
            processingNanos.add(nanos);
        }

        void failTimeout(String uri) {
            timeoutFailures.increment();
            failuresList.add("timeout: " + uri);
            timeoutUris.add(uri);
        }

        void failVersionMismatch(String uri, int expected, Integer actual) {
            versionMismatchFailures.increment();
            failuresList.add("version-miss: " + uri + " expected=" + expected + " actual=" + (actual == null ? "null" : actual));
            versionMismatchUris.add(uri);
        }

        void failException(String uri, Exception ex) {
            failuresList.add("error: " + uri + " - " + ex.getClass().getSimpleName() + ": " + ex.getMessage());
        }

        Queue<String> failures() {
            return failuresList;
        }

        Iterable<String> errorDiagnosticUris() {
            return errorDiagnosticUris;
        }

        Iterable<String> warningDiagnosticUris() {
            return warningDiagnosticUris;
        }

        Iterable<String> problemUris() {
            var combined = new ArrayList<String>();
            combined.addAll(timeoutUris);
            combined.addAll(versionMismatchUris);
            combined.addAll(errorDiagnosticUris);
            Collections.sort(combined);
            return combined;
        }

        Map<String, DiagnosticSummary> diagnosticsSummary() {
            return diagnosticsByUri;
        }
    }

    private static final class OpenGrepClient implements LanguageClient {
        private final Stats stats;
        private final DiagnosticsGate gate;
        private final LineLogger telemetryLogger;
        private volatile LanguageServer server;
        private final CountDownLatch rulesRefreshed = new CountDownLatch(1);

        OpenGrepClient(Stats stats, DiagnosticsGate gate, LineLogger telemetryLogger) {
            this.stats = stats;
            this.gate = gate;
            this.telemetryLogger = telemetryLogger;
        }

        void attachServer(LanguageServer server) {
            this.server = server;
        }

        @JsonNotification("semgrep/rulesRefreshed")
        public void rulesRefreshed() {
            rulesRefreshed.countDown();
        }

        @Override
        public void telemetryEvent(Object object) {
            String payload = toJson(object);
            Optional<MessageType> level = extractMessageType(object, payload);
            if (level.isPresent() && (level.get() == MessageType.Error || level.get() == MessageType.Warning)) {
                if (level.get() == MessageType.Error) {
                    stats.telemetryErrors.increment();
                } else {
                    stats.telemetryWarnings.increment();
                }
                telemetryLogger.log("telemetry-" + level.get().name().toLowerCase(Locale.ROOT) + ": " + payload);
            }
        }

        @Override
        public void publishDiagnostics(PublishDiagnosticsParams diagnostics) {
            gate.resolve(diagnostics);
        }

        @Override
        public CompletableFuture<Void> createProgress(WorkDoneProgressCreateParams params) {
            return CompletableFuture.completedFuture(null);
        }

        @Override
        public void notifyProgress(ProgressParams params) {
            // No-op: we only need to keep the server happy.
        }

        @Override
        public void showMessage(MessageParams messageParams) {
            logMessageType("show", messageParams);
        }

        @Override
        public CompletableFuture<MessageActionItem> showMessageRequest(ShowMessageRequestParams requestParams) {
            String message = requestParams.getMessage();
            telemetryLogger.log("show-request: " + message);
            return CompletableFuture.completedFuture(null);
        }

        @Override
        public void logMessage(MessageParams message) {
            logMessageType("log", message);
        }

        @Override
        public CompletableFuture<ApplyWorkspaceEditResponse> applyEdit(ApplyWorkspaceEditParams params) {
            return CompletableFuture.completedFuture(new ApplyWorkspaceEditResponse(false));
        }

        private void logMessageType(String source, MessageParams message) {
            MessageType type = message.getType();
            if (type == MessageType.Error) {
                stats.telemetryErrors.increment();
            } else if (type == MessageType.Warning) {
                stats.telemetryWarnings.increment();
            }
            if (type == MessageType.Error || type == MessageType.Warning) {
                telemetryLogger.log(source + "-" + type.name().toLowerCase(Locale.ROOT) + ": " + message.getMessage());
            }
        }

        private Optional<MessageType> extractMessageType(Object object, String payload) {
            if (object instanceof JsonObject) {
                JsonObject json = (JsonObject) object;
                MessageType type = messageTypeFromJson(json);
                if (type != null) {
                    return Optional.of(type);
                }
            }
            if (object instanceof Map) {
                Map<?, ?> map = (Map<?, ?>) object;
                MessageType type = messageTypeFromMap(map);
                if (type != null) {
                    return Optional.of(type);
                }
            }
            String lower = payload.toLowerCase(Locale.ROOT);
            if (lower.contains("\"level\":\"error\"") || lower.contains("\"severity\":\"error\"") || lower.contains("\"type\":\"error\"")) {
                return Optional.of(MessageType.Error);
            }
            if (lower.contains("\"level\":\"warn\"") || lower.contains("\"level\":\"warning\"")
                    || lower.contains("\"severity\":\"warn\"") || lower.contains("\"severity\":\"warning\"")) {
                return Optional.of(MessageType.Warning);
            }
            return Optional.empty();
        }

        private MessageType messageTypeFromJson(JsonObject json) {
            if (json.has("level")) {
                MessageType type = parseMessageType(json.get("level"));
                if (type != null) {
                    return type;
                }
            }
            if (json.has("severity")) {
                MessageType type = parseMessageType(json.get("severity"));
                if (type != null) {
                    return type;
                }
            }
            if (json.has("type")) {
                MessageType type = parseMessageType(json.get("type"));
                if (type != null) {
                    return type;
                }
            }
            return null;
        }

        private MessageType messageTypeFromMap(Map<?, ?> map) {
            Object level = map.get("level");
            MessageType type = parseMessageType(level);
            if (type != null) {
                return type;
            }
            Object severity = map.get("severity");
            type = parseMessageType(severity);
            if (type != null) {
                return type;
            }
            Object mapType = map.get("type");
            return parseMessageType(mapType);
        }

        private MessageType parseMessageType(Object value) {
            if (value == null) {
                return null;
            }
            if (value instanceof Number) {
                int code = ((Number) value).intValue();
                return messageTypeFromCode(code);
            }
            if (value instanceof JsonElement) {
                JsonElement element = (JsonElement) value;
                if (element.isJsonPrimitive()) {
                    if (element.getAsJsonPrimitive().isNumber()) {
                        return messageTypeFromCode(element.getAsInt());
                    }
                    if (element.getAsJsonPrimitive().isString()) {
                        return messageTypeFromString(element.getAsString());
                    }
                }
            }
            if (value instanceof String) {
                return messageTypeFromString((String) value);
            }
            return null;
        }

        private MessageType messageTypeFromCode(int code) {
            if (code == 1) {
                return MessageType.Error;
            }
            if (code == 2) {
                return MessageType.Warning;
            }
            if (code == 3) {
                return MessageType.Info;
            }
            if (code == 4) {
                return MessageType.Log;
            }
            return null;
        }

        private MessageType messageTypeFromString(String value) {
            String normalized = value.trim().toLowerCase(Locale.ROOT);
            if (normalized.equals("error") || normalized.equals("err")) {
                return MessageType.Error;
            }
            if (normalized.equals("warn") || normalized.equals("warning")) {
                return MessageType.Warning;
            }
            if (normalized.equals("info") || normalized.equals("information")) {
                return MessageType.Info;
            }
            if (normalized.equals("log")) {
                return MessageType.Log;
            }
            return null;
        }

        private String toJson(Object object) {
            if (object == null) {
                return "null";
            }
            try {
                return GSON.toJson(object);
            } catch (Exception ex) {
                return Objects.toString(object);
            }
        }
    }

    private static final class LineLogger implements Closeable {
        private final BufferedWriter writer;

        LineLogger(Path path) throws IOException {
            this.writer = Files.newBufferedWriter(path, StandardCharsets.UTF_8);
        }

        void log(String line) {
            synchronized (writer) {
                try {
                    writer.write(line);
                    writer.newLine();
                    writer.flush();
                } catch (IOException ignored) {
                }
            }
        }

        @Override
        public void close() {
            synchronized (writer) {
                try {
                    writer.flush();
                } catch (IOException ignored) {
                }
                try {
                    writer.close();
                } catch (IOException ignored) {
                }
            }
        }
    }

    private static final class DiagnosticSummary {
        private final LongAdder total = new LongAdder();
        private final LongAdder warnings = new LongAdder();
        private final LongAdder errors = new LongAdder();
    }

    private static LineLogger buildPerFileLogger(String suffix) throws IOException {
        String enable = System.getenv("PER_FILE_LOG");
        if (enable == null || enable.isBlank()) {
            return null;
        }
        return new LineLogger(Paths.get("per-file" + suffix + ".log").toAbsolutePath());
    }

    private static long safeFileSize(Path file) {
        try {
            return Files.size(file);
        } catch (IOException ignored) {
            return -1L;
        }
    }

    private static final class PendingDiagnostic {
        private final int expectedVersion;
        private final CompletableFuture<DiagnosticOutcome> future;

        private PendingDiagnostic(int expectedVersion, CompletableFuture<DiagnosticOutcome> future) {
            this.expectedVersion = expectedVersion;
            this.future = future;
        }
    }

    private static final class DiagnosticOutcome {
        private final PublishDiagnosticsParams params;
        private final OutcomeStatus status;
        private final Integer expectedVersion;
        private final Integer actualVersion;

        private DiagnosticOutcome(PublishDiagnosticsParams params, OutcomeStatus status, Integer expectedVersion, Integer actualVersion) {
            this.params = params;
            this.status = status;
            this.expectedVersion = expectedVersion;
            this.actualVersion = actualVersion;
        }

        private static DiagnosticOutcome ok(PublishDiagnosticsParams params) {
            return new DiagnosticOutcome(params, OutcomeStatus.OK, null, params.getVersion());
        }

        private static DiagnosticOutcome versionMismatch(PublishDiagnosticsParams params, int expected, Integer actual) {
            return new DiagnosticOutcome(params, OutcomeStatus.VERSION_MISMATCH, expected, actual);
        }
    }

    private enum OutcomeStatus {
        OK,
        VERSION_MISMATCH
    }
}
