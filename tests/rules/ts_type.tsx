export const ErrorDetails = (
  { error }: { error: TestError }
) => (
  {
    // ruleid: test
    __html: formatDiffMessage(error),
  }
);
