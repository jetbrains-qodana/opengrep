/*
 * semgrep-lua
 *
 * Extend the standard Lua grammar (grammar/tree-sitter-lua) with the
 * constructs needed to parse semgrep patterns.
 */

const standard_grammar = require('tree-sitter-lua/grammar');

module.exports = grammar(standard_grammar, {
  name: 'lua',

  rules: {
    // Alternate entry point for expression patterns such as '$X + 1' or
    // '"foo"', which are not statements. The pattern parser adds the
    // '__SEMGREP_EXPRESSION' prefix when parsing the pattern as statements
    // fails.
    chunk: ($, previous) => choice(
      previous,
      $.semgrep_expression,
    ),
    semgrep_expression: $ => seq('__SEMGREP_EXPRESSION', $.expression),

    // '...' standing for a sequence of statements. The upstream grammar
    // has no expression statements (only calls), unlike the previous Lua
    // grammar where any expression was a statement.
    statement: ($, previous) => choice(
      previous,
      $.semgrep_ellipsis_statement,
    ),
    semgrep_ellipsis_statement: $ => $.vararg_expression,

    // '...' anywhere in a parameter list, e.g. 'function f(..., $X)'.
    // This also accepts parameters after the vararg in a program, which
    // Lua rejects; that's harmless as we don't need to reject invalid code.
    _parameter_list: $ => seq(
      $._parameter,
      repeat(seq(',', $._parameter)),
    ),
    // '<... e ...>'
    expression: ($, previous) => choice(
      previous,
      $.deep_ellipsis,
    ),
    deep_ellipsis: $ => seq('<...', $.expression, '...>'),

    _parameter: $ => choice(
      field('name', $.identifier),
      $._vararg_parameter,
    ),

    // Short strings are single tokens. Upstream, the content of a short
    // string is made of several tokens and the external scanner can
    // recognize a comment, which is an extra, between them: '"--[[x"'
    // started a block comment that hid the code up to the next ']]'.
    // As in Lua, a line break must be escaped ('\' or '\z'), which also
    // keeps the error recovery from making a string of several lines.
    _quote_string: _ => token(choice(
      seq('"', repeat(choice(/[^"\\\r\n]+/, /\\z\s*/, /\\(.|\r?\n)/)), '"'),
      seq("'", repeat(choice(/[^'\\\r\n]+/, /\\z\s*/, /\\(.|\r?\n)/)), "'"),
    )),

    // Metavariables. Upstream identifiers already accept '$' ('$X'), we add
    // metavariable ellipses ('$...ARGS'). 'identifier' is the word token
    // (used for keyword extraction), so it must remain a single token: the
    // first alternative is the upstream definition.
    identifier: _ => token(choice(
      seq(
        /[^\p{Control}\s+\-*/%^#&~|<>=(){}\[\];:,.\\'"\d]/,
        /[^\p{Control}\s+\-*/%^#&~|<>=(){}\[\];:,.\\'"]*/,
      ),
      /\$\.\.\.[A-Z_][A-Z_0-9]*/,
    )),
  }
});
