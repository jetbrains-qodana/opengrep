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
    _parameter: $ => choice(
      field('name', $.identifier),
      $._vararg_parameter,
    ),
  }
});
