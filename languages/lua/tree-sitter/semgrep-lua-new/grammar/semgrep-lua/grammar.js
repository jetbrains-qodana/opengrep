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
    // '...' standing for a sequence of statements. The upstream grammar
    // has no expression statements (only calls), unlike the previous Lua
    // grammar where any expression was a statement.
    statement: ($, previous) => choice(
      previous,
      $.semgrep_ellipsis_statement,
    ),
    semgrep_ellipsis_statement: $ => $.vararg_expression,
  }
});
