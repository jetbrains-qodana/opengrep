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
  }
});
