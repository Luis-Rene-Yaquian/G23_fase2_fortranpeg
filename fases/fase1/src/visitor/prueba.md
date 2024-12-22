// Simple Arithmetics Grammar
// ==========================
//
// Accepts expressions like "2 * (3 + 4)" and computes their value.

Expression
  = head:Term tail:(_ ("+" / "-") _ Term)*

Term
  = head:Factor tail:(_ ("*" / "/") _ Factor)*

Factor
  = "(" _ expr:Expression _ ")"
  / _ Integer

Integer "integer"
  = [0-9]+

_ "whitespace"
  = [ \t\n\r]*



  