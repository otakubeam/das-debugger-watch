options indenting = 4
options no_unused_block_arguments = false
options no_unused_function_arguments = false
options no_aot = true
options strict_smart_pointers = true

module meta_ast

require daslib/match public
require daslib/fio
require daslib/rtti
require daslib/ast
require daslib/ast_boost
require daslib/templates_boost


// Note on Rule vs. Rule_:
//  Since variant (like tuple) is a weak type
//  you cannot reference variant Rule directly in Rule
//  definition. Thus Rule_ wraps it in a struct, which is strong  


variant Terminal
    // Literal: "Hello world"
    lit: string 

    // Range: [0-9]
    range_: tuple<int; int> 

    // Name of the result (int)
    number: string

    // WS <- [' ', '\n', '\t']*
    whitespace: void?

    // End of file (!.)
    EOF: void? 

    // "..." as <name>
    string_: string

    // double_ as <name>
    double_: string
    

typedef Nonterminal = string;


variant Rule
    seq: array<Rule_?>        // rule(R1, R2, R3)
    alt: array<Alternative>   // seq <| {...} ; seq <| {...}; etc
    subrule: Rule_?           // (R1)
    
    maybe_repeat: Rule_?      // * Rule
    repeat: Rule_?            // + Rule

    option: Rule_?            // MB(Rule)

    terminal: Terminal 
    nonterminal: Nonterminal
    bound_nonterminal: tuple<Nonterminal; string>

    // Do not consume the input
    not_rule: Rule_?     // !Rule
    and_rule: Rule_?     // PEEK(Rule)


struct Rule_
    name: string // Bound name for ? * + ()
    rule: Rule


// E.g. var <name>: <type_>
struct Definition
    name: string

    type_: TypeDeclPtr

    rule: Rule


struct Alternative
    rule: Rule_?
    action: ExpressionPtr