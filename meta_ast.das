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


variant Rule
    seq: array<Rule_?>   // R1 R2 R3
    alt: array<Alternative>   // R1 | R2 | R3
    subrule: Rule_?      // (R1)
    
    maybe_repeat: Rule_? // Rule*
    repeat: Rule_?       // Rule+

    option: Rule_?       // Rule?

    terminal: Terminal 
    nonterminal: Nonterminal
    bound_nonterminal: tuple<Nonterminal; string>

    // Do not consume the input
    not_rule: Rule_?     // !Rule
    and_rule: Rule_?     // &Rule


struct Rule_
    name: string // Bound name for ? * + ()
    rule: Rule


// E.g. rule_name: rule
struct Definition
    name: string

    type_: TypeDeclPtr

    rule: Rule
    

def abort(message: string)
    print("{message}\n")
    unsafe { exit(1); }
      

def iota
    return <- generator<int>() <| 
        var i: int = 0
        while true
            i += 1
            return false if i == 0 else yield i



typedef Result = tuple<success:bool; value:int; endpos:int>;


struct ParserGenerator
    // Provides information to generate unique names
    rule_counter: table<string; int>

    // Maps rule name into its declared type
    rule_types: table<string; TypeDeclPtr>

    // Currently generating code for 'rule'
    // For use in actions.
    current_rule: string


variant Terminal
    // Literal: "Hello world"
    lit: string 

    // Range: [0-9]
    range_: tuple<int; int> 

    // Name of the result (int)
    number: string

    // WS <- [' ', '\n', '\t']*
    whitespace: void? 


typedef Nonterminal = string;


// [_macro]
// def setup
//     if is_compiling_macros_in_module("meta_ast")
//         pass
//         // Add generated parser to the compiled code

struct Alternative
    rule: Rule_?
    action: ExpressionPtr
