require daslib/match public
require daslib/fio
require daslib/rtti
require daslib/ast
require daslib/ast_boost
require daslib/templates_boost


// variant Rule
//     seq: array<Rule?>   // R1 R2 R3
//     alt: array<Rule?>   // R1 | R2 | R3
    
//     maybe_repeat: Rule? // Rule*
//     repeat: Rule?       // Rule+

//     option: Rule?       // Rule?

//     terminal: Terminal 
//     nonterminal: Nonterminal

//     // Do not consume the input
//     not: Rule?     // !Rule
//     and: Rule?     // &Rule



// // E.g. rule_name: rule
// struct Definition
//     name: string
//     rule: Rule

    

def abort(message: string)
    print("{message}\n")
    unsafe { exit(1); }
      

def iota
    return <- generator<int>() <| 
        var i: int = 0
        while true
            i += 1
            return false if i == 0 else yield i    


struct ParserGenerator
    // Table of ASTs for every Definition name
    output: table<string; array<ExpressionPtr>>

    // Provides information to generate unique names
    rule_counter: table<string; int>

    // Currently generating code for 'rule'
    current_rule: string





def generate(var gen: ParserGenerator; gram: Grammar)
    pass


def generate(var gen: ParserGenerator; def: Definition)
    if gen.gen |> key_exists(def.name)
        abort("Duplicate rule definition")

    gen.output[def.name] <- [{}] // Init empty (needed?)
    gen.current_rule = def.name

    gen |> generate(def.rule)


def generate(var gen: ParserGenerator; rule: Rule) 
    var output& = gen.output[current_rule]

    match rule

        if [[Rule nonterminal = $v(nonterm)]]
            let name = nonterm
            let count = rule_counter[name] |> string()

            output |> push <| qmacro_block() <|
                // $i(name)$i(count) is a generic name for unnamed nonterminals
                var $i(name)$i(count) <- parser |> parse_$i(name)
                return [[Result]] if !$i(name)$i(count).success

            
        if [[Rule seq = $v(rules)]]

            for rule, i in rules, iota()
                generate(output, rule?)

        if [[Rule alt = $v(alts)]]

            var gen: array<ExpressionPtr> 

            for alt, i in alts.alternatives, iota()
                var block_name = "blk" + string(i)
                var parse_pos = "pos" + string(i)
                var result = "res" + string(i)

                gen |> push <| qmacro_block() <|
                    // "Alternative {i}: {rule}\n"
                                
                    parser |> reset(mark)
                    var parse_pos <- parser |> mark

                    var $i(block_name) <- $ <|
                        generate(alt?)         // Here I should expand the result somehow
                    
                    var $i(result) <- $i(block_name) |> invoke()

                    return $i(result) if $i(result).success

                    parser |> reset(parse_pos)
                
                // Turn gen array of AST block pointers to AST block pointer

        if _
            abort("TODO: more rules")


variant Terminal
    // Literal: "Hello world"
    lit: string 

    // Range: [0-9]
    range_: tuple<int; int> 



typedef Nonterminal = string;



def generate(var terminal: Terminal): ExpressionPtr

    var input: string

    match terminal 
        if [[Terminal lit = $v(l) ]]

            var q <- qmacro_block() <| $
                return [[Result]] if !parser |> matches($v(l))

            return q

        if [[Terminal range_ = $v(r) ]]
            abort("TODO: ranges")

        if _
            abort("")

    return [[ExpressionPtr]]



[export]
def main
    print("\n")
    
    generate([[Terminal lit = "123"]]) |> describe() |> print()
    
    for x,i,_ in  iota(), iota(), range(10)
        print("{x}, {i}\n")
