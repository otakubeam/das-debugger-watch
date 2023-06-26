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
    alt: array<Rule_?>   // R1 | R2 | R3
    
    maybe_repeat: Rule_? // Rule*
    repeat: Rule_?       // Rule+

    option: Rule_?       // Rule?

    terminal: Terminal 
    nonterminal: Nonterminal

    // Do not consume the input
    not_rule: Rule_?     // !Rule
    and_rule: Rule_?     // &Rule


struct Rule_
    rule: Rule


// E.g. rule_name: rule
struct Definition
    name: string
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


struct ParserGenerator
    // Table of ASTs for every Definition name
    output: table<string; array<ExpressionPtr>>

    // Provides information to generate unique names
    rule_counter: table<string; int>

    // Currently generating code for 'rule'
    // For use in actions.
    current_rule: string


def generate(var gen: ParserGenerator; def_: Definition)
    if gen.output |> key_exists(def_.name)
        abort("Duplicate rule definition")

    // gen.output[def_.name] <- array<ExpressionPtr> // Init empty (needed?)
    gen.current_rule = def_.name

    gen |> generate(def_.rule)

    var fun <- qmacro_function(def_.name) <| $ (var parser: Parser)
        $b(gen.output[gen.current_rule])

    fun |> describe |> print


def get_output(var gen: ParserGenerator): array<ExpressionPtr>&
    unsafe // Returning a ref to a member in the outer scope
        return gen.output[gen.current_rule]


def generate(var gen: ParserGenerator; rule_: Rule) 
    unsafe 
        var output& = gen |> get_output()

        match rule_

            if [[Rule nonterminal = $v(nonterm)]]
                let name = nonterm
                let count = gen.rule_counter[name] |> string()

                // varname is a generic name for unnamed nonterminals
                // e.g. add4, add5, mul0
                let varname = "{name}{count}"
                let parse_fun = "parse_{name}"

                output |> emplace_new <|  qmacro_expr(${
                    // Does not work: why?
                    // var $i(varname) <- parser |> $c(parse_fun)
                    let $i(varname) <- $c(parse_fun)(parser);
                })

                output |> emplace_new <|  qmacro_expr(${
                    return [[Result]] if !$i(varname).success;
                })
                
                
            if [[Rule seq = $v(rules)]]

                for rule, i in rules, iota()
                    gen |> generate(rule.rule)

            if [[Rule alt = $v(alts)]] 

                for alt, i in alts.alternatives, iota()
                    let block_name = "blk{i}"
                    let parse_pos = "pos{i}"
                    let result  = "res{i}"

                    output |> emplace_new <| qmacro() <| $
                        // "Alternative {i}: {rule}\n"
                                    
                        var parse_pos <- parser |> mark

                        var $i(block_name) <- $ <|
                            generate(*alt)       

                        var $i(result) <- $i(block_name) |> invoke()

                        return $i(result) if $i(result).success

                        parser |> reset(parse_pos)
                    
                    // Turn gen array of AST block pointers to AST block pointer

            if _
                abort("TODO: more rules")

        return



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


    var gen <- [[ParserGenerator]]

    gen |> generate([[Definition name = "add", 
        rule <- [[Rule nonterminal = "mul"]]]])

    for x,i,_ in  iota(), iota(), range(10)
        print("{x}, {i}\n")
