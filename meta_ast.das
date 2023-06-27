
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

typedef Result = tuple<success:bool; value:int; endpos:int>;

struct ParserGenerator
    // Table of ASTs for every Definition name
    output: table<string; array<ExpressionPtr>>

    // Provides information to generate unique names
    rule_counter: table<string; int>

    // Currently generating code for 'rule'
    // For use in actions.
    current_rule: string

// [_macro]
// def setup
//     if is_compiling_macros_in_module("meta_ast")
//         pass
//         // Add generated parser to the compiled code

def rule_left_recursive(name: string; rule: Rule): bool
    match rule
        if [[Rule alt = $v(alts)]] 
            for a in alts
                return true if rule_left_recursive(name, a.rule)
        
        if [[Rule seq = $v(seq)]]
            var first = seq[0]
            match first.rule 
                if [[Rule nonterminal = $v(name_)]]
                    return name_ == name 
        if _
            abort("TODO") // More generic algorithm

    return false

def generate_wrapper(var gen: ParserGenerator)
    let rule_name = gen.current_rule
    let cache_table = "{rule_name}_cache"
    let inner_parsing_fun = "parse_{rule_name}_inner"

    var t <- qmacro_block <|
        var mark = parser |> mark
        
        with parser 
            if $i(cache_table) |> key_exists(index)
                var result = $i(cache_table)[index]

                parser |> reset(result.endpos)
                return result

            var result = $c(inner_parsing_fun)(parser)
            // var endpos = parser |> mark
            $i(cache_table)[mark] = result

            return result
    
    var wrapper_fun <- qmacro_function("parse_{rule_name}") <| $ (var parser: Parser): Result
        $b([{auto[] t}])

    return wrapper_fun    

def generate_wrapper_leftrec(var gen: ParserGenerator)
    let rule_name = gen.current_rule
    let cache_table = "{rule_name}_cache"
    let inner_parsing_fun = "parse_{rule_name}_inner"

    

    var t <- qmacro_block <|
        var mark = parser |> mark
        
        with parser 
            if $i(cache_table) |> key_exists(index)
                var result = $i(cache_table)[index]

                parser |> reset(result.endpos)
                return result

            // Build new cache entry from scratch
            
            var res = [[Result]]
            $i(cache_table)[mark] = res

            while true
                parser |> reset(mark)

                var newres = $c(inner_parsing_fun)(parser)
                var endpos = parser |> mark

                // Break if no movement
                break if res.endpos >= endpos

                res = newres
                $i(cache_table)[mark] = res

            return res
    
    var wrapper_fun <- qmacro_function("parse_{rule_name}") <| $ (var parser: Parser): Result
        $b([{auto[] t}])

    return wrapper_fun

def generate(var gen: ParserGenerator; def_: Definition)
    if gen.output |> key_exists(def_.name)
        abort("Duplicate rule definition")

    // gen.output[def_.name] <- array<ExpressionPtr> // Init empty (needed?)
    gen.current_rule = def_.name

    gen |> generate(def_.rule)

    var fun <- qmacro_function("parse_{def_.name}_inner") <| $ (var parser: Parser)
        $b(gen.output[gen.current_rule])

    if rule_left_recursive(def_.name, def_.rule)
        gen |> generate_wrapper_leftrec |> describe |> print
    else
        gen |> generate_wrapper |> describe |> print

    fun |> describe |> print


def get_output(var gen: ParserGenerator): array<ExpressionPtr>&
    unsafe // Returning a ref to a member in the outer scope
        return gen.output[gen.current_rule]


def generate(var gen: ParserGenerator; rule_: Rule) 
    unsafe 
        var output& = gen |> get_output()

        match rule_
            if [[Rule terminal = $v(term)]]
                output |> emplace_new <| generate(term)


            if [[Rule nonterminal = $v(nonterm)]]
                let name = nonterm
                let count = gen.rule_counter[name] |> string()

                // varname is a generic name for unnamed nonterminals
                // e.g. add4, add5, mul0
                let varname = "{name}{count}"
                let parse_fun = "parse_{name}"

                var t <- qmacro <|
                    let $i(varname) <- $c(parse_fun)(parser)
                    return [[Result]] if !$i(varname).success

                output |> emplace_new <| t
                
            if [[Rule seq = $v(rules)]]

                for rule, i in rules, iota()
                    gen |> generate(rule.rule)

                var t <- qmacro <|
                    let val <- 42 // Perform action
                    return [[Result true, val, parser |> mark]]
                
                output |> emplace_new <| t


            if [[Rule alt = $v(alts)]] 

                var results: array<ExpressionPtr>

                for alt, i in alts, iota()
                    let block_name = "blk{i}"
                    let parse_pos = "pos{i}"
                    let result  = "res{i}"

                    gen |> generate(alt.rule)

                    var block_contents <- gen.output[gen.current_rule]
                    
                    var t <- qmacro <|
                        // "Alternative {i}: {rule}\n"
                        
                        var parse_pos <- parser |> mark

                        var $i(block_name) <- $ <|
                            $b(block_contents)

                        var $i(result) <- $i(block_name) |> invoke()

                        return $i(result) if $i(result).success

                        parser |> reset(parse_pos)
                    
                    results |> emplace_new <| t 
                
                gen.output[gen.current_rule] <- results
                
            if _
                abort("TODO: more rules")

        return



variant Terminal
    // Literal: "Hello world"
    lit: string 

    // Range: [0-9]
    range_: tuple<int; int> 



typedef Nonterminal = string;


def rule_append(var seq: Rule; var new_: Rule): Rule
    match seq
        if [[Rule seq = $v(rules)]]
            // Transform Rule into Rule_
            rules |> push (new [[Rule_ rule <- new_]])
            return <- [[Rule seq <- rules]]

        if [[Rule alt = $v(alts)]]
            alts |> push (new [[Rule_ rule <- new_]])
            return <- [[Rule alt <- alts]]

    abort("Unreachable")
    return <- seq


def generate(terminal: Terminal): ExpressionPtr

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


def make_primary_def(): Rule
    // Create first alternative: num

    var mul <- [[Rule nonterminal = "num"]]

    var empty_seq: array<Rule_?>
    var seq <- [[Rule seq <- empty_seq]]

    var n <- seq |> rule_append(mul)

    // Create second alternative: '(' add ')'

    var left <- [[Rule terminal <- [[Terminal lit = "("]]]]
    var add <- [[Rule nonterminal = "add"]]
    var right <- [[Rule terminal <- [[Terminal lit = ")"]]]]

    var empty_seq2: array<Rule_?>
    var seq2 <- [[Rule seq <- empty_seq2]]

    var n2 <- seq2 |> rule_append(left) |> rule_append(add) |> rule_append(right)

    // Assemble the rule

    var empty_alts: array<Rule_?>
    var alts <- [[Rule alt <- empty_alts]]

    alts <- alts |> rule_append(n) |> rule_append(n2)

    return <- alts


[export]
def main
    var gen <- [[ParserGenerator]]

    // Create first alternative: add '+' mul

    var mul <- [[Rule nonterminal = "mul"]]
    var plus <- [[Rule terminal <- [[Terminal lit = "+"]]]]
    var add <- [[Rule nonterminal = "add"]]

    var empty_seq: array<Rule_?>
    var seq <- [[Rule seq <- empty_seq]]

    var n <- seq |> rule_append(add) |> rule_append(plus) |> rule_append(mul)

    // Create second alternative: mul

    var simply_mul <- [[Rule nonterminal = "mul"]]

    var empty_seq2: array<Rule_?>
    var seq2 <- [[Rule seq <- empty_seq2]]

    var n2 <- seq2 |> rule_append(simply_mul)


    var empty_alts: array<Rule_?>
    var alts <- [[Rule alt <- empty_alts]]

    alts <- alts |> rule_append(n) |> rule_append(n2)

    print("Rule raw: {alts}\n")

    var alts2 <- make_primary_def()

    // gen |> generate([[Definition name = "add", rule <- alts ]])

    gen |> generate([[Definition name = "primary", rule <- alts2]])