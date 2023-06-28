module parser_generator

require daslib/match public
require daslib/fio
require daslib/rtti
require daslib/ast
require daslib/ast_boost
require daslib/templates_boost

require meta_ast
require gram


def rule_left_recursive(name: string; rule: Rule): bool
    match rule
        if [[Rule alt = $v(alts)]] 
            for a in alts
                return true if rule_left_recursive(name, a.rule.rule)
        
        if [[Rule seq = $v(seq)]]
            var first = seq[0]
            match first.rule 
                if [[Rule nonterminal = $v(name_)]]
                    return name_ == name 

                    
        if [[Rule maybe_repeat = $v(rule_)]]
            return true if rule_left_recursive(name, rule_.rule)
            
        if [[Rule nonterminal = $v(name_)]]
            return true if name == name_

        if _ 
            abort("TODO: rule_left_recursive")

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
    gen.current_rule = def_.name

    var coded_block <- gen |> generate(def_.rule)

    var fun <- qmacro_function("parse_{def_.name}_inner") <| $ (var parser: Parser): Result
        $b(coded_block)

    // var fun <- qmacro_function("parse_{def_.name}_inner", $b(coded_block))

    if rule_left_recursive(def_.name, def_.rule)
        gen |> generate_wrapper_leftrec |> describe |> print
    else
        gen |> generate_wrapper |> describe |> print

    fun |> describe |> print


def get_rule_type(var gen: ParserGenerator; name; rule: Rule): TypeDeclPtr
    match rule
        if [[Rule nonterminal = $v(nonterm)]]
            return <- gen.rule_types[nonterm]

        if [[Rule subrule = $v(subrule)]]
            abort("Infer subrule type")

    return [[TypeDeclPtr]]

def set_rule_type(var gen: ParserGenerator; rule_name: string; var type_: TypeDeclPtr)
    gen.rule_types[rule_name] <- type_


def get_action_block(var alternative: Alternative): ExpressionPtr
    return alternative.action


def generate(var gen: ParserGenerator; rule_: Rule): array<ExpressionPtr>

    match rule_
        if [[Rule terminal = $v(term)]]
            return <- [{auto[] generate(term)}]

        if [[Rule nonterminal = $v(nonterm)]]
            let name = nonterm
            let count = "{gen.rule_counter[name]}"

            // varname is a generic name for unnamed nonterminals
            // e.g. add4, add5, mul0
            let varname = "{name}{count}"
            let parse_fun = "parse_{name}"

            var t <- qmacro_block <|
                let $i(varname) <- $c(parse_fun)(parser)
                return [[Result]] if !$i(varname).success

            return <- [{auto[] t}]
            

        if [[Rule bound_nonterminal = $v(nonterm)]]
            let name = nonterm._0
            let result_name = nonterm._1
            let count = "{gen.rule_counter[name]}"

            // varname is a generic name for unnamed nonterminals
            // e.g. add4, add5, mul0
            let varname = "{name}{count}"
            let parse_fun = "parse_{name}"

            var t <- qmacro_block <|
                var $i(varname) <- $c(parse_fun)(parser)
                return [[Result]] if !$i(varname).success
                var $i(result_name) <- $i(varname).value

            return <- [{auto[] t}]
        if [[Rule seq = $v(rules)]]

            var result: array<ExpressionPtr>

            for rule, i in rules, iota()
                for expr in gen |> generate(rule.rule)
                    result |> emplace (expr)
            
            return <- result

        if [[Rule alt = $v(alts)]] 

            var results: array<ExpressionPtr>

            for alt, i in alts, iota()

                let block_name = "blk{i}"
                let parse_pos = "pos{i}"
                let result  = "res{i}"
                // XXX: local variable action can't init (move) from a constant value
                // var action <- alt.action
                // var action_block = [{auto action}]
                var action_block <- [{auto[] quote(${return 42;})}]

                var block_contents <- gen |> generate(alt.rule.rule)

                var t <- qmacro_block <|
                    let val <- invoke($b(action_block))
                    return [[Result true, val, parser |> mark]]

                block_contents |> emplace <| t
                
                // Add next alternative

                var alternative_code <- qmacro_block <|
                    var parse_pos <- parser |> mark

                    var $i(block_name) <- $ <|
                        $b(block_contents)

                    var $i(result) <- $i(block_name) |> invoke()

                    return $i(result) if $i(result).success

                    parser |> reset(parse_pos)
                
                results |> emplace_new <| alternative_code 
            
            return <- results

        if [[Rule maybe_repeat = $v(rule_)]]

            var rule_code <- gen |> generate(rule_.rule)
            
            var t : ExpressionPtr

            var type_decl: TypeDeclPtr <- gen |> get_rule_type(rule_.name, rule_.rule)

            // for c in rule_code
            //     print("{describe(c)}\n")

            if rule_.name == ""
                t := qmacro <|    
                    while true
                        var res <- invoke <| $
                            $b(rule_code)
                        break if !res.success
            else 
                t := qmacro_block <|
                    var vaules : array<$t(type_decl)>
                    while true
                        var res <- invoke <| $
                            $b(rule_code)
                        break if !res.success
                        values |> emplace <| res.value

            return <- [{auto[] t}]

        if [[Rule repeat = $v(rule_)]]

            var rule_code <- gen |> generate(rule_.rule)

            var t <- qmacro <|
            
                var res <- invoke <| $
                    $b(rule_code)
                return if !res.success

                while true
                    var res <- invoke <| $
                        $b(rule_code)
                    break if !res.success

            return <- [{auto[] t}]

        if [[Rule subrule = $v(subrule)]]
            // Create a new unnamed subrule for the (...)
            
            var rule_code <- gen |> generate(subrule.rule)

            var t <- qmacro <|
                var result <- invoke <| 
                    $b(rule_code)
                return [[Result]] if !result.success

            return <- [{auto[] t}]



    abort("Unreachable")
    return <- [[array<ExpressionPtr>]]
        




def generate(terminal: Terminal): ExpressionPtr

    var input: string

    match terminal 
        if [[Terminal lit = $v(l) ]]

            return qmacro <|
                return [[Result]] if !parser |> matches($v(l))


        if [[Terminal range_ = $v(r) ]]
            
            return qmacro <|
                return [[Result]] if !parser |> in_range($v(r))


    return [[ExpressionPtr]]


[export]
def main
    var gen <- [[ParserGenerator]]

    // var alts <- make_add_rule()
    // var alts2 <- make_primary_def()
    
    var alts3 <- make_named_repetitions()
    gen |> set_rule_type("mul", typeinfo(ast_typedecl type<int>))
    // gen |> generate([[Definition name = "add", rule <- alts ]])

    gen |> generate([[Definition name = "start", rule <- alts3]])