options indenting = 4
options no_unused_block_arguments = false
options no_unused_function_arguments = false
options no_aot = true
options strict_smart_pointers = true

module parser_generator

require daslib/match public
require daslib/ast
require daslib/fio
require daslib/rtti
require daslib/ast_boost
require daslib/templates_boost

//options log_infer_passes

require meta_ast


struct ParserGenerator
    // Provides information to generate unique names
    rule_counter: table<string; int>

    // Maps rule name into its declared type (typeof(add))
    // Action block returns the same result as in the mapping
    rule_types: table<string; TypeDeclPtr>

    // E.g. add => ResultAdd aka tuple<bool;typeof(add);int>
    return_types: table<string; TypeDeclPtr>

    // Currently generating code for 'rule'
    // For use in actions.
    current_rule: string


def abort(message: string)
    print("{message}\n")
    unsafe { exit(1); }
      

def iota
    return <- generator<int>() <| 
        var i: int = 0
        while true
            i += 1
            return false if i == 0 else yield i


def rule_left_recursive(name: string; rule: Rule): bool
    match rule
        if [[Rule alt = $v(alts)]] 
            for a in alts
                return true if rule_left_recursive(name, a.rule.rule)
        
        if [[Rule seq = $v(seq)]]
            return true if rule_left_recursive(name, seq[0].rule)
                    
        if [[Rule maybe_repeat = $v(rule_)]]
            return true if rule_left_recursive(name, rule_.rule)
            
        if [[Rule nonterminal = $v(name_)]]
            return true if name == name_

        if [[Rule bound_nonterminal = $v(tup)]]
            return true if tup._0 == name

        if [[Rule terminal = _]]
            return false

        if _ 
            abort("TODO: rule_left_recursive")

    return false




def generate_wrapper(var gen: ParserGenerator)
    let rule_name = gen.current_rule
    let cache_table = "{rule_name}_cache"
    let inner_parsing_fun = "parse_{rule_name}_inner"
    let result_type = "Result{gen.result_types[rule.name]}"
    
    var inscope wrapper_fun <- qmacro_function("parse_{rule_name}") <| $ (var parser: Parser): $t(result_type)
        var mark = parser |> mark
        
        with parser 
            if $i(cache_table) |> key_exists(index)
                var result := $i(cache_table)[index]
                parser |> reset(result.endpos)
                return <- result

            var result = $c(inner_parsing_fun)(parser)
            $i(cache_table)[mark] := result

            return <- result

    return <- wrapper_fun    


def generate_wrapper_leftrec(var gen: ParserGenerator)
    let rule_name = gen.current_rule
    let cache_table = "{rule_name}_cache"
    let inner_parsing_fun = "parse_{rule_name}_inner"
    let result_type = "Result{gen.result_types[rule.name]}"

    var inscope wrapper_fun <- qmacro_function("parse_{rule_name}") <| $ (var parser: Parser): $t(result_type)
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
                print("{$v(inner_parsing_fun)}: {newres}\n")
                var endpos = parser |> mark

                // Break if no movement
                break if res.endpos >= endpos

                res = newres
                $i(cache_table)[mark] = res
            

            parser |> reset(res.endpos)
            return res

    return <- wrapper_fun


def generate_parser_class(var gen: ParserGenerator; parser_name: string): StructurePtr
    var inscope s: StructurePtr

    // ParserCalculator, ParserJson, ParserDas
    s.name := "Parser{parser_name}"
    s._module = compiling_module()

    for rule_name, type_ in keys(gen.rule_types), values(gen.rule_types)
        s |> add_structure_field(rule_name, type_)

    return <- s


def add_structure_field( var struct_:StructurePtr; name:string; var t:TypeDeclPtr)
    //! Adds a field to the structure.
    
    let fi = length(struct_.fields)
    struct_.fields |> resize(fi + 1)
    
    struct_.fields[fi].name := name
    struct_.fields[fi]._type |> move <| t


def make_default_type_decl: TypeDeclPtr
    //! Make default result (field value to be replaced later)
    return <- typeinfo(ast_typedecl type<tuple<success:bool; value:int; endpos:int>>)


def generate_result_types(var gen: ParserGenerator; name: string)
    //! Result types are used internally in the compiler to store in the cache.
    //!
    //! Manually they would be defined like so:
    //!     typedef Result1 = tuple<success:bool; value:int; endpos:int>;
    //! for every possible rule type
    //!

    for rule, type_ in keys(gen.rule_types), values(gen.rule_types)
        if true
            var inscope rule_res_type <- make_default_type_decl()
            rule_res_type.argTypes[1] := type_ // <--- emplace `value` field in a tuple

            // Map name of the rule to its 'result' type
            gen.return_types[rule] |> move <| rule_res_type


def generate(var gen: ParserGenerator; var def_: Definition)
    gen.current_rule = def_.name

    let result_type = "Result{def_.name}"
    var inscope function_body <- gen |> generate(def_.rule)
    
    var inscope fun <- qmacro_function("parse_{def_.name}_inner") <| $ (var parser: Parser): $t(result_type)
        $b(function_body)
        return <- [[$t(result_type)]]

    compiling_module() |> add_function(fun)

    if rule_left_recursive(def_.name, def_.rule)
        var inscope wrapper <- gen |> generate_wrapper_leftrec
        compiling_module() |> add_function(wrapper)
    else
        var inscope wrapper <- gen |> generate_wrapper
        compiling_module() |> add_function(wrapper)
    

def get_rule_type(var gen: ParserGenerator; rule: Rule): TypeDeclPtr
    match rule
        if [[Rule nonterminal = $v(nonterm)]]
            var inscope type_ := gen.rule_types[nonterm]
            return <- type_

        if [[Rule subrule = $v(subrule)]]
            abort("Infer subrule type")

    return <- [[TypeDeclPtr]]


def set_rule_type(var gen: ParserGenerator; rule_name: string; type_: TypeDeclPtr)
    gen.rule_types[rule_name] |> move_new(type_ |> clone_type)


def get_action_block(var alternative: Alternative): ExpressionPtr
    return <- alternative.action


def flatten_block(var blk: ExprBlock?)
    var inscope result : array<ExpressionPtr>
    for e in blk.list
        result |> emplace <| e
    return <- result


def generate(var gen: ParserGenerator; var rule_: Rule): array<ExpressionPtr>
    match rule_
        if [[Rule terminal = $v(term)]]
            return <- flatten_block(generate(term) as ExprBlock)

        if [[Rule nonterminal = $v(nonterm)]]
            let name = nonterm
            let count = "{gen.rule_counter[name]}"
            let return_type := gen.result_types[name]

            // varname is a generic name for unnamed nonterminals
            // e.g. add4, add5, mul0
            let varname = "{name}{count}"
            let parse_fun = "parse_{name}"

            var inscope t  <- qmacro_block <|
                let $i(varname) <- $c(parse_fun)(parser)
                return [[$t(return_type)]] if !$i(varname).success

            return <- flatten_block(t as ExprBlock)
            

        if [[Rule bound_nonterminal = $v(nonterm)]]
            let name = nonterm._0
            let result_name = nonterm._1
            let count = "{gen.rule_counter[name]}"
            
            let return_type := gen.result_types[gen.current_rule]

            // varname is a generic name for unnamed nonterminals
            // e.g. add4, add5, mul0
            let varname = "{name}{count}"
            let parse_fun = "parse_{name}"

            var inscope t <- qmacro_block <|
                var $i(varname) <- $c(parse_fun)(parser)
                return [[$t(return_type)]] if !$i(varname).success
                var $i(result_name) <- $i(varname).value

            return <- flatten_block(t as ExprBlock)
            
        if [[Rule seq = $v(rules)]]

            var inscope result: array<ExpressionPtr>

            for rule in rules
                for expr in gen |> generate(rule.rule)
                    result |> emplace <| expr
            
            return <- result

        if [[Rule alt = $v(alts)]] 

            var inscope results: array<ExpressionPtr>

            for alt, i in alts, iota()

                if true 

                    let block_name = "blk{i}"
                    let parse_pos = "pos{i}"
                    let result  = "res{i}"

                    var inscope action <- alt.action
                    var inscope action_block <- action as ExprBlock |> flatten_block
                    var inscope block_contents <- gen |> generate(alt.rule.rule)

                    var inscope action_type := gen.rule_types[current_rule]
                    let return_type := gen.result_types[gen.current_rule]

                    var inscope t <- qmacro_block <|
                        let val <- invoke <| $(): $t(action_type)
                            $b(action_block)

                        // For some reason I cannot directly return [[Result]]
                        var result = [[$t(return_type) true, val, parser |> mark]]
                        return result

                    for e in flatten_block(t as ExprBlock)
                        block_contents |> emplace <| e
                    
                    // Add next alternative

                    var inscope alternative_code <- qmacro_block <|
                        var parse_pos <- parser |> mark

                        var $i(block_name) <- $ <|
                            $b(block_contents)

                        var $i(result) <- $i(block_name) |> invoke()

                        return $i(result) if $i(result).success

                        parser |> reset(parse_pos)
                    
                    results |> emplace <| alternative_code 
            
            return <- results

        if [[Rule maybe_repeat = $v(rule_)]]

            var result_handle = rule_.name
            var inscope rule_code <- gen |> generate(rule_.rule)
            var inscope type_decl <- gen |> get_rule_type(rule_.rule)

            if rule_.name == "" // Rule not bound by name, discard result

                var inscope t <- qmacro_block <|
                    while true
                        var res <- invoke($b(rule_code))
                        break if !res.success
                return <- flatten_block(t as ExprBlock)

	        // Bind result to the provided name 

            var inscope t <- qmacro_block <|
                var $i(result_handle) : array<$t(type_decl)>
                while true
                    var res <- invoke($b(rule_code))
                    break if !res.success
                    $i(result_handle) |> emplace <| res.value

            return <- flatten_block(t as ExprBlock)

        if [[Rule repeat = $v(rule_)]]

            var inscope rule_code <- gen |> generate(rule_.rule)

            var inscope t <- qmacro_block <|
            
                var res <- invoke <| $
                    $b(rule_code)
                return if !res.success

                while true
                    var res <- invoke <| $
                        $b(rule_code)
                    break if !res.success

            return <- flatten_block(t as ExprBlock)

        if [[Rule subrule = $v(subrule)]]
            // Create a new unnamed subrule for the (...)
            
            var inscope rule_code <- gen |> generate(subrule.rule)

            var inscope t <- qmacro_block <|
                var result <- invoke <| 
                    $b(rule_code)
                return [[Result]] if !result.success

            return <- flatten_block(t as ExprBlock)

    abort("Unreachable")
    return <- [[array<ExpressionPtr>]]


def generate(terminal: Terminal): ExpressionPtr
    match terminal 
        if [[Terminal lit = $v(l) ]]

            return <- qmacro_block <|
                return [[Result]] if !parser |> matches($v(l))

        if [[Terminal range_ = $v(r) ]]
            
            return <- qmacro_block <|
                return [[Result]] if !parser |> in_range($v(r))

        if [[Terminal number = $v(handle)]]
             
            return <- qmacro_block <|
                var res : Result = parser |> lex_number
                return [[Result]] if !res.success 
                var $i(handle) = res.value

        if [[Terminal whitespace = _]]

            return <- qmacro_block <|
                parser |> skip_whitespace
                
        if [[Terminal string_ = $v(handle)]]

            return <- qmacro_block <|
                var res <- parser |> match_string_literal
                return [[Result]] if !res.success 
                var $i(handle) = res._1

        if [[Terminal double_ = $v(handle)]]

            return <- qmacro_block <|
                var res <- parser |> match_double_literal
                return [[Result]] if !res.success 
                var $i(handle) = res._1



    abort("TODO")
    return <- [[ExpressionPtr]]

