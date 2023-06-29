module gram

require meta_ast
require daslib/ast_boost
require daslib/templates_boost

def rule_append(var seq: Rule; var new_: Rule): Rule
    var r = new [[Rule_ rule <- new_]]

    match seq
        if [[Rule seq = $v(rules)]]
            // Transform Rule into Rule_
            rules |> push (r)
            return <- [[Rule seq <- rules]]

        if [[Rule alt = $v(alts)]]
            alts |> emplace ([[Alternative rule <- r, action <- [[ExpressionPtr]]]])
            return <- [[Rule alt <- alts]]

    abort("Unreachable")
    return <- seq


def rule_append_with(var seq: Rule; var new_: Rule; var act: ExpressionPtr): Rule
    var r = new [[Rule_ rule <- new_]]

    match seq
        if [[Rule alt = $v(alts)]]
            alts |> emplace ([[Alternative rule <- r, action <- act]])
            return <- [[Rule alt <- alts]]

    abort("Unreachable")
    return <- seq


def make_primary_rule(): Rule
    
    var left <- [[Rule terminal <- [[Terminal lit = "("]]]]
    var right <- [[Rule terminal <- [[Terminal lit = ")"]]]]
    var add <- [[Rule bound_nonterminal = [[auto "mul", "m"]]]]
    var number <- [[Rule terminal = [[Terminal "NUMBER", "n"]]]]

    // Create first alternative: add '+' mul

    var seq1 <- [[Rule seq <- [[array<Rule_?>]]]]
    var a1 <- seq1 |> rule_append(left) |> rule_append(add) |> rule_append(right)

    // Create first alternative: NUMBER

    var seq2 <- [[Rule seq <- [[array<Rule_?>]]]]
    var a2 <- seq2 |> rule_append(number)

    // Create second alternative: NAME

    var seq3 <- [[Rule seq <- [[array<Rule_?>]]]]
    var a3 <- seq3 |> rule_append(unary)
    
    // Make alternatives and their actions
    
    var alts <- [[Rule alt <- [[array<Alternative>]]]]

    alts <- alts |> rule_append_with(a1) <| qmacro_block($ { return m * u; })
    alts <- alts |> rule_append_with(a2) <| qmacro_block($ { return m / u; })
    alts <- alts |> rule_append_with(a3) <| qmacro_block($ { return u; })

    return <- alts


def make_unary_def: Definition
    return <- [[Definition name = "primary", 
                type_ <- typeinfo(ast_typedecl type<int>), 
                rule <- make_unary_rule() ]]


def make_unary_rule(): Rule
    
    var minus <- [[Rule terminal <- [[Terminal lit = "-"]]]]
    var unary <- [[Rule bound_nonterminal = [[auto "unary", "u"]]]]
    var primary <- [[Rule bound_nonterminal = [[auto "prim", "p"]]]]

    // Create first alternative: '-' unary'

    var seq1 <- [[Rule seq <- [[array<Rule_?>]]]]
    var a1 <- seq1 |> rule_append(minus) |> rule_append(unary) 


    var seq2 <- [[Rule seq <- [[array<Rule_?>]]]]
    var a2 <- seq2 |> rule_append(primary)

    // Make alternatives and their actions
    
    var alts <- [[Rule alt <- [[array<Alternative>]]]]

    alts <- alts |> rule_append_with(a1) <| qmacro_block($ { return -u; })
    alts <- alts |> rule_append_with(a2) <| qmacro_block($ { return p; })

    return <- alts


def make_unary_def: Definition
    return <- [[Definition name = "unary", 
                type_ <- typeinfo(ast_typedecl type<int>), 
                rule <- make_unary_rule() ]]


def make_mul_rule(): Rule
    
    var div <- [[Rule terminal <- [[Terminal lit = "/"]]]]
    var times <- [[Rule terminal <- [[Terminal lit = "*"]]]]
    var mul <- [[Rule bound_nonterminal = [[auto "mul", "m"]]]]
    var unary <- [[Rule bound_nonterminal = [[auto "unary", "u"]]]]

    // Create first alternative: add '+' mul

    var seq1 <- [[Rule seq <- [[array<Rule_?>]]]]
    var a1 <- seq1 |> rule_append(mul) |> rule_append(times) |> rule_append(unary)

    // Create first alternative: add '-' mul

    var seq2 <- [[Rule seq <- [[array<Rule_?>]]]]
    var a2 <- seq2 |> rule_append(mul) |> rule_append(div) |> rule_append(unary)

    // Create second alternative: mul

    var seq3 <- [[Rule seq <- [[array<Rule_?>]]]]
    var a3 <- seq3 |> rule_append(unary)
    
    // Make alternatives and their actions
    
    var alts <- [[Rule alt <- [[array<Alternative>]]]]

    alts <- alts |> rule_append_with(a1) <| qmacro_block($ { return m * u; })
    alts <- alts |> rule_append_with(a2) <| qmacro_block($ { return m / u; })
    alts <- alts |> rule_append_with(a3) <| qmacro_block($ { return u; })

    return <- alts

def make_mul_def: Definition
    return <- [[Definition name = "mul", 
                type_ <- typeinfo(ast_typedecl type<int>), 
                rule <- make_mul_rule() ]]


def make_add_rule(): Rule
    
    var plus <- [[Rule terminal <- [[Terminal lit = "+"]]]]
    var minus <- [[Rule terminal <- [[Terminal lit = "-"]]]]
    var mul <- [[Rule bound_nonterminal = [[auto "mul", "m"]]]]
    var add <- [[Rule bound_nonterminal = [[auto "add", "a"]]]]

    // Create first alternative: add '+' mul

    var seq1 <- [[Rule seq <- [[array<Rule_?>]]]]
    var a1 <- seq1 |> rule_append(add) |> rule_append(plus) |> rule_append(mul)

    // Create first alternative: add '-' mul

    var seq2 <- [[Rule seq <- [[array<Rule_?>]]]]
    var a2 <- seq2 |> rule_append(add) |> rule_append(minus) |> rule_append(mul)

    // Create second alternative: mul

    var seq3 <- [[Rule seq <- [[array<Rule_?>]]]]
    var a3 <- seq3 |> rule_append(mul)
    
    // Make alternatives and their actions
    
    var alts <- [[Rule alt <- [[array<Alternative>]]]]

    alts <- alts |> rule_append_with(a1) <| qmacro_block($ { return a + m; })
    alts <- alts |> rule_append_with(a2) <| qmacro_block($ { return a - m; })
    alts <- alts |> rule_append_with(a3) <| qmacro_block($ { return m; })

    return <- alts

def make_add_def: Definition
    return <- [[Definition name = "add", 
                type_ <- typeinfo(ast_typedecl type<int>), 
                rule <- make_add_rule() ]]


def make_named_repetitions
    var mul <- [[Rule nonterminal = "mul"]]
    var rep <- [[Rule maybe_repeat = new [[Rule_ rule <- mul, name = "a"]] ]]
    
    var empty_alts: array<Alternative>
    var alts <- [[Rule alt <- empty_alts]]

    var t <- qmacro_block <| $
        var sum = 0
        for t in a 
            sum += t
        return sum

    alts <- alts |> rule_append_with(rep) <| t

    return <- alts 
