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

    var empty_alts: array<Alternative>
    var alts <- [[Rule alt <- empty_alts]]

    alts <- alts |> rule_append(n) |> rule_append(n2)

    return <- alts


def make_add_rule(): Rule
    
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


    var empty_alts: array<Alternative>
    var alts <- [[Rule alt <- empty_alts]]

    alts <- alts |> rule_append(n) |> rule_append(n2)

    return <- alts



def make_named_repetitions
    pass
