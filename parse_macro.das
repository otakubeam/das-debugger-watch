options indenting = 4
options no_unused_block_arguments = false
options no_unused_function_arguments = false
options no_aot = true
options strict_smart_pointers = true

module parse_macro

require daslib/templates_boost
require daslib/strings_boost
require daslib/macro_boost
require daslib/ast_boost
require daslib/defer

require meta_ast

struct MacroRule
    rule: ExprLet? // from let rule: type
    alternatives: array<ExprCall?>


[macro_function]
def add_alternative(var rules: array<MacroRule>; var expr: ExprCall?; prog:ProgramPtr)
    macro_verify(expr.name == "rule", prog, expr.at, "parsing rules alternatives should be called with `rule` name")
    (rules |> back).alternatives |> emplace <| expr

    
[macro_function]
def add_name(var rule: Rule; var name: string)
    match rule 
        if [[Rule repeat = $v(rep)]]
            rep.name <- name
        if [[Rule option = $v(opt)]]
            opt.name <- name
        if [[Rule maybe_repeat = $v(rep)]]
            rep.name <- name
        if _
            abort("Unreachable")


[macro_function]
def into_rule(var expr: ExpressionPtr): Rule
    if expr is ExprAsVariant && (expr as ExprAsVariant).value is ExprVar
        // Bound terminal

        var as_expr <- expr as ExprAsVariant
        var bound_name <- as_expr.name
        var bound_rule <- (as_expr.value as ExprVar).name
        return <- [[Rule bound_nonterminal = [[auto bound_rule, bound_name]]]]

    elif expr is ExprAsVariant
        // This way goes *<rule> as <name>,
        //               <rule>? as <name>,
        //               +<rule> as <name>, etc...

        var as_expr <- expr as ExprAsVariant 

        var rule: Rule <- into_rule(as_expr.value)
        rule |> add_name(as_expr.name)

        return <- rule

    elif expr is ExprVar
        // Nonbound terminal
        var name = (expr as ExprVar).name

        if name == "number"
            return <- [[Rule terminal <- [[Terminal number]]]]
        else 
            return <- [[Rule nonterminal <- name]]
    
    abort("TODO: Unreachable")
    return [[Rule]]


def transform(var rule: MacroRule): Definition
    var name = rule.rule.variables.name
    var type_ = rule.rule._type

    // Transform array<ExprCall?> into an array of Rules

    var alternatives: array<Alternative>
    
    for alt in expr.arguments

        var blk <- alt.arguments |> back
        expr.arguments |> pop
        
        var sequence: array<Rule_?>

        for sequence_elem in expr.arguments
            sequence |> push <| into_rule(sequence_elem)
    
        alternatives |> emplace_new <| [[Alternative 
                                         rule <- new [[Rule seq <- sequence]], 
                                         action <- blk]]
    
    return <- [[Definition name = "{name}", 
                type_ <- type_, 
                rule <- [[Rule alt <- alt]] ]]


[call_macro(name="parse")]
class ParseMacro : AstCallMacro
    //! Implements `parse` macro.
    //! Usage:
    //!    parse expression
    //!        let add : int
    //!    
    //!        rule(add as a, '+', mul as m) <|
    //!            return a + m
    //!    
    //!        rule(add as a, '-', mul as m) <|
    //!            return a - m
    //!    
    //!        rule(mul as m) <|
    //!            return m
    //!
    //!        let mul: int
    //!        ...
    //!
    //!    let my_var = expresssion("1+2")
    //!
    //! - The pattern is let <rule_name>: <type> followed by a series of calls to `rule` function
    //! - The typing in `let add: int` is significant and will not work otherwise.
    //! - The last argument of the rule function is a semantic action that will be exeuted
    //!     upon the matching of the rule. Names used in actions are defined with 
    //!     `<rule> as <name>` syntax. Actions must return the type as defined in let expression.
    //! - By the nature of PEG parsers, in the general case actions are executed nondeterministically.
    //!     THEREFORE, DO NOT PUT STATEFUL ACTIONS in the parser generator. Thank you for your attention. 


    def override canVisitArgument ( expr:smart_ptr<ExprCallMacro>; argIndex:int ) : bool
        // if we are reporting errors, and macro did not fold - no point showing errors inside match - there will be too many, and they are meaningless
        return !is_reporting_compilation_errors()


    def override canFoldReturnResult ( expr:smart_ptr<ExprCallMacro> ) : bool
        // function can't decide if its void or not, until match is folded
        return false

    def override preVisit ( prog:ProgramPtr; mod:Module?; expr:smart_ptr<ExprCallMacro> ) : void
        return if length(expr.arguments) != 2

        assume blk = expr.arguments[1]
        
        return if blk._type == null
        return if !blk._type.isGoodBlockType
        return if !(blk is ExprMakeBlock)
        
        // we mark all if's inside the match to not fold, so that we can fold the match later
        
        var expression_block = ((blk as ExprMakeBlock)._block as ExprBlock)
        for ee in expression_block.list
            if ee is ExprCall 
                (ee as ExprCall).if_flags |= IfFlags doNotFold


    def override visit ( prog:ProgramPtr; mod:Module?; expr:smart_ptr<ExprCallMacro> ) : ExpressionPtr
        macro_verify(length(expr.arguments) == 2, prog, expr.at, "expecting parse(what) <| block")
        
        assume what = expr.arguments[0]
        assume blk = expr.arguments[1]
        
        macro_verify(what._type != null && !what._type.isAutoOrAlias, prog, expr.at, "match `what` argument did not resolve")
        macro_verify(blk._type.isGoodBlockType, prog, expr.at, "match `block` argument did not resolve")
        macro_verify(blk is ExprMakeBlock, prog, expr.at, "match `block` argument must be immediate block declaration")
        
        var expression_block = ((blk as ExprMakeBlock)._block as ExprBlock)
        macro_verify(length(expression_block.finalList) == 0, prog, expr.at, "match `block` argument can't have finally section")
        macro_verify(expression_block.list[0] is ExprLet, prog, expr.at, "parse `block` argument should begin with let expression")
        
        // Collect
        
        var rules: array<MacroRule>
        
        for list_expr in expression_block.list
            if list_expr is ExprLet
                rules |> emplace <| [[Rule rule = list_expr as ExprLet]]
            elif list_expr is ExprCall
                rules |> add_alternative(list_expr as ExprCall, prog)
            else 
                macro_verify(false, prog, list_expr.at, "expressions in the parse block should be either let or call")

        // Transform
        
        var gram: array<Definition>

        for mrule in rules
            gram |> push(mrule |> transform)

        // Generate

        var gen <- [[ParserGenerator]]
        
        for rule in gram
            gen |> set_rule_type(rule.name, rule.type_)
        
        for rule in gram 
            gen |> generate(rule)

        // At this point the fucntions for rules are added to the compiling module

        return <- qmacro(1) // Just return something


def public match_type(anytype,anything) : void
    //! this is a macro, it will be replaced with a match expression of specified type
    pass

def public match_expr(anything) : void
    //! this is a macro, it will be replaced with a match expression
    pass

[_macro]
def private setup
    if is_compiling_macros_in_module("parse_macro")
        compiling_module() |> add_keyword("parse", false)
