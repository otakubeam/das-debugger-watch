require daslib/match


struct Grammar
    defs: array<Definition>


// E.g. rule_name: rule
struct Definition
    name: string
    alts: array<Alternative> // Here trying to build the grammar into data strucutres (it doesn't work like this)


struct Alternative
    alternatives: array<Rule?>
     

variant Rule
    seq: array<Rule?>  // R1 R2
    alt: Alternative // R1 | R2
    
    maybe_repeat: Rule? // Rule*
    repeat: Rule?       // Rule+

    option: Rule?  // Rule?

    terminal: Terminal 

    // Do not consume the input
    not: Rule?     // !Rule
    and: Rule?     // &Rule


def abort(message: string)
    print("{}\n")
    exit(1)


def iota
    var i: int = 0
    return <- generator<int>() <| $
        i += 1
        return false if i == 0
        yield i


def generate(rule : Rule)
    match rule
        if [[Rule seq = rules]]


        if [[Rule alt = alts]]
            with alts
                for alt in alternatives



        if _
            abort("TODO: more rules")


variant Terminal
    // Literal: "Hello world"
    lit: string 

    // Range: [0-9]
    range_: tuple<int; int> 


def generate(terminal: Terminal): bool

    var input: string

    match termainal
        if [[Terminal lit = l ]]

            var q = qmacro() <| $
                return [[Result]] if !parser |> matches($v(l))
            
            return true

        if [[Terminal range_ = r ]]
            abort("TODO: ranges")



def run 
    // Do not create grammar for now
    // Let's start with something simple
    // Like matching a `terminal` node
    pass
