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


def generate(rule : Rule)
    print("")


variant Terminal
    // Literal: "Hello world"
    lit: string 

    // Range: [0-9]
    range_: tuple<int; int> 


def matches(terminal: Terminal): bool

    var input: string

    match termainal
        if [[Terminal lit = .... l    ]]

            var idx = 0
            var against: array<u8> = input
        
            // if !input |> starts_with(l)
            //     return false

            for letter in l
                if letter != against[idx]
                    return false
                idx += 1
            
            input |> chop(0)            // if !input |> starts_with(l)
            //     return false0, idx)

        // if [[Terminal range = ...r... ]] 


def run 
    // Do not create grammar for now
    // Let's start with something simple
    // Like matching a `terminal` node
    pass
