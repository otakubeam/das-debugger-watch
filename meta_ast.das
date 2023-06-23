
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


struct Terminal
    // Literal: "Hello world"
    lit: string 

    // Range: [0-9]
    range: tuple<int; int> 



