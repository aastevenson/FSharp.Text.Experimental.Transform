module Transform

open Parser

/// Traverses the parse tree once, applying the transformation function 'f'.
/// The transformation function does not get applied to any replacements,
/// therefore this is guaranteed to return.
let applyOnePass targetName transform scope =
    let rec onePass = function
        | PNonterminal(name, node) when name = targetName -> 
            let newNode = transform (PNonterminal(name, node))
            if newNode = PNonterminal(name, node) then
                PNonterminal(name, onePass node)            // no change, so recurse into 'node'
            else
                newNode                                     // transformed, so don't recurse
        | PNonterminal(name, node)      -> PNonterminal(name, onePass node)
        | PChoice(idx, cnt, node)       -> PChoice(idx, cnt, onePass node)
        | POrder nodes                  -> POrder(List.map onePass nodes)
        | POptional(Some node)          -> POptional(Some(onePass node))
        | POptional None                -> POptional None
        | PRepeat nodes                 -> PRepeat(List.map onePass nodes)
        | PCommaSeparatedRepeat nodes   -> PCommaSeparatedRepeat(List.map onePass nodes)
        | PTerminal token               -> PTerminal token

    onePass scope

let applyOnePassRepeat targetName transform scope =
    let rec onePass = function
        | PRepeat(PNonterminal(name, node)::tail) when name = targetName ->         
            PRepeat(transform (PNonterminal(name, node)::tail))                 // BUG: [] result for OneOrMoreRepeat
        | PCommaSeparatedRepeat(PNonterminal(name, node)::tail) when name = targetName -> 
            PCommaSeparatedRepeat(transform (PNonterminal(name, node)::tail))
        | PNonterminal(name, node)      -> PNonterminal(name, onePass node)
        | PChoice(idx, cnt, node)       -> PChoice(idx, cnt, onePass node)
        | POrder nodes                  -> POrder(List.map onePass nodes)
        | POptional(Some node)          -> POptional(Some(onePass node))
        | POptional None                -> POptional None
        | PRepeat nodes                 -> PRepeat(List.map onePass nodes)
        | PCommaSeparatedRepeat nodes   -> PCommaSeparatedRepeat(List.map onePass nodes)
        | PTerminal token               -> PTerminal token

    onePass scope

/// Traverses the parse repeatedly, applying the transformation function 'transform',
/// until no more replacements are possible.
/// The transformation function is recursively applied to replacements.
/// If the transformation function's replacement pattern can be unified with
/// its match pattern then this will recurse infinitely.
let applyRecursively targetName transform scope =
    let mutable changed = true    
    let mutable currentTree = scope
    let rec recurse = function
        | PNonterminal(name, node) when name = targetName  -> 
            let newNode = transform (PNonterminal(name, node))
            if newNode = PNonterminal(name, node) then 
                PNonterminal(name, recurse node)
            else 
                changed <- true
                newNode
        | PNonterminal(name, node)      -> PNonterminal(name, recurse node)
        | PChoice(idx, cnt, node)       -> PChoice(idx, cnt, recurse node)
        | POrder nodes                  -> POrder(List.map recurse nodes)
        | POptional(Some node)          -> POptional(Some(recurse node))
        | POptional None                -> POptional None
        | PRepeat nodes                 -> PRepeat(List.map recurse nodes)
        | PCommaSeparatedRepeat nodes   -> PCommaSeparatedRepeat(List.map recurse nodes)
        | PTerminal token               -> PTerminal token

    while changed do
        changed <- false
        currentTree <- recurse currentTree

    currentTree

let applyRecursivelyRepeat targetName transform scope =
    let mutable changed = true    
    let mutable currentTree = scope
    let rec recurse = function
        | PRepeat(PNonterminal(name, node)::tail) when name = targetName  -> 
            let newList = transform (PNonterminal(name, node)::tail)
            if newList = PNonterminal(name, node)::tail then 
                PRepeat(List.map recurse newList)
            else 
                changed <- true
                PRepeat newList
        | PCommaSeparatedRepeat(PNonterminal(name, node)::tail) when name = targetName  -> 
            let newList = transform (PNonterminal(name, node)::tail)
            if newList = PNonterminal(name, node)::tail then 
                PCommaSeparatedRepeat(List.map recurse newList)
            else 
                changed <- true
                PCommaSeparatedRepeat newList
        | PNonterminal(name, node)      -> PNonterminal(name, recurse node)
        | PChoice(idx, cnt, node)       -> PChoice(idx, cnt, recurse node)
        | POrder nodes                  -> POrder(List.map recurse nodes)
        | POptional(Some node)          -> POptional(Some(recurse node))
        | POptional None                -> POptional None
        | PRepeat nodes                 -> PRepeat(List.map recurse nodes)
        | PCommaSeparatedRepeat nodes   -> PCommaSeparatedRepeat(List.map recurse nodes)
        | PTerminal token               -> PTerminal token

    while changed do
        changed <- false
        currentTree <- recurse currentTree

    currentTree