module GrammarTypes

open Lexer
open Parser
open FSharp.Reflection
open FSharp.Quotations
open ProviderImplementation.ProvidedTypes
open System.Collections.Generic
open System.Text.RegularExpressions

/// Used to fetch unique ids when needed.
/// Calling GetNext() multiple times with the same argument
/// will return the argument concatenated with an increasing
/// integer.
type Incrementer() =
    let map = Dictionary<string, int>()
    member __.Reset(key: string) = map.[key] <- 1
    member __.GetNext(key: string) =
        if map.ContainsKey key then
            let value = map.[key]
            map.[key] <- value + 1
            sprintf "%s%d" key value
        else
            map.[key] <- 2
            sprintf "%s1" key

let incrementer = Incrementer()

module RuntimeHelper =
    /// Returns a Choice type with number of choices equal to the given number
    let choiceUnionTypeForCases = function
        | 2 -> typeof<Choice<ParseTreeNode,ParseTreeNode>>
        | 3 -> typeof<Choice<ParseTreeNode,ParseTreeNode,ParseTreeNode>>
        | 4 -> typeof<Choice<ParseTreeNode,ParseTreeNode,ParseTreeNode,ParseTreeNode>>
        | 5 -> typeof<Choice<ParseTreeNode,ParseTreeNode,ParseTreeNode,ParseTreeNode,ParseTreeNode>>
        | 6 -> typeof<Choice<ParseTreeNode,ParseTreeNode,ParseTreeNode,ParseTreeNode,ParseTreeNode,ParseTreeNode>>
        | 7 -> typeof<Choice<ParseTreeNode,ParseTreeNode,ParseTreeNode,ParseTreeNode,ParseTreeNode,ParseTreeNode,ParseTreeNode>>
        | n -> failwithf "A disjunction grammar expression has %d alternatives, but only 2-7 alternatives are supported" n    

    let rec runtimeType = function
        | CaseInsensitiveTerminal _
        | Terminal _    -> typeof<unit>
        | CommaSeparatedRepeat node
        | OneOrMoreCommaSeparatedRepeat node
        | OneOrMoreRepeat node
        | Repeat node   -> typedefof<list<_>>.MakeGenericType [| runtimeType node |]
        | Optional node -> typedefof<option<_>>.MakeGenericType [| runtimeType node |]
        | Choice nodes  -> 
            let choiceType = (choiceUnionTypeForCases nodes.Length).GetGenericTypeDefinition()
            choiceType.MakeGenericType(nodes |> List.map runtimeType |> List.toArray)
        | Nonterminal _
        | Order _   -> typeof<ParseTreeNode>

    /// Returns the runtime object associated with a particular ParseTreeNode.
    /// This function must agree with the static typing structure of the provided types.
    let rec runtimeObject = function
        | PChoice(index, _count, pNode), Choice gNodes -> 
            let choiceType = runtimeType (Choice gNodes)
            let choiceXofY = 
                FSharpType.GetUnionCases(choiceType)
                |> Array.find (fun ty -> ty.Name.StartsWith(sprintf "Choice%d" (index+1)))
            // invoke the choice constructor for the discriminated union (e.g. Choice3of5)
            FSharpValue.MakeUnion(choiceXofY, [| runtimeObject (pNode, gNodes.[index]) |])
        | POptional None,        Optional _         -> None :> obj
        | POptional(Some pNode), Optional gNode     -> Some(runtimeObject(pNode, gNode)) :> obj
        | POrder pNodes,         Order _            -> POrder pNodes :> obj
        | PCommaSeparatedRepeat pNodes, (CommaSeparatedRepeat gNode | OneOrMoreCommaSeparatedRepeat gNode)        
        | PRepeat pNodes,        (Repeat gNode  | OneOrMoreRepeat gNode) -> 
            List.map (fun pNode -> runtimeObject(pNode, gNode)) pNodes :> obj
        | PNonterminal(name, node), Nonterminal _   -> PNonterminal(name, node) :> obj
        | PTerminal _,           (Terminal _ | CaseInsensitiveTerminal _) -> () :> obj
        | pNode, gNode -> failwithf "Internal error in 'runtimeObject', mismatched parse and grammar nodes.\nParse node:\n%A\n\nGrammar node:\n%A" pNode gNode

    let orderGetter partIndex = function
        | PNonterminal(_, POrder rtNodes), Order gNodes -> runtimeObject (rtNodes.[partIndex], gNodes.[partIndex])
        | node -> failwithf "Order GetterCode error, expected PNonterminal(_, POrder ...) but got\n%A" node

    let choiceGetter = function
        | PNonterminal(_, PChoice(index, count, node)), Choice gNodes -> runtimeObject (PChoice(index, count, node), Choice gNodes)
        | node -> failwithf "Choice GetterCode error, expected PNonterminal(_, PChoice ...) but got\n%A" node

    let repeatGetter = function
        | PNonterminal(_, PCommaSeparatedRepeat pNodes), repeatNode -> runtimeObject (PCommaSeparatedRepeat pNodes, repeatNode) :?> obj list
        | PNonterminal(_, PRepeat pNodes), repeatNode -> runtimeObject (PRepeat pNodes, repeatNode) :?> obj list
        | node -> failwithf "Repeat GetterCode error, expected PNonterminal(_, PRepeat ...) but got\n%A" node

    let castExpr grammarNode runtimeObjectExpr =
        match grammarNode with
        | CommaSeparatedRepeat node
        | OneOrMoreCommaSeparatedRepeat node
        | OneOrMoreRepeat node
        | Repeat node -> 
            let nodeType = runtimeType node
            // Lists are not covariant, so "obj list :?> T list" will not work even if obj :?> T works for all elements.
            // Instead we need to map over the list with a function that downcasts each element.
            // The following represents: List.map (fun n -> n :?> runtimeType) runtimeObject
            let listModule = System.Reflection.Assembly.GetAssembly(typeof<FSharp.Core.AbstractClassAttribute>).GetType("Microsoft.FSharp.Collections.ListModule")
            let mapFunction = listModule.GetMethod("Map").MakeGenericMethod [| typeof<obj>; nodeType |]
            let nVar = Var("n", typeof<obj>)
            let castLambda = Expr.Lambda(nVar, Expr.Coerce(Expr.Var nVar, nodeType))
            Expr.Call(mapFunction, [castLambda; runtimeObjectExpr])
        | CaseInsensitiveTerminal _
        | Terminal _
        | Optional _
        | Choice _
        | Nonterminal _
        | Order _ -> Expr.Coerce(runtimeObjectExpr, runtimeType grammarNode)

    let tokenToString tokenClassName = function
        | PNonterminal(ntName, PTerminal {RawText = rawText; Type=_; Location=_}) when ntName = tokenClassName -> rawText
        | _ -> failwith "Internal error in tokenToString"

    let defaultNonterminalToString = function
        | PNonterminal(_, PChoice(_,_, PNonterminal(ntName, terminalNode))) -> tokenToString ntName (PNonterminal(ntName, terminalNode))
        | _ -> failwith "Internal error in defaultNonterminalToString"

let pluralize (noun: string) =
    if noun.EndsWith("x") || noun.EndsWith("s") || noun.EndsWith("ch") || noun.EndsWith("sh") then
        noun + "es"
    else
        noun + "s"

/// Creates and returns a provided type for a token class. The caller is responsible for nesting it in an enclosing type.
let tokenClassToType(name, regex) =
    let ty = ProvidedTypeDefinition(name, Some typeof<ParseTreeNode>, HideObjectMethods=true)
    ty.AddXmlDoc("A token class matching regex: " + regex)
    ty.AddMember(ProvidedMethod("ToString", [], typeof<string>, 
                    InvokeCode= fun args -> <@@ RuntimeHelper.tokenToString name (%%args.[0] : ParseTreeNode) @@> ))
    ty.AddMember(ProvidedConstructor([ProvidedParameter("text", typeof<string>)], 
                    InvokeCode= fun args ->
                        <@@ let text = %%args.[0] : string
                            if Regex.IsMatch(text, regex) then
                                PNonterminal(name, PTerminal {Type= TokenClass name; RawText= text; Location= 0})
                            else
                                failwithf "Cannot construct terminal '%s' because given string '%s' does not conform to regular expression: %s"
                                    name text regex  @@> ))
    match name with
    | "Nat" | "NegInt" -> 
        ty.AddMember(ProvidedMethod("ToInt", [], typeof<int>, 
                        InvokeCode= fun args -> <@@ RuntimeHelper.tokenToString name (%%args.[0] : ParseTreeNode) |> int @@>))
    | "Dec" ->
        ty.AddMember(ProvidedMethod("ToFloat", [], typeof<float>, 
                        InvokeCode= fun args -> <@@ RuntimeHelper.tokenToString name (%%args.[0] : ParseTreeNode) |> float @@>))
    | "Stringlit" | "Charlit" ->
        ty.AddMember(ProvidedMethod("Unquote", [], typeof<string>, InvokeCode= fun args ->
                        <@@ let text = RuntimeHelper.tokenToString name (%%args.[0] : ParseTreeNode)
                            text.Substring(1, text.Length - 2) @@> ))
    | "Id" ->
        ty.AddMember(ProvidedMethod("Unique", [ProvidedParameter("prefix", typeof<string>)], ty, IsStaticMethod=true,
                        InvokeCode= fun args ->
                            <@@ let prefix = %%args.[0] : string
                                let idText = incrementer.GetNext prefix
                                PNonterminal("Id", PTerminal {Type = TokenClass "Id"; RawText = idText; Location=0}) @@> ))
    | _ -> ()
    ty

/// Returns a quotation representing the creation of a new PTerminal
let private createPTerminalExprWithKeyword keywordTokenClass (terminalText:string) =
    let tokenClass = 
        if Regex.IsMatch(terminalText, "^[a-zA-Z]+$") then
            Expr.Value keywordTokenClass
        else
            Expr.Value "Punctuation"
    let terminalTextExpr = Expr.Value terminalText
    <@@ PTerminal {Type = TokenClass %%tokenClass; RawText = %%terminalTextExpr; Location = 0} @@>

let private createPTerminalExpr = createPTerminalExprWithKeyword "Keyword"
let private createCaseInsensitivePTerminalExpr = createPTerminalExprWithKeyword "CIKeyword"
    
/// Returns a string representation of the given EbnfNode
let rec nodeAsString = function
    | Order nodes -> 
        nodes
        |> List.map nodeAsString
        |> String.concat " "
        |> sprintf "(%s)"
    | Choice nodes ->
        nodes
        |> List.map nodeAsString
        |> String.concat " | "
        |> sprintf "(%s)"
    | Repeat node                           -> sprintf "%s*" (nodeAsString node)
    | OneOrMoreRepeat node                  -> sprintf "%s+" (nodeAsString node)
    | CommaSeparatedRepeat node             -> sprintf "%s,*" (nodeAsString node)
    | OneOrMoreCommaSeparatedRepeat node    -> sprintf "%s,+" (nodeAsString node)
    | Optional node                         -> sprintf "%s?" (nodeAsString node)
    | Nonterminal nonterminalName           -> nonterminalName
    | CaseInsensitiveTerminal literalText   -> sprintf "\"%s\"" literalText
    | Terminal literalText                  -> sprintf "'%s'" literalText

/// Append a unique number suffix to duplicated elements in a list. 
/// e.g. ["a"; "b"; "a"; "c"; "c"] becomes ["a1"; "b"; "a2"; "c1"; "c2"]
let private numberDuplicates list =
    let rec duplicates = function
        | [] -> []
        | x::xs when List.contains x xs -> x::(duplicates xs)
        | _::xs -> duplicates xs

    let rec numberDuplicatesFor suffix list item =
        match list with
        | [] -> []
        | x::xs when x = item -> (sprintf "%s%d" item suffix)::(numberDuplicatesFor (suffix+1) xs item)
        | x::xs -> x::(numberDuplicatesFor suffix xs item)

    list
    |> duplicates
    |> List.fold (numberDuplicatesFor 1) list

/// Used to make provided types that are nested inside root types
let private internalTypeMaker (enclosingType: ProvidedTypeDefinition) typeName = 
    let ty = ProvidedTypeDefinition(typeName, Some typeof<ParseTreeNode>, HideObjectMethods=true)
    enclosingType.AddMember(ty)
    ty

/// Returns a provided type representing a root node. A root node is the EbnfNode at the root of the expression
/// tree representing the right-hand side of a grammar production. 'nonterminalName' is the left-hand side of
/// that production.
let rootNodeToType nonterminalName rootTypeMaker rootNode (typeDict: Dictionary<string, ProvidedTypeDefinition>) =
    
    /// Creates a .NET type corresponding to an EbnfNode
    let rec nodeToType newTypeName typeMaker = function
        | CommaSeparatedRepeat node
        | OneOrMoreCommaSeparatedRepeat node
        | Repeat node
        | OneOrMoreRepeat node ->
            ProvidedTypeBuilder.MakeGenericType(typedefof<list<_>>, [nodeToType newTypeName typeMaker node])
        | Optional node -> 
            ProvidedTypeBuilder.MakeGenericType(typedefof<option<_>>, [nodeToType newTypeName typeMaker node])
        | Nonterminal nonterminalName -> typeDict.[nonterminalName] :> System.Type
        | CaseInsensitiveTerminal _
        | Terminal _ -> typeof<unit>
        | Order nodes ->
            let orderType = typeMaker newTypeName
            let members() =
                let indexedNodesWithoutTerminals = 
                    nodes
                    |> List.mapi (fun i node -> i, node) 
                    |> List.filter (function _, (Terminal _ | CaseInsensitiveTerminal _) -> false | _ -> true)
                let propertyNamesForNodes = 
                    indexedNodesWithoutTerminals 
                    |> List.map (snd >> nodeAsString) 
                    |> numberDuplicates
                List.zip propertyNamesForNodes indexedNodesWithoutTerminals
                |> List.map (fun (propName, (i, node)) ->
                    let staticType = nodeToType (sprintf "Part%d" (i+1)) (internalTypeMaker orderType) node
                    ProvidedProperty(propName, staticType, GetterCode= fun args ->
                            QuotationHelper.quoteUnion (Order nodes) <| fun orderExpr ->
                            <@@ RuntimeHelper.orderGetter i ((%%args.[0] : ParseTreeNode), %%orderExpr) @@>))
            
            orderType.AddMembersDelayed members   
            orderType :> System.Type

        | Choice nodes ->
            let choiceUnionType = RuntimeHelper.choiceUnionTypeForCases nodes.Length
            let choiceTypes =
                nodes
                |> List.mapi (fun i node -> nodeToType (sprintf "Alternative%d" (i+1)) typeMaker node)
            ProvidedTypeBuilder.MakeGenericType(choiceUnionType.GetGenericTypeDefinition(), choiceTypes)
    
    // root types need to be ProvidedTypeDefinitions, so wrap any other types in those
    match rootNode with
    | Order _ -> nodeToType nonterminalName rootTypeMaker rootNode :?> ProvidedTypeDefinition
    | Choice [Nonterminal "Nat"; Nonterminal "NegInt"] when nonterminalName = "Int" ->
        let intType = rootTypeMaker nonterminalName
        intType.AddMember(ProvidedMethod("ToInt", [], typeof<int>, InvokeCode= fun args ->
                            <@@ RuntimeHelper.defaultNonterminalToString (%%args.[0] : ParseTreeNode) |> int @@> ))
        intType
    | Choice [Nonterminal "Dec"; Nonterminal "Nat"; Nonterminal "NegInt"] when nonterminalName = "Num" ->
        let floatType = rootTypeMaker nonterminalName
        floatType.AddMember(ProvidedMethod("ToFloat", [], typeof<float>, InvokeCode= fun args ->
                            <@@ RuntimeHelper.defaultNonterminalToString (%%args.[0] : ParseTreeNode) |> float @@> ))
        floatType
    | Choice [Terminal "true"; Terminal "false"] when nonterminalName = "Bool" ->
        let boolType = rootTypeMaker nonterminalName
        boolType.AddMember(ProvidedMethod("ToBool", [], typeof<bool>, InvokeCode= fun args ->
                            <@@ match RuntimeHelper.defaultNonterminalToString (%%args.[0] : ParseTreeNode) with
                                | "true" -> true
                                | "false" -> false
                                | s -> failwithf "Expected 'true' or 'false' but got %s" s @@> ))
        boolType
    | Choice _ -> 
        let choiceType = rootTypeMaker nonterminalName
        choiceType.AddMemberDelayed <| fun () ->
            let staticType = nodeToType nonterminalName (internalTypeMaker choiceType) rootNode     // Choice#
            ProvidedProperty("Chosen", staticType, GetterCode= fun args -> 
                    QuotationHelper.quoteUnion rootNode <| fun rootNode ->
                    <@@ RuntimeHelper.choiceGetter ((%%args.[0] : ParseTreeNode), %%rootNode) @@>)

        choiceType

    | CaseInsensitiveTerminal literalText ->
        let ty = rootTypeMaker nonterminalName
        ty.AddMember(ProvidedField(literalText, typeof<string>))
        ty.AddMemberDelayed <| fun () ->
            ProvidedConstructor([], InvokeCode= fun _ -> 
                let terminalExpr = createCaseInsensitivePTerminalExpr literalText
                <@@ PNonterminal(nonterminalName, (%%terminalExpr : ParseTreeNode) ) @@> )
        ty
    | Terminal literalText ->
        let ty = rootTypeMaker nonterminalName
        ty.AddMember(ProvidedField(literalText, typeof<string>))
        ty.AddMemberDelayed <| fun () ->
            ProvidedConstructor([], InvokeCode= fun _ -> 
                let terminalExpr = createPTerminalExpr literalText
                <@@ PNonterminal(nonterminalName, (%%terminalExpr : ParseTreeNode) ) @@> )
        ty
    | Nonterminal nonterminalName ->
        let ty = rootTypeMaker nonterminalName
        ty.AddMemberDelayed <| fun () ->
            ProvidedProperty(nonterminalName, typeDict.[nonterminalName], GetterCode= fun args -> 
                QuotationHelper.quoteUnion rootNode <| fun rootNode ->
                <@@ RuntimeHelper.runtimeObject ((%%args.[0] : ParseTreeNode), %%rootNode) @@>)
        ty.AddMemberDelayed <| fun () ->
            ProvidedConstructor([ProvidedParameter(nonterminalName, typeDict.[nonterminalName])], 
                InvokeCode= fun args -> <@@ PNonterminal(nonterminalName, (%%args.[0] : ParseTreeNode) ) @@> )
        ty

    | CommaSeparatedRepeat node
    | Repeat node ->
        let repeatType = rootTypeMaker nonterminalName
        repeatType.AddMemberDelayed <| fun () ->
            let propertyName = match node with Nonterminal ntName -> pluralize ntName | _ -> "Repetitions"
            let staticType = nodeToType nonterminalName (internalTypeMaker repeatType) rootNode  // List<_>
            ProvidedProperty(propertyName, staticType, GetterCode= fun args -> 
                    QuotationHelper.quoteUnion rootNode <| fun rootNodeExpr ->
                    RuntimeHelper.castExpr rootNode <@@ RuntimeHelper.repeatGetter ((%%args.[0] : ParseTreeNode), %%rootNodeExpr) @@>)

        repeatType

    | OneOrMoreCommaSeparatedRepeat node
    | OneOrMoreRepeat node ->
        let repeatType = rootTypeMaker nonterminalName
        repeatType.AddMemberDelayed <| fun () ->
            let propertyName = match node with Nonterminal ntName -> pluralize ntName | _ -> "Repetitions"
            let staticType = nodeToType nonterminalName (internalTypeMaker repeatType) rootNode  // List<_>
            ProvidedProperty(propertyName, staticType, GetterCode= fun args -> 
                    QuotationHelper.quoteUnion rootNode <| fun rootNodeExpr ->
                    RuntimeHelper.castExpr rootNode <@@ RuntimeHelper.repeatGetter ((%%args.[0] : ParseTreeNode), %%rootNodeExpr) @@>)

        repeatType
    
    | Optional _ ->
        let optionalType = rootTypeMaker nonterminalName
        optionalType.AddMemberDelayed <| fun () ->
            let staticType = nodeToType nonterminalName (internalTypeMaker optionalType) rootNode   // Option<_>
            ProvidedProperty("Value", staticType, GetterCode= fun args -> 
                    QuotationHelper.quoteUnion rootNode <| fun rootNode ->
                    <@@ RuntimeHelper.runtimeObject ((%%args.[0] : ParseTreeNode), %%rootNode) @@>)
        
        optionalType

