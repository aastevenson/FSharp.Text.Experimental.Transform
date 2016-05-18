module GrammarTypeMethods

open Lexer
open Parser
open ProviderImplementation.ProvidedTypes
open System.Collections.Generic
open FSharp.Quotations
open FSharp.Reflection

let unwrapRepeat = function
    | PCommaSeparatedRepeat nodes
    | PRepeat nodes -> nodes
    | _ -> failwith "Internal error in unwrapRepeat"

let unwrapOptional = function
    | POptional opt -> opt
    | _ -> failwith "Internal error in unwrapOptional"

type BindingType(bindings: Map<string, ParseTreeNode>) =
    /// Runtime representation of variable bindings to parse trees
    member __._map = bindings

type BindingTypeMaker(bindingTypeContainer: ProvidedTypeDefinition, typeDict: Dictionary<string, ProvidedTypeDefinition>) =
    /// Stores the binding types, keyed on a static parameter of provided methods
    let bindingTypeDict = Dictionary<string, ProvidedTypeDefinition>()
    let mutable bindingTypeCount = 0

    member __.Add(staticParameterText, holes) =
        if bindingTypeDict.ContainsKey staticParameterText then
            bindingTypeDict.[staticParameterText]
        else
            let capitalizeFirstLetter (str: string) = sprintf "%c%s" (str.ToUpper().[0]) (if str.Length = 1 then "" else str.[1..])
            let (|AnnotatedType|_|) (str: string) =
                let baseType = str.TrimEnd('*', '+', '?', ',')
                let suffix = str.Substring baseType.Length
                match suffix with
                | "?"| "*" | "+" | ",+" | ",*" -> Some(baseType, suffix)
                | _ -> None
    
            let holeTypeFromName = function
                | AnnotatedType(typeName, "?") -> ProvidedTypeBuilder.MakeGenericType(typeof<option<_>>, [typeDict.[typeName]])
                | AnnotatedType(typeName, ",*")
                | AnnotatedType(typeName, ",+")
                | AnnotatedType(typeName, "*")
                | AnnotatedType(typeName, "+") -> ProvidedTypeBuilder.MakeGenericType(typeof<FSharp.Collections.list<_>>, [typeDict.[typeName]])
                | typeName -> typeDict.[typeName] :> System.Type
            let getterCodeImpl holeVarName holeVarType (args: Expr list) =
                match holeVarType with
                | AnnotatedType(_, "?") -> <@@ Map.find holeVarName (%%args.[0] : BindingType)._map |> unwrapOptional @@>
                | AnnotatedType(_, ",*")
                | AnnotatedType(_, ",+")  
                | AnnotatedType(_, "*") 
                | AnnotatedType(_, "+") -> <@@ Map.find holeVarName (%%args.[0] : BindingType)._map |> unwrapRepeat @@>
                | _                     -> <@@ Map.find holeVarName (%%args.[0] : BindingType)._map @@>
                
            bindingTypeCount <- bindingTypeCount + 1
            let ty = ProvidedTypeDefinition(sprintf "BindingType%d" bindingTypeCount, Some typeof<BindingType>, HideObjectMethods=true)
            holes
            |> List.map (fun (holeVarName, holeTypeName) ->
                ProvidedProperty(capitalizeFirstLetter holeVarName,
                                 holeTypeFromName holeTypeName, 
                                 GetterCode= (getterCodeImpl holeVarName holeTypeName)))
            |> ty.AddMembers
            bindingTypeContainer.AddMember ty
            bindingTypeDict.Add(staticParameterText, ty)
            ty

let private scanAndParse parse text tokenClasses (grammar:EbnfGrammar) (topLevelGoal:string) =
    match Lexer.tokenize tokenClasses text with
    | UnmatchableText tokenPosition -> failwithf "Error scanning pattern %s" (Errors.scanMessage tokenPosition text)
    | SuccessfulScan tokens ->

    let isTokenClass tokenName =
        tokenClasses
        |> List.map (fun tokenClassDef -> tokenClassDef.Name)
        |> List.contains tokenName
                    
    match parse topLevelGoal grammar isTokenClass tokens with
    | SyntaxError failureToken
    | UnconsumedInputError failureToken -> failwithf "Error parsing pattern %s" (Errors.parseMessage failureToken text)
    | Success (parseTree:ParseTreeNode) -> parseTree

let private scanAndParsePattern text = scanAndParse Pattern.parse text
let private scanAndParseReplacement text = scanAndParse Replacement.parse text

let tryMatchMethod (bindingTypeMaker: BindingTypeMaker) tokenClasses (grammar: EbnfGrammar) topLevelGoal (enclosingType: ProvidedTypeDefinition) () =
    let m = ProvidedMethod("TryMatch", [], typeof<BindingType option>, IsStaticMethod=false)
    m.AddXmlDoc(Documentation.tryMatchMethod topLevelGoal)
    m.DefineStaticParameters([ProvidedStaticParameter("pattern", typeof<string>)], fun nm args ->
        let pattern = args.[0] :?> string                
        let patternParse = scanAndParsePattern pattern tokenClasses grammar topLevelGoal
        let returnType =
            let holes = Pattern.findAllNamedHoles patternParse
            let bindingType = bindingTypeMaker.Add(pattern, holes)
            ProvidedTypeBuilder.MakeGenericType(typeof<Option<_>>, [bindingType])
                
        let m2 = 
            ProvidedMethod(nm, [], returnType, IsStaticMethod=false, InvokeCode= fun args ->
                QuotationHelper.quoteUnion patternParse <| fun patternParse ->
                <@@ let candidateParse = %%args.[0] : ParseTreeNode
                    match Pattern.Matcher.tryMatch Map.empty (%%patternParse, candidateParse) with
                    | Some m -> Some (BindingType m)
                    | None -> None @@>)
        enclosingType.AddMember m2
        m2 )
    m

let tryFindMethod (bindingTypeMaker: BindingTypeMaker) tokenClasses (grammar: EbnfGrammar) targetTypeName (enclosingType: ProvidedTypeDefinition) =
    let m = ProvidedMethod("TryFind" + targetTypeName, [], typeof<BindingType option>)
    m.AddXmlDoc(Documentation.tryFindMethod targetTypeName)
    m.DefineStaticParameters([ProvidedStaticParameter("pattern", typeof<string>)], fun nm args ->
        let pattern = args.[0] :?> string                
        let patternParse = scanAndParsePattern pattern tokenClasses grammar targetTypeName
        let returnType =
            let holes = Pattern.findAllNamedHoles patternParse
            let bindingType = bindingTypeMaker.Add(pattern, holes)
            ProvidedTypeBuilder.MakeGenericType(typeof<Option<_>>, [bindingType])

        let m2 = ProvidedMethod(nm, [], returnType, InvokeCode= fun args ->
                    QuotationHelper.quoteUnion patternParse <| fun patternParse ->
                    <@@ let scopeParse = %%args.[0] : ParseTreeNode
                        match Pattern.Matcher.tryFind %%patternParse scopeParse with
                        | Some m -> Some (BindingType m)
                        | None -> None @@>)
        enclosingType.AddMember m2
        m2 )
    m

// cannot have a static variant because of: https://github.com/Microsoft/visualfsharp/issues/516
// but was apparently fixed by: https://github.com/Microsoft/visualfsharp/pull/705
let findAllMatchingMethod (bindingTypeMaker: BindingTypeMaker) tokenClasses (grammar: EbnfGrammar) targetTypeName scopeTypeName (enclosingType: ProvidedTypeDefinition) =
    let m = ProvidedMethod(sprintf "FindAll%sMatching" (GrammarTypes.pluralize targetTypeName), [], typeof<BindingType list>)
    m.AddXmlDoc(Documentation.findAllMatchingMethod)
    m.DefineStaticParameters([ProvidedStaticParameter("pattern", typeof<string>)], fun nm args ->
        let pattern = args.[0] :?> string                
        let patternParse = scanAndParsePattern pattern tokenClasses grammar targetTypeName
        let returnType =
            let holes = Pattern.findAllNamedHoles patternParse
            let bindingType = bindingTypeMaker.Add(pattern, holes)
            ProvidedTypeBuilder.MakeGenericType(typedefof<list<_>>, [bindingType])
                
        let m2 = 
            ProvidedMethod(nm, [], returnType, InvokeCode= fun args ->
                QuotationHelper.quoteUnion patternParse <| fun patternParse ->
                <@@ let scopeParse = %%args.[0] : ParseTreeNode
                    Pattern.Matcher.findAllMatching scopeTypeName %%patternParse scopeParse
                    |> List.map BindingType  @@>)
        enclosingType.AddMember m2
        m2 )
    m

let findAllInstanceMethod targetTypeName targetType scopeTypeName =
    let m = 
        ProvidedMethod("FindAll" + GrammarTypes.pluralize targetTypeName, [], ProvidedTypeBuilder.MakeGenericType(typedefof<list<_>>, [targetType]),
            InvokeCode= fun args -> <@@ Pattern.Matcher.findAll targetTypeName (%%args.[0] : ParseTreeNode) @@>)
    m.AddXmlDoc(sprintf "Returns a list of all '%s' instances in the given '%s' tree." targetTypeName scopeTypeName)
    m

let findAllStaticMethod targetTypeName targetType scopeTypeName scopeType =
    let m = 
        ProvidedMethod("FindAll" + GrammarTypes.pluralize targetTypeName, [ProvidedParameter("haystack", scopeType)], 
            ProvidedTypeBuilder.MakeGenericType(typedefof<list<_>>, [targetType]), IsStaticMethod=true, 
            InvokeCode= fun args -> <@@ Pattern.Matcher.findAll targetTypeName (%%args.[0] : ParseTreeNode) @@>)
    m.AddXmlDoc(sprintf "Returns a list of all '%s' instances in the given '%s' tree." targetTypeName scopeTypeName)
    m

let private holeNameToType (typeName:string) (typeDict: Dictionary<string, ProvidedTypeDefinition>) =
    let trimSuffix (str:string) = str.TrimEnd('?', '+', '*', ',')
    if typeName.EndsWith("?") then 
        ProvidedTypeBuilder.MakeGenericType(typedefof<option<_>>, [typeDict.[trimSuffix typeName]])
    elif typeName.EndsWith("*") || typeName.EndsWith("+") then 
        ProvidedTypeBuilder.MakeGenericType(typedefof<list<_>>, [typeDict.[trimSuffix typeName]])
    else
        typeDict.[typeName] :> System.Type

let constructMethod tokenClasses (grammar: EbnfGrammar) topLevelGoal (typeDict: Dictionary<string, ProvidedTypeDefinition>) (enclosingType: ProvidedTypeDefinition) () =
    let m = ProvidedMethod("Construct", [], typeDict.[topLevelGoal], IsStaticMethod=true)
    m.AddXmlDoc(Documentation.constructMethod topLevelGoal)
    m.DefineStaticParameters([ProvidedStaticParameter("pattern", typeof<string>)], fun nm args ->
        let pattern = args.[0] :?> string                
        let patternParse = scanAndParseReplacement pattern tokenClasses grammar topLevelGoal
        let holeTypes = Replacement.findAllHoleTypes patternParse
        let parameters =
            holeTypes
            |> List.map (fun holeTypeName -> ProvidedParameter(holeTypeName, holeNameToType holeTypeName typeDict))
            
        // quotation builder function that builds a sequence of enqueue method call commands and ends with 'finalCommand', e.g.
        // <@@ queueObj.Enqueue(%%args.[0])
        //     queueObj.Enqueue(%%args.[1])
        //     queueObj.Enqueue(%%args.[2])
        //     %%finalCommand @@>
        let rec enqueueCommands queueObj finalCommand = function
            | [] -> finalCommand
            | (arg:Expr)::args ->
                let command = 
                    if arg.Type = typeof<ParseTreeNode> then
                        <@@ (%%queueObj : System.Collections.Queue).Enqueue(%%arg : ParseTreeNode) @@>
                    elif arg.Type = typeof<ParseTreeNode option> then
                        <@@ (%%queueObj : System.Collections.Queue).Enqueue(%%arg : ParseTreeNode option) @@>
                    elif arg.Type = typeof<ParseTreeNode list> then
                        <@@ (%%queueObj : System.Collections.Queue).Enqueue(%%arg : ParseTreeNode list) @@>
                    else
                        failwithf "Unexpected value of type '%s' passed to Construct method" arg.Type.Name
                    
                Expr.Sequential(command, enqueueCommands queueObj finalCommand args)

        let m2 = 
            ProvidedMethod(nm, parameters, typeDict.[topLevelGoal], IsStaticMethod=true, InvokeCode= fun args ->
                QuotationHelper.quoteUnion patternParse <| fun patternParse -> 
                let fillingsVar = Var("fillings", typeof<System.Collections.Queue>)
                let fillingsObjRef = Expr.Var fillingsVar
                Expr.Let(fillingsVar, <@@ System.Collections.Queue() @@>, 
                    enqueueCommands fillingsObjRef <@@ Replacement.construct %%fillingsObjRef (%%patternParse:ParseTreeNode) @@> args
                ))
        enclosingType.AddMember m2
        m2 )
    m

let applyOnePassInstanceMethod scopeType searchType searchTypeName =
    let parameter = ProvidedParameter("transformer", FSharpType.MakeFunctionType(searchType, searchType))
    let m = ProvidedMethod("ApplyOnePass", [parameter], scopeType,
                InvokeCode= fun args ->
                    <@@ let this = %%args.[0] : ParseTreeNode
                        let transformer = %%args.[1] : ParseTreeNode -> ParseTreeNode
                        Transform.applyOnePass searchTypeName transformer this @@> )
    m.AddXmlDoc(Documentation.applyOnePassMethod searchTypeName)
    m

let applyOnePassRepeatInstanceMethod scopeType searchType searchTypeName =
    let searchTypeList = ProvidedTypeBuilder.MakeGenericType(typedefof<list<_>>, [searchType])
    let parameter = ProvidedParameter("transformer", FSharpType.MakeFunctionType(searchTypeList, searchTypeList))
    let m = ProvidedMethod("ApplyOnePass", [parameter], scopeType,
                InvokeCode= fun args ->
                    <@@ let this = %%args.[0] : ParseTreeNode
                        let transformer = %%args.[1] : ParseTreeNode list -> ParseTreeNode list
                        Transform.applyOnePassRepeat searchTypeName transformer this @@> )
    m.AddXmlDoc(Documentation.applyOnePassMethod (searchTypeName + " list"))
    m

let applyOnePassStaticMethod scopeType searchType searchTypeName =
    let parameter = ProvidedParameter("transformer", FSharpType.MakeFunctionType(searchType, searchType))
    let m = ProvidedMethod("ApplyOnePass", [parameter], FSharpType.MakeFunctionType(scopeType, scopeType), IsStaticMethod=true,
                InvokeCode= fun args ->
                    <@@ let transformer = %%args.[0] : ParseTreeNode -> ParseTreeNode
                        Transform.applyOnePass searchTypeName transformer @@> )
    m.AddXmlDoc(Documentation.applyOnePassMethod searchTypeName)
    m

let applyOnePassRepeatStaticMethod scopeType searchType searchTypeName =
    let searchTypeList = ProvidedTypeBuilder.MakeGenericType(typedefof<list<_>>, [searchType])
    let parameter = ProvidedParameter("transformer", FSharpType.MakeFunctionType(searchTypeList, searchTypeList))
    let m = ProvidedMethod("ApplyOnePass", [parameter], FSharpType.MakeFunctionType(scopeType, scopeType), IsStaticMethod=true,
                InvokeCode= fun args ->
                    <@@ let transformer = %%args.[0] : ParseTreeNode list -> ParseTreeNode list
                        Transform.applyOnePassRepeat searchTypeName transformer @@> )
    m.AddXmlDoc(Documentation.applyOnePassMethod (searchTypeName + " list"))
    m

let applyRecursivelyStaticMethod scopeType searchType searchTypeName =
    let parameter = ProvidedParameter("transformer", FSharpType.MakeFunctionType(searchType, searchType))
    let m = ProvidedMethod("ApplyRecursively", [parameter], FSharpType.MakeFunctionType(scopeType, scopeType), IsStaticMethod=true,
                InvokeCode= fun args ->
                    <@@ let transformer = %%args.[0] : ParseTreeNode -> ParseTreeNode
                        Transform.applyRecursively searchTypeName transformer @@> )
    m.AddXmlDoc(Documentation.applyRecursivelyMethod searchTypeName)
    m

let applyRecursivelyInstanceMethod scopeType searchType searchTypeName =
    let parameter = ProvidedParameter("transformer", FSharpType.MakeFunctionType(searchType, searchType))
    let m = ProvidedMethod("ApplyRecursively", [parameter], scopeType,
                InvokeCode= fun args ->
                    <@@ let this = %%args.[0] : ParseTreeNode
                        let transformer = %%args.[1] : ParseTreeNode -> ParseTreeNode
                        Transform.applyRecursively searchTypeName transformer this @@> )
    m.AddXmlDoc(Documentation.applyRecursivelyMethod searchTypeName)
    m

let applyRecursivelyRepeatStaticMethod scopeType searchType searchTypeName =
    let searchTypeList = ProvidedTypeBuilder.MakeGenericType(typedefof<list<_>>, [searchType])
    let parameter = ProvidedParameter("transformer", FSharpType.MakeFunctionType(searchTypeList, searchTypeList))
    let m = ProvidedMethod("ApplyRecursively", [parameter], FSharpType.MakeFunctionType(scopeType, scopeType), IsStaticMethod=true,
                InvokeCode= fun args ->
                    <@@ let transformer = %%args.[0] : ParseTreeNode list -> ParseTreeNode list
                        Transform.applyRecursivelyRepeat searchTypeName transformer @@> )
    m.AddXmlDoc(Documentation.applyOnePassMethod (searchTypeName + " list"))
    m

let applyRecursivelyRepeatInstanceMethod scopeType searchType searchTypeName =
    let searchTypeList = ProvidedTypeBuilder.MakeGenericType(typedefof<list<_>>, [searchType])
    let parameter = ProvidedParameter("transformer", FSharpType.MakeFunctionType(searchTypeList, searchTypeList))
    let m = ProvidedMethod("ApplyRecursively", [parameter], scopeType,
                InvokeCode= fun args ->
                    <@@ let this = %%args.[0] : ParseTreeNode
                        let transformer = %%args.[1] : ParseTreeNode list -> ParseTreeNode list
                        Transform.applyRecursivelyRepeat searchTypeName transformer this @@> )
    m.AddXmlDoc(Documentation.applyOnePassMethod (searchTypeName + " list"))
    m

let containsMethod searchType =
    let m = ProvidedMethod("Contains", [ProvidedParameter("item", searchType)], typeof<bool>,
                InvokeCode= fun args ->
                    <@@ let this = %%args.[0] : ParseTreeNode
                        let target = %%args.[1] : ParseTreeNode
                        Pattern.Matcher.contains target this @@> )
    m.AddXmlDoc("Returns true if this object contains the given item in its parse tree, otherwise returns false.")
    m

let prettyMethod formatGrammar scopeType =
    let m = ProvidedMethod("Pretty", [ProvidedParameter("parseTree", scopeType)], typeof<string>, IsStaticMethod=true,
                InvokeCode= fun args -> 
                    QuotationHelper.quoteDict formatGrammar <| fun formatGrammar ->
                    <@@ Formatting.pretty %%formatGrammar (%%args.[0] : ParseTreeNode) @@>)
    m.AddXmlDoc("Unparses the given parse tree into a prettily formatted string")
    m

let addReachabilityMethods (grammar: EbnfGrammar) (tokenClasses: TokenClassDefinition list) (typeDict: Dictionary<string, ProvidedTypeDefinition>) bindingTypeMaker =
    let allSymbols = 
        [for nonterminalName in grammar.Keys -> nonterminalName]
        |> List.append (tokenClasses |> List.filter (GrammarAnalysis.isPunctuationOrKeyword >> not) |> List.map (fun tc -> tc.Name))
        |> List.toArray
    let reachabilityMatrix = GrammarAnalysis.reachabilityMatrix grammar allSymbols
    reachabilityMatrix
    |> Array2D.iteri (fun fromIndex toIndex -> function
        | true ->
            let scopeType = typeDict.[allSymbols.[fromIndex]]
            let targetType = typeDict.[allSymbols.[toIndex]]
            // add methods where the declaring type represents the scope (haystack)
            scopeType.AddMember(applyOnePassStaticMethod scopeType targetType targetType.Name)
            scopeType.AddMember(applyOnePassInstanceMethod scopeType targetType targetType.Name)
            scopeType.AddMember(applyRecursivelyStaticMethod scopeType targetType targetType.Name)
            scopeType.AddMember(applyRecursivelyInstanceMethod scopeType targetType targetType.Name)
            scopeType.AddMember(containsMethod targetType)
            scopeType.AddMember(findAllInstanceMethod targetType.Name targetType scopeType.Name)
            scopeType.AddMember(findAllStaticMethod targetType.Name targetType scopeType.Name scopeType)
            scopeType.AddMember(tryFindMethod bindingTypeMaker (Pattern.patternTokenClasses @ tokenClasses) grammar targetType.Name scopeType)
            scopeType.AddMember(findAllMatchingMethod bindingTypeMaker (Pattern.patternTokenClasses @ tokenClasses) grammar targetType.Name scopeType.Name scopeType)
        | false -> () )

    for nonterminalName in grammar.Keys do
        for reachedSymbol in GrammarAnalysis.reachableRepeatSymbols grammar grammar.[nonterminalName] do
            let scopeType = typeDict.[nonterminalName]
            let targetType = typeDict.[reachedSymbol]
            scopeType.AddMember(applyOnePassRepeatStaticMethod scopeType targetType targetType.Name)
            scopeType.AddMember(applyOnePassRepeatInstanceMethod scopeType targetType targetType.Name)
            scopeType.AddMember(applyRecursivelyRepeatStaticMethod scopeType targetType targetType.Name)
            scopeType.AddMember(applyRecursivelyRepeatInstanceMethod scopeType targetType targetType.Name)