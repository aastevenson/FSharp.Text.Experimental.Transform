namespace FSharp.Text.Experimental.Transform

open ProviderImplementation.ProvidedTypes
open FSharp.Core.CompilerServices
open System.Collections.Generic
open System.Reflection
open System.IO
open GrammarTypes
open Parser
open Lexer

[<AutoOpen>]
module Helpers =    
    let (|FilePathFormat|GrammarFormat|) (str: string) =
        if str.Contains("\n") || str.Contains(";") then GrammarFormat else FilePathFormat

    let containsTokenClassName (tokenClasses: Lexer.TokenClassDefinition list) tokenName = 
        tokenClasses
        |> List.map (fun tokenClassDef -> tokenClassDef.Name) 
        |> List.contains tokenName

module ProviderRuntime =
    let scanAndParse inputText tokenClasses grammar =
        match Lexer.tokenize tokenClasses inputText with
        | UnmatchableText failureLocation ->
            failwithf "Syntax error scanning input %s. Cannot match any of the following token class regular expressions:\n%A"
                (Errors.scanMessage failureLocation inputText) tokenClasses
        | SuccessfulScan tokens ->

        let isObjectToken = containsTokenClassName tokenClasses

        match Parser.parse grammar isObjectToken tokens with
        | SyntaxError failureToken ->
            failwithf "Syntax error parsing input %s" (Errors.parseMessage failureToken inputText)
        | UnconsumedInputError failureToken ->
            failwithf "Syntax error. Parsing completed with unconsumed input %s" (Errors.parseMessage failureToken inputText)
        | Success parseTree -> parseTree

[<TypeProvider>]
type GrammarProvider(_config: TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces()

    let namespaceName = "FSharp.Text.Experimental.Transform"
    let thisAssembly = Assembly.GetExecutingAssembly()
    
    /// Stores the root provided types for each nonterminal in the grammar.
    /// These root types are available as nested types of the instantiated type.
    let typeDict = Dictionary<string, ProvidedTypeDefinition>()

    let createTypesForStaticParameters(typeName, grammar) =
        let grammarText =
            match grammar with
            | FilePathFormat -> File.ReadAllText(grammar)
            | GrammarFormat -> grammar

        let isBootstrapToken = containsTokenClassName Bootstrap.bootstrapTokenClasses 

        // scan and parse object grammar using bootstrap grammar        
        let bootstrapScanResult = Lexer.tokenize Bootstrap.bootstrapTokenClasses grammarText
        match bootstrapScanResult with
        | UnmatchableText failureLocation -> failwithf "Error scanning grammar %s" (Errors.scanMessage failureLocation grammarText)
        | SuccessfulScan tokens ->

        let bootstrapParseResult = Parser.parse Bootstrap.bootstrapGrammar isBootstrapToken tokens
        
        let bootstrapGrammarParse =
            match bootstrapParseResult with
            | Success parseTree -> parseTree
            | SyntaxError failureToken
            | UnconsumedInputError failureToken ->
                failwithf "Syntax error in grammar %s" (Errors.parseMessage failureToken grammarText)

        let objectTokenClasses, objectGrammarWithFormatting = Bootstrap.compileGrammar bootstrapGrammarParse
        let objectGrammar = Formatting.unformatEbnfGrammar objectGrammarWithFormatting
        let isObjectToken = containsTokenClassName objectTokenClasses 

        // ensure all symbols are defined in the grammar
        match GrammarAnalysis.tryFindUndefinedSymbol objectGrammar isObjectToken with
        | Some symbolName -> failwithf "Symbol '%s' is not defined in the grammar" symbolName
        | None ->

        // ensure there exists a start symbol in the grammar
        if not (objectGrammar.ContainsKey "Program") then failwith "Start symbol 'Program' is not defined in the grammar"
        else

        // ensure there are no left-recursive productions in the grammar
        match GrammarAnalysis.tryFindLeftRecursion objectGrammar with
        | Some cycle ->
            cycle
            |> String.concat " -> "
            |> failwithf "The grammar must not contain any direct or indirect left-recursive cycles: %s"
        | None ->

        let bindingTypeContainer = ProvidedTypeDefinition("BindingTypes", Some typeof<obj>)
        let bindingTypeMaker = GrammarTypeMethods.BindingTypeMaker(bindingTypeContainer, typeDict)

        /// Used to make provided types that represent grammar nonterminals
        let rootTypeMaker nonterminalName =
            let ty = ProvidedTypeDefinition(nonterminalName, Some typeof<ParseTreeNode>, HideObjectMethods=true)
            ty.AddMember(ProvidedMethod("ToString", [], typeof<string>, InvokeCode= fun args -> 
                QuotationHelper.quoteDict objectGrammarWithFormatting <| fun objectGrammarWithFormatting ->
                <@@ Formatting.pretty %%objectGrammarWithFormatting (%%args.[0] : ParseTreeNode) @@> ))
            ty

        // Build the provided types based on the object grammar structure
        typeDict.Clear()
        objectTokenClasses
        |> List.filter (GrammarAnalysis.isPunctuationOrKeyword >> not)
        |> List.map (fun {Name=name; Regex=regex; Ignored=_} -> name, tokenClassToType(name, regex))
        |> List.iter typeDict.Add
        
        for production in objectGrammar do
            let nonterminalName, rootNode = production.Key, production.Value
            let providedRootType = rootNodeToType nonterminalName rootTypeMaker rootNode typeDict
            providedRootType.AddXmlDoc(sprintf "This type represents the grammar nonterminal definition %s" (nodeAsString rootNode))
            providedRootType.AddMemberDelayed(GrammarTypeMethods.tryMatchMethod bindingTypeMaker (Pattern.patternTokenClasses @ objectTokenClasses) objectGrammar nonterminalName providedRootType)
            providedRootType.AddMemberDelayed(GrammarTypeMethods.constructMethod (Replacement.patternTokenClasses @ objectTokenClasses) objectGrammar nonterminalName typeDict providedRootType)
            typeDict.Add(nonterminalName, providedRootType)

        // add provided methods involving a reachability analysis
        GrammarTypeMethods.addReachabilityMethods objectGrammar objectTokenClasses typeDict bindingTypeMaker

        let parseStringMethod = 
            let m = 
                ProvidedMethod("ParseString", 
                    [ProvidedParameter("InputString", typeof<string>)], 
                    typeDict.["Program"], 
                    IsStaticMethod=true, 
                    InvokeCode= fun args ->
                        QuotationHelper.quoteList objectTokenClasses <| fun objectTokenClasses -> 
                        QuotationHelper.quoteDict objectGrammar      <| fun objectGrammar -> 
                        <@@ let inputString = %%(args.[0]) : string
                            ProviderRuntime.scanAndParse inputString %%objectTokenClasses %%objectGrammar @@> )
            m.AddXmlDoc("Parses an input string according to the grammar. Returns a Program object representing a parse tree of the input.")
            m

        let parseFileMethod = 
            let m = 
                ProvidedMethod("ParseFile", 
                    [ProvidedParameter("FilePath", typeof<string>)], 
                    typeDict.["Program"], 
                    IsStaticMethod=true, 
                    InvokeCode= fun args ->
                        QuotationHelper.quoteList objectTokenClasses <| fun objectTokenClasses -> 
                        QuotationHelper.quoteDict objectGrammar      <| fun objectGrammar -> 
                        <@@ let filePath = %%(args.[0]) : string
                            let inputString = File.ReadAllText filePath
                            ProviderRuntime.scanAndParse inputString %%objectTokenClasses %%objectGrammar @@> )
            m.AddXmlDoc("Parses an input file according to the grammar. Returns a Program object representing a parse tree of the input.")
            m

        let instantiatedTy = ProvidedTypeDefinition(thisAssembly, namespaceName, typeName, Some typeof<obj>)
        instantiatedTy.AddMember parseStringMethod
        instantiatedTy.AddMember parseFileMethod
        instantiatedTy.AddMember bindingTypeContainer
        instantiatedTy.AddMembers [for rootType in typeDict.Values -> rootType]
        instantiatedTy.AddMembers [for rootType in typeDict.Values -> GrammarTypeMethods.prettyMethod objectGrammarWithFormatting rootType]
        instantiatedTy

    let paramGrammarType =
        let ty = ProvidedTypeDefinition(thisAssembly, namespaceName, "GrammarProvider", Some typeof<obj>)
        let grammarParam = ProvidedStaticParameter("Grammar", typeof<string>)
        ty.DefineStaticParameters([grammarParam], fun typeName parameterValues ->
            match parameterValues with
            | [| :? string as grammar|] -> 
                createTypesForStaticParameters(typeName, grammar)
            | _ -> failwith "unexpected static parameter values, expected (string)")
        ty

    // add the root items to the namespace
    do this.AddNamespace(namespaceName, [paramGrammarType])

[<assembly:TypeProviderAssembly>] 
do()