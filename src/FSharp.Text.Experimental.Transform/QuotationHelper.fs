module QuotationHelper

open FSharp.Quotations
open FSharp.Reflection
open System.Collections.Generic

let rec private coerceValues fieldTypeLookup fields = 
    Array.mapi (fun i v ->
            let expr = 
                if v = null then simpleTypeExpr v
                elif FSharpType.IsUnion (v.GetType()) then unionExpr v |> snd
                elif FSharpType.IsRecord (v.GetType()) then recordExpr v |> snd
                elif FSharpType.IsTuple (v.GetType()) then tupleExpr v |> snd
                //elif v :? List<_> then listExpr v |> snd
                else simpleTypeExpr v
            Expr.Coerce(expr, fieldTypeLookup i)
    ) fields |> List.ofArray

and private simpleTypeExpr instance = Expr.Value(instance)

and private unionExpr instance = 
    let caseInfo, fields = FSharpValue.GetUnionFields(instance, instance.GetType())    
    let fieldInfo = caseInfo.GetFields()
    let fieldTypeLookup indx = fieldInfo.[indx].PropertyType
    caseInfo.DeclaringType, Expr.NewUnionCase(caseInfo, coerceValues fieldTypeLookup fields)

and private recordExpr instance = 
    let ty = instance.GetType()
    let fields = FSharpValue.GetRecordFields(instance)
    let fieldInfo = FSharpType.GetRecordFields(ty)
    let fieldTypeLookup indx = fieldInfo.[indx].PropertyType
    ty, Expr.NewRecord(instance.GetType(), coerceValues fieldTypeLookup fields)

and private tupleExpr instance =
    let ty = instance.GetType()
    let fields = FSharpValue.GetTupleFields(instance)
    let fieldInfo = FSharpType.GetTupleElements(ty)
    let fieldTypeLookup indx = fieldInfo.[indx]
    ty, Expr.NewTuple(coerceValues fieldTypeLookup fields)

and private arrayExpr (instance : 'a array) =
    let ty = typeof<'a>
    let arrayType = instance.GetType()
    let exprs = coerceValues (fun _ -> ty) (instance |> Array.map box)
    arrayType, Expr.NewArray(ty, exprs)

and private listExpr (instance: 'a list) =
    let caseInfo = FSharpType.GetUnionCases(typeof<'a list>)
    let emptyCase = caseInfo.[0]
    let consCase = caseInfo.[1]
    let rec toListExpr = function
        | [] -> Expr.NewUnionCase(emptyCase, [])
        | x::xs -> Expr.NewUnionCase(consCase, [x; toListExpr xs])
    let instanceExpr =
        instance
        |> List.map box
        |> List.toArray
        |> coerceValues (fun _ -> typeof<'a>)
        |> toListExpr
    typeof<'a list>, instanceExpr

/// Returns a quotation to build a new copy of a dictionary, like this:
///    <@@ let newDict = Dictionary<string, int>() :> IDictionary<'k,'v>
///        newDict.Add("1", 1)
///        newDict.Add("2", 2)
///        newDict.Add("3", 3) 
///        newDict @@>
/// A "newDict.Add(...)" method call will be included for each entry in the original
/// dictionary so the new dictionary contains the same elements as the original.
and private dictExpr (instance: IDictionary<'k,'v>) =
    let defaultConstructor = typeof<Dictionary<'k,'v>>.GetConstructor([||])
    let newDict = Expr.Coerce(Expr.NewObject(defaultConstructor, []), typeof<IDictionary<'k,'v>>) // new Dictionary<'k,'v>() :> IDictionary<'k,'v>
    if instance.Count = 0 then 
        typeof<IDictionary<'k,'v>>, newDict
    else
        let newDictVar = Var("newDict", typeof<IDictionary<'k,'v>>)
        let addMethod = typeof<IDictionary<'k,'v>>.GetMethod("Add")
        let rec toSequential = function
            | [key, value] -> Expr.Call(Expr.Var(newDictVar), addMethod, coerceValues (function 0 -> typeof<'k> | 1 -> typeof<'v>) [|key; value|])
            | x::xs -> Expr.Sequential(toSequential [x], toSequential xs)
        let body = Expr.Sequential([for entry in instance -> entry.Key, entry.Value] |> toSequential, Expr.Var newDictVar)
        typeof<IDictionary<'k,'v>>, Expr.Let(newDictVar, newDict, body)    

let private typeExpr (instance: System.Type) =
    let fsharpCoreAsm = System.Reflection.Assembly.GetAssembly(typeof<FSharp.Core.AbstractClassAttribute>)
    let operatorsType = fsharpCoreAsm.GetType("Microsoft.FSharp.Core.Operators")
    let genericMethodInfo = operatorsType.GetMethod("TypeOf")
    let methodInfo = genericMethodInfo.MakeGenericMethod(instance)
    typeof<System.Type>, Expr.Call(methodInfo, [])

let private createLetExpr varType instance body = 
    let var = Var("instance", varType)  
    Expr.Let(var, instance, body (Expr.Var var))

let quoteUnion instance = unionExpr instance ||> createLetExpr
let quoteRecord instance = recordExpr instance ||> createLetExpr
let quoteTuple instance = tupleExpr instance ||> createLetExpr
let quoteArray instance = arrayExpr instance ||> createLetExpr
let quoteList instance = listExpr instance ||> createLetExpr
let quoteDict instance = dictExpr instance ||> createLetExpr
let quoteType instance = typeExpr instance ||> createLetExpr
