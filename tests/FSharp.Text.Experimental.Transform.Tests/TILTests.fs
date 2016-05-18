module FSharp.Text.Experimental.Transform.TILTests

// System.Environment.CurrentDirectory <- @"C:\Users\Andrew\Documents\Visual Studio 2015\Projects\FSharp.Text.Experimental.Transform\tests\FSharp.Text.Experimental.Transform.Tests"

open FSharp.Text.Experimental.Transform
open NUnit.Framework
open System.IO

type TIL = GrammarProvider<"../../grammars/til.grm">

// define active patterns for statement types to simplify and improve the readability of our transformation functions
[<AutoOpen>]
module HelperPatterns =
    let (|Declaration|_|) (stmt: TIL.Statement) =
        match stmt.TryMatch<"var name$Id;">() with
        | Some b -> Some b.Name
        | None   -> None

    let (|ForLoop|_|) (stmt: TIL.Statement) =
        stmt.TryMatch<"""
            for loopVar$Id := lower$Expression to upper$Expression do
                body$Statement*
            end """>()
        |> function
            | Some b -> Some(b.LoopVar, b.Lower, b.Upper, b.Body)
            | None   -> None

    let (|WhileLoop|_|) (stmt: TIL.Statement) =
        stmt.TryMatch<"""
            while cond$Expression do
                body$Statement*
            end """>()
        |> function
            | Some b -> Some(b.Cond, b.Body)
            | None   -> None

    let (|IfThen|_|) (stmt: TIL.Statement) =
        stmt.TryMatch<"""
            if cond$Expression then
                body$Statement*
            end """>()
        |> function
            | Some b -> Some(b.Cond, b.Body)
            | None   -> None

    let (|IfElse|_|) (stmt: TIL.Statement) =
        stmt.TryMatch<"""
            if cond$Expression then
                trueBody$Statement*
            else
                falseBody$Statement*
            end """>()
        |> function
            | Some b -> Some(b.Cond, b.TrueBody, b.FalseBody)
            | None   -> None

let INPUT_DIRECTORY = __SOURCE_DIRECTORY__ + "../../testdata/input"
let EXPECTED_OUTPUT_DIRECTORY = __SOURCE_DIRECTORY__ + "../../testdata/expectedoutput"

[<Test>]
let ``TIL Chairmark 2.1`` () =      // declare loop variables explicitly
    let rec forToNondeclaringFor (inp: TIL.Statement) =
        match inp with
        | ForLoop(loopVar, lower, upper, body) ->
            [ TIL.Statement.Construct<"var $Id;">(loopVar)
              TIL.Statement.Construct<"""
                for $Id := $Expression to $Expression do
                    $Statement*
                end """>(loopVar, lower, upper, body |> List.collect forToNondeclaringFor) ]
        | _ -> [inp]

    let actual = 
        TIL.ParseFile(INPUT_DIRECTORY + "/chairmark2.1.txt")
        |> TIL.Program.ApplyOnePass (List.collect forToNondeclaringFor)
        |> TIL.Pretty
    Assert.AreEqual(File.ReadAllText(EXPECTED_OUTPUT_DIRECTORY + "/chairmark2.1.txt"), actual)

[<Test>]
let ``TIL Chairmark 2.2`` () =      // convert for loops to while loops
    let rec forToWhile (inp: TIL.Statement) =
        match inp with
        | ForLoop(loopVar, lower, upper, body) ->
            let upperBoundVar = TIL.Id.Unique "upper"
            let newBody = (body |> List.collect forToWhile) @ [TIL.Statement.Construct<"$Id := $Id + 1;">(loopVar, loopVar)]
            [   TIL.Statement.Construct<"var $Id;">(loopVar)
                TIL.Statement.Construct<"$Id := $Expression;">(loopVar, lower)
                TIL.Statement.Construct<"var $Id;">(upperBoundVar)
                TIL.Statement.Construct<"$Id := ($Expression) + 1;">(upperBoundVar, upper)
                TIL.Statement.Construct<"while $Id - $Id do $Statement* end">(loopVar, upperBoundVar, newBody) ]
        | _ -> [inp]

    let actual = 
        TIL.ParseFile(INPUT_DIRECTORY + "/chairmark2.2.txt")
        |> TIL.Program.ApplyRecursively (List.collect forToWhile)
        |> TIL.Pretty
    Assert.AreEqual(File.ReadAllText(EXPECTED_OUTPUT_DIRECTORY + "/chairmark2.2.txt"), actual)

[<Test>]
let ``TIL Chairmark 2.3`` () =      // declare all variables in global scope
    let removeDeclarations (inp: TIL.Statement list) =
        inp |> List.filter (function Declaration _ -> false | _ -> true)

    let program = TIL.ParseFile(INPUT_DIRECTORY + "/chairmark2.3.txt")
    let declarations = program.FindAllDeclarations() |> List.map (fun decl -> TIL.Statement.Construct<"$Declaration"> decl)
    let programWithoutDeclarations = program.ApplyRecursively removeDeclarations
    let actual = TIL.Program.Construct<"$Statement*">(declarations @ programWithoutDeclarations.Statements).ToString()

    Assert.AreEqual(File.ReadAllText(EXPECTED_OUTPUT_DIRECTORY + "/chairmark2.3.txt"), actual)

[<Test>]
let ``TIL Chairmark 2.4`` () =      // move all variables to most local scope
    let rec localizeDeclarations (inp: TIL.Statement list) =
        match inp with
        // move declarations past statements that don't contain the variable
        | ((Declaration varName) as stmt1)::stmt2::tail when not (stmt2.Contains varName) ->
            stmt2::(localizeDeclarations (stmt1::tail))
        // inject declarations inside a for loop if the statements following the for loop don't contain the variable
        | ((Declaration varName) as stmt1)::(ForLoop(loopVar, lower, upper, body))::tail 
            when varName <> loopVar 
              && not (lower.Contains varName) 
              && not (upper.Contains varName) 
              && tail |> List.forall (fun s -> not (s.Contains varName)) ->
                (TIL.Statement.Construct<"""
                    for $Id := $Expression to $Expression do
                        $Statement*
                    end """>(loopVar, lower, upper, localizeDeclarations(stmt1::body)))::tail
        // inject declarations inside a while loop if the statements following the while loop don't contain the variable
        | ((Declaration varName) as stmt1)::(WhileLoop(cond, body))::tail 
            when not (cond.Contains varName) && tail |> List.forall (fun s -> not (s.Contains varName)) ->
                (TIL.Statement.Construct<"""
                    while $Expression do
                        $Statement*
                    end """>(cond, localizeDeclarations(stmt1::body)))::tail
        // inject declarations inside an 'if-then' statement if the statements following don't contain the variable
        | ((Declaration varName) as stmt1)::(IfThen(cond, body))::tail 
            when not (cond.Contains varName) && tail |> List.forall (fun s -> not (s.Contains varName)) ->
                (TIL.Statement.Construct<"""
                    if $Expression then
                        $Statement*
                    end """>(cond, localizeDeclarations(stmt1::body)))::tail
        // inject declarations inside the true branch of an 'if-then-else' statement if neither the false branch statements 
        // nor the statements following the if statement contain the variable
        | ((Declaration varName) as stmt1)::(IfElse(cond, trueBody, falseBody))::tail 
            when not (cond.Contains varName) 
              && tail |> List.forall (fun s -> not (s.Contains varName)) 
              && trueBody |> List.forall (fun s -> not (s.Contains varName)) ->
                (TIL.Statement.Construct<"""
                    if $Expression then
                        $Statement*
                    else
                        $Statement*
                    end """>(cond, trueBody, localizeDeclarations(stmt1::falseBody)))::tail
        // inject declarations inside the false branch of an 'if-then-else' statement if neither the true branch statements 
        // nor the statements following the if statement contain the variable
        | ((Declaration varName) as stmt1)::(IfElse(cond, trueBody, falseBody))::tail 
            when not (cond.Contains varName) 
              && tail |> List.forall (fun s -> not (s.Contains varName)) 
              && falseBody |> List.forall (fun s -> not (s.Contains varName)) ->
                (TIL.Statement.Construct<"""
                    if $Expression then
                        $Statement*
                    else
                        $Statement*
                    end """>(cond, localizeDeclarations(stmt1::trueBody), falseBody))::tail
        | stmt::tail -> stmt::(localizeDeclarations tail)
        | [] -> []

    let actual = 
        TIL.ParseFile(INPUT_DIRECTORY + "/chairmark2.4.txt")
        |> TIL.Program.ApplyRecursively localizeDeclarations
        |> TIL.Pretty

    Assert.AreEqual(File.ReadAllText(EXPECTED_OUTPUT_DIRECTORY + "/chairmark2.4.txt"), actual)