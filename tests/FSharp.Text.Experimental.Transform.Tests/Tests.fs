module FSharp.Text.Experimental.Transform.Tests

open FSharp.Text.Experimental.Transform
open NUnit.Framework

[<Literal>]
let grammar = """
    ignore terminal LineComment : "//.*?\n";
    ignore terminal BlockComment: "/\*.*?\*/";

    Program         :   Statement*;
    Statement       :   AssignStatement | IfStatement;
    AssignStatement :   Id '=' Expr / ;
    IfStatement     :   'if' Expr 'then' /
                    >       Statement
                    <   'else'           /
                    >       Statement    <;
    Expr            :   Id TernarySuffix? ;
    TernarySuffix   :   '?' Expr ':' Expr;
"""

type Test = GrammarProvider<grammar>
let parseResult = Test.ParseString("""
    if true then
        if isActive then
            max = a
        else
            max = b
    else
        max = false
""")

let assignStmt = Test.AssignStatement.Construct<"max = a">()
let stmt = Test.Statement.Construct<"$AssignStatement">(assignStmt)
let prog = Test.Program.Construct<"$Statement">(stmt)

let ifToTernary (inp: Test.Statement) =
    inp.TryMatch<"""
        if cond$Expr then 
            var$Id = trueExpr$Expr 
        else 
            var$Id = falseExpr$Expr """>()
    |> function
        | Some b -> Test.Statement.Construct<"$Id = $Id ? $Expr : $Expr">(b.Var, b.Cond.Id, b.TrueExpr, b.FalseExpr)
        | None -> inp

parseResult
|> Test.Program.ApplyRecursively ifToTernary
|> Test.Pretty
|> printfn "%s"

//[<Test>]
//let ``hello returns 42`` () =
//  let result = Library.hello 42
//  printfn "%i" result
//  Assert.AreEqual(42,result)

[<Literal>]
let hwGrammar = """
    Program :   Greeting 'world';
    Greeting:   'welcome' | 'hello';
"""

type HW = GrammarProvider<hwGrammar>

let welcomeToHello (inp: HW.Greeting) =
    inp.TryMatch<"welcome">()
    |> function
        | Some _ -> HW.Greeting.Construct<"hello">()
        | None -> inp

HW.ParseString("welcome world")
|> HW.Program.ApplyOnePass welcomeToHello
|> HW.Pretty
|> printfn "%s"
