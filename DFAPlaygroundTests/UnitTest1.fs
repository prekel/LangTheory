module DFAPlaygroundTests

open System
open NUnit.Framework

[<SetUp>]
let Setup () = ()

[<Test>]
[<Ignore(")")>]
let Test1 () =
    let dfa = Lab01Var01b.Dfa
    let random = Random()

    let cases: seq<char> list =
        [ "12312312212312123" |> seq
          "121212122212121" |> seq
          "12121221212112" |> seq
          "2323131313" |> seq
          "313131313132" |> seq
          "121212121323" |> seq
          "1223112111112" |> seq
          "111111111111" |> seq
          "111111111131" |> seq
          "2222222222" |> seq
          "22222223" |> seq
          "113111111112" |> seq ]
        |> List.append
            ([ 0 .. 1000 ]
             |> List.map (fun _ ->
                 [ 0 .. random.Next(5) ]
                 |> List.map (fun _ ->
                     match random.Next(1, 3) with
                     | 1 -> '1'
                     | 2 -> '2'
                     | 3 -> '3')
                 |> List.toSeq))

    cases
    |> List.iter (fun case ->
        Assert.That
            (CustomDfa.verifyCustomDfa4
                dfa
                 (case
                  |> Seq.map (function
                      | '1' -> Lab01Var01b.A1
                      | '2' -> Lab01Var01b.A2
                      | '3' -> Lab01Var01b.A3)),
             Is.EqualTo
                 (case
                  |> (fun case -> case |> Seq.take ((case |> Seq.length) - 1))
                  |> Seq.contains (case |> Seq.last))))
