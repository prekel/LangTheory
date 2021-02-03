module Lab1bTests

open System
open NUnit.Framework

let nToStr (n: int) len =
    let nonull = Convert.ToString(n, 2)
    (String.init (len - nonull.Length) (fun _ -> "0")) + nonull

let isV (s:string) =
    let zeros = s |> Seq.filter (fun c -> c = '0') |> Seq.length
    let ones = s |> Seq.filter (fun c -> c = '1') |> Seq.length
    zeros >= 2 && ones = 2


[<Test>]
let test () =
    let lens = [1..19]
    let r = lens |> List.collect (fun l -> [0 .. (1 <<< l) - 1] |> List.map (fun n -> nToStr n l))
    let verify str = Fa.verifyNfa Nfa.Nfa (str |> Seq.map Nfa.charToAlphabet)
    List.iter (fun i ->
        Assert.That((verify i, i), Is.EqualTo((isV i, i)))) r
    