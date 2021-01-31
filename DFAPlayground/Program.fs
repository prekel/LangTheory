[<EntryPoint>]
let main argv =
    let a1 =
        [ "00101010100"
          "0011"
          "01"
          "10"
          "1"
          "0"
          "001"
          "010"
          "100" ]

    let r1 =
        a1
        |> List.map (fun s -> (s, Lecture2Example1.verify s))

    let r2 =
        a1
        |> List.map (fun s -> (s, Lecture2Example1.verifyViaFold s))


    let r3 =
        a1
        |> List.map (fun s ->
            (s, CustomDfa.verifyCustomDfa Lecture2Example1.State_q0 Lecture2Example1.State_q2 Lecture2Example1.delta s))

    printfn "%A" [ r1; r2; r3 ]

    0
