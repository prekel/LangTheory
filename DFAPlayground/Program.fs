[<EntryPoint>]
let main _ =
    let a =
        [ "00101010100"
          "0011"
          "01"
          "10"
          "1"
          "0"
          "001"
          "010"
          "100"
          ""
          "00000011" ]

    let v =
        [ Lecture2Example1.verify
          Lecture2Example1.verifyViaFold
          CustomDfa.verifyCustomDfa Lecture2Example1.State_q0 Lecture2Example1.State_q2 Lecture2Example1.delta
          Lecture1Example2.verify
          Seq.map Lecture1Example2.charToAlphabet
          >> CustomDfa.verifyCustomDfa1 Lecture1Example2.dfa ]

    let r =
        a
        |> List.map (fun s -> (s, v |> List.map (fun f -> f s)))

    printfn "%A" r

    let a2 =
        a
        |> List.map (fun s -> s.Replace('0', 'a').Replace('1', 'b'))
        |> List.append [ "aaa"; "aaba" ]

    let f =
        Seq.map (function
            | 'a' -> Lab01Var01.A
            | 'b' -> Lab01Var01.B
            | _ -> failwith "never")
        >> CustomDfa.verifyCustomDfa2 Lab01Var01.Dfa

    let f1 =
        Seq.map (function
            | 'a' -> Lab01Var01.A
            | 'b' -> Lab01Var01.B
            | _ -> failwith "never")
        >> CustomDfa.verifyCustomDfa4 Lab01Var01.Dfa44

    printfn "%A" (a2 |> List.map (fun c -> (c, f1 c)))
    
    
    let f =
        Seq.map (function
            | '0' -> Lab01Var08a.A0
            | '1' -> Lab01Var08a.A1
            | _ -> failwith "never")
        >> CustomDfa.verifyCustomDfa2 Lab01Var08a.Dfa 

    printfn "%A" (a |> List.map (fun c -> (c, f c)))
    
    let f = CustomDfa.stateDfa4 Else.Dfa 

    printfn "%A" (["else"; "elsr" ; "elser" ; "els" ] |> List.map (fun c -> (c, f c)))

    0
