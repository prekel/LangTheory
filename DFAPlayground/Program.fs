
[<EntryPoint>]
let main argv =
    let a1 = ["00101010100"
              "0011"
              "01"
              "10"
              "1"
              "0"
              "001"
              "010"
              "100"]
    let r = a1 |> List.map (fun s -> (s, Lecture2Example1.verify s))
                            
    printfn "%A" r
    0
