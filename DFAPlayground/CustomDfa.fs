module CustomDfa

let verifyCustomDfa start finish delta str =
    (start, str)
    ||> Seq.fold delta
    |> (=) finish
