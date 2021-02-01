module Lab01Var08a

type Q =
    | Q0
    | Q1
    | Q2
    | Q3
    | Q4
    | Q5
    | Q6

type A =
    | A0
    | A1


let delta =
    function
    | Q0 ->
        function
        | A0 -> Q1
        | A1 -> Q0
    | Q1 ->
        function
        | A0 -> Q2
        | A1 -> Q0
    | Q2 ->
        function
        | A0 -> Q6
        | A1 -> Q3
    | Q3 ->
        function
        | A0 -> Q4
        | A1 -> Q3
    | Q4 ->
        function
        | A0 -> Q5
        | A1 -> Q3
    | Q5 ->
        function
        | A0 -> Q6
        | A1 -> Q3
    | Q6 ->
        function
        | A0 -> Q6
        | A1 -> Q6
        
let deltaSome =
    function
    | Q0 ->
        function
        | A0 -> Some Q1
        | A1 -> Some Q0
    | Q1 ->
        function
        | A0 -> Some Q2
        | A1 -> Some Q0
    | Q2 ->
        function
        | A0 -> Some Q6
        | A1 -> Some Q3
    | Q3 ->
        function
        | A0 -> Some Q4
        | A1 -> Some Q3
    | Q4 ->
        function
        | A0 -> Some Q5
        | A1 -> Some Q3
    | Q5 ->
        function
        | A0 -> Some Q6
        | A1 -> Some Q3
    | Q6 ->
        function
        | A0 -> Some Q6
        | A1 -> Some Q6

open CustomDfa

let Dfa =
    { Delta = delta
      Initial = Q0
      Final = Set.ofList [ Q2; Q3; Q4; Q5 ] }
