module Nfa

type State =
    | Initial // Q0
    | Q1A
    | Q1B
    | Q1C
    | Q2A
    | Q2B
    | Q2C
    | Q3A
    | Q3B
    | Q3C
    | Q4A
    | Q4B
    | Q4C
    | Q5A
    | Q5B
    | Q5C
    | Q6A
    | Q6B
    | Q6C
    | Q7A
    | Q7B
    | Q7C
    | Final // QF

type Alphabet =
    | A0 // 0
    | A1 // 1

let delta q a =
    match q with
    | Initial ->
        match a with
        | A0 -> Set.ofList [ Q1A; Q2A; Q3A; Q4A ]
        | A1 -> Set.ofList [ Q5A; Q6A; Q7A ]
    | Q1A ->
        match a with
        | A0 -> Set.singleton Q1A
        | A1 -> Set.singleton Q1B
    | Q1B ->
        match a with
        | A0 -> Set.singleton Q1C
        | A1 -> Set.empty
    | Q1C ->
        match a with
        | A0 -> Set.singleton Q1C
        | A1 -> Set.singleton Final
    | Q2A ->
        match a with
        | A0 -> Set.singleton Q2B
        | A1 -> Set.empty
    | Q2B ->
        match a with
        | A0 -> Set.singleton Q2B
        | A1 -> Set.singleton Q2C
    | Q2C ->
        match a with
        | A0 -> Set.empty
        | A1 -> Set.singleton Final
    | Q3A ->
        match a with
        | A0 -> Set.singleton Q3A
        | A1 -> Set.singleton Q3B
    | Q3B ->
        match a with
        | A0 -> Set.singleton Q3C
        | A1 -> Set.empty
    | Q3C ->
        match a with
        | A0 -> Set.empty
        | A1 -> Set.singleton Final
    | Q4A ->
        match a with
        | A0 -> Set.singleton Q4A
        | A1 -> Set.singleton Q4B
    | Q4B ->
        match a with
        | A0 -> Set.empty
        | A1 -> Set.singleton Q4C
    | Q4C ->
        match a with
        | A0 -> Set.singleton Final
        | A1 -> Set.empty
    | Q5A ->
        match a with
        | A0 -> Set.singleton Q5B
        | A1 -> Set.empty
    | Q5B ->
        match a with
        | A0 -> Set.singleton Q5B
        | A1 -> Set.empty
    | Q5C ->
        match a with
        | A0 -> Set.empty
        | A1 -> Set.singleton Final
    | Q6A ->
        match a with
        | A0 -> Set.singleton Q6B
        | A1 -> Set.empty
    | Q6B ->
        match a with
        | A0 -> Set.empty
        | A1 -> Set.singleton Q6C
    | Q6C ->
        match a with
        | A0 -> Set.singleton Final
        | A1 -> Set.empty
    | Q7A ->
        match a with
        | A0 -> Set.empty
        | A1 -> Set.singleton Q7B
    | Q7B ->
        match a with
        | A0 -> Set.singleton Q7C
        | A1 -> Set.empty
    | Q7C ->
        match a with
        | A0 -> Set.singleton Final
        | A1 -> Set.empty
    | Final ->
        match a with
        | A0 -> Set.singleton Final
        | A1 -> Set.empty

let charToAlphabet c =
    match c with
    | '0' -> A0
    | '1' -> A1
    | _ -> failwith "Неверный символ"

open Fa

let Nfa =
    { Nfa.Transition = delta
      Initial = Initial
      Final = Set.singleton Final }
