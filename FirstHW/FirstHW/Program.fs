// Learn more about F# at http://fsharp.net
let rec fact n =
    if n = 0 then 1
    else n * fact(n - 1)
printf "%d \n" (fact 10)

let rec fact1 n = [1..n] |> List.fold (*) 1
printf "%d\n" (fact1 10)

let rec fact2 n = List.fold (*) 1 [1..n]
printf "\n%d" (fact2 10)

let rec fibonacci n =
    match n with
    | 0 -> 1
    | 1 -> 1
    | _ -> fibonacci (n - 1) + (fibonacci (n - 2))
printf "\n%d" (fibonacci 10)

let fib n =
    let fibSeq =
        Seq.unfold (fun (a,b) -> Some( a+b, (b, a+b) ) ) ( 0, 1)
    match n with
    | 1 -> 1
    | 2 -> 1
    | _ -> fibSeq |> Seq.nth ( n - 1 )
printf "\n %d \n" (fib 8)


