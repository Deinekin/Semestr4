//task number 2
let rhombus n =
    let rec anotherRhombus n length step =
        let list = [ for i in 1 .. (2 * n - 1) -> if (i <= (n - length) || i >= (n + length)) then ' ' else '*' ]
        printfn "%s" (System.String.Concat(Array.ofList(list)))
        if (step < 2*n) then
            if (step < n) then (anotherRhombus n (length + 1) (step + 1)) else (anotherRhombus n (length - 1) (step + 1))
    anotherRhombus n 0 0
rhombus 6
rhombus 5
rhombus 4

///task number 3
open System

exception EmptyQueueException

type Queue<'a>() =
    let mutable list : 'a list = []
    member this.Enqueue elem = 
        list <- list @ [elem]
    member this.Dequeue = 
        match list with
        | hd :: tl -> 
            list <- tl
            hd
        | [] -> raise EmptyQueueException
    member this.Count = List.length list

let queue = Queue<int>()
for i = 1 to 5 do
    queue.Enqueue (i)
printfn "%A" queue.Count
for i = 1 to queue.Count do
    printfn "%A" queue.Dequeue
printfn "%A" queue.Count


//task number 1 :(

let sinMap list func = List.sumBy (func) list
printfn "%A" (sinMap [1.0; 2.0; 3.0] (fun x -> sin x))