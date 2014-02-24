// Learn more about F# at http://fsharp.net

let reverse list =
    let rec loop list result =
        match list with
        | head :: tail ->
            let result = head :: result
            loop list.Tail result
        | [] -> result
    loop list []
printfn "%A \n" (reverse [1 .. 10])

let pow = List.init 5 (fun index -> 2.0 **(float index))
printf "%A \n" (pow)

let rec multiplyDigitals n =
    if n < 10 then n else n % 10 * multiplyDigitals(n / 10)
printf "\n %A \n" (multiplyDigitals 22222)

let firstEntry list n =
    let rec loop list n depth =
        if List.isEmpty list then None else
            if ( n = List.head list) then Some(depth)
                else loop(List.tail list) n (depth + 1)
    loop list n 1
printf "\n %A \n" (firstEntry[5; 3; 8; 3] 3)
printf "\n %A \n" (firstEntry[1; 2; 3] 7)

let rec isPalindrom str =
    let string = String.length str
    match string with
    | 0 | 1 -> true
    |_ -> if (str.[0] = str.[string - 1]) then isPalindrom(str.[1..string - 2]) else false
System.Console.WriteLine(isPalindrom("aaajjjkjkjjjaaa"))