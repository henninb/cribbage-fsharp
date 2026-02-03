open System

type Suit = Clubs | Diamonds | Hearts | Spades

type Rank = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King

type Card = Rank * Suit

type Hand = Card list
type Deck = Card list

let suits = [ Hearts; Spades; Clubs; Diamonds ]
let ranks = [ Ace; Two; Three; Four; Five; Six; Seven; Eight; Nine; Ten; Jack; Queen; King ]

let unshuffledDeck =
    [ for suit in suits do
          for rank in ranks do
              yield Card(rank, suit) ]

let shuffle deck =
    let random = Random()
    deck |> List.sortBy (fun _ -> random.Next())

let getRank (card: Card) = fst card

let getSuit (card: Card) = snd card

let cardValue card =
    match getRank card with
    | Ace -> 1
    | Two -> 2
    | Three -> 3
    | Four -> 4
    | Five -> 5
    | Six -> 6
    | Seven -> 7
    | Eight -> 8
    | Nine -> 9
    | Ten | Jack | Queen | King -> 10

let rankOrder card =
    match getRank card with
    | Ace -> 1
    | Two -> 2
    | Three -> 3
    | Four -> 4
    | Five -> 5
    | Six -> 6
    | Seven -> 7
    | Eight -> 8
    | Nine -> 9
    | Ten -> 10
    | Jack -> 11
    | Queen -> 12
    | King -> 13

let rec combinations (cards: Card list) : Card list list =
    match cards with
    | [] -> [ [] ]
    | x :: xs ->
        let rest = combinations xs
        rest @ (rest |> List.map (fun subset -> x :: subset))

let subsets (cards: Card list) : Card list list =
    cards
    |> combinations
    |> List.tail
    |> List.sort

let scoreFifteen (cards: Card list) : int =
    if (cards |> List.sumBy cardValue) = 15 then 2 else 0

let scorePair (cards: Card list) : int =
    match cards with
    | [ c1; c2 ] when getRank c1 = getRank c2 -> 2
    | _ -> 0

let scoreSubset (cards: Card list) : int =
    scoreFifteen cards + scorePair cards

let scoreRun (cards: Card list) : int =
    if List.length cards < 3 then
        0
    else
        let sorted = cards |> List.sortBy rankOrder
        let isConsecutive =
            sorted
            |> List.pairwise
            |> List.forall (fun (a, b) -> rankOrder b = rankOrder a + 1)
        if isConsecutive then List.length cards else 0

let scoreAllRuns (cards: Card list) : int =
    let allSubsets = subsets cards
    let five = allSubsets |> List.filter (fun s -> List.length s = 5) |> List.sumBy scoreRun
    if five > 0 then
        five
    else
        let four = allSubsets |> List.filter (fun s -> List.length s = 4) |> List.sumBy scoreRun
        if four > 0 then
            four
        else
            allSubsets |> List.filter (fun s -> List.length s = 3) |> List.sumBy scoreRun

let scoreFlush (isCrib: bool) (hand: Card list) (starter: Card) : int =
    let handSuits = hand |> List.map getSuit
    let allHandSame = handSuits |> List.forall (fun s -> s = handSuits.Head)
    if allHandSame && getSuit starter = handSuits.Head then
        5
    elif allHandSame && not isCrib then
        4
    else
        0

let scoreNobs (hand: Card list) (starter: Card) : int =
    hand
    |> List.filter (fun card -> getRank card = Jack && getSuit card = getSuit starter)
    |> List.length

let scoreHand (isCrib: bool) (starter: Card) (hand: Card list) : int =
    let allCards = starter :: hand
    let fifteensAndPairs = allCards |> subsets |> List.sumBy scoreSubset
    let runs = scoreAllRuns allCards
    let flush = scoreFlush isCrib hand starter
    let nobs = scoreNobs hand starter
    fifteensAndPairs + runs + flush + nobs

let cardToString (card: Card) : string =
    let rankStr =
        match getRank card with
        | Ace -> "A"
        | Two -> "2"
        | Three -> "3"
        | Four -> "4"
        | Five -> "5"
        | Six -> "6"
        | Seven -> "7"
        | Eight -> "8"
        | Nine -> "9"
        | Ten -> "10"
        | Jack -> "J"
        | Queen -> "Q"
        | King -> "K"
    let suitStr =
        match getSuit card with
        | Clubs -> "C"
        | Diamonds -> "D"
        | Hearts -> "H"
        | Spades -> "S"
    rankStr + suitStr

let handToString (cards: Card list) : string =
    cards |> List.map cardToString |> String.concat ", "

let verifyHand (label: string) (isCrib: bool) (starter: Card) (hand: Card list) (expected: int) =
    let actual = scoreHand isCrib starter hand
    let status = if actual = expected then "PASS" else "FAIL"
    printfn "  [%s] %s: hand=[%s] starter=%s => Score: %d (expected %d)"
        status label (handToString hand) (cardToString starter) actual expected

[<EntryPoint>]
let main argv =
    printfn "=== Cribbage Scoring ==="
    printfn ""

    // Deal a random hand
    let shuffledDeck = shuffle unshuffledDeck
    let hand = shuffledDeck |> List.take 4
    let starter = shuffledDeck.[4]
    let total = scoreHand false starter hand

    printfn "Random hand: [%s]  Starter: %s" (handToString hand) (cardToString starter)
    let allCards = starter :: hand
    let fifteensAndPairs = allCards |> subsets |> List.sumBy scoreSubset
    let runs = scoreAllRuns allCards
    let flush = scoreFlush false hand starter
    let nobs = scoreNobs hand starter
    printfn "  Fifteens & Pairs: %d" fifteensAndPairs
    printfn "  Runs:             %d" runs
    printfn "  Flush:            %d" flush
    printfn "  Nobs:             %d" nobs
    printfn "  Total:            %d" total
    printfn ""

    // Verification hands
    printfn "=== Verification ==="
    printfn ""

    // Perfect 29 hand: three fives + jack of matching suit, starter is remaining five
    printfn "-- Perfect 29 hand --"
    verifyHand "29 hand" false (Five, Clubs) [ (Five, Spades); (Five, Diamonds); (Five, Hearts); (Jack, Clubs) ] 29

    printfn ""
    printfn "-- Known hands from test suite --"

    // Starter: Jack of Diamonds, Hand: 4C 4D 5C 6H => 14
    verifyHand "test1" false (Jack, Diamonds) [ (Four, Clubs); (Four, Diamonds); (Five, Clubs); (Six, Hearts) ] 14

    // Starter: Jack of Diamonds, Hand: 4C 7H 5C 6H => 8
    verifyHand "test2" false (Jack, Diamonds) [ (Four, Clubs); (Seven, Hearts); (Five, Clubs); (Six, Hearts) ] 8

    // Starter: Eight of Diamonds, Hand: 4C 7H 5C 6H => 9
    verifyHand "test3" false (Eight, Diamonds) [ (Four, Clubs); (Seven, Hearts); (Five, Clubs); (Six, Hearts) ] 9

    // Starter: Five of Clubs, Hand: 5S 5D 5H JC => 29
    verifyHand "test4" false (Five, Clubs) [ (Five, Spades); (Five, Diamonds); (Five, Hearts); (Jack, Clubs) ] 29

    // Starter: Jack of Clubs, Hand: 5S 5D 5H 5C => 28
    verifyHand "test5" false (Jack, Clubs) [ (Five, Spades); (Five, Diamonds); (Five, Hearts); (Five, Clubs) ] 28

    // Starter: Six of Hearts, Hand: 7H 8D 7C 8C => 24
    verifyHand "test6" false (Six, Hearts) [ (Seven, Hearts); (Eight, Diamonds); (Seven, Clubs); (Eight, Clubs) ] 24

    // Starter: Six of Hearts, Hand: JD 8D 4D 10C => 0
    verifyHand "test7" false (Six, Hearts) [ (Jack, Diamonds); (Eight, Diamonds); (Four, Diamonds); (Ten, Clubs) ] 0

    // Starter: Four of Diamonds, Hand: JD 8D 10C 6H => 1
    verifyHand "test8" false (Four, Diamonds) [ (Jack, Diamonds); (Eight, Diamonds); (Ten, Clubs); (Six, Hearts) ] 1

    // Starter: Ten of Clubs, Hand: JD 4D 8D 6D => 4 (not crib, flush of diamonds in hand)
    verifyHand "test9" false (Ten, Clubs) [ (Jack, Diamonds); (Four, Diamonds); (Eight, Diamonds); (Six, Diamonds) ] 4

    // Starter: Ten of Diamonds, Hand: 2D 4D 8D 6D => 5 (5-card flush)
    verifyHand "test10" false (Ten, Diamonds) [ (Two, Diamonds); (Four, Diamonds); (Eight, Diamonds); (Six, Diamonds) ] 5

    // Starter: Ten of Clubs, Hand: JD 4D 8D 6D => 0 (crib, no 4-card flush allowed)
    verifyHand "test11-crib" true (Ten, Clubs) [ (Jack, Diamonds); (Four, Diamonds); (Eight, Diamonds); (Six, Diamonds) ] 0

    // Starter: Ten of Diamonds, Hand: 2D 4D 8D 6D => 5 (crib, 5-card flush still counts)
    verifyHand "test12-crib" true (Ten, Diamonds) [ (Two, Diamonds); (Four, Diamonds); (Eight, Diamonds); (Six, Diamonds) ] 5

    0
