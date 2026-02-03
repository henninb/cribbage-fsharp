open System

type Suit = Clubs | Diamonds | Hearts | Spades

type Rank = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King

type Card = Rank * Suit

type Hand = Card list
type Deck = Card list

type Player = Human | Computer

type ScoreBreakdown = {
    Fifteens: int
    Pairs: int
    Runs: int
    Flush: int
    Nobs: int
    Total: int
}

type RoundResult = {
    DealerPlayer: Player
    PlayerScore: ScoreBreakdown
    ComputerScore: ScoreBreakdown
    CribScore: ScoreBreakdown
    HisHeels: bool
}

let suits = [ Hearts; Spades; Clubs; Diamonds ]
let ranks = [ Ace; Two; Three; Four; Five; Six; Seven; Eight; Nine; Ten; Jack; Queen; King ]

let unshuffledDeck =
    [ for suit in suits do
          for rank in ranks do
              yield Card(rank, suit) ]

let shuffle deck =
    let random = Random()
    deck |> List.sortBy (fun _ -> random.Next())

let deal (deck: Deck) : Card list * Card list * Deck =
    let hand1 = deck |> List.take 6
    let hand2 = deck |> List.skip 6 |> List.take 6
    let remaining = deck |> List.skip 12
    (hand1, hand2, remaining)

let cutStarter (deck: Deck) : Card * Deck =
    (List.head deck, List.tail deck)

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

let checkHisHeels (card: Card) : bool =
    getRank card = Jack

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
    let reset = "\x1b[0m"
    let red = "\x1b[31m"
    let white = "\x1b[37m"
    let (color, suitStr) =
        match getSuit card with
        | Clubs -> (white, "♧")
        | Diamonds -> (red, "♢")
        | Hearts -> (red, "♡")
        | Spades -> (white, "♤")
    sprintf "%s%s%s%s" color rankStr suitStr reset

let handToString (cards: Card list) : string =
    cards |> List.map cardToString |> String.concat ", "

let removeCards (toRemove: Card list) (hand: Card list) : Card list =
    hand |> List.filter (fun c -> not (List.contains c toRemove))

let parseCardIndex (input: string) (maxIndex: int) : int option =
    match System.Int32.TryParse(input.Trim()) with
    | (true, n) when n >= 1 && n <= maxIndex -> Some(n - 1)
    | _ -> None

let promptHumanDiscard (hand: Card list) : Card list * Card list =
    let sorted = hand |> List.sortBy rankOrder
    let displayHand (cards: Card list) (discarded: int list) =
        let cardStrs =
            cards |> List.mapi (fun i c ->
                if List.contains i discarded then
                    sprintf "[%d:--]" (i + 1)
                else
                    sprintf "[%d:%s]" (i + 1) (cardToString c))
        printfn ""
        printfn "Your hand:  %s" (cardStrs |> String.concat "  ")
        printfn ""
    let rec getIndex (prompt: string) (excluded: int list) =
        printf "%s" prompt
        let input = Console.ReadLine()
        match parseCardIndex input (List.length sorted) with
        | Some idx when not (List.contains idx excluded) -> idx
        | Some _ ->
            printfn "You already selected that card. Try again."
            getIndex prompt excluded
        | None ->
            printfn "Invalid choice. Enter a number between 1 and %d." (List.length sorted)
            getIndex prompt excluded
    displayHand sorted []
    let idx1 = getIndex "Discard card #1 of 2: " []
    displayHand sorted [idx1]
    let idx2 = getIndex "Discard card #2 of 2: " [idx1]
    let discards = [ sorted.[idx1]; sorted.[idx2] ]
    let kept = removeCards discards sorted
    (kept, discards)

let computerDiscard (hand: Card list) : Card list * Card list =
    let indexed = hand |> List.mapi (fun i c -> (i, c))
    let discardPairs =
        [ for i in 0 .. List.length hand - 2 do
              for j in i + 1 .. List.length hand - 1 do
                  yield (i, j) ]
    let best =
        discardPairs
        |> List.maxBy (fun (i, j) ->
            let kept = indexed |> List.filter (fun (idx, _) -> idx <> i && idx <> j) |> List.map snd
            let dummyStarter = (Ace, Spades)
            scoreHand false dummyStarter kept)
    let (i, j) = best
    let discards = [ hand.[i]; hand.[j] ]
    let kept = removeCards discards hand
    printfn "Computer discards 2 cards to the crib."
    (kept, discards)

let computeScoreBreakdown (isCrib: bool) (starter: Card) (hand: Card list) : ScoreBreakdown =
    let allCards = starter :: hand
    let fifteens = allCards |> subsets |> List.sumBy scoreFifteen
    let pairs = allCards |> subsets |> List.sumBy scorePair
    let runs = scoreAllRuns allCards
    let flush = scoreFlush isCrib hand starter
    let nobs = scoreNobs hand starter
    { Fifteens = fifteens; Pairs = pairs; Runs = runs; Flush = flush; Nobs = nobs;
      Total = fifteens + pairs + runs + flush + nobs }

let printScoreBreakdown (label: string) (hand: Card list) (starter: Card) (score: ScoreBreakdown) : unit =
    let sorted = hand |> List.sortBy rankOrder
    printfn "%s: [%s]  Starter: %s" label (handToString sorted) (cardToString starter)
    printfn "  Fifteens: %d" score.Fifteens
    printfn "  Pairs:    %d" score.Pairs
    printfn "  Runs:     %d" score.Runs
    printfn "  Flush:    %d" score.Flush
    printfn "  Nobs:     %d" score.Nobs
    printfn "  Total:    %d" score.Total

let playRound (dealer: Player) : RoundResult =
    let dealerName = match dealer with Human -> "Human" | Computer -> "Computer"
    printfn "=== New Round (Dealer: %s) ===" dealerName
    printfn ""

    let shuffled = shuffle unshuffledDeck
    let (hand1, hand2, remaining) = deal shuffled

    let (humanHand, computerHand) =
        match dealer with
        | Human -> (hand1, hand2)
        | Computer -> (hand1, hand2)

    let (humanKept, humanDiscards) = promptHumanDiscard humanHand
    let (computerKept, computerDiscards) = computerDiscard computerHand

    let crib = humanDiscards @ computerDiscards

    let (starter, _) = cutStarter remaining
    let heels = checkHisHeels starter
    printfn ""
    printfn "Starter: %s" (cardToString starter)
    if heels then
        printfn "His Heels! %s scores 2 points for Jack starter." dealerName
    printfn ""

    let (nonDealerLabel, nonDealerHand, dealerLabel, dealerHand) =
        match dealer with
        | Human -> ("Computer", computerKept, "Human (Dealer)", humanKept)
        | Computer -> ("Human", humanKept, "Computer (Dealer)", computerKept)

    let nonDealerScore = computeScoreBreakdown false starter nonDealerHand
    printScoreBreakdown nonDealerLabel nonDealerHand starter nonDealerScore
    printfn ""

    let dealerScore = computeScoreBreakdown false starter dealerHand
    printScoreBreakdown dealerLabel dealerHand starter dealerScore
    printfn ""

    let cribScore = computeScoreBreakdown true starter crib
    printScoreBreakdown (sprintf "Crib (%s)" dealerName) crib starter cribScore
    printfn ""

    let (playerScore, compScore) =
        match dealer with
        | Human -> (dealerScore, nonDealerScore)
        | Computer -> (nonDealerScore, dealerScore)

    { DealerPlayer = dealer
      PlayerScore = playerScore
      ComputerScore = compScore
      CribScore = cribScore
      HisHeels = heels }

let verifyHand (label: string) (isCrib: bool) (starter: Card) (hand: Card list) (expected: int) =
    let actual = scoreHand isCrib starter hand
    let status = if actual = expected then "PASS" else "FAIL"
    printfn "  [%s] %s: hand=[%s] starter=%s => Score: %d (expected %d)"
        status label (handToString hand) (cardToString starter) actual expected

[<EntryPoint>]
let main argv =
    let args = argv |> Array.toList
    let skipGame = List.contains "--skip-game" args
    let runTests = List.contains "--test" args || skipGame

    if not skipGame then
        let _result = playRound Human
        printfn ""

    if runTests then
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
