# cribbage-fsharp

Cribbage game written in F# targeting .NET 10.

## Requirements

- .NET SDK 10.0+

## Run

```bash
# Play an interactive round
dotnet run

# Run verification tests only (no interactive input)
dotnet run -- --skip-game

# Play a round then run verification tests
dotnet run -- --test
```

## Gameplay

Each round follows standard cribbage dealing and scoring:

1. Deck is shuffled and 6 cards are dealt to each player
2. Human picks 2 cards to discard to the crib
3. Computer picks 2 cards to discard (greedy strategy maximizing kept hand score)
4. Starter card is cut from the remaining deck
5. If the starter is a Jack, the dealer scores 2 points (his heels)
6. Non-dealer hand, dealer hand, and crib are scored with full breakdowns (fifteens, pairs, runs, flush, nobs)

## Scoring

The scoring engine handles fifteens (2 pts), pairs (2 pts), runs (1 pt per card), flushes (4 or 5 pts), and nobs (1 pt). Crib scoring follows crib-specific flush rules (5-card flush only).

## Verification

12 built-in test hands validate the scoring engine, including the perfect 29 hand, crib flush rules, and nobs.
