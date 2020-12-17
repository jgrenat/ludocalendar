module Day18Dice exposing (Die(..), Status(..), checkScore)

import List.Extra as List


type Die
    = Soldier
    | Captain
    | Hero
    | Traitor
    | BetrayedHero
    | Cursed
    | Magus
    | CalculatedMagus Int


type Status
    = Success Int
    | Failure ( Int, Int )


checkScore : List Die -> List Die -> Status
checkScore firstGroup secondGroup =
    let
        firstGroupScore =
            checkGroupScore firstGroup

        secondGroupScore =
            checkGroupScore secondGroup
    in
    if firstGroupScore == secondGroupScore then
        Success firstGroupScore

    else
        Failure ( firstGroupScore, secondGroupScore )


checkGroupScore : List Die -> Int
checkGroupScore group =
    List.foldl applyEffects group group
        |> Debug.log "score"
        |> List.map diceScore
        |> Debug.log "score"
        |> List.foldl (+) 0


applyEffects : Die -> List Die -> List Die
applyEffects dice dices =
    case dice of
        Traitor ->
            List.elemIndex Hero dices
                |> Maybe.map (\index -> List.setAt index BetrayedHero dices)
                |> Maybe.withDefault dices

        Soldier ->
            dices

        Captain ->
            dices

        Hero ->
            dices

        BetrayedHero ->
            dices

        Cursed ->
            dices

        Magus ->
            let
                count =
                    List.count ((/=) Magus) dices
            in
            List.setIf ((==) Magus) (CalculatedMagus count) dices

        CalculatedMagus _ ->
            dices


diceScore : Die -> Int
diceScore dice =
    case dice of
        Soldier ->
            1

        Captain ->
            2

        Hero ->
            3

        Traitor ->
            1

        BetrayedHero ->
            0

        Cursed ->
            -1

        Magus ->
            0

        CalculatedMagus score ->
            score
