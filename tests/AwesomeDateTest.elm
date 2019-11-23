module AwesomeDateTest exposing (suite)

import AwesomeDate as Date exposing (Date)
import Expect
import Fuzz exposing (Fuzzer, int, intRange)
import Random
import Shrink
import Test exposing (..)
import TestData exposing (validLeapYears)


exampleDate : Date
exampleDate =
    Date.create 2012 6 2


leapDate : Date
leapDate =
    Date.create 2012 2 29


dateFuzzer : Fuzzer ( Int, Int, Int )
dateFuzzer =
    let
        randomYear =
            Random.int Random.minInt Random.maxInt

        randomMonth =
            Random.int 1 12

        generator =
            Random.pair randomYear randomMonth
                |> Random.andThen
                    (\( year, month ) ->
                        Random.int 1 (Date.daysInMonth year month)
                            |> Random.map (\day -> ( year, month, day ))
                    )

        shrinker dateTuple =
            Shrink.tuple3 ( Shrink.int, Shrink.int, Shrink.int ) dateTuple
    in
    Fuzz.custom generator shrinker


testToDateString : Test
testToDateString =
    describe "toDateString"
        [ fuzz dateFuzzer
            "created a valid date string"
            (\( year, month, day ) ->
                Date.create year month day
                    |> Date.toDateString
                    |> Expect.equal
                        (String.fromInt day
                            ++ "/"
                            ++ String.fromInt month
                            ++ "/"
                            ++ String.fromInt year
                        )
            )
        ]


expectDate : Int -> Int -> Int -> Date -> Expect.Expectation
expectDate year month day actualDate =
    let
        expectedDate =
            Date.create year month day
    in
    if actualDate == expectedDate then
        Expect.pass

    else
        Expect.fail <|
            Date.toDateString actualDate
                ++ "\n╷\n│expectDate\n╵\n"
                ++ Date.toDateString expectedDate


testDateParts : Test
testDateParts =
    describe "date part getters"
        [ test "retrieves the year from a date"
            (\() ->
                Date.year exampleDate
                    |> Expect.equal 2012
            )
        , test "retrieves the month from a date"
            (\() ->
                Date.month exampleDate
                    |> Expect.equal 6
            )
        , test "retrieves the day from a date"
            (\() ->
                Date.day exampleDate
                    |> Expect.equal 2
            )
        ]


testIsLeapYear : Test
testIsLeapYear =
    describe "isLeapYear"
        [ fuzz (intRange -400 3000)
            "determines leap years correctly"
            (\year ->
                if List.member year validLeapYears then
                    Date.isLeapYear year
                        |> Expect.true "Expected leap year"

                else
                    Date.isLeapYear year
                        |> Expect.false "Did not expect leap year"
            )
        ]


testAddYears : Test
testAddYears =
    describe "addYears"
        [ test "change a date's year"
            (\() ->
                Date.addYears 2 exampleDate
                    |> expectDate 2014 6 2
            )
        , test "prevent leap days on non-leap years"
            (\() ->
                Date.addYears 1 leapDate
                    |> expectDate 2013 2 28
            )
        , fuzz int
            "changes the year by the amount given"
            (\years ->
                let
                    newDate =
                        Date.addYears years exampleDate
                in
                (Date.year newDate - Date.year exampleDate)
                    |> Expect.equal years
            )
        ]


suite : Test
suite =
    describe "AwesomeDate"
        [ testDateParts
        , testIsLeapYear
        , testAddYears
        , testToDateString
        ]
