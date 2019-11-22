module AwesomeDateTest exposing (suite)

import AwesomeDate as Date exposing (Date)
import Expect
import Test exposing (..)


exampleDate : Date
exampleDate =
    Date.create 2012 6 2


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
        [ test "returns true if divisible by 4 but not 100"
            (\() ->
                Date.isLeapYear 2012
                    |> Expect.true "Expected leap year"
            )
        , test "returns false if divisible by 4 and 100 but not 400"
            (\() ->
                Date.isLeapYear 3000
                    |> Expect.false "Did not expect leap year"
            )
        , test "returns true if divisible by 4, 100, and 400"
            (\() ->
                Date.isLeapYear 2000
                    |> Expect.true "Expected leap year"
            )
        ]


suite : Test
suite =
    describe "AwesomeDate"
        [ testDateParts
        , testIsLeapYear
        ]
