module Prime.Stream exposing (Stream, advance, stream)


type Stream
    = Stream
        { next : Int
        , divisors : List Int
        }


stream : Stream
stream =
    Stream
        { next = 2
        , divisors = []
        }


advance : Stream -> ( Int, Stream )
advance (Stream { next, divisors }) =
    ( next, find (next + 1) <| divisors ++ [ next ] )


find : Int -> List Int -> Stream
find start divisors =
    if not <| List.any (divides start) divisors then
        Stream
            { next = start
            , divisors = divisors
            }

    else
        find (start + 1) divisors


divides : Int -> Int -> Bool
divides n d =
    0 == modBy d n
