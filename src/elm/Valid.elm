module Valid exposing
    ( Valid
    , Validator
    , andThenValidator
    , apply
    , map
    , unwrap
    , validator
    )

import Html exposing (a)


type Valid a
    = Valid a


type alias Validator error input output =
    input -> Result error (Valid output)


validator : (input -> Result error output) -> Validator error input output
validator f =
    f >> Result.map Valid


andThenValidator : (a -> Result error b) -> Validator error input a -> Validator error input b
andThenValidator f validator_ =
    \input ->
        case validator_ input of
            Ok (Valid a) ->
                f a |> Result.map Valid

            Err error ->
                Err error


map : (a -> b) -> Valid a -> Valid b
map f (Valid a) =
    Valid (f a)


apply :
    Valid (a -> b)
    -> Valid a
    -> Valid b
apply (Valid f) (Valid a) =
    Valid (f a)


unwrap : Valid a -> a
unwrap (Valid a) =
    a
