module Valid exposing
    ( Valid
    , Validator
    , andThenValidator
    , apply
    , applyValidator
    , applyWith
    , map
    , mapValidator
    , pureValidator
    , unwrap
    , validator
    )


type Valid a
    = Valid a


type alias Validator input error output =
    input -> Result error (Valid output)


validator : (input -> Result error output) -> Validator input error output
validator f =
    f >> Result.map Valid


mapValidator : (a -> b) -> Validator input error a -> Validator input error b
mapValidator f =
    (<<) (map f)


applyValidator : (input -> Result error a) -> Validator input error (a -> b) -> Validator input error b
applyValidator validatorA validatorF =
    \input ->
        validatorF input
            |> Result.andThen
                (\(Valid f) ->
                    validatorA input |> Result.map (\a -> Valid (f a))
                )


pureValidator : a -> Validator input error a
pureValidator a =
    \_ -> Ok (Valid a)



--   mapValidator a validatorF input)
-- \input ->
--     validatorF input
--         |> \x -> x
-- applyValidator : (input -> Result error (a -> b)) -> Validator input error a -> Validator input error b
-- applyValidator validatorF validatorA =
--     \input ->
--         validatorF input
--             |> Result.andThen (\f -> mapValidator f validatorA input)


andThenValidator : (a -> Result error b) -> Validator input error a -> Validator input error b
andThenValidator f validator_ =
    \input ->
        case validator_ input of
            Ok (Valid a) ->
                f a |> Result.map Valid

            Err error ->
                Err error



-- map : (a -> b) -> Valid a -> Valid b
-- map f (Valid a) =
--     Valid (f a)


map : (a -> b) -> Result e (Valid a) -> Result e (Valid b)
map f =
    Result.map (\(Valid a) -> Valid (f a))


applyWith : (e -> e -> e) -> (x -> e) -> Result x (Valid a) -> Result e (Valid (a -> b)) -> Result e (Valid b)
applyWith combineErrors liftError resA resF =
    case resF of
        Ok (Valid f) ->
            map f resA |> Result.mapError liftError

        Err e1 ->
            case resA of
                Ok _ ->
                    Err e1

                Err e2 ->
                    Err (combineErrors e1 (liftError e2))


apply : Result e (Valid a) -> Result e (Valid (a -> b)) -> Result e (Valid b)
apply resA =
    Result.andThen (\(Valid f) -> map f resA)



-- apply :
--     Valid (a -> b)
--     -> Valid a
--     -> Valid b
-- apply (Valid f) (Valid a) =
--     Valid (f a)


unwrap : Valid a -> a
unwrap (Valid a) =
    a
