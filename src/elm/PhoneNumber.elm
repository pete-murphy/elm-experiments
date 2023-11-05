-- Copied from https://github.com/robinheghan/elm-phone-numbers
-- because I got 404 when trying to `elm install` it


module PhoneNumber exposing
    ( Country, NumberType(..), NumberTypeData
    , ValidationConfig, anyType, valid
    , matches
    , PhoneNumber, validate
    )

{-| Functions for validating phone numbers


# Metadata

You don't need to define any of these records yourself, please see the `PhoneNumber.Countries` module.

@docs Country, NumberType, NumberTypeData


# Validation

@docs ValidationConfig, anyType, valid


# Query

@docs matches

-}

import Regex exposing (Regex)


{-| Contains phone related metadata for a country
-}
type alias Country =
    { id : String
    , countryCode : String
    , internationalPrefix : Maybe String
    , nationalPrefix : Maybe String
    , generalNumberPattern : Regex
    , numberTypes : List NumberTypeData
    }


{-| What kind of a number are we talking about? Fixed line? Mobile number? Pager number? etc.
-}
type NumberType
    = FixedLine
    | Mobile
    | TollFree
    | PremiumRate
    | SharedCost
    | PersonalNumber
    | Voip
    | Pager
    | Uan
    | Emergency
    | Voicemail
    | ShortCode
    | StandardRate
    | CarrierSpecific
    | SmsServices
    | NoInternationalDialling


{-| Describes a NumberType with an example number, as well as a regex to validate if a given number is of
the set numberType
-}
type alias NumberTypeData =
    { numberType : NumberType
    , exampleNumber : String
    , pattern : Regex
    }


{-| Pass this record to `valid` or `matches` to define how validation should occur.

Validation will occur against the default country as well as the list of other countries,
and validation will only occur against the list of number types. Providing a empty list of
number types might still succeed, as every country has a general number pattern that will be checked.

Any number that doesn't have an international prefix will be treated as if it belongs to the
default country.

-}
type alias ValidationConfig =
    { defaultCountry : Country
    , otherCountries : List Country
    , types : List NumberType
    }



-- API


{-| A simple test to see if the provided number matches anything provided by the validation config.
-}
valid : ValidationConfig -> String -> Bool
valid config number =
    case matches config number of
        [] ->
            False

        _ ->
            True


{-| Returns a list of tuples where the first element is a country that the number can belong to,
and the second list contains the different types the number can be. It's possible the the second
element is an empty list, as every country has a general number pattern that will be checked if the
number type cannot be determined.
-}
matches : ValidationConfig -> String -> List ( Country, List NumberType )
matches config number =
    let
        sanitizedNumber =
            sanitizeNumber number
    in
    List.filterMap
        (matchingCountry sanitizedNumber config.defaultCountry config.types)
        (config.defaultCountry :: config.otherCountries)


{-| A list of all number types. Use this if you don't care what kind of number something is,
but want to know if the number can belong to a certain country.
-}
anyType : List NumberType
anyType =
    [ FixedLine
    , Mobile
    , TollFree
    , PremiumRate
    , SharedCost
    , PersonalNumber
    , Voip
    , Pager
    , Uan
    , Emergency
    , Voicemail
    , ShortCode
    , StandardRate
    , CarrierSpecific
    , SmsServices
    , NoInternationalDialling
    ]



-- HELPERS


matchingCountry : String -> Country -> List NumberType -> Country -> Maybe ( Country, List NumberType )
matchingCountry number defaultCountry relevantTypes country =
    let
        maybeLocalNumber =
            localizeNumber country number

        matchesSpec localNumber desc =
            regexExactMatch desc.pattern localNumber

        isDefaultCountry =
            defaultCountry.id == country.id
    in
    case maybeLocalNumber of
        Nothing ->
            Nothing

        Just localNumber ->
            if localNumber == number && not isDefaultCountry then
                Nothing

            else
                let
                    matchingTypes =
                        country.numberTypes
                            |> List.filter (\nt -> List.member nt.numberType relevantTypes)
                            |> List.filter (matchesSpec localNumber)
                            |> List.map .numberType
                in
                case matchingTypes of
                    [] ->
                        if regexExactMatch country.generalNumberPattern localNumber then
                            Just ( country, [] )

                        else
                            Nothing

                    _ ->
                        Just
                            ( country
                            , matchingTypes
                            )


localizeNumber : Country -> String -> Maybe String
localizeNumber country number =
    case country.internationalPrefix of
        Nothing ->
            Just number

        Just prefix ->
            let
                prefixLength =
                    String.length prefix

                countryCodeLength =
                    String.length country.countryCode
            in
            if String.startsWith "+" number then
                if
                    number
                        |> String.dropLeft 1
                        |> String.left countryCodeLength
                        |> (==) country.countryCode
                then
                    Just <| String.dropLeft (countryCodeLength + 1) number

                else
                    Nothing

            else if String.startsWith prefix number then
                if
                    number
                        |> String.dropLeft prefixLength
                        |> String.left countryCodeLength
                        |> (==) country.countryCode
                then
                    Just <| String.dropLeft (prefixLength + countryCodeLength) number

                else
                    Nothing

            else
                Just number


sanitizeNumber : String -> String
sanitizeNumber str =
    String.filter (\c -> c /= ' ') str


regexExactMatch : Regex -> String -> Bool
regexExactMatch regex str =
    case Regex.find regex str of
        [ match ] ->
            match.match == str

        _ ->
            False



-- ADDITIONAL


type PhoneNumber
    = PhoneNumber String


validate : ValidationConfig -> String -> Maybe PhoneNumber
validate config str =
    if valid config str then
        Just (PhoneNumber str)

    else
        Nothing
