module UserId exposing
    ( UserId
    , decoder
    , encode
    , generator
    , toString
    )

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Random exposing (Generator)
import UUID exposing (UUID)


type UserId
    = UserId UUID


encode : UserId -> Encode.Value
encode (UserId uuid) =
    UUID.toValue uuid


decoder : Decoder UserId
decoder =
    UUID.jsonDecoder
        |> Decode.map UserId


generator : Generator UserId
generator =
    UUID.generator
        |> Random.map UserId


toString : UserId -> String
toString (UserId uuid) =
    UUID.toString uuid
