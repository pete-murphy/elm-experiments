module SessionId exposing
    ( SessionId
    , decoder
    , encode
    , generator
    , toString
    )

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Random exposing (Generator)
import UUID exposing (UUID)


type SessionId
    = SessionId UUID


encode : SessionId -> Encode.Value
encode (SessionId uuid) =
    UUID.toValue uuid


decoder : Decoder SessionId
decoder =
    UUID.jsonDecoder
        |> Decode.map SessionId


generator : Generator SessionId
generator =
    UUID.generator
        |> Random.map SessionId


toString : SessionId -> String
toString (SessionId uuid) =
    UUID.toString uuid
