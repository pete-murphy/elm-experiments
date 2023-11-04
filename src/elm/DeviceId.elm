module DeviceId exposing
    ( DeviceId
    , decoder
    , encode
    , generator
    , toString
    )

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Random exposing (Generator)
import UUID exposing (UUID)


type DeviceId
    = DeviceId UUID


encode : DeviceId -> Encode.Value
encode (DeviceId uuid) =
    UUID.toValue uuid


decoder : Decoder DeviceId
decoder =
    UUID.jsonDecoder
        |> Decode.map DeviceId


generator : Generator DeviceId
generator =
    UUID.generator
        |> Random.map DeviceId


toString : DeviceId -> String
toString (DeviceId uuid) =
    UUID.toString uuid
