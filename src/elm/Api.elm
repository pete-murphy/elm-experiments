module Api exposing
    ( DeviceId
    , Flags
    , SessionId
    , UserId
    , application
    )

import Browser exposing (Document)
import Browser.Navigation as Nav
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Time
import Url exposing (Url)


type alias DeviceId =
    -- TODO: Newtype
    String


type alias UserId =
    -- TODO: Newtype
    String


type alias SessionId =
    -- TODO: Newtype
    String


type alias Flags =
    { deviceId : Maybe DeviceId
    , sessionId : Maybe SessionId
    , userId : Maybe UserId
    , now : Maybe Time.Posix
    }


application :
    Decoder Flags
    ->
        { init : Result Decode.Error Flags -> Url -> Nav.Key -> ( model, Cmd msg )
        , view : model -> Document msg
        , update : msg -> model -> ( model, Cmd msg )
        , subscriptions : model -> Sub msg
        , onUrlRequest : Browser.UrlRequest -> msg
        , onUrlChange : Url -> msg
        }
    -> Program Encode.Value model msg
application decoder config =
    let
        init : Encode.Value -> Url -> Nav.Key -> ( model, Cmd msg )
        init value url navKey =
            let
                result =
                    Decode.decodeValue Decode.string value
                        |> Result.andThen (Decode.decodeString decoder)
            in
            config.init result url navKey
    in
    Browser.application
        { init = init
        , view = config.view
        , update = config.update
        , subscriptions = config.subscriptions
        , onUrlRequest = config.onUrlRequest
        , onUrlChange = config.onUrlChange
        }
