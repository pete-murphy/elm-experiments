module Main exposing (main)

import Api
import Browser exposing (Document)
import Browser.Navigation as Nav
import Html
import Html.Attributes as Attributes
import Json.Decode as Decode exposing (Decoder, Error(..))
import Json.Decode.Pipeline as Pipeline
import Random
import Register
import Task
import Time
import Url exposing (Url)


type Problems
    = Problems (List ( String, Problems ))


type Model
    = Initial Api.PartialFlags
    | DecodeError Decode.Error
    | Register Register.Model



-- MODEL


init :
    Result Decode.Error Api.PartialFlags
    -> Url
    -> Nav.Key
    -> ( Model, Cmd Msg )
init result _ _ =
    case result of
        Err decodeError ->
            ( DecodeError decodeError, Cmd.none )

        Ok flags ->
            let
                _ =
                    Debug.log "flags" flags

                getDeviceId : Cmd Msg
                getDeviceId =
                    case flags.deviceId of
                        Nothing ->
                            Random.generate GotDeviceId (Random.constant "generated-device-id")

                        Just _ ->
                            Cmd.none

                getSessionId : Cmd Msg
                getSessionId =
                    case flags.sessionId of
                        Nothing ->
                            Random.generate GotSessionId (Random.constant "generated-session-id")

                        Just _ ->
                            Cmd.none

                getUserId : Cmd Msg
                getUserId =
                    case flags.userId of
                        Nothing ->
                            Random.generate GotUserId (Random.constant "generated-user-id")

                        Just _ ->
                            Cmd.none

                getNow : Cmd Msg
                getNow =
                    case flags.now of
                        Nothing ->
                            -- Random.generate GotNow (Random.float 0 1700000000000 |> Random.map (floor >> Time.millisToPosix))
                            Time.now |> Task.perform GotNow

                        Just _ ->
                            Cmd.none
            in
            ( Initial flags
            , Cmd.batch
                [ getDeviceId
                , getSessionId
                , getUserId
                , getNow
                ]
            )



-- VIEW


view : Model -> Document Msg
view model =
    let
        { title, body } =
            case model of
                Initial flags ->
                    { title = "Initial"
                    , body =
                        [ Html.h1 [] [ Html.text "Initial" ]
                        , Html.pre []
                            [ Html.text (Debug.toString flags)
                            ]
                        ]
                    }

                DecodeError decodeError ->
                    { title = "DecodeError"
                    , body =
                        [ Html.h1 [ Attributes.class "text-red-500" ] [ Html.text "DecodeError" ]
                        , Html.pre [] [ Html.text (Decode.errorToString decodeError) ]
                        ]
                    }

                Register register ->
                    let
                        registerDocument =
                            Register.view register
                    in
                    { title = registerDocument.title
                    , body =
                        List.map (Html.map GotRegisterMsg) registerDocument.body
                    }
    in
    { title = title
    , body =
        [ Html.div [ Attributes.class "flex flex-col flex-wrap items-center mt-32" ]
            [ Html.div [ Attributes.class "prose w-full" ] body ]
        ]
    }



-- UPDATE


type Msg
    = ChangedUrl Url
    | ClickedLink Browser.UrlRequest
    | GotRegisterMsg Register.Msg
    | GotDeviceId Api.DeviceId
    | GotSessionId Api.SessionId
    | GotUserId Api.UserId
    | GotNow Time.Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( GotRegisterMsg subMsg, Register subModel ) ->
            let
                ( newSubModel, cmd ) =
                    Register.update subMsg subModel
            in
            ( Register newSubModel, Cmd.map GotRegisterMsg cmd )

        ( GotDeviceId id, _ ) ->
            ( updateInitial (\flags -> { flags | deviceId = Just id }) model, Cmd.none )

        ( GotSessionId id, _ ) ->
            ( updateInitial (\flags -> { flags | sessionId = Just id }) model, Cmd.none )

        ( GotUserId id, _ ) ->
            ( updateInitial (\flags -> { flags | userId = Just id }) model, Cmd.none )

        ( GotNow now, _ ) ->
            ( updateInitial (\flags -> { flags | now = Just now }) model, Cmd.none )

        _ ->
            ( model, Cmd.none )


updateInitial : (Api.PartialFlags -> Api.PartialFlags) -> Model -> Model
updateInitial transform model =
    case model of
        Initial partialFlags ->
            let
                { deviceId, sessionId, userId, now } =
                    transform partialFlags
            in
            case Maybe.map4 Api.Flags deviceId sessionId userId now of
                Just flags ->
                    let
                        ( m, _ ) =
                            Register.init flags
                    in
                    Register m

                _ ->
                    Initial (Api.PartialFlags deviceId sessionId userId now)

        _ ->
            model



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- MAIN


main : Program Decode.Value Model Msg
main =
    let
        decoderFlags : Decoder Api.PartialFlags
        decoderFlags =
            Decode.succeed Api.PartialFlags
                |> Pipeline.optional "deviceId" (Decode.nullable Decode.string) Nothing
                |> Pipeline.optional "sessionId" (Decode.nullable Decode.string) Nothing
                |> Pipeline.optional "userId" (Decode.nullable Decode.string) Nothing
                |> Pipeline.optional "now" (Decode.nullable Decode.int |> Decode.map (Maybe.map Time.millisToPosix)) Nothing
    in
    Api.application
        decoderFlags
        { init = init
        , onUrlChange = ChangedUrl
        , onUrlRequest = ClickedLink
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
