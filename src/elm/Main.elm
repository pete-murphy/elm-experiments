module Main exposing (main)

import Api
import Browser exposing (Document)
import Browser.Navigation as Nav
import DeviceId exposing (DeviceId)
import Flags exposing (Flags, PartialFlags)
import Html
import Html.Attributes as Attributes
import Json.Decode as Decode exposing (Decoder, Error(..))
import Json.Decode.Pipeline as Pipeline
import Page.Register as Register
import Random
import SessionId exposing (SessionId)
import Task
import Time
import Url exposing (Url)
import UserId exposing (UserId)


type Problems
    = Problems (List ( String, Problems ))


type Model
    = Initial PartialFlags
    | NoTimeZone
    | DecodeError Decode.Error
    | Register Register.Model



-- MODEL


init :
    Result Decode.Error PartialFlags
    -> Url
    -> Nav.Key
    -> ( Model, Cmd Msg )
init result _ _ =
    case result of
        Err decodeError ->
            ( DecodeError decodeError, Cmd.none )

        Ok flags ->
            let
                getDeviceId : Cmd Msg
                getDeviceId =
                    case flags.deviceId of
                        Nothing ->
                            Random.generate GotDeviceId DeviceId.generator

                        Just _ ->
                            Cmd.none

                getSessionId : Cmd Msg
                getSessionId =
                    case flags.sessionId of
                        Nothing ->
                            Random.generate GotSessionId SessionId.generator

                        Just _ ->
                            Cmd.none

                getUserId : Cmd Msg
                getUserId =
                    case flags.userId of
                        Nothing ->
                            Random.generate GotUserId UserId.generator

                        Just _ ->
                            Cmd.none

                getNow : Cmd Msg
                getNow =
                    case flags.now of
                        Nothing ->
                            Time.now |> Task.perform GotNow

                        Just _ ->
                            Cmd.none

                getTimeZone : Cmd Msg
                getTimeZone =
                    case flags.now of
                        Nothing ->
                            Time.here
                                |> Task.attempt (Result.toMaybe >> GotTimeZone)

                        Just _ ->
                            Cmd.none
            in
            ( Initial flags
            , Cmd.batch
                [ getDeviceId
                , getSessionId
                , getUserId
                , getNow
                , getTimeZone
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

                NoTimeZone ->
                    { title = "NoTimeZone"
                    , body =
                        [ Html.h1 [ Attributes.class "text-red-500" ] [ Html.text "NoTimeZone" ]
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
    | GotDeviceId DeviceId
    | GotSessionId SessionId
    | GotUserId UserId
    | GotNow Time.Posix
    | GotTimeZone (Maybe Time.Zone)


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
            updateInitial (\flags -> { flags | deviceId = Just id }) model

        ( GotSessionId id, _ ) ->
            updateInitial (\flags -> { flags | sessionId = Just id }) model

        ( GotUserId id, _ ) ->
            updateInitial (\flags -> { flags | userId = Just id }) model

        ( GotNow now, _ ) ->
            updateInitial (\flags -> { flags | now = Just now }) model

        ( GotTimeZone (Just timeZone), _ ) ->
            updateInitial (\flags -> { flags | timeZone = Just timeZone }) model

        ( GotTimeZone Nothing, _ ) ->
            ( NoTimeZone, Cmd.none )

        _ ->
            ( model, Cmd.none )


updateInitial : (PartialFlags -> PartialFlags) -> Model -> ( Model, Cmd Msg )
updateInitial transform model =
    case model of
        Initial partialFlags ->
            let
                { deviceId, sessionId, userId, now, timeZone } =
                    transform partialFlags
            in
            case Maybe.map5 Flags deviceId sessionId userId now timeZone of
                Just flags ->
                    let
                        ( m, _ ) =
                            Register.init flags
                    in
                    ( Register m, Cmd.none )

                _ ->
                    ( Initial (PartialFlags deviceId sessionId userId now timeZone), Cmd.none )

        _ ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- MAIN


main : Program Decode.Value Model Msg
main =
    let
        decoderFlags : Decoder PartialFlags
        decoderFlags =
            Decode.succeed PartialFlags
                |> Pipeline.optional "deviceId" (Decode.nullable DeviceId.decoder) Nothing
                |> Pipeline.optional "sessionId" (Decode.nullable SessionId.decoder) Nothing
                |> Pipeline.optional "userId" (Decode.nullable UserId.decoder) Nothing
                |> Pipeline.optional "now" (Decode.nullable Decode.int |> Decode.map (Maybe.map Time.millisToPosix)) Nothing
                |> Pipeline.optional "timeZoneOffset" (Decode.nullable Decode.int |> Decode.map (Maybe.map (\offset -> Time.customZone offset []))) Nothing
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
