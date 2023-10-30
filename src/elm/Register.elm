module Register exposing
    ( Model
    , Msg
    , init
    , update
    , view
    )

import Accessibility as Html exposing (Html)
import Accessibility.Aria as Aria
import Api exposing (DeviceId, SessionId, UserId)
import Date exposing (Date)
import Email
import Html.Attributes as Attributes
import Html.Events as Events
import Iso8601
import Json.Encode as Encode
import Maybe.Extra as Maybe
import Regex
import Time



-- MODEL


type Model
    = Model InternalModel


type alias InternalModel =
    { unvalidatedForm : UnvalidatedForm
    , dirty : Bool
    , flags : Api.Flags
    }


type alias UnvalidatedForm =
    { firstName : String
    , lastName : String
    , email : String
    , birthday : Maybe Date
    , phoneNumber : String
    , username : String
    , password : String
    , confirmPassword : String
    }


type alias ValidatedForm =
    { firstName : String
    , lastName : String
    , email : String
    , birthday : Date
    , phoneNumber : String -- TODO: Make module
    , username : String
    , password : String
    }


init :
    Api.Flags
    -> ( Model, Cmd Msg )
init flags =
    let
        _ =
            Debug.log "flags" flags
    in
    ( Model
        { unvalidatedForm =
            { firstName = ""
            , lastName = ""
            , email = ""
            , birthday = Nothing
            , phoneNumber = ""
            , username = ""
            , password = ""
            , confirmPassword = ""
            }
        , dirty = False
        , flags = flags
        }
    , Cmd.none
    )



-- VIEW


view : Model -> { title : String, body : List (Html Msg) }
view (Model model) =
    let
        firstNameErrorId =
            "first-name-error"

        firstNameInvalid =
            if not model.dirty then
                Nothing

            else if String.trim model.unvalidatedForm.firstName == "" then
                Just "Must not be blank"

            else
                Nothing

        lastNameErrorId =
            "last-name-error"

        lastNameInvalid =
            if not model.dirty then
                Nothing

            else if String.trim model.unvalidatedForm.lastName == "" then
                Just "Must not be blank"

            else
                Nothing

        emailErrorId =
            "email-error"

        emailInvalid =
            if not model.dirty then
                Nothing

            else if String.trim model.unvalidatedForm.email == "" then
                Just "Must not be blank"

            else if Maybe.isNothing (Email.fromString model.unvalidatedForm.email) then
                Just "Must be a valid e-mail"

            else
                Nothing

        encodedFlags =
            Encode.encode 4
                (Encode.object
                    [ ( "deviceId", Encode.string model.flags.deviceId )
                    , ( "sessionId", Encode.string model.flags.sessionId )
                    , ( "userId", Encode.string model.flags.userId )
                    , ( "now", Iso8601.encode model.flags.now )
                    ]
                )
    in
    { title = "Register"
    , body =
        [ Html.div []
            [ Html.form
                [ Attributes.class "flex flex-col gap-1" ]
                [ Html.div [ Attributes.class "flex flex-col w-fit gap-1" ]
                    [ Html.labelBefore []
                        (Html.div
                            []
                            [ Html.text "First name", Html.span [ Aria.hidden True ] [ Html.text "*" ] ]
                        )
                        (Html.inputText model.unvalidatedForm.firstName
                            [ Attributes.attribute "autocomplete" "given-name"
                            , Aria.required True
                            , Aria.invalid (Maybe.isJust firstNameInvalid)
                            , Aria.describedBy (Maybe.toList firstNameInvalid |> List.map (\_ -> firstNameErrorId))
                            , Events.onInput EnteredFirstName
                            , Attributes.class "rounded-md focus:ring-2 focus:ring-offset-1 focus:ring-blue-400 transition-all"
                            , Attributes.classList [ ( "border-red-500", Maybe.isJust firstNameInvalid ) ]
                            ]
                        )
                    , Html.div
                        [ Attributes.class "text-xs"
                        , Attributes.classList [ ( "text-red-500", Maybe.isJust firstNameInvalid ) ]
                        ]
                        (Maybe.toList (Maybe.or firstNameInvalid (Just "\u{00A0}"))
                            |> List.map (\error -> Html.span [ Attributes.id firstNameErrorId ] [ Html.text error ])
                        )
                    ]
                , Html.div []
                    [ Html.labelBefore []
                        (Html.div
                            []
                            [ Html.text "Last name", Html.span [ Aria.hidden True ] [ Html.text "*" ] ]
                        )
                        (Html.inputText model.unvalidatedForm.lastName
                            [ Attributes.attribute "autocomplete" "family-name"
                            , Aria.required True
                            , Aria.invalid (Maybe.isJust lastNameInvalid)
                            , Aria.describedBy (Maybe.toList lastNameInvalid |> List.map (\_ -> lastNameErrorId))
                            , Events.onInput EnteredLastName
                            , Attributes.class "rounded-md focus:ring-2 focus:ring-offset-1 focus:ring-blue-400 transition-all"
                            , Attributes.classList [ ( "border-red-500", Maybe.isJust lastNameInvalid ) ]
                            ]
                        )
                    , Html.div
                        [ Attributes.class "text-xs"
                        , Attributes.classList [ ( "text-red-500", Maybe.isJust lastNameInvalid ) ]
                        ]
                        (Maybe.toList (Maybe.or lastNameInvalid (Just "\u{00A0}"))
                            |> List.map (\error -> Html.span [ Attributes.id lastNameErrorId ] [ Html.text error ])
                        )
                    ]
                , Html.div []
                    [ Html.labelBefore []
                        (Html.div
                            []
                            [ Html.text "E-mail", Html.span [ Aria.hidden True ] [ Html.text "*" ] ]
                        )
                        (Html.inputText model.unvalidatedForm.email
                            [ Attributes.attribute "autocomplete" "email"
                            , Attributes.type_ "email"
                            , Aria.required True
                            , Aria.invalid (Maybe.isJust emailInvalid)
                            , Aria.describedBy (Maybe.toList emailInvalid |> List.map (\_ -> emailErrorId))
                            , Events.onInput EnteredEmail
                            , Attributes.class "rounded-md focus:ring-2 focus:ring-offset-1 focus:ring-blue-400 transition-all"
                            , Attributes.classList [ ( "border-red-500", Maybe.isJust emailInvalid ) ]
                            ]
                        )
                    , Html.div
                        [ Attributes.class "text-xs"
                        , Attributes.classList [ ( "text-red-500", Maybe.isJust emailInvalid ) ]
                        ]
                        (Maybe.toList (Maybe.or emailInvalid (Just "\u{00A0}"))
                            |> List.map (\error -> Html.span [ Attributes.id emailErrorId ] [ Html.text error ])
                        )
                    ]
                , Html.div []
                    [ Html.pre [] [ Html.text encodedFlags ]
                    ]
                ]
            ]
        ]
    }



-- UPDATE


type Msg
    = SubmittedForm
    | EnteredFirstName String
    | EnteredLastName String
    | EnteredEmail String
    | EnteredBirthday (Maybe Date)
    | EnteredPhoneNumber String
    | EnteredUsername String
    | EnteredPassword String
    | EnteredConfirmPassword String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EnteredFirstName firstName ->
            updateForm (\form -> { form | firstName = firstName }) model

        EnteredLastName lastName ->
            updateForm (\form -> { form | lastName = lastName }) model

        EnteredEmail email ->
            updateForm (\form -> { form | email = email }) model

        EnteredBirthday birthday ->
            updateForm (\form -> { form | birthday = birthday }) model

        EnteredPhoneNumber phoneNumber ->
            updateForm (\form -> { form | phoneNumber = phoneNumber }) model

        EnteredUsername username ->
            updateForm (\form -> { form | username = username }) model

        EnteredPassword password ->
            updateForm (\form -> { form | password = password }) model

        EnteredConfirmPassword confirmPassword ->
            updateForm (\form -> { form | confirmPassword = confirmPassword }) model

        SubmittedForm ->
            Debug.todo "SubmittedForm"


updateForm : (UnvalidatedForm -> UnvalidatedForm) -> Model -> ( Model, Cmd Msg )
updateForm transform (Model model) =
    ( Model { model | unvalidatedForm = transform model.unvalidatedForm, dirty = True }, Cmd.none )



-- VALIDATION


type alias Error =
    ()


validate : UnvalidatedForm -> Result ( Error, List Error ) ValidatedForm
validate unvalidatedForm =
    Debug.todo "validate"
