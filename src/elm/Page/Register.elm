module Page.Register exposing
    ( Model
    , Msg
    , init
    , update
    , view
    )

import Accessibility as Html exposing (Attribute, Html)
import Accessibility.Aria as Aria
import Date exposing (Date)
import DeviceId
import Email
import Flags exposing (Flags)
import Html.Attributes as Attributes
import Html.Events as Events
import Iso8601
import Json.Encode as Encode
import Maybe.Extra as Maybe
import Result.Extra as Result
import SessionId
import UserId
import Valid exposing (Valid)



-- MODEL


type Model
    = Model InternalModel


type alias InternalModel =
    { unvalidatedForm : UnvalidatedForm
    , dirty : Bool
    , hasSubmitted : Bool
    , flags : Flags
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


type alias Form =
    { firstName : String
    , lastName : String
    , email : String
    , birthday : Date
    , phoneNumber : String -- TODO: Make module
    , username : String
    , password : String
    }


init :
    Flags
    -> ( Model, Cmd Msg )
init flags =
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
        , hasSubmitted = False
        , flags = flags
        }
    , Cmd.none
    )



-- VALIDATION


type alias Validator output =
    Valid.Validator String String output


requiredString : Validator String
requiredString =
    Valid.validator
        (\value ->
            if String.trim value == "" then
                Err "Must not be blank"

            else
                Ok value
        )


type alias FormValidators =
    { firstName : Validator String
    , lastName : Validator String
    , email : Validator String
    , birthday : Validator Date
    , phoneNumber : Validator String
    , username : Validator String
    , password : Validator String
    , confirmPassword : Validator String
    }


mkValidators :
    Model
    -> FormValidators
mkValidators (Model model) =
    { firstName = requiredString
    , lastName = requiredString
    , email =
        requiredString
            |> Valid.andThenValidator
                (\email ->
                    if Email.fromString email == Nothing then
                        Err "Must be a valid e-mail"

                    else
                        Ok email
                )
    , username = requiredString
    , birthday =
        requiredString
            |> Valid.andThenValidator
                Date.fromIsoString
            |> Valid.andThenValidator
                (\birthday ->
                    let
                        today : Date
                        today =
                            Date.fromPosix model.flags.timeZone model.flags.now
                    in
                    if Date.compare birthday today == GT then
                        Err "Must be in the past"

                    else
                        Ok birthday
                )
    , phoneNumber = requiredString
    , password = requiredString
    , confirmPassword = requiredString
    }


type alias Error =
    ()


validate : UnvalidatedForm -> Result ( Error, List Error ) (Valid Form)
validate unvalidatedForm =
    Debug.todo ""



-- VIEW


view : Model -> { title : String, body : List (Html Msg) }
view (Model model) =
    let
        validators : FormValidators
        validators =
            mkValidators (Model model)

        showValidation : Bool
        showValidation =
            model.dirty && model.hasSubmitted

        maskValidator : a -> Maybe a
        maskValidator =
            if showValidation then
                Just

            else
                \_ -> Nothing

        encodedFlags : String
        encodedFlags =
            Encode.encode 4
                (Encode.object
                    [ ( "deviceId", DeviceId.encode model.flags.deviceId )
                    , ( "sessionId", SessionId.encode model.flags.sessionId )
                    , ( "userId", UserId.encode model.flags.userId )
                    , ( "now", Iso8601.encode model.flags.now )
                    ]
                )
    in
    { title = "Register"
    , body =
        [ Html.div []
            [ Html.formWithListeners
                [ Attributes.class "flex flex-col gap-1"
                , Events.onSubmit SubmittedForm
                ]
                [ inputText
                    { value = model.unvalidatedForm.firstName
                    , type_ = "text"
                    , onInput = EnteredFirstName
                    , description = Nothing
                    , id = "first-name"
                    , label = Html.text "First name"
                    , required = Just True
                    , inputAttributes =
                        [ Attributes.attribute "autocomplete" "given-name"
                        ]
                    , validator = maskValidator << validators.firstName
                    }
                , inputText
                    { value = model.unvalidatedForm.lastName
                    , type_ = "text"
                    , onInput = EnteredLastName
                    , description = Nothing
                    , id = "last-name"
                    , label = Html.text "Last name"
                    , required = Just True
                    , inputAttributes =
                        [ Attributes.attribute "autocomplete" "family-name"
                        ]
                    , validator = maskValidator << validators.lastName
                    }
                , inputText
                    { value = model.unvalidatedForm.email
                    , type_ = "email"
                    , onInput = EnteredEmail
                    , description = Nothing
                    , id = "email"
                    , label = Html.text "E-mail"
                    , required = Just True
                    , inputAttributes =
                        [ Attributes.attribute "autocomplete" "email"
                        ]
                    , validator = maskValidator << validators.email
                    }
                , inputText
                    { value = model.unvalidatedForm.phoneNumber
                    , type_ = "tel"
                    , onInput = EnteredPhoneNumber
                    , description = Nothing
                    , id = "phone-number"
                    , label = Html.text "Phone number"
                    , required = Nothing
                    , inputAttributes =
                        [ Attributes.attribute "autocomplete" "tel"
                        ]
                    , validator = maskValidator << validators.phoneNumber
                    }
                , inputText
                    { value =
                        model.unvalidatedForm.birthday |> Maybe.map Date.toIsoString |> Maybe.withDefault ""
                    , type_ = "date"
                    , onInput = Date.fromIsoString >> Result.toMaybe >> EnteredBirthday
                    , description = Nothing
                    , id = "birthday"
                    , label = Html.text "Date of birth"
                    , required = Just True
                    , inputAttributes =
                        [ Attributes.attribute "autocomplete" "bday"
                        ]
                    , validator = maskValidator << validators.birthday
                    }
                , Html.button
                    []
                    [ Html.text "Submit" ]
                , Html.div []
                    [ Html.pre [] [ Html.text encodedFlags ]
                    ]
                ]
            ]
        ]
    }


inputText :
    { value : String
    , type_ : String
    , onInput : String -> msg
    , description : Maybe String
    , id : String
    , label : Html msg
    , required : Maybe Bool
    , inputAttributes : List (Attribute msg)
    , validator : String -> Maybe (Result String validated)
    }
    -> Html msg
inputText args =
    let
        validatedResult : Maybe (Result String validated)
        validatedResult =
            args.validator args.value

        isInvalid : Bool
        isInvalid =
            Maybe.withDefault False (validatedResult |> Maybe.map Result.isErr)

        ( hintText, maybeHintId ) =
            case validatedResult of
                Just (Err error) ->
                    ( error, Just (args.id ++ "-error") )

                _ ->
                    case args.description of
                        Just description ->
                            ( description, Just (args.id ++ "-hint") )

                        Nothing ->
                            let
                                zeroWidthSpace : String
                                zeroWidthSpace =
                                    "\u{200B}"
                            in
                            ( zeroWidthSpace, Nothing )

        explicitlyRequired : Bool
        explicitlyRequired =
            args.required == Just True
    in
    Html.div [ Attributes.class "flex flex-col w-fit gap-1" ]
        [ Html.labelBefore []
            (Html.div
                []
                (Html.text "First name"
                    :: (if explicitlyRequired then
                            [ Html.span [ Aria.hidden True ] [ Html.text "*" ] ]

                        else
                            []
                       )
                )
            )
            (Html.inputText args.value
                ([ Aria.invalid isInvalid
                 , Aria.describedBy (Maybe.toList maybeHintId)
                 , Events.onInput args.onInput
                 , Attributes.class "rounded-md focus:ring-2 focus:ring-offset-1 focus:ring-blue-400 transition-[--tw-ring-offset-width] duration-200"
                 , Attributes.classList [ ( "border-red-500", isInvalid ) ]
                 ]
                    ++ (args.required |> Maybe.map Aria.required |> Maybe.toList)
                    ++ args.inputAttributes
                )
            )
        , Html.div
            [ Attributes.class "text-xs"
            , Attributes.classList [ ( "text-red-500", isInvalid ) ]
            ]
            [ Html.span (maybeHintId |> Maybe.map Attributes.id |> Maybe.toList) [ Html.text hintText ] ]
        ]



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
update msg (Model model) =
    Tuple.mapFirst Model <|
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
                ( { model | hasSubmitted = True }, Cmd.none )


updateForm : (UnvalidatedForm -> UnvalidatedForm) -> InternalModel -> ( InternalModel, Cmd Msg )
updateForm transform model =
    ( { model | unvalidatedForm = transform model.unvalidatedForm, dirty = True }, Cmd.none )
