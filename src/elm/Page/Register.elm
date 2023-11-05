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
import Email exposing (Email)
import Flags exposing (Flags)
import Html.Attributes as Attributes
import Html.Events as Events
import Iso8601
import Json.Encode as Encode
import Maybe.Extra as Maybe
import PhoneNumber exposing (PhoneNumber)
import PhoneNumber.Countries as Countries
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
    , birthday : String
    , phoneNumber : String
    , username : String
    , password : String
    , confirmPassword : String
    }


type alias Form =
    { firstName : String
    , lastName : String
    , email : Email
    , birthday : Date
    , phoneNumber : PhoneNumber
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
            , birthday = ""
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


requiredStringWithMessage : String -> Validator String
requiredStringWithMessage message =
    Valid.validator
        (\value ->
            if String.trim value == "" then
                Err message

            else
                Ok value
        )


requiredString : Validator String
requiredString =
    requiredStringWithMessage "This field is required"


type alias FormValidators =
    { firstName : Validator String
    , lastName : Validator String
    , email : Validator Email
    , birthday : Validator Date
    , phoneNumber : Validator PhoneNumber
    , username : Validator String
    , password : String -> Validator String
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
                (\str ->
                    case Email.fromString str of
                        Nothing ->
                            Err "Must be a valid e-mail"

                        Just email ->
                            Ok email
                )
    , username =
        requiredString
            |> Valid.andThenValidator
                (\str ->
                    if String.length str < 6 then
                        Err "Must be at least 6 characters long"

                    else
                        Ok str
                )
    , birthday =
        requiredString
            |> Valid.andThenValidator Date.fromIsoString
            |> Valid.andThenValidator
                (\date ->
                    let
                        today : Date
                        today =
                            Date.fromPosix model.flags.timeZone model.flags.now
                    in
                    if Date.compare date today == GT then
                        Err "Must be in the past"

                    else
                        Ok date
                )
    , phoneNumber =
        Valid.validator
            (\str ->
                let
                    validate =
                        PhoneNumber.validate { defaultCountry = Countries.countryUS, otherCountries = [], types = PhoneNumber.anyType }
                in
                case validate str of
                    Just number ->
                        Ok number

                    Nothing ->
                        Err "Must be a valid phone number"
            )
    , password =
        \confirmPassword ->
            requiredString
                |> Valid.andThenValidator
                    (\password ->
                        if password == confirmPassword then
                            Ok password

                        else
                            Err "Passwords must match"
                    )
    }


validateForm : Model -> Result String (Valid Form)
validateForm (Model model) =
    let
        validators =
            mkValidators (Model model)
    in
    Valid.map Form (validators.firstName model.unvalidatedForm.firstName)
        |> Valid.apply (validators.lastName model.unvalidatedForm.lastName)
        |> Valid.apply (validators.email model.unvalidatedForm.email)
        |> Valid.apply (validators.birthday model.unvalidatedForm.birthday)
        |> Valid.apply (validators.phoneNumber model.unvalidatedForm.phoneNumber)
        |> Valid.apply (validators.username model.unvalidatedForm.username)
        |> Valid.apply (validators.password model.unvalidatedForm.confirmPassword model.unvalidatedForm.password)


view : Model -> { title : String, body : List (Html Msg) }
view (Model model) =
    let
        validators : FormValidators
        validators =
            mkValidators (Model model)

        showValidation : Bool
        showValidation =
            -- model.dirty &&
            model.hasSubmitted

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
                    { value = model.unvalidatedForm.birthday
                    , type_ = "date"
                    , onInput = EnteredBirthday
                    , description = Nothing
                    , id = "birthday"
                    , label = Html.text "Date of birth"
                    , required = Just True
                    , inputAttributes =
                        [ Attributes.attribute "autocomplete" "bday"
                        ]
                    , validator = maskValidator << validators.birthday
                    }
                , inputText
                    { value = model.unvalidatedForm.username
                    , type_ = "text"
                    , onInput = EnteredUsername
                    , description = Just "Must be at least 6 characters long"
                    , id = "username"
                    , label = Html.text "Username"
                    , required = Just True
                    , inputAttributes = []
                    , validator = maskValidator << validators.username
                    }
                , inputText
                    { value = model.unvalidatedForm.password
                    , type_ = "password"
                    , onInput = EnteredPassword
                    , description = Nothing
                    , id = "password"
                    , label = Html.text "Password"
                    , required = Just True
                    , inputAttributes = []
                    , validator = maskValidator << validators.password model.unvalidatedForm.confirmPassword
                    }
                , inputText
                    { value = model.unvalidatedForm.confirmPassword
                    , type_ = "password"
                    , onInput = EnteredConfirmPassword
                    , description = Nothing
                    , id = "password"
                    , label = Html.text "Confirm password"
                    , required = Just True
                    , inputAttributes = []
                    , validator = maskValidator << validators.password model.unvalidatedForm.password
                    }
                , Html.div [ Attributes.class "mt-4" ]
                    [ Html.button
                        [ Attributes.type_ "submit"
                        , Attributes.class "bg-blue-800 text-white px-4 py-2 rounded-md focus:ring-2 focus:ring-offset-1 focus:ring-blue-400 transition-[--tw-ring-offset-width] duration-200"
                        ]
                        [ Html.text "Submit" ]
                    ]
                ]
            , Html.div []
                [ Html.pre [] [ Html.text encodedFlags ]
                ]
            , Html.div []
                [ Html.pre [] [ Html.text (Debug.toString (validateForm (Model model))) ]
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
    , label : Html Never
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
                                -- To avoid layout shift when showing/hiding the
                                -- hint we want to always render something, so
                                -- even when there is no hint error or
                                -- description we render a zero-width space (the
                                -- empty string "" won't work here since it
                                -- doesn't take up any height).
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
                (args.label
                    :: (if explicitlyRequired then
                            [ Html.span [ Aria.hidden True ] [ Html.text "*" ] ]

                        else
                            []
                       )
                )
            )
            (Html.inputText args.value
                ([ Attributes.type_ args.type_
                 , Aria.invalid isInvalid
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
    | EnteredBirthday String
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
