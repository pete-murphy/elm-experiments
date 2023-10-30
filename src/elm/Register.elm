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
import Html.Attributes as Attributes
import Html.Events as Events
import Maybe.Extra as Maybe
import Time



-- MODEL


type Model
    = Model InternalModel


type alias InternalModel =
    { unvalidatedForm : UnvalidatedForm
    , dirty : Bool
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
    { deviceId : DeviceId
    , sessionId : SessionId
    , userId : UserId
    , now : Time.Posix
    }
    -> ( Model, Cmd Msg )
init _ =
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
        }
    , Cmd.none
    )



-- VIEW


view : Model -> { title : String, body : List (Html Msg) }
view (Model { unvalidatedForm }) =
    let
        firstNameErrorId =
            "first-name-error"

        firstNameInvalid =
            if String.trim unvalidatedForm.firstName == "" then
                Just "Must not be blank"

            else
                Nothing
    in
    { title = "Register"
    , body =
        [ Html.div []
            [ Html.form
                [ Attributes.class "p-1"
                ]
                [ Html.labelBefore []
                    (Html.div
                        []
                        [ Html.text "First name", Html.sup [ Aria.hidden True ] [ Html.text "✱" ] ]
                    )
                    (Html.inputText unvalidatedForm.firstName
                        [ Attributes.attribute "autocomplete" "given-name"
                        , Aria.required True
                        , Aria.invalid (Maybe.isJust firstNameInvalid)
                        , Aria.describedBy (Maybe.toList firstNameInvalid |> List.map (\_ -> firstNameErrorId))
                        , Events.onInput EnteredFirstName
                        , Attributes.class "rounded-md"
                        , Attributes.classList [ ( "border-red-500", Maybe.isJust firstNameInvalid ) ]
                        ]
                    )
                , Html.div
                    [ Attributes.classList [ ( "text-red-500", Maybe.isJust firstNameInvalid ) ] ]
                    -- [ Html.span [ Attributes.id firstNameErrorId ] [ "Must not be blank" ] ]
                    (Maybe.toList firstNameInvalid |> List.map (\error -> Html.span [ Attributes.id firstNameErrorId ] [ Html.text error ]))
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
