module Main exposing (main)

import Array
import Browser
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (get)
import Ionicon as Ion exposing (checkmarkRound)
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline as JDP exposing (required)
import List.Extra as LX exposing (groupsOf)
import Random exposing (generate)
import Regex
import Task


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Background =
    { name : String
    , page : Int
    , freeSkill : String
    , quickSkills : List String
    , growthList : List String
    , learningList : List String
    }


attributes : List String
attributes =
    [ "Strength"
    , "Dexterity"
    , "Constitution"
    , "Intelligence"
    , "Wisdom"
    , "Charisma"
    ]


attributeModifiers : List ( Int, List Int )
attributeModifiers =
    [ ( -2, [ 3 ] )
    , ( -1, [ 4, 5, 6, 7 ] )
    , ( 0, [ 8, 9, 10, 11, 12, 13 ] )
    , ( 1, [ 14, 15, 16, 17 ] )
    , ( 2, [ 18 ] )
    ]


type alias Skill =
    { name : String
    , description : String
    , level : Int
    }


type alias PsychicSkill =
    { name : String
    , page : Int
    , coreAbility : List String
    , level : Int
    }


type alias ClassStats =
    { partial : List String
    , full : List String
    , hpBonus : Int
    , atkBonus : Int
    }


type BaseClass
    = Warrior ClassStats
    | Expert ClassStats
    | Psychic ClassStats


type
    Class
    -- NOTE: If class includes Warrior, +2 HP and +1 BAB
    = Full BaseClass
    | Partial BaseClass BaseClass


type alias Focus =
    { name : String
    , level : Int
    , description : List String
    }


type alias Equipment =
    { name : String
    , loadout : List String
    }


type alias GameData =
    { backgrounds : List Background
    , classes : List BaseClass
    , equipment : List Equipment
    , foci : List Focus
    , skills : List Skill
    , psySkills : List PsychicSkill
    }


type SkillSelectionStyle
    = Quick
    | Roll
    | Pick
    | NoSkill


type ClassSelectionStyle
    = Whole
    | Half


type AttrSelectionStyle
    = Standard
    | Random


type alias AppState =
    { step : Int
    , attrArray : List Int
    , attrStyle : AttrSelectionStyle
    , roll : List Int
    , skillRolls : Int
    , bgRolls : List String
    , skillChoices : Int
    , bgPicks : List String
    , skillStyle : SkillSelectionStyle
    , classStyle : ClassSelectionStyle
    , selectedPsySkills : ( Maybe PsychicSkill, Maybe PsychicSkill )
    , selectedClasses : ( Maybe BaseClass, Maybe BaseClass )
    , selectedAttrIndex : ( Maybe Int, Maybe Int )
    , selectedArrayIndex : ( Maybe Int, Maybe Int )
    , nameLocked : Bool
    }


type alias Model =
    { data : GameData
    , error : Maybe String
    , state : AppState
    , name : String
    , isHuman : Bool
    , attributes : List Int
    , background : Background
    , skills : List Skill
    , psySkills : List PsychicSkill
    , class : Maybe Class
    , foci : List Focus
    , equipment : Equipment
    }



-- INIT


init : () -> ( Model, Cmd Msg )
init _ =
    update GetAllData initModel


initModel : Model
initModel =
    { data = initGameData
    , error = Nothing
    , state = initAppState
    , name = ""
    , isHuman = True
    , attributes = 0 |> List.repeat 6
    , background = initBackground
    , skills = []
    , psySkills = []
    , class = Nothing
    , foci = []
    , equipment = Equipment "" []
    }


initGameData : GameData
initGameData =
    { backgrounds = []
    , classes = []
    , equipment = []
    , foci = []
    , skills = []
    , psySkills = []
    }


initAppState : AppState
initAppState =
    { step = 0
    , attrArray = [ 14, 12, 11, 10, 9, 7 ]
    , attrStyle = Standard
    , roll = []
    , skillRolls = 3
    , bgRolls = []
    , skillChoices = 2
    , bgPicks = []
    , skillStyle = NoSkill
    , classStyle = Whole
    , selectedPsySkills = ( Nothing, Nothing )
    , selectedClasses = ( Nothing, Nothing )
    , selectedAttrIndex = ( Nothing, Nothing )
    , selectedArrayIndex = ( Nothing, Nothing )
    , nameLocked = False
    }


initBackground : Background
initBackground =
    { name = ""
    , page = 0
    , freeSkill = ""
    , quickSkills = []
    , growthList = []
    , learningList = []
    }



-- DECODERS


decodeBackgrounds : JD.Decoder (List Background)
decodeBackgrounds =
    JD.list
        (JD.succeed Background
            |> JDP.required "name" JD.string
            |> JDP.required "page" JD.int
            |> JDP.required "freeSkill" JD.string
            |> JDP.required "quickSkills" (JD.list JD.string)
            |> JDP.required "growth" (JD.list JD.string)
            |> JDP.required "learning" (JD.list JD.string)
        )


decodeBaseClasses : JD.Decoder (List BaseClass)
decodeBaseClasses =
    JD.map
        (\classDict ->
            classDict
                |> Dict.toList
                |> List.filterMap
                    (\( k, v ) ->
                        case k of
                            "Warrior" ->
                                Just (Warrior v)

                            "Expert" ->
                                Just (Expert v)

                            "Psychic" ->
                                Just (Psychic v)

                            invalidName ->
                                Maybe.Nothing
                    )
        )
        (JD.dict decodeClassStats)


decodeClassStats : JD.Decoder ClassStats
decodeClassStats =
    JD.succeed ClassStats
        |> JDP.required "partial" (JD.list JD.string)
        |> JDP.required "full" (JD.list JD.string)
        |> JDP.required "hpBonus" JD.int
        |> JDP.required "atkBonus" JD.int


decodeEquipment : JD.Decoder (List Equipment)
decodeEquipment =
    JD.list
        (JD.succeed Equipment
            |> JDP.required "name" JD.string
            |> JDP.required "loadout" (JD.list JD.string)
        )


decodeFoci : JD.Decoder (List Focus)
decodeFoci =
    JD.list
        (JD.succeed Focus
            |> JDP.required "name" JD.string
            |> JDP.hardcoded 0
            |> JDP.required "description" (JD.list JD.string)
        )


decodeSkills : JD.Decoder (List Skill)
decodeSkills =
    JD.list
        (JD.succeed Skill
            |> JDP.required "name" JD.string
            |> JDP.required "description" JD.string
            |> JDP.hardcoded 0
        )


decodePsySkills : JD.Decoder (List PsychicSkill)
decodePsySkills =
    JD.list
        (JD.succeed PsychicSkill
            |> JDP.required "name" JD.string
            |> JDP.required "page" JD.int
            |> JDP.required "core" (JD.list JD.string)
            |> JDP.hardcoded 0
        )


decodeAllData : JD.Decoder GameData
decodeAllData =
    JD.succeed GameData
        |> JDP.required "backgrounds" decodeBackgrounds
        |> JDP.required "classes" decodeBaseClasses
        |> JDP.required "equipment" decodeEquipment
        |> JDP.required "foci" decodeFoci
        |> JDP.required "skills" decodeSkills
        |> JDP.required "psychic_skills" decodePsySkills



-- JSON GETTER


getAllData : (Result Http.Error GameData -> Msg) -> Cmd Msg
getAllData cmd =
    Http.get
        { url = "../data/data.json"
        , expect = Http.expectJson cmd decodeAllData
        }



-- UPDATE


type Msg
    = NoOp
    | GetAllData
    | GotGameData (Result Http.Error GameData)
    | RollDice Int Int (Cmd Msg)
    | AssignRollResult (Cmd Msg) (List Int)
    | ChangeStep Int Msg
      -- Step 0
    | ChooseRace Bool
      -- Step 1
    | ChooseBackground Background
      -- Step 2
    | ChooseSkillStyle SkillSelectionStyle
    | StoreRolledBgSkill (List String)
    | ResetBgRoll Int
    | PickBgSkill String
    | ResetBgPick Int
      -- Step 3
    | ChooseFullClass BaseClass
    | ChoosePartialClass BaseClass
    | ChooseClassStyle ClassSelectionStyle
    | LockClass
      -- Step 4
    | ChoosePsychicSkill PsychicSkill Bool
    | UntogglePsychicSkill PsychicSkill
    | RemovePsychicSkills
    | LockPsychicSkills
      -- Step 5
    | ChooseFocus Focus
    | RemoveFocus
      -- Step 6/7
    | ChooseFreeSkill Skill
    | RemoveSkill
      -- Step 8
    | ChooseEquipment Equipment
      -- Step 9
    | ChooseAttributeStyle AttrSelectionStyle
    | SelectAttrArrayItem Int
    | SelectAttributeItem Int
    | SwitchSelectedArrayItems
    | SwitchSelectedAttrItems
    | SwitchSelectedItems
      -- Step 10
    | ToggleCharacterNameLock
    | SetCharacterName String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        GetAllData ->
            ( model, getAllData GotGameData )

        GotGameData result ->
            case result of
                Err httpError ->
                    case httpError of
                        Http.BadBody errStr ->
                            ( { model | error = Just errStr }, Cmd.none )

                        _ ->
                            ( model, Cmd.none )

                Ok data ->
                    ( { model | data = data, error = Nothing }, Cmd.none )

        RollDice dice faces callback ->
            ( model, generate (AssignRollResult callback) (rollDice dice faces) )

        AssignRollResult callback results ->
            let
                oldState =
                    model.state

                newState =
                    { oldState | roll = results }
            in
            ( { model | state = newState }, callback )

        ChangeStep diff callback ->
            let
                oldState =
                    model.state

                ( rolled, chosen ) =
                    case model.state.skillStyle of
                        Pick ->
                            ( [], model.state.bgPicks )

                        Roll ->
                            ( model.state.bgRolls, [] )

                        _ ->
                            ( [], [] )
            in
            ( { model
                | state =
                    { oldState
                        | step = oldState.step + diff
                        , bgPicks = chosen
                        , bgRolls = rolled
                    }
              }
            , Task.succeed callback |> Task.perform identity
            )

        ChooseRace isHuman ->
            ( { model | isHuman = isHuman, state = model.state |> setStep 1 }, Cmd.none )

        ChooseBackground background ->
            ( { model | background = background, state = model.state |> setStep 2 }, Cmd.none )

        ChooseSkillStyle clickedStyle ->
            let
                oldState =
                    model.state
            in
            ( { model
                | state =
                    { oldState
                        | skillStyle =
                            if model.state.skillStyle == clickedStyle then
                                NoSkill

                            else
                                clickedStyle
                    }
              }
            , Cmd.none
            )

        StoreRolledBgSkill rollTable ->
            let
                rollResult =
                    model.state.roll |> List.head |> Maybe.withDefault 1 |> (+) -1

                oldState =
                    model.state

                newState =
                    { oldState
                        | bgRolls =
                            rollTable
                                |> LX.getAt rollResult
                                |> Maybe.withDefault ""
                                |> List.singleton
                                |> List.append model.state.bgRolls
                        , skillRolls =
                            if model.state.skillRolls > 0 then
                                model.state.skillRolls - 1

                            else
                                model.state.skillRolls
                    }
            in
            ( { model | state = newState }, Cmd.none )

        ResetBgRoll index ->
            let
                oldState =
                    model.state

                newState =
                    { oldState
                        | bgRolls = model.state.bgRolls |> LX.removeAt index
                        , skillRolls =
                            if model.state.skillRolls < 3 then
                                model.state.skillRolls + 1

                            else
                                model.state.skillRolls
                    }
            in
            ( { model | state = newState }, Cmd.none )

        PickBgSkill skill ->
            let
                oldState =
                    model.state

                ( picks, pickList ) =
                    if model.state.skillChoices > 0 then
                        ( model.state.skillChoices - 1
                        , skill
                            |> List.singleton
                            |> List.append model.state.bgPicks
                        )

                    else
                        ( 0, model.state.bgPicks )
            in
            ( { model
                | state =
                    { oldState
                        | bgPicks = pickList
                        , skillChoices = picks
                    }
              }
            , Cmd.none
            )

        ResetBgPick index ->
            let
                oldState =
                    model.state

                newState =
                    { oldState
                        | bgPicks = model.state.bgPicks |> LX.removeAt index
                        , skillChoices =
                            if model.state.skillChoices < 2 then
                                model.state.skillChoices + 1

                            else
                                2
                    }
            in
            ( { model | state = newState }, Cmd.none )

        ChooseFullClass baseClass ->
            let
                step =
                    if baseClassToString baseClass == "Psychic" then
                        4

                    else
                        5
            in
            ( { model | class = Just (Full baseClass), state = model.state |> setStep step }, Cmd.none )

        ChoosePartialClass baseClass ->
            ( { model | state = baseClass |> togglePartialClass model.state }, Cmd.none )

        ChooseClassStyle classStyle ->
            let
                oldState =
                    model.state
            in
            ( { model
                | state =
                    { oldState
                        | classStyle = classStyle
                        , selectedClasses = ( Nothing, Nothing )
                    }
              }
            , Cmd.none
            )

        LockClass ->
            let
                newClass =
                    case model.state.selectedClasses of
                        ( Nothing, Nothing ) ->
                            Nothing

                        ( Just a, Nothing ) ->
                            Just (Full a)

                        ( Nothing, Just a ) ->
                            Just (Full a)

                        ( Just a, Just b ) ->
                            Just (Partial a b)
            in
            ( { model | class = newClass }, Cmd.none )

        ChoosePsychicSkill psySkill isPartialPsychic ->
            let
                newStep =
                    if isPartialPsychic then
                        model.state.step + 1

                    else
                        model.state.step

                newState =
                    psySkill |> togglePsychicSkill model.state
            in
            ( { model | state = { newState | step = newStep } }, Task.succeed identity |> Task.perform (\_ -> LockPsychicSkills) )

        UntogglePsychicSkill psySkill ->
            ( { model | state = psySkill |> removePsychicSkill model.state }, Cmd.none )

        RemovePsychicSkills ->
            let
                oldState =
                    model.state
            in
            ( { model | state = { oldState | selectedPsySkills = ( Nothing, Nothing ) } }, Cmd.none )

        LockPsychicSkills ->
            let
                newSkills =
                    case model.state.selectedPsySkills of
                        ( Nothing, Nothing ) ->
                            []

                        ( Just a, Nothing ) ->
                            [ a ]

                        ( Nothing, Just b ) ->
                            [ b ]

                        ( Just a, Just b ) ->
                            [ a, b ]
            in
            ( { model | psySkills = newSkills }, Cmd.none )

        ChooseFocus focus ->
            let
                oldState =
                    model.state
            in
            ( { model
                | foci = focus :: model.foci
                , state = { oldState | step = model.state.step + 1 }
              }
            , Cmd.none
            )

        RemoveFocus ->
            let
                oldState =
                    model.state
            in
            ( { model
                | foci =
                    model.foci
                        |> List.tail
                        |> Maybe.withDefault []
              }
            , Cmd.none
            )

        ChooseFreeSkill skill ->
            let
                oldState =
                    model.state
            in
            ( { model
                | skills = skill :: model.skills
                , state = { oldState | step = model.state.step + 1 }
              }
            , Cmd.none
            )

        RemoveSkill ->
            let
                oldState =
                    model.state
            in
            ( { model
                | skills =
                    model.skills
                        |> List.tail
                        |> Maybe.withDefault []
              }
            , Cmd.none
            )

        ChooseEquipment equipment ->
            let
                oldState =
                    model.state
            in
            ( { model
                | equipment = equipment
                , state = { oldState | step = model.state.step + 1 }
              }
            , Cmd.none
            )

        ChooseAttributeStyle attrStyle ->
            let
                oldState =
                    model.state

                rolls =
                    case attrStyle of
                        Standard ->
                            [ 14, 12, 11, 10, 9, 7 ]

                        Random ->
                            model.state.roll
                                |> LX.groupsOf 3
                                |> List.map List.sum
            in
            ( { model
                | state =
                    { oldState
                        | attrStyle = attrStyle
                        , attrArray = rolls
                    }
                , attributes = 0 |> List.repeat 6
              }
            , Cmd.none
            )

        SelectAttrArrayItem index ->
            let
                newIdx =
                    model.state.selectedArrayIndex |> manageClickedItemToggle model index

                cmd =
                    case newIdx of
                        ( Just _, Just _ ) ->
                            Task.succeed identity |> Task.perform (\_ -> SwitchSelectedArrayItems)

                        _ ->
                            case model.state.selectedAttrIndex of
                                ( Just _, Just _ ) ->
                                    -- Should never happen anyways
                                    Cmd.none

                                ( Nothing, Nothing ) ->
                                    Cmd.none

                                _ ->
                                    Task.succeed identity |> Task.perform (\_ -> SwitchSelectedItems)

                oldState =
                    model.state
            in
            ( { model | state = { oldState | selectedArrayIndex = newIdx } }, cmd )

        SelectAttributeItem index ->
            let
                newIdx =
                    model.state.selectedAttrIndex |> manageClickedItemToggle model index

                cmd =
                    case newIdx of
                        ( Just _, Just _ ) ->
                            Task.succeed identity |> Task.perform (\_ -> SwitchSelectedAttrItems)

                        _ ->
                            case model.state.selectedArrayIndex of
                                ( Just _, Just _ ) ->
                                    -- Should never happen anyways
                                    Cmd.none

                                ( Nothing, Nothing ) ->
                                    Cmd.none

                                _ ->
                                    Task.succeed identity |> Task.perform (\_ -> SwitchSelectedItems)

                oldState =
                    model.state
            in
            ( { model | state = { oldState | selectedAttrIndex = newIdx } }, cmd )

        SwitchSelectedArrayItems ->
            let
                ( firstIdx, secondIdx ) =
                    model.state.selectedArrayIndex
                        |> Tuple.mapBoth (Maybe.withDefault 0) (Maybe.withDefault 0)

                newArrayList =
                    model.state.attrArray
                        |> LX.swapAt firstIdx secondIdx

                oldState =
                    model.state
            in
            ( { model
                | state =
                    { oldState
                        | selectedAttrIndex = ( Nothing, Nothing )
                        , selectedArrayIndex = ( Nothing, Nothing )
                        , attrArray = newArrayList
                    }
              }
            , Cmd.none
            )

        SwitchSelectedAttrItems ->
            let
                ( firstIdx, secondIdx ) =
                    model.state.selectedAttrIndex
                        |> Tuple.mapBoth (Maybe.withDefault 0) (Maybe.withDefault 0)

                newAttrList =
                    model.attributes
                        |> LX.swapAt firstIdx secondIdx

                oldState =
                    model.state
            in
            ( { model
                | state =
                    { oldState
                        | selectedAttrIndex = ( Nothing, Nothing )
                        , selectedArrayIndex = ( Nothing, Nothing )
                    }
                , attributes = newAttrList
              }
            , Cmd.none
            )

        SwitchSelectedItems ->
            let
                arrayIndex =
                    model.state.selectedArrayIndex |> Tuple.first |> Maybe.withDefault 0

                attrIndex =
                    model.state.selectedAttrIndex |> Tuple.first |> Maybe.withDefault 0

                ( newArrayList, newAttrList ) =
                    ( model.state.attrArray, arrayIndex )
                        |> swapArrayItemsWith ( model.attributes, attrIndex )

                oldState =
                    model.state
            in
            ( { model
                | state =
                    { oldState
                        | selectedAttrIndex = ( Nothing, Nothing )
                        , selectedArrayIndex = ( Nothing, Nothing )
                        , attrArray = newArrayList
                    }
                , attributes = newAttrList
              }
            , Cmd.none
            )

        ToggleCharacterNameLock ->
            let
                oldState =
                    model.state
            in
            ( { model | state = { oldState | nameLocked = not model.state.nameLocked } }, Cmd.none )

        SetCharacterName text ->
            ( { model | name = text }, Cmd.none )



-- UPDATE FUNCTIONS


rollDice : Int -> Int -> Random.Generator (List Int)
rollDice dice faces =
    Random.list dice (Random.int 1 faces)


setStep : Int -> AppState -> AppState
setStep newStep state =
    { state | step = newStep }


togglePsychicSkill : AppState -> PsychicSkill -> AppState
togglePsychicSkill appState psychicSkill =
    let
        newSkills =
            case appState.selectedPsySkills of
                ( Nothing, Nothing ) ->
                    ( Just psychicSkill, Nothing )

                ( Just a, Nothing ) ->
                    ( Just a, Just psychicSkill )

                ( Nothing, Just b ) ->
                    ( Just b, Just psychicSkill )

                ( Just a, Just b ) ->
                    if a.name == psychicSkill.name then
                        ( Just b, Nothing )

                    else if b.name == psychicSkill.name then
                        ( Just a, Nothing )

                    else
                        ( Just a, Just b )
    in
    { appState | selectedPsySkills = newSkills }


removePsychicSkill : AppState -> PsychicSkill -> AppState
removePsychicSkill appState psychicSkill =
    let
        newSkills =
            case appState.selectedPsySkills of
                ( Nothing, Nothing ) ->
                    ( Nothing, Nothing )

                ( Just a, Nothing ) ->
                    if a.name == psychicSkill.name then
                        ( Nothing, Nothing )

                    else
                        ( Just a, Nothing )

                ( Nothing, Just b ) ->
                    if b.name == psychicSkill.name then
                        ( Nothing, Nothing )

                    else
                        ( Just b, Nothing )

                ( Just a, Just b ) ->
                    if a.name == psychicSkill.name then
                        ( Just b, Nothing )

                    else if b.name == psychicSkill.name then
                        ( Just a, Nothing )

                    else
                        ( Just a, Just b )
    in
    { appState | selectedPsySkills = newSkills }


togglePartialClass : AppState -> BaseClass -> AppState
togglePartialClass appState baseClass =
    let
        classStr =
            baseClassToString baseClass

        newClasses =
            case appState.selectedClasses of
                ( Nothing, Nothing ) ->
                    ( Just baseClass, Nothing )

                ( Just a, Nothing ) ->
                    if baseClassToString a == classStr then
                        ( Nothing, Nothing )

                    else
                        ( Just a, Just baseClass )

                ( Just a, Just b ) ->
                    if baseClassToString a == classStr then
                        ( Just b, Nothing )

                    else if baseClassToString b == classStr then
                        ( Just a, Nothing )

                    else
                        ( Just a, Just b )

                ( Nothing, Just a ) ->
                    if baseClassToString a == classStr then
                        ( Nothing, Nothing )

                    else
                        ( Just a, Just baseClass )
    in
    { appState | selectedClasses = newClasses }


baseClassToString : BaseClass -> String
baseClassToString baseClass =
    case baseClass of
        Warrior _ ->
            "Warrior"

        Expert _ ->
            "Expert"

        Psychic _ ->
            "Psychic"


manageClickedItemToggle : Model -> Int -> ( Maybe Int, Maybe Int ) -> ( Maybe Int, Maybe Int )
manageClickedItemToggle model index toggleSet =
    case toggleSet of
        ( Nothing, Nothing ) ->
            ( Just index, Nothing )

        ( Just a, Nothing ) ->
            ( Just a, Just index )

        ( Nothing, Just b ) ->
            ( Just b, Just index )

        ( Just a, Just b ) ->
            if a == index then
                ( Nothing, Just b )

            else if b == index then
                ( Just a, Nothing )

            else
                ( Just b, Just index )


swapArrayItemsWith : ( List Int, Int ) -> ( List Int, Int ) -> ( List Int, List Int )
swapArrayItemsWith ( otherList, otherIdx ) ( thisList, thisIdx ) =
    let
        thisArray =
            thisList |> Array.fromList

        otherArray =
            otherList |> Array.fromList
    in
    ( thisArray
        |> Array.set thisIdx (Maybe.withDefault 0 <| Array.get otherIdx otherArray)
        |> Array.toList
    , otherArray
        |> Array.set otherIdx (Maybe.withDefault 0 <| Array.get thisIdx thisArray)
        |> Array.toList
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


{-| Step 0: RACE
-}
renderRaceQuery : Model -> Html Msg
renderRaceQuery model =
    -- Step 0
    div []
        [ h1 [ class "center-text title-text" ] [ text "Are you playing a Human?" ]
        , button [ onClick (ChooseRace True), class "stretch" ] [ text "Yes" ]
        , button [ onClick (ChooseRace False), class "stretch" ] [ text "No" ]
        ]


{-| Step 1: BACKGROUND
-}
renderBackgroundSelection : Model -> Html Msg
renderBackgroundSelection model =
    -- Step 1
    div []
        [ h1 [ class "center-text title-text" ] [ text "Background" ]
        , div [ class "card-list" ]
            (model.data.backgrounds |> List.map renderBackgroundCard)
        ]


renderBackgroundCard : Background -> Html Msg
renderBackgroundCard background =
    div
        [ onClick (ChooseBackground background)
        , class "card"
        ]
        [ div [ class "card-header" ]
            [ p [ class "card-info" ]
                [ text ("(p." ++ String.fromInt background.page ++ ")") ]
            , h3 [ class "card-title" ]
                [ text background.name ]
            ]
        , div [ class "card-content" ]
            [ strong []
                [ text "Key Skills: " ]
            , p [ class "inline" ]
                [ text
                    (background.learningList
                        |> LX.unique
                        |> String.join ", "
                    )
                ]
            ]
        ]


{-| Step 2: BACKGROUND DETAILS
-}
renderBackgroundDetailSelection : Model -> Html Msg
renderBackgroundDetailSelection model =
    let
        ( qs, rs, ps ) =
            case model.state.skillStyle of
                Quick ->
                    ( "nav-active", "", "" )

                Roll ->
                    ( "", "nav-active", "" )

                Pick ->
                    ( "", "", "nav-active" )

                NoSkill ->
                    ( "", "", "" )

        ( content, nextDisabled ) =
            case model.state.skillStyle of
                Quick ->
                    ( renderQuickSkills model, False )

                Roll ->
                    ( renderRollSkills model, model.state.skillRolls > 0 )

                Pick ->
                    ( renderPickSkills model, model.state.skillChoices > 0 )

                NoSkill ->
                    ( div [] [], True )
    in
    -- Step 2
    div []
        (List.append
            [ h1 [ class "center-text title-text" ]
                [ text ("Customize " ++ model.background.name ++ " Background") ]
            , div [ class "nav" ]
                [ h2
                    [ class ("nav-item " ++ qs)
                    , onClick (ChooseSkillStyle Quick)
                    ]
                    [ text "Quick Skills" ]
                , h2
                    [ class ("nav-item " ++ rs)
                    , onClick (ChooseSkillStyle Roll)
                    ]
                    [ text "Roll Skills" ]
                , h2
                    [ class ("nav-item " ++ ps)
                    , onClick (ChooseSkillStyle Pick)
                    ]
                    [ text "Pick Skills" ]
                ]
            ]
            [ content
            , button
                [ class "back"
                , onClick (ChangeStep -1 NoOp)
                ]
                [ text "Background" ]
            , button
                [ class "next"
                , disabled nextDisabled
                , onClick (ChangeStep 1 NoOp)
                ]
                [ text "Class" ]
            ]
        )


renderQuickSkills : Model -> Html Msg
renderQuickSkills model =
    div []
        [ h1 [ class "center-text" ]
            [ text ("Quick Skills for " ++ model.background.name) ]
        , model.background.quickSkills |> renderSkillCardList Nothing
        ]


renderRollSkills : Model -> Html Msg
renderRollSkills model =
    div []
        [ h1 [ class "center-text" ] [ text "Roll 3 Skills Randomly:" ]
        , h4 [ class "center-text" ] [ em [] [ text "(click to clear cell):" ] ]
        , model.state.bgRolls |> renderSkillCardList (Just ResetBgRoll)
        , div [ class "flex" ]
            [ "Growth"
                |> renderSkillRollTable model.state.skillRolls model.background.growthList
            , "Learning"
                |> renderSkillRollTable model.state.skillRolls model.background.learningList
            ]
        ]


renderSkillRollTable : Int -> List String -> String -> Html Msg
renderSkillRollTable rolls skillList txt =
    let
        callback =
            case txt of
                "Growth" ->
                    RollDice 1 6 (Task.perform StoreRolledBgSkill (Task.succeed skillList))

                _ ->
                    RollDice 1 8 (Task.perform StoreRolledBgSkill (Task.succeed skillList))
    in
    table []
        [ thead [] [ th [ colspan 2, class "center-text" ] [ h2 [] [ text (txt ++ " List") ] ] ]
        , thead []
            [ th [] [ text "Roll" ]
            , th [] [ text "Result" ]
            ]
        , tbody []
            (List.append
                (skillList
                    |> List.indexedMap
                        (\i s ->
                            tr []
                                [ td [] [ text <| String.fromInt <| i + 1 ]
                                , td [] [ text s ]
                                ]
                        )
                )
                [ tr []
                    [ td [ colspan 2 ]
                        [ button
                            [ onClick callback
                            , class "stretch"
                            , disabled (rolls == 0)
                            ]
                            [ text ("Roll " ++ txt) ]
                        ]
                    ]
                ]
            )
        ]


renderPickSkills : Model -> Html Msg
renderPickSkills model =
    div []
        [ h1 [ class "center-text" ] [ text "Pick 2 Skills Manually:" ]
        , h4 [ class "center-text" ]
            [ text "(click to clear cell):" ]
        , model.state.bgPicks |> renderSkillCardList (Just ResetBgPick)
        , div [ class "flex half" ]
            (model.background.learningList
                |> List.filter (\s -> s |> String.contains "Any" |> not)
                |> LX.unique
                |> List.map
                    (\s ->
                        button
                            [ onClick (PickBgSkill s)
                            , disabled (model.state.skillChoices == 0)
                            ]
                            [ text s ]
                    )
            )
        ]


renderSkillCardList : Maybe (Int -> Msg) -> List String -> Html Msg
renderSkillCardList msg skillNames =
    div [ class "card-list half" ] (skillNames |> List.indexedMap (renderSkillCard msg))


renderSkillCard : Maybe (Int -> Msg) -> Int -> String -> Html Msg
renderSkillCard msg index skillName =
    let
        attrs =
            case msg of
                Just m ->
                    [ class "card-title", onClick (m index) ]

                Nothing ->
                    [ class "card-title" ]
    in
    div [ class "card skill-card" ]
        [ h3 attrs [ text skillName ] ]


{-| Step 3: CLASS
-}
renderClassSelection : Model -> Html Msg
renderClassSelection model =
    -- Step 3
    let
        ( wc, hc ) =
            case model.state.classStyle of
                Whole ->
                    ( "nav-active", "" )

                Half ->
                    ( "", "nav-active" )

        nextDisabled =
            case model.state.selectedClasses of
                ( Just a, Just b ) ->
                    False

                _ ->
                    True

        selectedClasses =
            case model.state.selectedClasses of
                ( Nothing, Nothing ) ->
                    []

                ( Just a, Nothing ) ->
                    [ baseClassToString a ]

                ( Just a, Just b ) ->
                    [ baseClassToString a, baseClassToString b ]

                ( Nothing, Just b ) ->
                    [ baseClassToString b ]

        stepChange =
            if selectedClasses |> List.member "Psychic" then
                1

            else
                2
    in
    div []
        [ h1 [ class "center-text title-text" ]
            [ text "Class" ]
        , div [ class "nav" ]
            [ h2
                [ class ("nav-item-full " ++ wc)
                , onClick (ChooseClassStyle Whole)
                ]
                [ text "Full" ]
            , h2
                [ class ("nav-item-full " ++ hc)
                , onClick (ChooseClassStyle Half)
                ]
                [ text "Partial" ]
            ]
        , div []
            [ h2 [ class "center-text" ] [ text "Choose Your Class" ]
            , div [ class "card-list" ]
                (model.data.classes
                    |> List.map (baseClassToCard model.state.classStyle selectedClasses)
                )
            ]
        , button
            [ class "back"
            , onClick (ChangeStep -2 NoOp)
            ]
            [ text "Background" ]
        , button
            [ class "next"
            , disabled nextDisabled
            , onClick (ChangeStep stepChange LockClass)
            ]
            [ text "Next" ]
        ]


baseClassToCard : ClassSelectionStyle -> List String -> BaseClass -> Html Msg
baseClassToCard classStyle selectedClasses baseClass =
    let
        classCard : String -> ClassStats -> Html Msg
        classCard name stats =
            let
                ( cmd, bonuses ) =
                    case classStyle of
                        Whole ->
                            ( ChooseFullClass, List.append stats.partial stats.full )

                        Half ->
                            ( ChoosePartialClass, stats.partial )

                cardClass =
                    if selectedClasses |> List.member name then
                        "card-selected"

                    else
                        "card"
            in
            div
                [ onClick (cmd baseClass)
                , class cardClass
                ]
                [ div [ class "card-header" ]
                    [ h3 [ class "card-title" ]
                        [ text name ]
                    ]
                , div
                    [ class "card-content" ]
                    [ ul []
                        (bonuses |> List.map (\data -> li [] [ text data ]))
                    ]
                ]
    in
    case baseClass of
        Warrior stats ->
            classCard "Warrior" stats

        Expert stats ->
            classCard "Expert" stats

        Psychic stats ->
            classCard "Psychic" stats


{-| Step 4: PSYCHIC SKILLS
-}
renderPsychicSkillSelection : Model -> Html Msg
renderPsychicSkillSelection model =
    -- Step 5
    let
        ( nextDisabled, skills ) =
            case model.state.selectedPsySkills of
                ( Just a, Just b ) ->
                    ( False, [ a, b ] )

                ( Just a, Nothing ) ->
                    ( True, [ a ] )

                ( Nothing, Just b ) ->
                    ( True, [ b ] )

                _ ->
                    ( True, [] )

        ( isPartialPsychic, infoTxt ) =
            case model.state.selectedClasses of
                ( Just a, Just b ) ->
                    ( [ a, b ] |> List.map baseClassToString |> List.member "Psychic"
                    , "Select 1 Psychic Skill"
                    )

                _ ->
                    ( False, "Select 2 Psychic Skills" )
    in
    div []
        [ h1 [ class "center-text title-text" ] [ text "Psychic Skills" ]
        , h2 [ class "center-text" ] [ text infoTxt ]
        , div [ class "card-list half" ] (skills |> List.map renderMiniPsySkillCard)
        , div [ class "card-list" ]
            (model.data.psySkills |> List.map (renderPsySkillCard isPartialPsychic))
        , button
            [ class "back"
            , onClick (ChangeStep -1 RemovePsychicSkills)
            ]
            [ text "Class" ]
        , button
            [ class "next"
            , disabled nextDisabled
            , onClick (ChangeStep 1 LockPsychicSkills)
            ]
            [ text "Focus" ]
        ]


renderMiniPsySkillCard : PsychicSkill -> Html Msg
renderMiniPsySkillCard psySkill =
    div [ class "card skill-card" ]
        [ h3
            [ class "card-title"
            , onClick (UntogglePsychicSkill psySkill)
            ]
            [ text psySkill.name ]
        ]


renderPsySkillCard : Bool -> PsychicSkill -> Html Msg
renderPsySkillCard isPartialPsychic psySkill =
    div
        [ onClick (ChoosePsychicSkill psySkill isPartialPsychic)
        , class "card row-of-2"
        ]
        [ div [ class "card-header" ]
            [ p [ class "card-info" ]
                [ text ("(p." ++ String.fromInt psySkill.page ++ ")") ]
            , h3 [ class "card-title" ]
                [ text psySkill.name ]
            ]
        , div [ class "card-content" ]
            (psySkill.coreAbility
                |> List.indexedMap
                    (\i txt ->
                        p []
                            [ strong [] [ text ("Level-" ++ String.fromInt i ++ ": ") ]
                            , p [ class "inline" ] [ text txt ]
                            ]
                    )
            )
        ]


{-| Step 5: FOCI
-}
renderFociSelection : Model -> Html Msg
renderFociSelection model =
    -- Step 5
    div []
        [ h1 [ class "center-text title-text" ] [ text "Focus" ]
        , div [ class "card-list" ]
            (model.data.foci |> List.map renderFocusCard)
        , button
            [ class "back"
            , onClick (ChangeStep -2 RemovePsychicSkills)
            ]
            [ text "Class" ]
        ]


renderFocusCard : Focus -> Html Msg
renderFocusCard focus =
    div
        [ onClick (ChooseFocus focus)
        , class "card row-of-2"
        ]
        [ div [ class "card-header" ]
            [ h3 [ class "card-title" ]
                [ text focus.name ]
            ]
        , div [ class "card-content" ]
            (focus.description
                |> List.indexedMap
                    (\i d ->
                        [ strong []
                            [ text ("Level " ++ String.fromInt (i + 1) ++ ": ") ]
                        , p [ class "inline" ]
                            [ text d ]
                        ]
                    )
                |> List.intersperse [ hr [] [] ]
                |> List.concat
            )
        ]


renderNonHumanFocus : Model -> Html Msg
renderNonHumanFocus _ =
    -- Step 3
    div []
        [ h1 [ class "center-text" ] [ text "Choose Your Focus" ]
        , h2 [ class "center-text half" ]
            [ text "As a Non-Human character, your free Level-0 Focus is replaced with the benefits gained from your Alien or AI background.  Work with your GM to define this." ]
        , div [ class "flex" ]
            [ button
                [ class "flex-item half", onClick (ChangeStep 1 NoOp) ]
                [ text "Continue" ]
            ]
        ]


{-| Step 6-7: FREE SKILLS
-}
renderCombatSkillSelection : Model -> Html Msg
renderCombatSkillSelection model =
    -- Step 6
    let
        combatSkills =
            [ "Punch", "Stab", "Shoot" ]
    in
    div []
        [ h1 [ class "center-text title-text" ] [ text "Free Combat Skill" ]
        , div [ class "card-list" ]
            (model.data.skills
                |> List.filter (\s -> combatSkills |> List.member s.name)
                |> List.map (renderLargeSkillCard "")
            )
        , button
            [ class "back"
            , onClick (ChangeStep -1 RemoveFocus)
            ]
            [ text "Focus" ]
        ]


renderFreeSkillSelection : Model -> Html Msg
renderFreeSkillSelection model =
    -- Step 7
    div []
        [ h1 [ class "center-text title-text" ] [ text "Free Skill" ]
        , h3 [ class "center-text" ] [ em [] [ text "(Psychic Skills Excluded)" ] ]
        , div [ class "card-list" ]
            (model.data.skills |> List.map (renderLargeSkillCard "row-of-2"))
        , button
            [ class "back"
            , onClick (ChangeStep -1 RemoveSkill)
            ]
            [ text "Free Combat Skill" ]
        ]


renderLargeSkillCard : String -> Skill -> Html Msg
renderLargeSkillCard classMod skill =
    div
        [ onClick (ChooseFreeSkill skill)
        , class ("card " ++ classMod)
        ]
        [ div [ class "card-header" ]
            [ h3 [ class "card-title" ]
                [ text skill.name ]
            ]
        , div [ class "card-content" ]
            [ p [ class "inline" ] [ text skill.description ] ]
        ]


{-| Step 8: EQUIPMENT
-}
renderEquipmentSelection : Model -> Html Msg
renderEquipmentSelection model =
    -- Step 8
    div []
        [ h1 [ class "center-text title-text" ] [ text "Equipment Loadout" ]
        , div [ class "card-list" ]
            (model.data.equipment |> List.map renderEquipmentCard)
        , button
            [ class "back"
            , onClick (ChangeStep -1 RemoveSkill)
            ]
            [ text "Free Skill" ]
        ]


renderEquipmentCard : Equipment -> Html Msg
renderEquipmentCard equipment =
    div
        [ onClick (ChooseEquipment equipment)
        , class "card"
        ]
        [ div [ class "card-header" ]
            [ h3 [ class "card-title" ]
                [ text equipment.name ]
            ]
        , div [ class "card-content" ]
            (equipment.loadout |> List.map (\item -> p [] [ text item ]))
        ]


{-| Step 9: ATTRIBUTES
-}
renderAttributeSelection : Model -> Html Msg
renderAttributeSelection model =
    -- Step 9
    let
        ( aStd, aRnd, btnClass ) =
            case model.state.attrStyle of
                Standard ->
                    ( "nav-active", "", "hide" )

                Random ->
                    ( "", "nav-active", "" )

        bonusList =
            (model.state.bgPicks ++ model.state.bgRolls)
                |> List.filter (String.contains "+")

        choose : AttrSelectionStyle -> Msg
        choose aStyle =
            Task.succeed aStyle
                |> Task.perform ChooseAttributeStyle
                |> RollDice 18 6
    in
    div []
        [ h1 [ class "center-text title-text" ]
            [ text "Attributes" ]
        , div [ class "nav" ]
            [ h2
                [ class ("nav-item-full " ++ aStd)
                , onClick (choose Standard)
                ]
                [ text "Standard Array" ]
            , h2
                [ class ("nav-item-full " ++ aRnd)
                , onClick (choose Random)
                ]
                [ text "Roll Randomly" ]
            ]
        , div []
            [ renderArrayValues model
            , div [ class "flex" ] [ button [ onClick (choose Random), class btnClass ] [ text "Reroll" ] ]
            , h1 [ class "center-text" ] [ text "Assign Attributes:" ]
            , renderAttrTable model
            ]
        , button
            [ class "back"
            , onClick (ChangeStep -1 NoOp)
            ]
            [ text "Equipment" ]
        , button
            [ class "next"
            , disabled False
            , onClick (ChangeStep 1 NoOp)
            ]
            [ text "Name" ]
        ]


renderArrayValues : Model -> Html Msg
renderArrayValues model =
    div
        [ class "attr-rolls" ]
        (model.state.attrArray
            |> List.indexedMap (renderArrayValue model)
        )


renderArrayValue : Model -> Int -> Int -> Html Msg
renderArrayValue model idx attr =
    let
        classStr =
            if model.state.selectedArrayIndex |> eitherItemEquals idx then
                " highlighted"

            else
                ""
    in
    h1
        [ class ("attr-roll" ++ classStr)
        , onClick (SelectAttrArrayItem idx)
        ]
        [ text (String.fromInt attr) ]


renderAttrTable : Model -> Html Msg
renderAttrTable model =
    div [ class "flex" ]
        (attributes
            |> LX.groupsOf 3
            |> List.indexedMap
                (\i attrSet ->
                    div [ class "row-of-2" ]
                        (attrSet
                            |> List.indexedMap
                                (\idx attr ->
                                    attr |> renderAttributeCell model ((i * 3) + idx)
                                )
                        )
                )
        )


renderAttributeCell : Model -> Int -> String -> Html Msg
renderAttributeCell model idx attr =
    let
        classStr =
            if model.state.selectedAttrIndex |> eitherItemEquals idx then
                " highlighted"

            else
                ""

        value =
            model.attributes
                |> LX.getAt idx
                |> Maybe.withDefault 0
                |> String.fromInt
    in
    div [ class "attr-wrap" ]
        [ h1 [ class "inline attr-name" ] [ text attr ]
        , div
            [ class ("inline attr-target" ++ classStr)
            , onClick (SelectAttributeItem idx)
            ]
            [ h1 [] [ text value ] ]
        ]


eitherItemEquals : Int -> ( Maybe Int, Maybe Int ) -> Bool
eitherItemEquals item ( one, two ) =
    (one |> Maybe.withDefault -1 |> (==) item) || (two |> Maybe.withDefault -1 |> (==) item)


{-| Step 10: NAME
-}
renderNameSelection : Model -> Html Msg
renderNameSelection model =
    -- Step 10
    let
        btnText =
            if model.state.nameLocked then
                "Edit Name"

            else
                "Set Name"
    in
    div []
        [ h1 [ class "center-text title-text" ] [ text "Character Name" ]
        , h2 [ class "center-text" ] [ text "(Refer to p.238 of the Rulebook for roll tables)" ]
        , div [ class "name-wrap" ]
            [ input
                [ type_ "text"
                , class "name-input"
                , onInput SetCharacterName
                , disabled model.state.nameLocked
                ]
                []
            , button
                [ onClick ToggleCharacterNameLock
                , disabled (model.name == "")
                ]
                [ text btnText ]
            ]
        , button
            [ class "back"
            , onClick (ChangeStep -1 NoOp)
            ]
            [ text "Focus" ]
        , button
            [ class "next"
            , onClick (ChangeStep 1 NoOp)
            , disabled <| not <| model.state.nameLocked
            ]
            [ text "Summary" ]
        ]


{-| Step 11: SUMMARY
-}
renderSummary : Model -> Html Msg
renderSummary model =
    -- Step 11
    let
        -- Skills
        ( skillList, nonSkillList ) =
            (model.state.bgPicks
                ++ model.state.bgRolls
                ++ (model.skills |> List.map .name)
                ++ (model.psySkills |> List.map .name)
            )
                |> List.partition
                    (\name ->
                        model.data.skills
                            |> List.map .name
                            |> List.append (model.data.psySkills |> List.map .name)
                            |> List.member name
                    )

        -- Dict String Int of (<Attr_name>, <Attr_score>)
        attributeMap =
            model.attributes
                |> LX.zip attributes
                |> Dict.fromList

        -- HP
        hp =
            attributeMap
                |> getModOf "Constitution"
                |> (+)
                    (if isWarrior model.class then
                        8

                     else
                        6
                    )
                |> String.fromInt

        -- AC
        bestArmorAc =
            model.equipment.loadout
                |> List.filter (String.contains "(AC")
                |> List.map
                    (\item ->
                        item
                            |> Regex.find
                                ("\\d+"
                                    |> Regex.fromString
                                    |> Maybe.withDefault Regex.never
                                )
                            |> List.map
                                (\matchObj ->
                                    matchObj.match
                                        |> String.toInt
                                        |> Maybe.withDefault 0
                                )
                    )
                |> List.concat
                |> List.maximum

        armorClass =
            case bestArmorAc of
                Nothing ->
                    attributeMap
                        |> getModOf "Dexterity"
                        |> (+) 10
                        |> String.fromInt

                Just armor ->
                    if
                        model.equipment.loadout
                            |> List.filter (String.contains "shield")
                            |> List.length
                            |> (<) 0
                    then
                        String.fromInt (armor + 1)

                    else
                        String.fromInt armor

        -- Saves
        saves =
            15
                |> List.repeat 3
                |> List.indexedMap
                    (\idx baseVal ->
                        [ [ "Strength", "Constitution" ]
                        , [ "Dexterity", "Intelligence" ]
                        , [ "Wisdom", "Charisma" ]
                        ]
                            |> LX.getAt idx
                            |> Maybe.withDefault [ "", "" ]
                            |> List.map (\stat -> attributeMap |> getModOf stat)
                            |> List.maximum
                            |> Maybe.withDefault 0
                            |> (-) baseVal
                            |> String.fromInt
                    )
                |> LX.zip [ "Physical", "Evasion", "Mental" ]

        -- Skills with levels (String, Int)
        skillLevels =
            skillList
                |> List.sort
                |> LX.groupWhile (==)
                |> List.map
                    (\( skill, matches ) ->
                        Tuple.pair skill <| List.length <| matches
                    )

        -- Atk
        atkBonus =
            if isWarrior model.class then
                "1"

            else
                "0"

        -- Extras
        classExtras =
            case model.class of
                Just inner ->
                    case inner of
                        Full (Psychic stats) ->
                            stats.full |> getAsListAt 1

                        Full (Warrior stats) ->
                            stats.partial |> getAsListAt 0

                        Full (Expert stats) ->
                            stats.partial |> getAsListAt 0

                        Partial one two ->
                            case ( one, two ) of
                                ( Psychic _, Warrior stats ) ->
                                    stats.partial |> getAsListAt 0

                                ( Psychic _, Expert stats ) ->
                                    stats.partial |> getAsListAt 0

                                ( Warrior stats, Psychic _ ) ->
                                    stats.partial |> getAsListAt 0

                                ( Expert stats, Psychic _ ) ->
                                    stats.partial |> getAsListAt 0

                                ( Warrior stat1, Expert stat2 ) ->
                                    [ stat1.partial, stat2.partial ]
                                        |> List.map (getAsListAt 0)
                                        |> List.concat

                                ( Expert stat1, Warrior stat2 ) ->
                                    [ stat1.partial, stat2.partial ]
                                        |> List.map (getAsListAt 0)
                                        |> List.concat

                                _ ->
                                    [ "NOTHING" ]

                Nothing ->
                    [ "NOTHING" ]

        focusExtras =
            model.foci
                |> List.map
                    (\x ->
                        let
                            desc =
                                x.description |> List.head |> Maybe.withDefault "NOTHING"
                        in
                        if [ "Ironhide", "Wild Psychic" ] |> List.member x.name then
                            desc

                        else
                            desc |> String.split "." |> List.head |> Maybe.withDefault "NOTHING"
                    )

        equipExtras =
            if model.equipment.name == "Barbarian" then
                "Shield grants you 13 AC if your AC is lower, otherwise +1 AC"

            else
                "NOTHING"

        attrExtras =
            "Change any 1 stat to a score of 14"

        allExtras =
            [ ( "Extra Skills", nonSkillList )
            , ( "From Class", classExtras )
            , ( "From Foci", focusExtras )
            , ( "From Equipment", [ equipExtras ] )
            , ( "Misc", [ attrExtras ] )
            ]
    in
    div [ class "summary-grid" ]
        [ div [ class "name-cell" ]
            [ h1 [ class "center-text" ] [ text "Name" ]
            , h2 [ class "center-text" ] [ text model.name ]
            ]
        , div [ class "race-cell" ]
            [ h1 [ class "center-text" ] [ text "Race" ]
            , h2 [ class "center-text" ]
                [ text
                    (if model.isHuman then
                        "Human"

                     else
                        "Non-Human"
                    )
                ]
            ]
        , div [ class "class-cell" ]
            [ h1 [ class "center-text" ] [ text "Class" ]
            , h2 [ class "center-text" ]
                [ text
                    (case model.class of
                        Nothing ->
                            "ERROR"

                        Just (Full baseClass) ->
                            baseClassToString baseClass

                        Just (Partial one two) ->
                            baseClassToString one ++ " / " ++ baseClassToString two
                    )
                ]
            ]
        , div [ class "bg-cell" ]
            [ h1 [ class "center-text" ] [ text "Background" ]
            , h2 [ class "center-text" ] [ text model.background.name ]
            ]
        , div [ class "focus-cell" ]
            [ h1 [ class "center-text" ] [ text "Focus" ]
            , h2 [ class "center-text" ]
                [ text
                    (case model.foci |> List.head of
                        Nothing ->
                            "ERROR"

                        Just { name } ->
                            name
                    )
                ]
            , ul []
                (case List.head model.foci of
                    Nothing ->
                        [ li [] [ text "ERROR" ] ]

                    Just { description } ->
                        description
                            |> List.indexedMap
                                (\i desc ->
                                    li []
                                        [ strong []
                                            [ text ("Level-" ++ String.fromInt (i + 1) ++ ": ") ]
                                        , text desc
                                        ]
                                )
                )
            ]
        , div [ class "skills-cell" ]
            [ h1 [ class "center-text" ] [ text "Skills" ]
            , ul []
                (skillLevels
                    |> List.map
                        (\( skill, lvl ) ->
                            li [] [ h3 [] [ text (skill ++ "-" ++ String.fromInt lvl) ] ]
                        )
                )
            ]
        , div [ class "attr-cell" ]
            (attributes
                |> List.map (\a -> attributeMap |> renderAttrCell a)
                |> List.concat
            )
        , div [ class "equip-cell" ]
            [ h1 [ class "center-text" ] [ text "Equipment" ]
            , ul []
                (model.equipment.loadout
                    |> List.map
                        (\item ->
                            li [] [ h4 [] [ text item ] ]
                        )
                )
            ]
        , div [ class "derive-cell" ]
            [ h1 [ class "center-text" ] [ text "Other Stats" ]
            , ul []
                [ li [] [ h3 [] [ text ("HP: " ++ hp) ] ]
                , li [] [ h3 [] [ text ("AC: " ++ armorClass) ] ]
                , li [] [ h3 [] [ text ("Attack Bonus: " ++ atkBonus) ] ]
                , li []
                    [ h3 [] [ text "Saving Throws:" ]
                    , ul []
                        (saves
                            |> List.map
                                (\( save, score ) ->
                                    li [] [ h4 [] [ text (save ++ ": " ++ score) ] ]
                                )
                        )
                    ]
                ]
            ]
        , div [ class "info-cell" ]
            [ h1 [ class "center-text" ] [ text "Things I Didn't Do For You:" ]
            , div [ class "extras-wrap" ]
                (allExtras
                    |> List.map
                        (\( title, strList ) ->
                            div [ class "extras-list" ]
                                [ h2 [ class "center-text" ] [ text title ]
                                , ul [] (strList |> List.map (\s -> li [] [ h3 [] [ text s ] ]))
                                ]
                        )
                )
            ]
        ]


isWarrior : Maybe Class -> Bool
isWarrior checkClass =
    case checkClass of
        Just class ->
            case class of
                Full cls ->
                    case cls of
                        Warrior _ ->
                            True

                        _ ->
                            False

                Partial cls1 cls2 ->
                    case ( cls1, cls2 ) of
                        ( Warrior _, _ ) ->
                            True

                        ( _, Warrior _ ) ->
                            True

                        _ ->
                            False

        Nothing ->
            False


getModOf : String -> Dict.Dict String Int -> Int
getModOf attr attrMap =
    attributeModifiers
        |> List.filter
            (\modSet ->
                modSet
                    |> Tuple.second
                    |> List.member
                        (attrMap |> Dict.get attr |> Maybe.withDefault 10)
            )
        |> List.head
        |> Maybe.withDefault ( 0, [] )
        |> Tuple.first


renderAttrCell : String -> Dict.Dict String Int -> List (Html Msg)
renderAttrCell attrName attributeMap =
    let
        abbrev =
            abbreviate attrName

        score =
            attributeMap
                |> Dict.get attrName
                |> Maybe.withDefault 0
                |> String.fromInt

        modifier =
            (if (attributeMap |> getModOf attrName) < 0 then
                ""

             else
                "+"
            )
                ++ (attributeMap |> getModOf attrName |> String.fromInt)
    in
    [ h1 [ class (abbrev ++ "-name") ] [ text attrName ]
    , h1 [ class (abbrev ++ "-score") ] [ text score ]
    , h1 [ class (abbrev ++ "-mod") ] [ text ("[ " ++ modifier ++ " ]") ]
    ]


abbreviate : String -> String
abbreviate attr =
    attr
        |> String.split ""
        |> List.take 3
        |> List.map String.toLower
        |> String.join ""


getAsListAt : Int -> List String -> List String
getAsListAt idx someList =
    someList |> LX.getAt idx |> Maybe.withDefault "NOTHING" |> List.singleton


{-| ERROR MESSAGE
-}
renderErrorPage : Model -> Html Msg
renderErrorPage model =
    h3 [] [ text (Maybe.withDefault "No Error" model.error) ]


view : Model -> Html Msg
view model =
    let
        focusFn =
            if model.isHuman then
                renderFociSelection

            else
                renderNonHumanFocus

        renderFns =
            [ renderRaceQuery
            , renderBackgroundSelection
            , renderBackgroundDetailSelection
            , renderClassSelection
            , renderPsychicSkillSelection
            , focusFn
            , renderCombatSkillSelection
            , renderFreeSkillSelection
            , renderEquipmentSelection
            , renderAttributeSelection
            , renderNameSelection
            , renderSummary
            ]
    in
    div [ class "container" ]
        [ model
            |> (renderFns
                    |> LX.getAt model.state.step
                    |> Maybe.withDefault renderErrorPage
               )
        ]
