module Main exposing (main)

import Array exposing (Array)
import Browser
import Dict exposing (Dict)
import Element exposing (..)
import Element.Font as Font
import Element.Input as Input
import FormatNumber exposing (format)
import FormatNumber.Locales exposing (spanishLocale)


type alias Years =
    Float


type alias Currency =
    Float


type alias InterestRate =
    Float


type alias Year =
    Int


type alias Mortgage =
    { bank : String
    , rate : { first : InterestRate, rest : InterestRate }
    , extraExpenses : Currency
    , amount : Currency
    , years : Years
    }


type alias EditableMortgage =
    { bank : String
    , rate : { first : String, rest : String }
    , extraExpenses : String
    , amount : String
    , years : String
    }


emptyMortgage : EditableMortgage
emptyMortgage =
    { bank = ""
    , rate = { first = "1", rest = "1" }
    , extraExpenses = "0"
    , amount = "100000"
    , years = "20"
    }


type alias Amortization =
    { yearly : Currency }


type alias EditableAmortization =
    { yearly : String }


type alias Report =
    { mortgage : Mortgage
    , amortization : Amortization
    , results : ReportResults
    }


type alias ReportResults =
    { monthlyPayment : { first : Currency, rest : Currency }
    , finishesPayingIn : Years
    , totalInterests : Currency
    , records : Array MonthlyRecord
    }


type MortgageInProgress
    = MortgageNotChosen
    | MortgageEditing EditableMortgage
    | MortgageChosen Mortgage


type AmortizationInProgress
    = AmortizationEditing EditableAmortization
    | AmortizationChosen Amortization


type alias ReportInProgress =
    { mortgage : MortgageInProgress
    , amortization : AmortizationInProgress
    , results : Maybe ReportResults
    }


type alias MonthlyRecord =
    { year : Year
    , interestPayed : Currency
    , amortized : Currency
    , extraAmortization : Currency
    , remainingLoan : Currency
    }


monthlyPayment : Currency -> InterestRate -> Years -> Currency
monthlyPayment amount rate years =
    (amount * (rate / 12)) / (100 * (1 - (1 + (rate / 12) / 100) ^ -(years * 12)))


mortgageFromEditableMortgage : EditableMortgage -> Mortgage
mortgageFromEditableMortgage editableMortgage =
    { bank = editableMortgage.bank
    , rate =
        { first = floatFromString editableMortgage.rate.first
        , rest = floatFromString editableMortgage.rate.rest
        }
    , extraExpenses = floatFromString editableMortgage.extraExpenses
    , amount = floatFromString editableMortgage.amount
    , years = floatFromString editableMortgage.years
    }


mortgageToEditableMortgage : Mortgage -> EditableMortgage
mortgageToEditableMortgage mortgage =
    { bank = mortgage.bank
    , rate =
        { first = String.fromFloat mortgage.rate.first
        , rest = String.fromFloat mortgage.rate.rest
        }
    , extraExpenses = String.fromFloat mortgage.extraExpenses
    , amount = String.fromFloat mortgage.amount
    , years = String.fromFloat mortgage.years
    }


amortizationFromEditableAmortization : EditableAmortization -> Amortization
amortizationFromEditableAmortization amortization =
    { yearly = floatFromString amortization.yearly }


amortizationToEditableAmortization : Amortization -> EditableAmortization
amortizationToEditableAmortization amortization =
    { yearly = String.fromFloat amortization.yearly }


floatFromString : String -> Float
floatFromString str =
    str
        |> String.replace "," "."
        |> String.toFloat
        |> Maybe.withDefault 0.0


caixa : Mortgage
caixa =
    { bank = "Caixa"
    , rate = { first = 0.9, rest = 0.9 }
    , amount = 130000
    , years = 15.0
    , extraExpenses = 0
    }


type alias Flags =
    ()


type alias Model =
    { mortgages : List Mortgage
    , reports : List Report
    , page : Page
    }


type Page
    = Home
    | AddNewReport ReportInProgress
    | ViewReport Report


init : Flags -> ( Model, Cmd Msg )
init () =
    { mortgages = [ caixa ]
    , reports = []
    , page = Home
    }
        |> update NavigateToNewReport


type Msg
    = NavigateToViewExistingReport Report
    | NavigateToNewReport
    | NavigateToHome
    | SelectMortgage Mortgage
    | NewMortgage
    | EditMortgage Mortgage
    | UpdateEditingMortgage EditableMortgage
    | ResetMortgage
    | UpdateEditingAmortization EditableAmortization
    | SelectAmortization Amortization


update msg ({ page } as model) =
    case msg of
        NavigateToHome ->
            ( { model | page = Home }, Cmd.none )

        NavigateToNewReport ->
            ( { model
                | page =
                    AddNewReport
                        { mortgage = MortgageNotChosen
                        , amortization = AmortizationChosen { yearly = 0.0 }
                        , results = Nothing
                        }
              }
            , Cmd.none
            )

        NavigateToViewExistingReport report ->
            ( { model | page = ViewReport report }, Cmd.none )

        SelectMortgage mortgage ->
            case page of
                AddNewReport r ->
                    ( { model
                        | page = AddNewReport { r | mortgage = MortgageChosen mortgage }
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        NewMortgage ->
            case page of
                AddNewReport r ->
                    ( { model
                        | page = AddNewReport { r | mortgage = MortgageEditing emptyMortgage }
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        EditMortgage mortgage ->
            case page of
                AddNewReport r ->
                    ( { model
                        | page = AddNewReport { r | mortgage = MortgageEditing (mortgageToEditableMortgage mortgage) }
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        UpdateEditingMortgage mortgage ->
            case page of
                AddNewReport r ->
                    ( { model
                        | page = AddNewReport { r | mortgage = MortgageEditing mortgage }
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        ResetMortgage ->
            case page of
                AddNewReport r ->
                    ( { model
                        | page = AddNewReport { r | mortgage = MortgageNotChosen }
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        UpdateEditingAmortization a ->
            case page of
                AddNewReport r ->
                    ( { model
                        | page = AddNewReport { r | amortization = AmortizationEditing a }
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        SelectAmortization amortization ->
            case page of
                AddNewReport r ->
                    ( { model
                        | page = AddNewReport { r | amortization = AmortizationChosen amortization }
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )


view model =
    { title = labels.pageTitle
    , body = [ body model ]
    }


space =
    20


body model =
    layout []
        (column
            [ centerX
            , padding space
            , spacing space
            , width (fill |> maximum 800)
            ]
            [ menu model.page
            , viewPage model
            ]
        )


menu : Page -> Element Msg
menu page =
    row [ centerX, spacing space ]
        [ button NavigateToHome labels.home
        , button NavigateToNewReport labels.newReport
        ]


viewPage : Model -> Element Msg
viewPage { mortgages, page } =
    column [ width fill ] <|
        case page of
            Home ->
                [ heading 1 labels.home ]

            AddNewReport reportInProgress ->
                [ heading 1 labels.newReport
                , viewReportInProgress mortgages reportInProgress
                ]

            ViewReport report ->
                [ heading 1 "NewReport view" ]


viewReportInProgress : List Mortgage -> ReportInProgress -> Element Msg
viewReportInProgress savedMortgages { mortgage, amortization, results } =
    column [ width fill, padding space, spacing (space * 2) ]
        [ viewMortgageForm savedMortgages mortgage
        , viewAmortizationForm amortization
        , results |> Maybe.map viewReportResults |> Maybe.withDefault none
        ]


viewMortgageSelector : List Mortgage -> Element Msg
viewMortgageSelector savedMortgages =
    row [ width fill, spaceEvenly ]
        [ Input.radio []
            { onChange = SelectMortgage
            , selected = Nothing
            , label = Input.labelAbove [ paddingXY 0 space ] (label labels.selectAMortgage)
            , options =
                savedMortgages
                    |> List.map (\m -> Input.option m (viewMortgageShortSummary m))
            }
        , el [] (button NewMortgage labels.newMortgage)
        ]


viewMortgageShortSummary : Mortgage -> Element Msg
viewMortgageShortSummary m =
    row [ spacing (space * 2 // 3) ]
        [ text m.bank
        , viewSmallStackedLabels (formatNumber m.rate.first ++ "&")
            (formatNumber m.rate.rest)
        , if m.extraExpenses > 0 then
            text (String.fromFloat m.extraExpenses)

          else
            none
        , viewSmallStackedLabels (String.fromFloat m.years ++ " years")
            (formatCurrency m.amount)
        ]


formatCurrency amount =
    formatNumber amount ++ " €"


formatNumber n =
    format labels.numberLocale n


viewSmallStackedLabels top bottom =
    column [ spacing (theme.fontSize // 5) ]
        [ el
            [ centerX
            , Font.size (theme.fontSize * 0.5 |> round)
            , spacing (space // 5)
            ]
            (Element.text top)
        , el
            [ centerX
            , Font.size (theme.fontSize * 0.75 |> round)
            ]
            (Element.text bottom)
        ]


viewStackedKeyValue top bottom =
    column [ spacing (theme.fontSize // 3) ]
        [ el [ Font.variant Font.smallCaps ]
            (smallLabel top)
        , el [] bottom
        ]


viewMortgageForm : List Mortgage -> MortgageInProgress -> Element Msg
viewMortgageForm savedMortgages mortgageInProgress =
    case mortgageInProgress of
        MortgageNotChosen ->
            viewMortgageSelector savedMortgages

        MortgageEditing ({ rate } as mortgage) ->
            let
                input labelText updater txt =
                    Input.text []
                        { label = Input.labelAbove [] (label labelText)
                        , onChange = UpdateEditingMortgage << updater
                        , placeholder = Just (Input.placeholder [ clip ] (label labelText))
                        , text = txt
                        }
            in
            row [ width fill, spacing space ]
                [ column [ height fill, width (fillPortion 1), spacing space ]
                    [ input labels.bankName (\bank -> { mortgage | bank = bank }) mortgage.bank
                    , el [ alignBottom ] (button ResetMortgage labels.back)
                    , el [ alignBottom ] (button (SelectMortgage (mortgageFromEditableMortgage mortgage)) labels.saveMortgage)
                    ]
                , column [ spacing space, alignTop, width (fillPortion 1) ]
                    [ input labels.rateFirstYear (\first -> { mortgage | rate = { rate | first = first } }) mortgage.rate.first
                    , input labels.rateRestOfYears (\rest -> { mortgage | rate = { rate | rest = rest } }) mortgage.rate.rest
                    , input labels.extraExpenses (\extraExpenses -> { mortgage | extraExpenses = extraExpenses }) mortgage.extraExpenses
                    ]
                , column [ spacing space, alignTop, width (fillPortion 1) ]
                    [ input labels.loanAmount (\amount -> { mortgage | amount = amount }) mortgage.amount
                    , input labels.years (\years -> { mortgage | years = years }) mortgage.years
                    ]
                ]

        MortgageChosen mortgage ->
            row [ width fill, spacing space, spaceEvenly ]
                [ column [ height fill, spacing space ]
                    [ heading 2 mortgage.bank
                    , el [ alignBottom ] (button (EditMortgage mortgage) labels.editCurrentMortgage)
                    , el [ alignBottom ] (button ResetMortgage labels.changeMortgage)
                    ]
                , column [ spacing (space // 2), alignTop ]
                    [ viewStackedKeyValue labels.rateFirstYear (text <| formatNumber mortgage.rate.first)
                    , viewStackedKeyValue labels.rateRestOfYears (text <| formatNumber mortgage.rate.rest)
                    , viewStackedKeyValue labels.extraExpenses (text <| formatCurrency mortgage.extraExpenses)
                    ]
                , column [ spacing space, alignTop ]
                    [ viewStackedKeyValue labels.loanAmount (bigText <| formatCurrency mortgage.amount)
                    , viewStackedKeyValue labels.years (bigText <| String.fromFloat mortgage.years)
                    ]
                ]


viewAmortizationForm : AmortizationInProgress -> Element Msg
viewAmortizationForm amortizationInProgress =
    column [ spacing space, width fill ]
        [ heading 2 labels.selectAmortization
        , case amortizationInProgress of
            AmortizationEditing amortization ->
                row [ spacing space, width fill ]
                    [ Input.text [ spacing space ]
                        { label = Input.labelLeft [ centerY ] (label labels.amortizeEveryYear)
                        , onChange = UpdateEditingAmortization << (\yearly -> { amortization | yearly = yearly })
                        , placeholder = Just (Input.placeholder [ clip ] (label labels.amortizeEveryYear))
                        , text = amortization.yearly
                        }
                    , el [ alignRight ]
                        (button
                            (SelectAmortization (amortizationFromEditableAmortization amortization))
                            labels.save
                        )
                    ]

            AmortizationChosen amortization ->
                row [ spacing space, width fill ]
                    [ label labels.amortizeEveryYear
                    , text <| formatCurrency amortization.yearly
                    , el [ alignRight ]
                        (button
                            (UpdateEditingAmortization (amortizationToEditableAmortization amortization))
                            labels.editAmortization
                        )
                    ]
        ]


viewReportResults : ReportResults -> Element Msg
viewReportResults results =
    none


text : String -> Element msg
text txt =
    el [ Font.size theme.fontSize ] (Element.text txt)


smallText : String -> Element msg
smallText txt =
    el [ Font.size (theme.fontSize * 0.75 |> round) ] (Element.text txt)


bigText : String -> Element msg
bigText txt =
    el [ Font.size (theme.fontSize * 1.5 |> round) ] (Element.text txt)


heading : Int -> String -> Element Msg
heading lvl txt =
    el [ centerX, Font.bold, Font.size (theme.fontSize + ((4 - lvl) * 2)) ] (Element.text txt)


button : msg -> String -> Element msg
button msg txt =
    Input.button [ Font.underline, Font.color theme.colors.activeText ]
        { onPress = Just msg
        , label = text txt
        }


label : String -> Element msg
label txt =
    el [ Font.color theme.colors.lightText ] (text txt)


smallLabel : String -> Element msg
smallLabel txt =
    el [ Font.color theme.colors.lightText ] (smallText txt)


theme =
    { fontSize = 20
    , colors =
        { lightText = rgb 0.8 0.8 0.8
        , activeText = rgb 0 0.3 0.8
        }
    }


numberLocale locale =
    { locale | decimals = 2 }


labels =
    { numberLocale = numberLocale spanishLocale
    , pageTitle = "Calculadora de hipotecas"
    , newReport = "Nuevo caso de hipoteca"
    , home = "Inicio"
    , selectAMortgage = "Selecciona una hipoteca"
    , changeMortgage = "Cambiar hipoteca"
    , newMortgage = "Nueva hipoteca"
    , rateFirstYear = "Interes primer año"
    , rateRestOfYears = "Interes resto"
    , extraExpenses = "Gastos hipoteca"
    , loanAmount = "Cantidad de prestamo"
    , years = "Años"
    , selectAmortization = "Selecciona estrategia de amortizacion"
    , amortizeEveryYear = "Amortizar cada año"
    , bankName = "Nombre del banco"
    , back = "Volver atrás"
    , saveMortgage = "Guardar hipoteca"
    , editCurrentMortgage = "Editar hipoteca elegida"
    , editAmortization = "Cambiar amortizacion"
    , save = "Guardar"
    }


main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
