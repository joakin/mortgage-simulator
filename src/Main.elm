module Main exposing (main)

import Array exposing (Array)
import Browser
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Element.Lazy as Lazy
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


type alias Month =
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
    , totalExpensesAndInterests : Currency
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
    }


type alias MonthlyRecord =
    { year : Year
    , month : Month
    , monthlyPayment : Currency
    , interestPayed : Currency
    , amortized : Currency
    , extraAmortization : Currency
    , remainingLoan : Currency
    }


calculateMonthlyPayment : Currency -> InterestRate -> Years -> Currency
calculateMonthlyPayment amount rate years =
    (amount * (rate / 12)) / (100 * (1 - (1 + (rate / 12) / 100) ^ -(years * 12)))


isLoanPayed : Float -> Bool
isLoanPayed amount =
    (amount * 100 |> floor) == 0


calculateRecords : Mortgage -> Amortization -> MonthlyRecord -> Int -> Array MonthlyRecord
calculateRecords mortgage amortization prevRecord untilMonth =
    let
        rate month =
            if month <= 12 then
                mortgage.rate.first

            else
                mortgage.rate.rest

        calculateMonth lastRecord =
            let
                month =
                    lastRecord.month + 1

                monthlyPayment =
                    if month == 13 && mortgage.rate.first /= mortgage.rate.rest then
                        calculateMonthlyPayment lastRecord.remainingLoan (rate month) (mortgage.years - 1)

                    else
                        lastRecord.monthlyPayment

                interestPayed =
                    lastRecord.remainingLoan * (rate month / 100) / 12

                amortized =
                    min (monthlyPayment - interestPayed) lastRecord.remainingLoan

                loanAmountAmortized =
                    lastRecord.remainingLoan - amortized

                extraAmortization =
                    if isLoanPayed loanAmountAmortized then
                        0

                    else if (month |> modBy 12) == 0 then
                        min amortization.yearly loanAmountAmortized

                    else
                        0

                remainingLoan =
                    max (loanAmountAmortized - extraAmortization) 0
            in
            { year = ((month - 1) // 12) + 1
            , month = month
            , monthlyPayment = monthlyPayment
            , interestPayed = interestPayed
            , amortized = amortized
            , extraAmortization = extraAmortization
            , remainingLoan = remainingLoan
            }

        iter : MonthlyRecord -> Array MonthlyRecord -> Array MonthlyRecord
        iter lastRecord records =
            if isLoanPayed lastRecord.remainingLoan || lastRecord.month > untilMonth then
                records

            else
                let
                    record =
                        calculateMonth lastRecord
                in
                iter record (Array.push record records)
    in
    iter prevRecord Array.empty


calculateReportResults : Mortgage -> Amortization -> ReportResults
calculateReportResults mortgage amortization =
    let
        monthlyPaymentFirstYear =
            calculateMonthlyPayment mortgage.amount mortgage.rate.first mortgage.years

        -- Base record to start generating all the monthly records. This is
        -- a bit hacky, given it relies in knowing that calculateRecords only
        -- uses the remainingLoan, month, and monthlyPayment from the previous
        -- record to operate
        baseRecord =
            { year = 1
            , month = 0
            , monthlyPayment = monthlyPaymentFirstYear
            , interestPayed = 0
            , amortized = 0
            , extraAmortization = 0
            , remainingLoan = mortgage.amount
            }

        records =
            calculateRecords mortgage amortization baseRecord (mortgage.years * 12 |> floor)

        totalInterests =
            Array.foldl (\{ interestPayed } sum -> sum + interestPayed) 0 records
    in
    { monthlyPayment =
        { first = monthlyPaymentFirstYear
        , rest = Array.get 13 records |> Maybe.map (\r -> r.monthlyPayment) |> Maybe.withDefault 0.0
        }
    , finishesPayingIn = (toFloat <| Array.length records) / 12
    , totalInterests = totalInterests
    , totalExpensesAndInterests = totalInterests + mortgage.extraExpenses
    , records = records
    }


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


init : Flags -> ( Model, Cmd Msg )
init () =
    ( { mortgages = []
      , reports = []
      , page = Home
      }
    , Cmd.none
    )



-- |> Tuple.first
-- |> update NavigateToNewReport


type Msg
    = NavigateToNewReport
    | NavigateToHome
    | SelectMortgage Mortgage
    | NewMortgage
    | EditMortgage Mortgage
    | UpdateEditingMortgage EditableMortgage
    | ResetMortgage
    | UpdateEditingAmortization EditableAmortization
    | SelectAmortization Amortization
    | SaveReport ReportResults


update : Msg -> Model -> ( Model, Cmd Msg )
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
                        }
              }
            , Cmd.none
            )

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

        SaveReport results ->
            case page of
                AddNewReport r ->
                    case ( r.mortgage, r.amortization ) of
                        ( MortgageChosen m, AmortizationChosen a ) ->
                            let
                                report =
                                    { amortization = a, mortgage = m, results = results }
                            in
                            ( { model
                                | page = Home
                                , reports =
                                    -- Don't add reports if there is another one
                                    -- like it. Don't compare report results
                                    -- since they are derived from the other two
                                    -- parameters.
                                    if List.any (\r_ -> a == r_.amortization && m == r_.mortgage) model.reports then
                                        model.reports

                                    else
                                        report :: model.reports
                                , mortgages =
                                    -- Don't add mortgage if there is another
                                    -- one like it
                                    if List.any ((==) m) model.mortgages then
                                        model.mortgages

                                    else
                                        m :: model.mortgages
                              }
                            , Cmd.none
                            )

                        _ ->
                            ( model, Cmd.none )

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
viewPage { reports, mortgages, page } =
    column [ width fill ] <|
        case page of
            Home ->
                [ heading 1 labels.home
                , viewHome reports
                ]

            AddNewReport reportInProgress ->
                [ heading 1 labels.newReport
                , viewReportInProgress mortgages reportInProgress
                ]


viewHome : List Report -> Element Msg
viewHome reports =
    column [ padding space ] <| List.map viewReportSummary reports


viewReportSummary report =
    row [ width fill, spaceEvenly, spacing space ]
        [ viewMortgageShortSummary report.mortgage
        , viewAmortizationShortSummary report.amortization
        , viewReportResultsShortSummary report.results
        ]


viewAmortizationShortSummary : Amortization -> Element Msg
viewAmortizationShortSummary a =
    viewSmallStackedLabels labels.amortizeEveryYear (formatCurrency a.yearly)


viewReportResultsShortSummary : ReportResults -> Element Msg
viewReportResultsShortSummary results =
    row [ spacing (space * 2 // 3) ]
        [ viewSmallStackedLabels labels.totalExpensesAndInterests (formatCurrency results.totalExpensesAndInterests)
        , viewSmallStackedLabels labels.payedIn (labels.nYears results.finishesPayingIn)
        , viewSmallStackedLabels labels.monthlyPaymentFirstYear (formatCurrency results.monthlyPayment.first)
        , viewSmallStackedLabels labels.monthlyPaymentRest (formatCurrency results.monthlyPayment.rest)
        ]


viewReportInProgress : List Mortgage -> ReportInProgress -> Element Msg
viewReportInProgress savedMortgages { mortgage, amortization } =
    column [ width fill, padding space, spacing (space * 2) ]
        [ viewMortgageForm savedMortgages mortgage
        , viewAmortizationForm amortization
        , case ( mortgage, amortization ) of
            ( MortgageChosen m, AmortizationChosen a ) ->
                Lazy.lazy2 viewReportResultsInProgress m a

            _ ->
                none
        ]


viewReportResultsInProgress : Mortgage -> Amortization -> Element Msg
viewReportResultsInProgress m a =
    let
        results =
            calculateReportResults m a
    in
    column [ width fill, spacing space ]
        [ row [ width fill, spacing space ]
            [ heading 2 labels.reportResults
            , button (SaveReport results) labels.saveReport
            ]
        , viewReportResults results
        , button (SaveReport results) labels.saveReport
        ]


viewMortgageSelector : List Mortgage -> Element Msg
viewMortgageSelector savedMortgages =
    row [ width fill, spacing space, spaceEvenly ]
        [ Input.radio [ spacing space ]
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
        [ text [] m.bank
        , viewSmallStackedLabels labels.loanAmount (formatCurrency m.amount)
        , viewSmallStackedLabels labels.years (labels.nYears m.years)
        , viewSmallStackedLabels labels.rateFirstYear (formatNumber m.rate.first ++ "&")
        , viewSmallStackedLabels labels.rateRestOfYears (formatNumber m.rate.rest)
        , viewSmallStackedLabels labels.extraExpenses (formatCurrency m.extraExpenses)
        ]


formatCurrency amount =
    formatNumber amount ++ " €"


formatNumber n =
    format labels.numberLocale n


formatYears n =
    format labels.yearLocale n


viewSmallStackedLabels top bottom =
    column [ spacing (theme.fontSize // 5) ]
        [ el [ centerX ] (tinyLabel top)
        , el [ centerX, Font.size (theme.fontSize * 0.75 |> round) ]
            (Element.text bottom)
        ]


viewStackedKeyValue : String -> Element Msg -> Element Msg
viewStackedKeyValue top bottom =
    column [ spacing (theme.fontSize // 3) ]
        [ smallLabel top
        , bottom
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
                    [ viewStackedKeyValue labels.rateFirstYear (text [] <| formatNumber mortgage.rate.first)
                    , viewStackedKeyValue labels.rateRestOfYears (text [] <| formatNumber mortgage.rate.rest)
                    , viewStackedKeyValue labels.extraExpenses (text [] <| formatCurrency mortgage.extraExpenses)
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
                    , text [] <| formatCurrency amortization.yearly
                    , el [ alignRight ]
                        (button
                            (UpdateEditingAmortization (amortizationToEditableAmortization amortization))
                            labels.editAmortization
                        )
                    ]
        ]


viewReportResults : ReportResults -> Element Msg
viewReportResults results =
    column [ width fill, spacing space ]
        [ row [ width fill, spacing space, spaceEvenly ]
            [ viewStackedKeyValue labels.totalExpensesAndInterests (bigText <| formatCurrency results.totalExpensesAndInterests)
            , viewStackedKeyValue labels.payedIn (bigText <| labels.nYears results.finishesPayingIn)
            , column [ spacing (space // 2) ]
                [ viewStackedKeyValue labels.monthlyPaymentFirstYear (text [] <| formatCurrency results.monthlyPayment.first)
                , viewStackedKeyValue labels.monthlyPaymentRest (text [] <| formatCurrency results.monthlyPayment.rest)
                ]
            ]
        , table []
            { data = results.records |> Array.toList
            , columns = monthlyRecordColumns
            }
        ]


monthlyRecordColumns : List (Column MonthlyRecord Msg)
monthlyRecordColumns =
    [ { header = monthlyRecordHeading labels.year
      , width = fill
      , view = monthlyRecordCell (\{ year } -> text [ alignRight ] <| String.fromInt year)
      }
    , { header = monthlyRecordHeading labels.month
      , width = fill
      , view = monthlyRecordCell (\{ month } -> text [ alignRight ] <| String.fromInt month)
      }
    , { header = monthlyRecordHeading labels.payment
      , width = fill
      , view = monthlyRecordCell (\{ monthlyPayment } -> text [ alignRight ] <| formatCurrency monthlyPayment)
      }
    , { header = monthlyRecordHeading labels.interest
      , width = fill
      , view = monthlyRecordCell (\{ interestPayed } -> text [ alignRight ] <| formatCurrency interestPayed)
      }
    , { header = monthlyRecordHeading labels.amortized
      , width = fill
      , view = monthlyRecordCell (\{ amortized } -> text [ alignRight ] <| formatCurrency amortized)
      }
    , { header = monthlyRecordHeading labels.extraAmortization
      , width = fill
      , view = monthlyRecordCell (\{ extraAmortization } -> text [ alignRight ] <| formatCurrency extraAmortization)
      }
    , { header = monthlyRecordHeading labels.loanAmount
      , width = fill
      , view = monthlyRecordCell (\{ remainingLoan } -> text [ alignRight ] <| formatCurrency remainingLoan)
      }
    ]


monthlyRecordHeading s =
    el [ padding (space // 3) ] (heading 4 s)


monthlyRecordCell viewField record =
    el
        [ padding (space // 3)
        , alignRight
        , Background.color <|
            if (record.month |> modBy 12) == 0 then
                rgb 1 0.980392 0.870588

            else if (record.month |> modBy 2) == 0 then
                rgb 0.980392 0.976471 1

            else
                rgb 1 1 1
        ]
        (viewField record)


text : List (Attribute msg) -> String -> Element msg
text attrs txt =
    el ([ Font.size theme.fontSize ] ++ attrs) (Element.text txt)


smallText : String -> Element msg
smallText txt =
    text [ Font.size (theme.fontSize * 0.75 |> round) ] txt


bigText : String -> Element msg
bigText txt =
    text [ Font.size (theme.fontSize * 1.5 |> round) ] txt


heading : Int -> String -> Element Msg
heading lvl txt =
    el [ centerX, Font.bold, Font.size (theme.fontSize + ((4 - lvl) * 2)) ] (Element.text txt)


button : msg -> String -> Element msg
button msg txt =
    Input.button [ Font.underline, Font.color theme.colors.activeText ]
        { onPress = Just msg
        , label = text [] txt
        }


label : String -> Element msg
label txt =
    text [ Font.color theme.colors.lightText ] txt


smallLabel : String -> Element msg
smallLabel txt =
    text [ Font.color theme.colors.lightText, Font.size (theme.fontSize * 0.75 |> round) ] txt


tinyLabel : String -> Element msg
tinyLabel txt =
    text [ Font.color theme.colors.lightText, Font.size (theme.fontSize * 0.5 |> round) ] txt


theme =
    { fontSize = 20
    , colors =
        { lightText = rgb 0.8 0.8 0.8
        , activeText = rgb 0 0.3 0.8
        }
    }


numberLocale locale =
    { locale | decimals = 2 }


yearLocale locale =
    { locale | decimals = 1 }


labels =
    { numberLocale = numberLocale spanishLocale
    , yearLocale = yearLocale spanishLocale
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
    , year = "Año"
    , selectAmortization = "Selecciona estrategia de amortizacion"
    , amortizeEveryYear = "Amortizar cada año"
    , bankName = "Nombre del banco"
    , back = "Volver atrás"
    , saveMortgage = "Guardar hipoteca"
    , editCurrentMortgage = "Editar hipoteca elegida"
    , editAmortization = "Cambiar amortizacion"
    , save = "Guardar"
    , reportResults = "Resultados"
    , nYears = \nYears -> formatYears nYears ++ " years"
    , payedIn = "Pagado en"
    , totalExpensesAndInterests = "Total gastos + intereses"
    , monthlyPaymentFirstYear = "Cuota mensual 1er año"
    , monthlyPaymentRest = "Cuota mensual resto"
    , month = "Mes"
    , payment = "Cuota"
    , interest = "Interes"
    , amortized = "Amortizado"
    , extraAmortization = "Amort. extra"
    , saveReport = "Guardar resultados"
    }


main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
