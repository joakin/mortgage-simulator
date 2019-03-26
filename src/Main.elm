module Main exposing (main)

import Array exposing (Array)
import Browser
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Lazy as Lazy
import File exposing (File)
import File.Download as Download
import File.Select as Select
import FormatNumber exposing (format)
import FormatNumber.Locales exposing (spanishLocale)
import Html.Attributes as HA
import Json.Decode as D
import Json.Encode as E
import Task


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


encodeMortgage : Mortgage -> E.Value
encodeMortgage m =
    E.object
        [ ( "bank", E.string m.bank )
        , ( "rate", E.object [ ( "first", E.float m.rate.first ), ( "rest", E.float m.rate.rest ) ] )
        , ( "extraExpenses", E.float m.extraExpenses )
        , ( "amount", E.float m.amount )
        , ( "years", E.float m.years )
        ]


decodeMortgage : D.Decoder Mortgage
decodeMortgage =
    D.map5 Mortgage
        (D.field "bank" D.string)
        (D.field "rate"
            (D.map2 (\first rest -> { first = first, rest = rest })
                (D.field "first" D.float)
                (D.field "rest" D.float)
            )
        )
        (D.field "extraExpenses" D.float)
        (D.field "amount" D.float)
        (D.field "years" D.float)


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


encodeAmortization : Amortization -> E.Value
encodeAmortization a =
    E.object [ ( "yearly", E.float a.yearly ) ]


decodeAmortization : D.Decoder Amortization
decodeAmortization =
    D.map Amortization <| D.field "yearly" D.float


type alias EditableAmortization =
    { yearly : String }


type alias Report =
    { mortgage : Mortgage
    , amortization : Amortization
    , results : ReportResults
    }


encodeReport : Report -> E.Value
encodeReport r =
    E.object
        [ ( "mortgage", encodeMortgage r.mortgage )
        , ( "amortization", encodeAmortization r.amortization )
        , ( "results", encodeReportResults r.results )
        ]


decodeReport : D.Decoder Report
decodeReport =
    D.map3 Report
        (D.field "mortgage" decodeMortgage)
        (D.field "amortization" decodeAmortization)
        (D.field "results" decodeReportResults)


type alias ReportResults =
    { monthlyPayment : { first : Currency, rest : Currency }
    , finishesPayingIn : Years
    , totalInterests : Currency
    , totalExpensesAndInterests : Currency
    , records : Array MonthlyRecord
    }


encodeReportResults : ReportResults -> E.Value
encodeReportResults r =
    E.object
        [ ( "monthlyPayment"
          , E.object
                [ ( "first", E.float r.monthlyPayment.first )
                , ( "rest", E.float r.monthlyPayment.rest )
                ]
          )
        , ( "finishesPayingIn", E.float r.finishesPayingIn )
        , ( "totalInterests", E.float r.totalInterests )
        , ( "totalExpensesAndInterests", E.float r.totalExpensesAndInterests )
        , ( "records", E.array encodeMonthlyRecord r.records )
        ]


decodeReportResults : D.Decoder ReportResults
decodeReportResults =
    D.map5 ReportResults
        (D.field "monthlyPayment"
            (D.map2 (\first rest -> { first = first, rest = rest })
                (D.field "first" D.float)
                (D.field "rest" D.float)
            )
        )
        (D.field "finishesPayingIn" D.float)
        (D.field "totalInterests" D.float)
        (D.field "totalExpensesAndInterests" D.float)
        (D.field "records" (D.array decodeMonthlyRecord))


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


encodeMonthlyRecord r =
    E.object
        [ ( "year", E.int r.year )
        , ( "month", E.int r.month )
        , ( "monthlyPayment", E.float r.monthlyPayment )
        , ( "interestPayed", E.float r.interestPayed )
        , ( "amortized", E.float r.amortized )
        , ( "extraAmortization", E.float r.extraAmortization )
        , ( "remainingLoan", E.float r.remainingLoan )
        ]


decodeMonthlyRecord =
    D.map7 MonthlyRecord
        (D.field "year" D.int)
        (D.field "month" D.int)
        (D.field "monthlyPayment" D.float)
        (D.field "interestPayed" D.float)
        (D.field "amortized" D.float)
        (D.field "extraAmortization" D.float)
        (D.field "remainingLoan" D.float)


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


compareReports : Report -> Report -> Order
compareReports a b =
    case compare a.mortgage.amount b.mortgage.amount of
        EQ ->
            case compare a.mortgage.years b.mortgage.years of
                EQ ->
                    case compare a.amortization.yearly b.amortization.yearly of
                        EQ ->
                            case compare a.mortgage.bank b.mortgage.bank of
                                EQ ->
                                    EQ

                                other ->
                                    other

                        other ->
                            other

                other ->
                    other

        other ->
            other


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
    | DownloadSession
    | LoadSession
    | SessionSelected File
    | SessionLoaded String
    | OpenReportDetail Report


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
                                        report
                                            :: model.reports
                                            |> List.sortWith compareReports
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

        DownloadSession ->
            ( model, downloadSession model )

        LoadSession ->
            ( model, loadSession )

        SessionSelected file ->
            ( model
            , Task.perform SessionLoaded (File.toString file)
            )

        SessionLoaded sessionStr ->
            ( case decodeSession sessionStr of
                Just { reports, mortgages } ->
                    { model
                        | reports = reports |> List.sortWith compareReports
                        , mortgages = mortgages
                    }

                Nothing ->
                    model
            , Cmd.none
            )

        OpenReportDetail report ->
            ( { model | page = ViewReport report }
            , Cmd.none
            )


encodeSession : Model -> E.Value
encodeSession model =
    E.object
        [ ( "reports", E.list encodeReport model.reports )
        , ( "mortgages", E.list encodeMortgage model.mortgages )
        ]


decodeSession : String -> Maybe { reports : List Report, mortgages : List Mortgage }
decodeSession sessionStr =
    D.decodeString
        (D.map2 (\reports mortgages -> { reports = reports, mortgages = mortgages })
            (D.field "reports" (D.list decodeReport))
            (D.field "mortgages" (D.list decodeMortgage))
        )
        sessionStr
        |> Result.toMaybe


downloadSession : Model -> Cmd msg
downloadSession model =
    E.encode 0 (encodeSession model)
        |> Download.string "mortgages.json" "application/json"


loadSession : Cmd Msg
loadSession =
    Select.file [ "application/json" ] SessionSelected


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
            , padding (space * 2)
            , spacing space
            , width fill
            ]
            [ menu model.page
            , viewPage model
            ]
        )


menu : Page -> Element Msg
menu page =
    row [ centerX, spacing space ]
        [ button [] NavigateToHome <| text [] labels.home
        , button [] NavigateToNewReport <| text [] labels.newReport
        , button [] DownloadSession <| text [] labels.downloadSession
        , button [] LoadSession <| text [] labels.loadSession
        ]


viewPage : Model -> Element Msg
viewPage { reports, mortgages, page } =
    column [ width fill ] <|
        case page of
            Home ->
                [ heading [] 1 labels.home
                , viewHome reports
                ]

            AddNewReport reportInProgress ->
                [ heading [] 1 labels.newReport
                , viewReportInProgress mortgages reportInProgress
                ]

            ViewReport report ->
                [ viewReport report ]


viewHome : List Report -> Element Msg
viewHome reports =
    column [ width fill, padding space, spacing space ]
        [ viewReports reports ]


paddingBottom b =
    paddingEach { top = 0, left = 0, right = 0, bottom = b }


viewReports reports =
    case reports of
        [] ->
            label [ centerX ] labels.noReportsSaved

        _ ->
            el [ paddingBottom space, width fill, clipX, scrollbarX, htmlAttribute (HA.style "overflow-y" "hidden") ] <|
                indexedTable []
                    { data = reports
                    , columns =
                        [ { header = reportHeading " "
                          , width = fill
                          , view = reportCell (\r -> button [] (OpenReportDetail r) <| smallText [] labels.view)
                          }
                        , { header = reportHeading labels.bank
                          , width = fill
                          , view = reportCell (\r -> smallText [] r.mortgage.bank)
                          }
                        , { header = reportHeading labels.amount
                          , width = fill
                          , view = reportCell (\r -> smallText [ alignRight ] <| formatCurrency r.mortgage.amount)
                          }
                        , { header = reportHeading labels.years
                          , width = fill
                          , view = reportCell (\r -> smallText [ alignRight ] <| formatNumber r.mortgage.years)
                          }
                        , { header = reportHeading labels.rateFirstAndRest
                          , width = fill
                          , view = reportCell (\r -> smallText [ alignRight ] <| formatNumber r.mortgage.rate.first ++ " / " ++ formatNumber r.mortgage.rate.rest)
                          }
                        , { header = reportHeading labels.extraExpenses
                          , width = fill
                          , view = reportCell (\r -> smallText [ alignRight ] <| formatCurrency r.mortgage.extraExpenses)
                          }
                        , { header = reportHeading labels.amortizeEveryYear
                          , width = fill
                          , view = reportCell (\r -> smallText [ alignRight ] <| formatCurrency r.amortization.yearly)
                          }
                        , { header = reportHeading labels.totalExpensesAndInterests
                          , width = fill
                          , view = reportCell (\r -> smallText [ alignRight ] <| formatCurrency r.results.totalExpensesAndInterests)
                          }
                        , { header = reportHeading labels.payedIn
                          , width = fill
                          , view = reportCell (\r -> smallText [ alignRight ] <| labels.nYears r.results.finishesPayingIn)
                          }
                        , { header = reportHeading labels.monthlyPaymentFirstYear
                          , width = fill
                          , view = reportCell (\r -> smallText [ alignRight ] <| formatCurrency r.results.monthlyPayment.first)
                          }
                        , { header = reportHeading labels.monthlyPaymentRest
                          , width = fill
                          , view = reportCell (\r -> smallText [ alignRight ] <| formatCurrency r.results.monthlyPayment.rest)
                          }
                        ]
                    }


reportHeading : String -> Element Msg
reportHeading s =
    tinyText
        ([ padding (space // 3), Font.bold, Font.center ] ++ tableHeadingBorder)
        s


reportCell : (Report -> Element Msg) -> Int -> Report -> Element Msg
reportCell viewField i record =
    el
        [ padding (space // 3)
        , evenOddBackground (i + 1)
        ]
        (viewField record)


viewReportSummary report =
    row [ centerX, spaceEvenly, spacing space ]
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


viewReport : Report -> Element Msg
viewReport report =
    column [ width fill, padding space, spacing space ]
        [ row [ centerX, spacing space ]
            [ viewMortgage report.mortgage
            , el [ alignTop ] (viewAmortization report.amortization)
            ]
        , viewReportResults report.results
        ]


viewReportInProgress : List Mortgage -> ReportInProgress -> Element Msg
viewReportInProgress savedMortgages { mortgage, amortization } =
    column [ centerX, padding space, spacing (space * 2) ]
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
        [ heading [ centerX ] 2 labels.reportResults
        , button [ alignRight ] (SaveReport results) <| text [] labels.saveReport
        , viewReportResults results
        , button [ alignRight ] (SaveReport results) <| text [] labels.saveReport
        ]


viewMortgageSelector : List Mortgage -> Element Msg
viewMortgageSelector savedMortgages =
    row [ centerX, spacing space ]
        [ Input.radio [ spacing space ]
            { onChange = SelectMortgage
            , selected = Nothing
            , label = Input.labelAbove [ paddingXY 0 space ] (label [] labels.selectAMortgage)
            , options =
                savedMortgages
                    |> List.map (\m -> Input.option m (viewMortgageShortSummary m))
            }
        , el [] (button [] NewMortgage <| text [] labels.newMortgage)
        ]


viewMortgageShortSummary : Mortgage -> Element Msg
viewMortgageShortSummary m =
    row [ spacing (space * 2 // 3) ]
        [ text [ width (fillPortion 1) ] m.bank
        , viewSmallStackedLabels labels.loanAmount (formatCurrency m.amount)
        , viewSmallStackedLabels labels.years (labels.nYears m.years)
        , viewSmallStackedLabels labels.rateFirstYear (formatNumber m.rate.first)
        , viewSmallStackedLabels labels.rateRestOfYears (formatNumber m.rate.rest)
        , viewSmallStackedLabels labels.extraExpenses (formatCurrency m.extraExpenses)
        ]


formatCurrency amount =
    formatNumber amount ++ " €"


formatNumber n =
    format labels.numberLocale n


formatYears n =
    format labels.yearLocale n


viewSmallStackedLabels : String -> String -> Element Msg
viewSmallStackedLabels top bottom =
    column [ spacing (theme.fontSize // 5) ]
        [ tinyLabel [ centerX ] top
        , smallText [ centerX ] bottom
        ]


viewStackedKeyValue : String -> Element Msg -> Element Msg
viewStackedKeyValue top bottom =
    column [ spacing (theme.fontSize // 3) ]
        [ smallLabel [] top
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
                        { label = Input.labelAbove [] (label [] labelText)
                        , onChange = UpdateEditingMortgage << updater
                        , placeholder = Just (Input.placeholder [ clip ] (label [] labelText))
                        , text = txt
                        }
            in
            row [ centerX, spacing space ]
                [ column [ height fill, width (fillPortion 1), spacing space ]
                    [ input labels.bankName (\bank -> { mortgage | bank = bank }) mortgage.bank
                    , button [ alignBottom ] ResetMortgage <| text [] labels.back
                    , button [ alignBottom ] (SelectMortgage (mortgageFromEditableMortgage mortgage)) <| text [] labels.saveMortgage
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
            row [ centerX, spacing space ]
                [ viewMortgage mortgage
                , column [ spacing space, alignTop ]
                    [ button [ alignBottom ] (EditMortgage mortgage) <| text [] labels.editCurrentMortgage
                    , button [ alignBottom ] ResetMortgage <| text [] labels.changeMortgage
                    ]
                ]


viewMortgage : Mortgage -> Element Msg
viewMortgage mortgage =
    row [ centerX, spacing space ]
        [ column [ height fill, spacing space ]
            [ heading [] 2 mortgage.bank ]
        , column [ spacing (space // 2), alignTop ]
            [ viewStackedKeyValue labels.rateFirstYear (text [] <| formatNumber mortgage.rate.first)
            , viewStackedKeyValue labels.rateRestOfYears (text [] <| formatNumber mortgage.rate.rest)
            , viewStackedKeyValue labels.extraExpenses (text [] <| formatCurrency mortgage.extraExpenses)
            ]
        , column [ spacing space, alignTop ]
            [ viewStackedKeyValue labels.loanAmount (bigText [] <| formatCurrency mortgage.amount)
            , viewStackedKeyValue labels.years (bigText [] <| String.fromFloat mortgage.years)
            ]
        ]


viewAmortizationForm : AmortizationInProgress -> Element Msg
viewAmortizationForm amortizationInProgress =
    column [ centerX, spacing space ]
        [ heading [] 2 labels.selectAmortization
        , case amortizationInProgress of
            AmortizationEditing amortization ->
                row [ spacing space, centerX ]
                    [ Input.text [ spacing space ]
                        { label = Input.labelLeft [ centerY ] (label [] labels.amortizeEveryYear)
                        , onChange = UpdateEditingAmortization << (\yearly -> { amortization | yearly = yearly })
                        , placeholder = Just (Input.placeholder [ clip ] (label [] labels.amortizeEveryYear))
                        , text = amortization.yearly
                        }
                    , button [ alignRight ] (SelectAmortization (amortizationFromEditableAmortization amortization)) <|
                        text [] labels.save
                    ]

            AmortizationChosen amortization ->
                row [ centerX, spacing space ]
                    [ viewAmortization amortization
                    , button [ alignRight ] (UpdateEditingAmortization (amortizationToEditableAmortization amortization)) <|
                        text [] labels.editAmortization
                    ]
        ]


viewAmortization : Amortization -> Element Msg
viewAmortization amortization =
    viewStackedKeyValue labels.amortizeEveryYear <|
        text [] (formatCurrency amortization.yearly)


viewReportResults : ReportResults -> Element Msg
viewReportResults results =
    column [ width fill, spacing space ]
        [ row [ centerX, spacing space ]
            [ viewStackedKeyValue labels.totalExpensesAndInterests (bigText [] <| formatCurrency results.totalExpensesAndInterests)
            , viewStackedKeyValue labels.payedIn (bigText [] <| labels.nYears results.finishesPayingIn)
            , column [ spacing (space // 2) ]
                [ viewStackedKeyValue labels.monthlyPaymentFirstYear (text [] <| formatCurrency results.monthlyPayment.first)
                , viewStackedKeyValue labels.monthlyPaymentRest (text [] <| formatCurrency results.monthlyPayment.rest)
                ]
            ]

        -- Hide the vertical scrollbar that shows up for some reason even if
        -- it is tall enough ...
        , el [ width fill, clipX, scrollbarX, htmlAttribute (HA.style "overflow-y" "hidden") ] <|
            table [ centerX ]
                { data = results.records |> Array.toList
                , columns = monthlyRecordColumns
                }
        ]


monthlyRecordColumns : List (Column MonthlyRecord Msg)
monthlyRecordColumns =
    [ { header = monthlyRecordHeading labels.year
      , width = shrink
      , view = monthlyRecordCell (\{ year } -> text [ alignRight ] <| String.fromInt year)
      }
    , { header = monthlyRecordHeading labels.month
      , width = shrink
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
    , { header = monthlyRecordHeading labels.amount
      , width = fill
      , view = monthlyRecordCell (\{ remainingLoan } -> text [ alignRight ] <| formatCurrency remainingLoan)
      }
    ]


monthlyRecordHeading s =
    heading ([ padding (space // 3), Font.center ] ++ tableHeadingBorder) 4 s


monthlyRecordCell viewField record =
    el
        [ padding (space // 3)
        , alignRight
        , if (record.month |> modBy 12) == 0 then
            Background.color theme.colors.highlightedRowBackground

          else
            evenOddBackground record.month
        ]
        (viewField record)


tinyText : List (Attribute msg) -> String -> Element msg
tinyText attrs txt =
    text ([ Font.size (theme.fontSize * 0.5 |> round) ] ++ attrs) txt


text : List (Attribute msg) -> String -> Element msg
text attrs txt =
    el ([ Font.size theme.fontSize ] ++ attrs) (Element.text txt)


smallText : List (Attribute msg) -> String -> Element msg
smallText attrs txt =
    text ([ Font.size (theme.fontSize * 0.75 |> round) ] ++ attrs) txt


bigText : List (Attribute msg) -> String -> Element msg
bigText attrs txt =
    text ([ Font.size (theme.fontSize * 1.5 |> round) ] ++ attrs) txt


heading : List (Attribute msg) -> Int -> String -> Element msg
heading attrs lvl txt =
    text ([ centerX, Font.bold, Font.size (theme.fontSize + ((4 - lvl) * 2)) ] ++ attrs) txt


button : List (Attribute msg) -> msg -> Element msg -> Element msg
button attrs msg txt =
    Input.button ([ Font.underline, Font.color theme.colors.activeText ] ++ attrs)
        { onPress = Just msg
        , label = txt
        }


label : List (Attribute msg) -> String -> Element msg
label attrs txt =
    text ([ Font.color theme.colors.lightText ] ++ attrs) txt


smallLabel : List (Attribute msg) -> String -> Element msg
smallLabel attrs txt =
    text ([ Font.color theme.colors.lightText, Font.size (theme.fontSize * 0.75 |> round) ] ++ attrs) txt


tinyLabel : List (Attribute msg) -> String -> Element msg
tinyLabel attrs txt =
    text ([ Font.color theme.colors.lightText, Font.size (theme.fontSize * 0.5 |> round) ] ++ attrs) txt


evenOddBackground : Int -> Attribute msg
evenOddBackground n =
    Background.color <|
        if (n |> modBy 2) == 0 then
            theme.colors.evenRowBackground

        else
            theme.colors.oddRowBackground


tableHeadingBorder : List (Attribute msg)
tableHeadingBorder =
    [ Border.color theme.colors.tableHeadingBorder
    , Border.widthEach { bottom = 2, top = 0, left = 0, right = 0 }
    ]


theme =
    { fontSize = 20
    , colors =
        { lightText = rgb 0.8 0.8 0.8
        , activeText = rgb 0 0.3 0.8
        , highlightedRowBackground = rgb 1 0.980392 0.870588
        , oddRowBackground = rgb 1 1 1
        , evenRowBackground = rgb 0.980392 0.976471 1
        , tableHeadingBorder = rgb 0.917647 0.917647 0.917647
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
    , rateFirstAndRest = "Interes (1año / resto)"
    , extraExpenses = "Gastos hipoteca"
    , loanAmount = "Cantidad de prestamo"
    , amount = "Cantidad"
    , years = "Años"
    , year = "Año"
    , selectAmortization = "Selecciona estrategia de amortizacion"
    , amortizeEveryYear = "Amortizar cada año"
    , bank = "Banco"
    , bankName = "Nombre del banco"
    , back = "Volver atrás"
    , saveMortgage = "Guardar hipoteca"
    , editCurrentMortgage = "Editar hipoteca"
    , editAmortization = "Cambiar amortizacion"
    , save = "Guardar"
    , reportResults = "Resultados"
    , nYears = \nYears -> formatYears nYears ++ " años"
    , payedIn = "Pagado en"
    , totalExpensesAndInterests = "Total gastos + intereses"
    , monthlyPaymentFirstYear = "Cuota mensual 1er año"
    , monthlyPaymentRest = "Cuota mensual resto"
    , month = "Mes"
    , payment = "Cuota"
    , interest = "Interes"
    , amortized = "Amortizado"
    , extraAmortization = "Amort. extra"
    , saveReport = "Guardar caso en sesión"
    , downloadSession = "Guardar sesión"
    , loadSession = "Cargar sesión"
    , view = "Ver"
    , noReportsSaved = "No hay casos guardados"
    }


main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
