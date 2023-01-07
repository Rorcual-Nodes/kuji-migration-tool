port module Main exposing (Model, Msg(..), amountDecoder, init, main, update, view)

import Browser
import Debug
import Html exposing (Html, button, div, input, p, text)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Decode
import String exposing (fromFloat)



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = \flags -> init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- INIT


init : ( Model, Cmd Msg )
init =
    ( { address = ""
      , urlUnbonding = ""
      , urlLP = ""
      , urlOG = ""
      , amount = 0
      , stake = 0
      , bonded = 0
      , available = 0
      , mapped = ""
      }
    , Cmd.none
    )



-- TYPES




type alias Model =
    { address : String
    , urlUnbonding : String
    , urlLP : String
    , urlOG : String
    , amount : Float
    , stake : Float
    , bonded : Float
    , available : Float
    , mapped : String
    }


-- PORTS


port sendAddress : String -> Cmd msg
port receiveAddress : (String -> msg) -> Sub msg


-- MESSAGES


type Msg
    = UpdateAddress String                                  -- Updates the address on Model
    | SubmitRequest                                         -- Submits HTTP Requests for the Migration JSONs
    | AmountCompleted (Result Http.Error Float)             -- Unbonding request completed
    | BondedCompleted (Result Http.Error Float)             -- LP request completed
    | GovernanceCompleted (Result Http.Error Float)         -- Old governance stake completed
    | Governance2Completed (Result Http.Error Float)        -- Old governance pending claims completed
    | AvailableCompleted (Result Http.Error Float)           -- Available for migration
    | Recv String
    


blockheight : String
blockheight = "%22%7D%7D&height=7607789"


unbondingContract : String
unbondingContract = "https://fcd.terra.dev/wasm/contracts/terra1vvj874nwtmxk0u0spj83d364xyhqk2e652jrck/store?query_msg=%7B%22claims%22:%7B%22address%22:%22"


farmingContract : String
farmingContract = "https://fcd.terra.dev/wasm/contracts/terra1cf9q9lq7tdfju95sdw78y9e34a6qrq3rrc6dre/store?query_msg=%7B%22staker_info%22:%7B%22staker%22:%22"


governanceContract : String
governanceContract = "https://fcd.terra.dev/wasm/contracts/terra1w7gtx76rs7x0e27l7x2e88vcr52tp9d8g4umjz/store?query_msg=%7B%22staked%22:%7B%22address%22:%22"

claimsContract : String
claimsContract = "https://fcd.terra.dev/wasm/contracts/terra1w7gtx76rs7x0e27l7x2e88vcr52tp9d8g4umjz/store?query_msg=%7B%22claims%22:%7B%22address%22:%22"

queryDenom : String
queryDenom = "/by_denom?denom=ukuji"

availableBalance : String
availableBalance = "https://lcd.kaiyo.kujira.setten.io/cosmos/bank/v1beta1/balances/"


-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateAddress address ->
            ( { model | address = address }, Cmd.none )

        Recv str ->
            ( { model | mapped = str }
            , Cmd.none
            )


        SubmitRequest ->
            let
                request1 =
                    Http.get
                        { url = unbondingContract ++ model.address ++ blockheight
                        , expect =
                            Http.expectJson
                                (\response1 ->
                                    let
                                        _ =
                                            Debug.log "Response1: " response1
                                    in
                                    AmountCompleted response1
                                )
                                amountDecoder
                        }

                request2 =
                    Http.get
                        { url = farmingContract ++ model.address ++ blockheight
                        , expect =
                            Http.expectJson
                                (\response2 ->
                                    let
                                        _ =
                                            Debug.log "Response2: " response2
                                    in
                                    BondedCompleted response2
                                )
                                stakeDecoder
                        }

                
                request4 =
                    Http.get
                        { url = claimsContract ++ model.address ++ blockheight
                        , expect =
                            Http.expectJson
                                (\response4 ->
                                    let
                                        _ =
                                            Debug.log "Response4: " response4
                                    in
                                    Governance2Completed response4
                                )
                                governance2Decoder
                        }

                

            in
            ( model, Cmd.batch [ sendAddress model.address, request1, request2, request4 ])

        AmountCompleted (Ok amount) ->
            ( { model
                | amount = amount
                , urlUnbonding =
                    if amount > 0 then
                        unbondingContract ++ model.address ++ blockheight

                    else
                        "-"
              }
            , Http.get
                        { url = availableBalance ++ model.mapped ++ queryDenom
                        , expect =
                            Http.expectJson
                                (\response5 ->
                                    let
                                        _ =
                                            Debug.log "Response5: " response5
                                    in
                                    AvailableCompleted response5
                                )
                                availableDecoder
                        }
            )

        AmountCompleted (Err error) ->
            ( { model
                | urlUnbonding = "-"
                , amount = 0
              }
            , Http.get
                        { url = availableBalance ++ model.mapped ++ queryDenom
                        , expect =
                            Http.expectJson
                                (\response5 ->
                                    let
                                        _ =
                                            Debug.log "Response5: " response5
                                    in
                                    AvailableCompleted response5
                                )
                                availableDecoder
                        }
            )

        BondedCompleted (Ok bonded) ->
            ( { model
                | bonded = bonded
                , urlLP =
                    if bonded > 0 then
                        farmingContract ++ model.address ++ blockheight

                    else
                        "-"
              }
            , Cmd.none
            )

        BondedCompleted (Err error) ->
            ( { model | bonded = 0 }, Cmd.none )

        GovernanceCompleted (Ok stake) ->
            if stake == 0 then
                ( model, Cmd.none )

            else
                ( { model
                    | stake = stake
                    , urlOG =
                        if stake > 0 then
                            governanceContract ++ model.address ++ blockheight

                        else
                            "-"
                  }
                , Cmd.none
                )

        GovernanceCompleted (Err error) ->
            ( { model | stake = 0 }, Cmd.none )

        Governance2Completed (Ok stake) ->
            ( { model
                | stake = stake
                , urlOG =
                    if stake > 0 then
                        claimsContract ++ model.address ++ blockheight

                    else
                        "-"
              }
            , Http.get
                        { url = governanceContract ++ model.address ++ blockheight
                        , expect =
                            Http.expectJson
                                (\response3 ->
                                    let
                                        _ =
                                            Debug.log "Response3: " response3
                                    in
                                    GovernanceCompleted response3
                                )
                                governanceDecoder
                        }
            )

        Governance2Completed (Err error) ->
            ( { model | stake = 0 }
            , Http.get
                        { url = governanceContract ++ model.address ++ blockheight
                        , expect =
                            Http.expectJson
                                (\response3 ->
                                    let
                                        _ =
                                            Debug.log "Response3: " response3
                                    in
                                    GovernanceCompleted response3
                                )
                                governanceDecoder
                        } )

        AvailableCompleted (Ok available) ->
            ( { model
                | available = available
              }
            , Cmd.none
            )

        AvailableCompleted (Err error) ->
            ( { model | available = 0 }, Cmd.none )


        



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ input [ onInput UpdateAddress  ] []
        , button [ onClick ( SubmitRequest ) ] [ text "Check" ]
        , p [] [ text ("Unbonding: " ++ String.fromFloat model.amount) ]
        , p [] [ text ("Proof: " ++ model.urlUnbonding) ]
        , p [] [ text ("LPs: " ++ String.fromFloat model.bonded) ]
        , p [] [ text ("Proof: " ++ model.urlLP) ]
        , p [] [ text ("Old Governance: " ++ String.fromFloat model.stake) ]
        , p [] [ text ("Proof: " ++ model.urlOG) ]
        , p [] [ text ("Available: " ++ String.fromFloat model.available) ]
        , p [] [ text ("Mapped Address: " ++ model.mapped) ]
        ]



-- DECODERS


amountDecoder : Decode.Decoder Float
amountDecoder =
    Decode.field "result"
        (Decode.field "claims"
            (Decode.list
                (Decode.field "amount"
                    (Decode.string
                        |> Decode.andThen
                            (\stringAmount ->
                                case String.toFloat stringAmount of
                                    Just floatAmount ->
                                        -- The string was successfully converted to a float
                                        Decode.succeed floatAmount

                                    Nothing ->
                                        -- The string could not be parsed as a float
                                        Decode.fail "Invalid float value"
                            )
                    )
                )
            )
        )|> Decode.map List.sum


stakeDecoder : Decode.Decoder Float
stakeDecoder =
    Decode.field "result"
        (Decode.field "bond_amount"
            (Decode.string
                |> Decode.andThen
                    (\stringStake ->
                        case String.toFloat stringStake of
                            Just floatStake ->
                                -- The string was successfully converted to a float
                                Decode.succeed floatStake

                            Nothing ->
                                -- The string could not be parsed as a float
                                Decode.fail "Invalid float value"
                    )
            )
        )


governanceDecoder : Decode.Decoder Float
governanceDecoder =
    Decode.field "result"
        (Decode.field "stake"
            (Decode.string
                |> Decode.andThen
                    (\stringStake ->
                        case String.toFloat stringStake of
                            Just floatStake ->
                                -- The string was successfully converted to a float
                                Decode.succeed floatStake

                            Nothing ->
                                -- The string could not be parsed as a float
                                Decode.fail "Invalid float value"
                    )
            )
        )


governance2Decoder : Decode.Decoder Float
governance2Decoder =
    Decode.field "result"
        (Decode.field "claims"
            (Decode.list
                (Decode.field "amount"
                    (Decode.string
                        |> Decode.andThen
                            (\stringAmount ->
                                case String.toFloat stringAmount of
                                    Just floatAmount ->
                                        -- The string was successfully converted to a float
                                        Decode.succeed floatAmount

                                    Nothing ->
                                        -- The string could not be parsed as a float
                                        Decode.fail "Invalid float value"
                            )
                    )
                )
            )
        )
        |> Decode.map List.sum


availableDecoder : Decode.Decoder Float
availableDecoder =
    Decode.field "balance"
        (Decode.field "amount"
            (Decode.string
                |> Decode.andThen
                    (\stringStake ->
                        case String.toFloat stringStake of
                            Just floatStake ->
                                -- The string was successfully converted to a float
                                Decode.succeed floatStake

                            Nothing ->
                                -- The string could not be parsed as a float
                                Decode.fail "Invalid float value"
                    )
            )
        )

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    receiveAddress Recv



