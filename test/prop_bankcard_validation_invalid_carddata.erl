-module(prop_bankcard_validation_invalid_carddata).
-include_lib("proper/include/proper.hrl").
-include_lib("damsel/include/dmsl_base_thrift.hrl").

-define(MIN_CARD_NUMBER_LENGTH, 12).
-define(MAX_CARD_NUMBER_LENGTH, 20).
-define(MIN_INVALID_CVC_LENGTH, 5).

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
-spec prop_invalid_card_number() -> boolean().
prop_invalid_card_number() ->
    ?FORALL(
        {PaymentSystem, Card},
        {known_payment_system(), invalid_card_data()},
        check_invalid_card_data(PaymentSystem, Card)
    ).

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%
check_invalid_card_data(PaymentSystem, Card) ->
    ok =/= bankcard_validator:validate(Card, undefined, PaymentSystem, #{}).

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%
%% Retrieves name of one of known rules
known_payment_system() ->
    oneof(bankcard_validator_legacy:get_known_rule_names()).

%% Generates invalid card data
invalid_card_data() ->
    ?LET(
        Cardholder,
        cardholder(),
        ?LET(
            CVC,
            vector(?MIN_INVALID_CVC_LENGTH, choose($0, $9)),
            ?LET(
                InvalidExpDate,
                invalid_exp_date(),
                ?LET(
                    CardNumberLength,
                    choose(?MIN_CARD_NUMBER_LENGTH, ?MAX_CARD_NUMBER_LENGTH),
                    ?SUCHTHAT(
                        #{card_number := CardNumber},
                        ?LET(
                            CardNumber,
                            vector(CardNumberLength, choose($0, $9)),
                            #{
                                card_number => list_to_binary(CardNumber),
                                exp_date => InvalidExpDate,
                                cvc => list_to_binary(CVC),
                                cardholder => list_to_binary(Cardholder)
                            }
                        ),
                        not is_luhn(CardNumber, 0)
                    )
                )
            )
        )
    ).

%% Generate random caldholder name
cardholder() ->
    list(oneof([choose($A, $Z), 32, $.])).

%% Generate strictly valid bank card expiration date
invalid_exp_date() ->
    {{Y, M, _}, _} = calendar:universal_time(),
    Year = Y - 1,
    {M, Year}.

%%%%%%%%%%%%%%%%%%
%%%  Utilites  %%%
%%%%%%%%%%%%%%%%%%
is_luhn(<<CheckSum>>, Sum) ->
    case Sum * 9 rem 10 of
        M when M =:= CheckSum - $0 ->
            true;
        _M ->
            false
    end;
is_luhn(<<N, Rest/binary>>, Sum) when byte_size(Rest) rem 2 =:= 1 ->
    case (N - $0) * 2 of
        M when M >= 10 ->
            is_luhn(Rest, Sum + M div 10 + M rem 10);
        M ->
            is_luhn(Rest, Sum + M)
    end;
is_luhn(<<N, Rest/binary>>, Sum) ->
    is_luhn(Rest, Sum + N - $0).
