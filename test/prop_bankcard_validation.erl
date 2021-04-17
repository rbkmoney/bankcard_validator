-module(prop_bankcard_validation).
-include_lib("proper/include/proper.hrl").
-include_lib("damsel/include/dmsl_base_thrift.hrl").

-define(MIN_CARD_NUMBER_LENGTH, 12).
-define(MAX_CARD_NUMBER_LENGTH, 20).

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
known_payment_system() ->
    oneof(bankcard_validator_legacy:get_known_rule_names()).

invalid_card_data() ->
    ?LET(InvalidExpDate,
        invalid_exp_date(),
        ?LET(CardNumberLength,
            choose(?MIN_CARD_NUMBER_LENGTH, ?MAX_CARD_NUMBER_LENGTH),
            ?SUCHTHAT(
                #{card_number := CardNumber},
                ?LET(
                    CardNumber,
                    vector(CardNumberLength, choose($0, $9)),
                    #{card_number => list_to_binary(CardNumber), exp_date => InvalidExpDate}
                ),
                not is_luhn(CardNumber, 0)
            )
        )
    ).

%% Generate strictly valid bank card expiration date
invalid_exp_date() ->
    {{Y, M, _}, _} = calendar:system_time_to_local_time(erlang:system_time(), native),
    Year = (Y rem 100) - 1,
    {M, Year}.

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
