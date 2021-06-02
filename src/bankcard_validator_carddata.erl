%%%
%%% Copyright 2021 RBKmoney
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%

-module(bankcard_validator_carddata).

-include_lib("cds_proto/include/cds_proto_storage_thrift.hrl").

-type bank_info() :: #{
    payment_system := binary(),
    payment_system_deprecated := dmsl_domain_thrift:'LegacyBankCardPaymentSystem'(),
    bank_name := binary(),
    issuer_country := dmsl_domain_thrift:'Residence'() | undefined,
    category := binary() | undefined,
    metadata := {_MetaNS :: binary(), map()}
}.

-type card_data() :: cds_proto_storage_thrift:'PutCardData'().
-type session_data() :: cds_proto_storage_thrift:'SessionData'().
-type extra_card_data() :: #{
    cardholder => binary() | undefined,
    exp_data => {integer(), integer()}
}.

-type bankcard_data() :: #{
    card_number := binary(),
    cardholder => binary() | undefined,
    exp_data => {integer(), integer()},
    cvc => binary()
}.

-type payment_system() :: binary().

-export_type([bank_info/0]).
-export_type([card_data/0]).
-export_type([bankcard_data/0]).

%% API
-export([payment_system/1]).
-export([decode_issuer_country/1]).
-export([build_bankcard_data/3]).

-spec payment_system(bank_info()) -> payment_system().
payment_system(BankInfo) ->
    maps:get(payment_system, BankInfo).

%% Residence mapping
%%
-spec decode_issuer_country(binary() | undefined) -> dmsl_domain_thrift:'Residence'() | undefined.
decode_issuer_country(Residence) when is_binary(Residence) ->
    try
        {enum, Variants} = dmsl_domain_thrift:enum_info('CountryCode'),
        Variant = erlang:list_to_existing_atom(string:to_lower(erlang:binary_to_list(Residence))),
        element(1, lists:keyfind(Variant, 1, Variants))
    catch
        error:badarg ->
            _ = logger:warning("unknown residence encountered: ~s", [Residence]),
            erlang:throw({invalid, issuer_country})
    end;
decode_issuer_country(undefined) ->
    undefined.

-spec build_bankcard_data(card_data(), extra_card_data(), session_data() | undefined) -> bankcard_data().
build_bankcard_data(CardData, ExtraCardData, undefined) ->
    maps:merge(convert_card_data(CardData), ExtraCardData);
build_bankcard_data(CardData, ExtraCardData, #cds_SessionData{auth_data = AuthData}) ->
    CVV = get_cvv_from_session_data(AuthData),
    CardDataMap0 = convert_card_data(CardData),
    CardDataMap1 = maps:merge(CardDataMap0, ExtraCardData),
    CardDataMap1#{cvc => maybe_undefined(CVV)}.

get_cvv_from_session_data({card_security_code, AuthData}) ->
    AuthData#cds_CardSecurityCode.value;
get_cvv_from_session_data(_) ->
    undefined.

convert_card_data(CardData) ->
    #cds_PutCardData{
        pan = PAN
    } = CardData,
    #{
        card_number => PAN
    }.

maybe_undefined(<<>>) ->
    undefined;
maybe_undefined(CVV) ->
    CVV.
