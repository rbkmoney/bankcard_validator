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

-module(bankcard_validator_payment_system).

%% API
-export([decode/1]).

-define(invalid(What), erlang:throw({invalid, What})).

%% Payment system mapping
%%
%% List of known payment systems as of https://github.com/rbkmoney/binbase-data/commit/dcfabb1e.
%% Please keep in sorted order.
-spec decode(binary()) -> dmsl_domain_thrift:'BankCardPaymentSystem'().
decode(<<"AMERICAN EXPRESS">>) ->
    <<"amex">>;
decode(<<"AMERICAN EXPRESS COMPANY">>) ->
    <<"amex">>;
decode(<<"CHINA UNION PAY">>) ->
    <<"unionpay">>;
decode(<<"DANKORT">>) ->
    <<"dankort">>;
decode(<<"DINERS CLUB INTERNATIONAL">>) ->
    <<"dinersclub">>;
decode(<<"DISCOVER">>) ->
    <<"discover">>;
decode(<<"JCB">>) ->
    <<"jcb">>;
decode(<<"MAESTRO">>) ->
    <<"maestro">>;
decode(<<"MASTERCARD">>) ->
    <<"mastercard">>;
decode(<<"NSPK MIR">>) ->
    <<"nspkmir">>;
decode(<<"UNIONPAY">>) ->
    <<"unionpay">>;
decode(<<"VISA">>) ->
    <<"visa">>;
decode(<<"DUMMY">>) ->
    <<"dummy">>;
% supposedly 🤔
decode(<<"VISA/DANKORT">>) ->
    <<"visa">>;
decode(<<"UZCARD">>) ->
    <<"uzcard">>;
decode(<<"VPAY">>) ->
    ?invalid(payment_system);
decode(<<"DUET">>) ->
    ?invalid(payment_system);
decode(<<"EBT">>) ->
    ?invalid(payment_system);
decode(<<"EFTPOS">>) ->
    ?invalid(payment_system);
decode(<<"ELO">>) ->
    ?invalid(payment_system);
decode(<<"ELO/DISCOVER">>) ->
    ?invalid(payment_system);
decode(<<"EUROSHELL FUEL CARD">>) ->
    ?invalid(payment_system);
decode(<<"FUEL CARD">>) ->
    ?invalid(payment_system);
decode(<<"GE CAPITAL">>) ->
    ?invalid(payment_system);
decode(<<"GLOBAL BC">>) ->
    ?invalid(payment_system);
decode(<<"HIPERCARD">>) ->
    ?invalid(payment_system);
decode(<<"HRG STORE CARD">>) ->
    ?invalid(payment_system);
decode(<<"LOCAL BRAND">>) ->
    ?invalid(payment_system);
decode(<<"LOCAL CARD">>) ->
    ?invalid(payment_system);
decode(<<"LOYALTY CARD">>) ->
    ?invalid(payment_system);
decode(<<"LUKOIL FUEL CARD">>) ->
    ?invalid(payment_system);
decode(<<"NEWDAY">>) ->
    ?invalid(payment_system);
decode(<<"DFS/DCI">>) ->
    ?invalid(payment_system);
decode(<<"DINACARD">>) ->
    ?invalid(payment_system);
decode(<<"CHJONES FUEL CARD">>) ->
    ?invalid(payment_system);
decode(<<"CIRRUS">>) ->
    ?invalid(payment_system);
decode(<<"COMPROCARD">>) ->
    ?invalid(payment_system);
decode(<<"ATM CARD">>) ->
    ?invalid(payment_system);
decode(<<"ATOS PRIVATE LABEL">>) ->
    ?invalid(payment_system);
decode(<<"AURA">>) ->
    ?invalid(payment_system);
decode(<<"BANKCARD(INACTIVE)">>) ->
    ?invalid(payment_system);
decode(<<"BP FUEL CARD">>) ->
    ?invalid(payment_system);
decode(<<"CABAL">>) ->
    ?invalid(payment_system);
decode(<<"CARNET">>) ->
    ?invalid(payment_system);
decode(<<"OUROCARD">>) ->
    ?invalid(payment_system);
decode(<<"PAYPAL">>) ->
    ?invalid(payment_system);
decode(<<"PHH FUEL CARD">>) ->
    ?invalid(payment_system);
decode(<<"PRIVATE LABEL">>) ->
    ?invalid(payment_system);
decode(<<"PRIVATE LABEL CARD">>) ->
    ?invalid(payment_system);
decode(<<"PROSTIR">>) ->
    ?invalid(payment_system);
decode(<<"RBS GIFT CARD">>) ->
    ?invalid(payment_system);
decode(<<"RED FUEL CARD">>) ->
    ?invalid(payment_system);
decode(<<"RED LIQUID FUEL CARD">>) ->
    ?invalid(payment_system);
decode(<<"RUPAY">>) ->
    ?invalid(payment_system);
decode(<<"SBERCARD">>) ->
    ?invalid(payment_system);
decode(<<"SODEXO">>) ->
    ?invalid(payment_system);
decode(<<"STAR REWARDS">>) ->
    ?invalid(payment_system);
decode(<<"TROY">>) ->
    ?invalid(payment_system);
decode(<<"UATP">>) ->
    ?invalid(payment_system);
decode(<<"UK FUEL CARD">>) ->
    ?invalid(payment_system);
decode(PaymentSystem) ->
    _ = logger:warning("unknown payment system encountered: ~s", [PaymentSystem]),
    ?invalid(payment_system).
