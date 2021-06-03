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

-module(bankcard_validator_legacy).

-include_lib("damsel/include/dmsl_domain_thrift.hrl").

-export([get_payment_system_ruleset/1]).

-type payment_system() :: dmsl_domain_thrift:'BankCardPaymentSystem'().
-type card_validation_rule() :: dmsl_domain_thrift:'PaymentCardValidationRule'().
-type validation_rules() :: ordsets:ordset(card_validation_rule()).

-export_type([payment_system/0]).

-define(KNOWN_RULES, #{
    <<"visa">> => [
        {card_number, [
            {ranges, [#'IntegerRange'{lower = 13, upper = 13}, #'IntegerRange'{lower = 16, upper = 16}]},
            {checksum, {luhn, #'domain_PaymentCardNumberChecksumLuhn'{}}}
        ]},
        {cvc, {length, #'IntegerRange'{lower = 3, upper = 3}}},
        {exp_date, {exact_exp_date, #'domain_PaymentCardExactExpirationDate'{}}}
    ],

    <<"mastercard">> => [
        {card_number, [
            {ranges, [#'IntegerRange'{lower = 16, upper = 16}]},
            {checksum, {luhn, #'domain_PaymentCardNumberChecksumLuhn'{}}}
        ]},
        {cvc, {length, #'IntegerRange'{lower = 3, upper = 3}}},
        {exp_date, {exact_exp_date, #'domain_PaymentCardExactExpirationDate'{}}}
    ],

    %% Maestro Global Rules
    %% https://www.mastercard.com/hr/merchants/_assets/Maestro_rules.pdf
    %%
    %% 6.2.1.3 Primary Account Number (PAN)
    %%
    %% The PAN must be no less than twelve (12) and no more than nineteen (19)
    %% digits in length. All digits of the PAN must be numeric. It is strongly
    %% recommended that Members issue Cards with a PAN of nineteen (19) digits.
    %%
    %% The IIN appears in the first six (6) digits of the PAN and must be assigned
    %% by the ISO Registration Authority, and must be unique.
    <<"maestro">> => [
        {card_number, [
            {ranges, [#'IntegerRange'{lower = 12, upper = 12}, #'IntegerRange'{lower = 19, upper = 19}]},
            {checksum, {luhn, #'domain_PaymentCardNumberChecksumLuhn'{}}}
        ]},
        {cvc, {length, #'IntegerRange'{lower = 3, upper = 3}}},
        {exp_date, {exact_exp_date, #'domain_PaymentCardExactExpirationDate'{}}}
    ],

    <<"nspkmir">> => [
        {card_number, [
            {ranges, [#'IntegerRange'{lower = 16, upper = 16}, #'IntegerRange'{lower = 19, upper = 20}]},
            {checksum, {luhn, #'domain_PaymentCardNumberChecksumLuhn'{}}}
        ]},
        {cvc, {length, #'IntegerRange'{lower = 3, upper = 3}}},
        {exp_date, {exact_exp_date, #'domain_PaymentCardExactExpirationDate'{}}}
    ],

    <<"amex">> => [
        {card_number, [
            {ranges, [#'IntegerRange'{lower = 15, upper = 15}]},
            {checksum, {luhn, #'domain_PaymentCardNumberChecksumLuhn'{}}}
        ]},
        {cvc, {length, #'IntegerRange'{lower = 3, upper = 4}}},
        {exp_date, {exact_exp_date, #'domain_PaymentCardExactExpirationDate'{}}}
    ],

    <<"dinersclub">> => [
        {card_number, [
            {ranges, [#'IntegerRange'{lower = 14, upper = 14}, #'IntegerRange'{lower = 19, upper = 19}]},
            {checksum, {luhn, #'domain_PaymentCardNumberChecksumLuhn'{}}}
        ]},
        {cvc, {length, #'IntegerRange'{lower = 3, upper = 3}}},
        {exp_date, {exact_exp_date, #'domain_PaymentCardExactExpirationDate'{}}}
    ],

    <<"discover">> => [
        {card_number, [
            {ranges, [#'IntegerRange'{lower = 16, upper = 16}]},
            {checksum, {luhn, #'domain_PaymentCardNumberChecksumLuhn'{}}}
        ]},
        {cvc, {length, #'IntegerRange'{lower = 3, upper = 3}}},
        {exp_date, {exact_exp_date, #'domain_PaymentCardExactExpirationDate'{}}}
    ],

    <<"unionpay">> => [
        {card_number, [
            {ranges, [#'IntegerRange'{lower = 16, upper = 16}, #'IntegerRange'{lower = 19, upper = 19}]},
            {checksum, {luhn, #'domain_PaymentCardNumberChecksumLuhn'{}}}
        ]},
        {cvc, {length, #'IntegerRange'{lower = 3, upper = 3}}},
        {exp_date, {exact_exp_date, #'domain_PaymentCardExactExpirationDate'{}}}
    ],

    <<"jcb">> => [
        {card_number, [
            {ranges, [#'IntegerRange'{lower = 16, upper = 16}]},
            {checksum, {luhn, #'domain_PaymentCardNumberChecksumLuhn'{}}}
        ]},
        {cvc, {length, #'IntegerRange'{lower = 3, upper = 3}}},
        {exp_date, {exact_exp_date, #'domain_PaymentCardExactExpirationDate'{}}}
    ],

    <<"forbrugsforeningen">> => [
        {card_number, [
            {ranges, [#'IntegerRange'{lower = 16, upper = 16}]},
            {checksum, {luhn, #'domain_PaymentCardNumberChecksumLuhn'{}}}
        ]},
        {cvc, {length, #'IntegerRange'{lower = 3, upper = 3}}},
        {exp_date, {exact_exp_date, #'domain_PaymentCardExactExpirationDate'{}}}
    ],

    <<"dankort">> => [
        {card_number, [
            {ranges, [#'IntegerRange'{lower = 16, upper = 16}]},
            {checksum, {luhn, #'domain_PaymentCardNumberChecksumLuhn'{}}}
        ]},
        {cvc, {length, #'IntegerRange'{lower = 3, upper = 3}}},
        {exp_date, {exact_exp_date, #'domain_PaymentCardExactExpirationDate'{}}}
    ]
}).

-ifdef(TEST).
-export([get_known_rule_names/0]).
-spec get_known_rule_names() -> [binary()].
get_known_rule_names() ->
    maps:keys(?KNOWN_RULES).
-endif.

% config

-spec get_payment_system_ruleset(payment_system()) -> validation_rules() | undefined.
get_payment_system_ruleset(PaymentSystem) ->
    maps:get(
        PaymentSystem,
        ?KNOWN_RULES,
        undefined
    ).
