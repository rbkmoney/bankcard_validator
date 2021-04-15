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

-module(bankcard_validator_test).

-include_lib("eunit/include/eunit.hrl").

-spec test() -> _.

-spec visa_test() -> _.

visa_test() ->
    M = 01,
    Y = 2030,
    Card = #{card_number => <<"4242424242424242">>, cardholder => <<"CARD HOLDER">>, exp_data => {M, Y}},
    ?assertEqual(ok, bankcard_validator:validate(Card, undefined, <<"visa">>, #{})).
