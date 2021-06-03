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

-module(bankcard_validator_test_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([
    all/0,
    init_per_suite/1,
    end_per_suite/1
]).

-export([init/1]).

-export([test_invalid_carddata/1]).
-export([test_valid_carddata/1]).

-type config() :: [{atom(), any()}].
-type case_name() :: atom().

-spec all() -> [case_name()].
-spec init_per_suite(config()) -> config().
-spec end_per_suite(config()) -> any().
-spec test_invalid_carddata(config()) -> ok.
-spec test_valid_carddata(config()) -> ok.

-behaviour(supervisor).

-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    {ok, {#{strategy => one_for_all, intensity => 1, period => 1}, []}}.

%%
%% tests descriptions
%%
all() ->
    [
        test_invalid_carddata,
        test_valid_carddata
    ].

%%
%% starting/stopping
%%
init_per_suite(C) ->
    % dbg:tracer(), dbg:p(all, c),
    % dbg:tpl({bankcard_validator, do, 4}, x),
    bankcard_validator_ct_helper:init_suite(?MODULE, C).

end_per_suite(C) ->
    _ = bankcard_validator_ct_helper:stop_mocked_service_sup(?config(suite_test_sup, C)),
    _ = [application:stop(App) || App <- proplists:get_value(apps, C)],
    ok.

%%
%% tests
%%
test_invalid_carddata(_C) ->
    R = proper:quickcheck(
        bankcard_validation_invalid_carddata:invalid_card_number_test(),
        % default options
        [noshrink]
    ),
    case R of
        true -> ok;
        Error -> exit(Error)
    end.

test_valid_carddata(_C) ->
    R = proper:quickcheck(
        bankcard_validation_valid_carddata:valid_card_number_test(),
        % default options
        [noshrink]
    ),
    case R of
        true -> ok;
        Error -> exit(Error)
    end.
