%%% @doc This module makes it easier to run PropEr tests from eunit.
%%%
%%% If a property PropEr is tested from EUnit, output of PropEr is not visible
%%% <a href="http://erlang.org/doc/apps/eunit/chapter.html#Running_EUnit">
%%% (because EUnit captures standard output)</a>. This module is a set of
%%% helpers, which make PropEr test output to be visible when invoked from
%%% eunit.
%%%
%%% Example usage:
%%% ```
%%%    -module(arith_test).
%%%    -include("eunit/include/eunit.hrl").
%%%    z_string_test_() ->
%%%        [
%%%            {"Simple and quick test",
%%%                proper_utils:qc_(t1())},
%%%            {"More complicated and a very long-running test",
%%%                {timeout, 3600, proper_utils:qc_(t2(), [{numtests, 10000}])}}
%%%        ].
%%%    
%%%    t1() -> ?FORALL(I, pos_integer(), 0 < I).
%%%    t2() -> ?FORALL( ... ).
%%% '''
%%%
%%% `t1/0' is the simpliest example how to use the helper.
%%% `t2/0' is a long test, for which also EUnit timeout has to be adjusted.
%%%
%%% For more information about EUnit generators, see
%%% <a href="http://erlang.org/doc/apps/eunit/chapter.html#EUnit_test_representation">
%%% EUnit test representation</a>.

-module(proper_eunit).

-export([qc_/1, qc_/2, print_stdout/1]).

-include_lib("eunit/include/eunit.hrl").

%% @doc Run a function, redirecting standard output to real standard output
%%
%% PropEr writes to standard output, and EUnit captures it. If one wants to see
%% the output of any EUnit test, this function should be used.
-spec print_stdout(fun(() -> A)) -> A.
print_stdout(Fun) ->
    EunitLeader = erlang:group_leader(),
    erlang:group_leader(whereis(user), self()),
    Ret = Fun(),
    erlang:group_leader(EunitLeader, self()),
    Ret.

%% @doc like {@link proper:quickcheck/2}, but work around eunit stdout printing.
%%
%% `Options' is passed as second argument to {@link proper:quickcheck/2}.
-spec qc(proper:outer_test(), proper:user_opts()) -> proper:result().
qc(Proper, UserOpts) ->
    print_stdout(fun() -> proper:quickcheck(Proper, UserOpts) end).

%% @equiv qc_(Proper, [])
qc_(Proper) ->
    qc_(Proper, []).

%% @doc EUnit generator for {@link proper:quickcheck/2}.
%%
%% Makes EUnit test pass if all PropEr properties passes;
%% Makes it fail if any PropEr property fails.
-spec qc_(proper:outer_test(), proper:user_opts()) -> no_return().
qc_(Proper, UserOpts) ->
    ?_assertEqual(true, qc(Proper, UserOpts)).
