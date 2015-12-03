%%%-------------------------------------------------------------------
%% @doc rest public API
%% @end
%%%-------------------------------------------------------------------

-module('rest_app').

-behaviour(application).

%% Application callbacks
-export([start/2
        ,stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
	Dispatch = cowboy_router:compile([
	    %% {HostMatch, list({PathMatch, Handler, Opts})}
	    {'_', [
	    	{"/get_employee/:empid", handler_emp_get, []},
	    	{"/set_employee/:empid", handler_emp_set, []},
	    	{"/delete_employee/:empid", handler_emp_delete, []}
	    ]}
	]),
	%% Name, NbAcceptors, TransOpts, ProtoOpts
	cowboy:start_http(my_http_listener, 100,
	    [{port, 8080}],
	    [{env, [{dispatch, Dispatch}]}]
	),
    'rest_sup':start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
