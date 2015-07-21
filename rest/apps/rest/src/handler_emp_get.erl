-module(handler_emp_get).
-behaviour(cowboy_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Type, Req, _State) ->
	{ok, Req, []}.

handle(Req, _State) ->
	{ID, _Req} = cowboy_req:binding(empid, Req),

	{ok, Payload} = gen_server:call(rest_api, {get_emp, binary_to_integer(ID)}),
	{ok, Req2} = cowboy_req:reply(200,
			        [{<<"content-type">>, <<"application/json">>}],
			        Payload,
			        Req
			     ),
	{ok, Req2, []}.

terminate(_Reason, _Req, _State) ->
	ok.