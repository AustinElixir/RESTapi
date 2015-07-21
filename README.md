RESTapi
=======
This is a bare bones project that includes a very simple database interface for a collection of Mnesia tables.

The intent of this project is to provide a framework to quickly build a small HTTPrest API on top of Mnesia.

Follow these steps to complete the tutorial.

Please note that we'll be doing some things you might not want in production as purely educational exercises.

Setup Guide
===========
#### Dowload Project Stucture

- `git clone https://github.com/austinerlang/RESTapi`

#### Setup Virtual Dev Environment

##### DOCKER

Make sure you have docker [installed](https://docs.docker.com/installation/)

TO DO - We are working on a docker file for this. You can test this:

- `sudo docker run -it -p 8080:8080 austinerlang/restapi /bin/bash`

##### VAGRANT

Make sure you have vagrant [installed](http://docs.vagrantup.com/v2/installation/)

- `vagrant up`
- `vagrant ssh`

#### Mnesia DB

Verify Mnesia built the schema files correctly.

- `ls -l /var/rest_db/`

If the directory is empty, there is a bug with vagrant / erlang shell during post provisioning. Manually fix:

- `cd /vagrant/rest`
- `rebar3 release`
- `erl -pa _build/default/lib/*/ebin -sname rest -mnesia dir '"/var/rest_db"' -run rest_db initial_setup -s init stop`

Now verify the files exist in /var/rest_db.

#### Tutorial

#### Checkout tutorial branch

- `git fetch`
- `git checkout tutorial`

#### Build and run release

- `cd /vagrant/rest`
- `rebar3 release`
- `rebar3 run`

#### Play with the Mnesia Database using provided interface

```erlang
rest_db:create_employee("Johnny", "William", "Carson").
rest_db:get_employee(Id).
rest_db:update_employee(Id, First, Middle, Last).
rest_db:delete_employee(Id).
```

Ensure you have some entries and make a note of the ID numbers.

#### Create a gen server from your template with these callbacks

rest/apps/rest/src/rest_api.erl

```erlang
handle_call({get_emp, ID}, _From, State) ->
	{ok, Emp} = rest_db:get_employee(ID),
    Reply = {ok, jsx:encode(Emp)},
    {reply, Reply, State};

handle_call({update_emp, ID, Emp}, _From, State) ->
	EmpDecoded = jsx:decode(Emp, [return_maps]),
	First = maps:get(<<"first">>, EmpDecoded),
	Middle = maps:get(<<"middle">>, EmpDecoded),
	Last = maps:get(<<"last">>, EmpDecoded),
	{ok, EmpNew} = rest_db:update_employee(ID, First, Middle, Last),
    Reply = {ok, jsx:encode(EmpNew)},
    {reply, Reply, State};
```

#### Supervise the gen_server

rest/apps/rest/src/rest_sup.erl

```erlang
		#{ id => rest_api,
           start => {rest_api, start_link, []},
           restart => permanent,
           shutdown => 1000,
           type => worker }
```

#### Add HTTP routing

Add your routing to your application in: rest_app.erl

```erlang
	Dispatch = cowboy_router:compile([
	    %% {HostMatch, list({PathMatch, Handler, Opts})}
	    {'_', [
	    	{"/get_employee/:empid", handler_emp_get, []},
	    	{"/set_employee/:empid", handler_emp_set, []}
	    ]}
	]),
	%% Name, NbAcceptors, TransOpts, ProtoOpts
	cowboy:start_http(my_http_listener, 100,
	    [{port, 8080}],
	    [{env, [{dispatch, Dispatch}]}]
	),
```

#### Create your handlers

rest/apps/rest/src/handler_emp_get.erl

```erlang
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
```

rest/apps/rest/src/handler_emp_set.erl

```erlang
-module(handler_emp_set).
-behaviour(cowboy_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Type, Req, _State) ->
	{ok, Req, []}.

handle(Req, _State) ->
	{ID, _Req} = cowboy_req:binding(empid, Req),
	respond(Req, ID, cowboy_req:has_body(Req)).

respond(Req, ID, true) ->
	{ok, Body, _Req} = cowboy_req:body(Req),
	io:format("BODY: ~p~n", [Body]),
	{ok, Payload} = gen_server:call(rest_api, {update_emp, binary_to_integer(ID), Body}),
	{ok, ReqNew} = cowboy_req:reply(200,
			        [{<<"content-type">>, <<"application/json">>},
			         {<<"Access-Control-Allow-Origin">>, <<"*">>}], % chrome security
			        Payload,
			        Req
			     ),
	{ok, ReqNew, []};

respond(Req, ID, false) ->
	{ok, Payload} = gen_server:call(rest_api, {get_emp, binary_to_integer(ID)}),
	{ok, ReqNew} = cowboy_req:reply(200,
			        [{<<"content-type">>, <<"application/json">>}],
			        Payload,
			        Req
			     ),
	{ok, ReqNew, []}.

terminate(_Reason, _Req, _State) ->
	ok.
```