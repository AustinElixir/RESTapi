%%% Mnesia DB setup
-module(rest_db).

%%% External code
-include_lib ("stdlib/include/qlc.hrl").
-include_lib ("../include/rest.hrl").

%%% Exported Functions
-export([ initial_setup/0 ]).

%%%===================================================================
%%% Public Functions
%%%===================================================================
%% Setup mnesia tables for the first time (see Vagrantfile / Dockerfile)
initial_setup() ->
	mnesia:create_schema([node()]),
	mnesia:start(),
	create_tables().

%% Create tables after scheme is made
create_tables() ->
	mnesia:create_table(employees,
		[{type, ordered_set},
		 {disc_copies, [node()]},
		 {attributes, record_info(fields, employees)}]),

	mnesia:create_table(locations,
		[{type, ordered_set},
		 {disc_copies, [node()]},
		 {attributes, record_info(fields, locations)}]),

	mnesia:create_table(el_link,
		[{type, ordered_set},
		 {disc_copies, [node()]},
		 {attributes, record_info(fields, el_link)}]),

	ok.
%%%%%%%%%%%%%%%%%%%%%%
%%% MNESIA QUERIES %%%
%%%%%%%%%%%%%%%%%%%%%%