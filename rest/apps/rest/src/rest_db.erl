%%% Mnesia DB setup
-module(rest_db).

%%% External code
-include_lib ("stdlib/include/qlc.hrl").
-include_lib ("../include/rest.hrl").

%% Testing
-compile([export_all]).

%%% Employee crud functions
-export([ initial_setup/0,
		  create_employee/3,
		  update_employee/4,
		  get_employee/1,
		  delete_employee/1
		]).

%%% Location crud functions
% -export([ create_location/1,
% 		  get_location/1,
% 		  update_location/2,
% 		  delete_location/1,
% 		  add_link/2,
% 		  delete_link/2,
% 		  employee_location_links/1,
% 		  location_employee_links/1
% 		]).

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
	mnesia:create_table(employee,
		[{type, ordered_set},
		 {disc_copies, [node()]},
		 {attributes, record_info(fields, employee)}]),

	mnesia:create_table(location,
		[{type, ordered_set},
		 {disc_copies, [node()]},
		 {attributes, record_info(fields, location)}]),

	mnesia:create_table(el_link,
		[{type, bag},
		 {disc_copies, [node()]},
		 {attributes, record_info(fields, el_link)}]),

	mnesia:create_table(id,
		[{type, ordered_set},
		 {disc_copies, [node()]},
		 {attributes, record_info(fields, id)}]),

	mnesia:dirty_write(#id{ type = employee,
						    value = 1 }),
	mnesia:dirty_write(#id{ type = location,
						    value = 1 }),
	ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% EMPLOYEE CRUD OPERATIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Get the next available id and then update it with +1
get_empid() ->
    [Item] = mnesia:read( id, employee, read ),
    N = Item#id.value,
	mnesia:write( Item#id{value = N + 1} ),
	N.

create_employee(First, Middle, Last) ->
	First2 = ensure_list(First),
	Mid2 = ensure_list(Middle),
	Last2 = ensure_list(Last),
    {ok, Employee} = create_employee_aux(First2, Mid2, Last2),
    {ok, emp_to_map(Employee)}.

get_employee(Id) ->
    {ok, Employee} = get_employee_aux(Id),
    {ok, emp_to_map(Employee)}.

update_employee(Id, First, Middle, Last) ->
	First2 = ensure_list(First),
	Mid2 = ensure_list(Middle),
	Last2 = ensure_list(Last),
	{ok, Emp} = get_employee_aux(Id),
	update_employee_aux(Id, First2, Mid2, Last2, Emp#employee.created).

delete_employee(Id) ->
    {ok, Employee} = get_employee_aux(Id),
    mnesia:transaction( fun() ->
                            mnesia:delete_object(Employee)
                        end).


%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Internal functions %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%
ensure_list(Input) ->
	case Input of
		Val when is_binary(Val) -> binary_to_list(Val);
		Val when is_list(Val) -> Val;
		_ -> erlang:throw("Invalid Type for First Name, must be a string/list")
	end.

get_employee_aux(Id) ->
    {atomic, Employee} = mnesia:transaction( fun() ->
    	case mnesia:read( employee, Id, read ) of
			[Item] -> Item;
			[] -> []
		end
    end),
    {ok, Employee}.

create_employee_aux(First, Middle, Last) ->
    Timestamp = erlang:system_time(),

    {atomic, Employee} = mnesia:transaction( fun() ->
    	NextID = get_empid(),
    	New = #employee{ id = NextID,
	                   first = First,
	                   middle = Middle,
	                   last = Last,
	                   created = Timestamp,
	                   modified = Timestamp },
	    mnesia:write( New ),
	    New
    end),
    {ok, Employee}.

update_employee_aux(Id, First, Middle, Last, Created) ->
    Timestamp = erlang:system_time(),

    {atomic, Employee} = mnesia:transaction( fun() ->
    	New = #employee{ id = Id,
	                   first = First,
	                   middle = Middle,
	                   last = Last,
	                   created = Created,
	                   modified = Timestamp },
	    mnesia:write( New ),
	    New
    end),
    {ok, emp_to_map(Employee)}.

%% Convert an employee record to a map object
emp_to_map(E) ->
	case E of
		[] -> #{};
		_ -> #{
			<<"id">> => E#employee.id,
			<<"first">> => list_to_binary(E#employee.first),
			<<"middle">> => list_to_binary(E#employee.middle),
			<<"last">> => list_to_binary(E#employee.last),
			<<"created">> => E#employee.created,
			<<"modified">> => E#employee.modified
		}
	end.

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% %%% LOCATION CRUD OPERATIONS %%%
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% %% Get the next available id and then update it with +1
% get_locid() ->
%     [Item] = mnesia:read( id, location, read ),
%     N = Item#id.value,
% 	mnesia:write( Item#id{value = N + 1} ),
% 	N.

% % Create entry in the location table
% create_location(Name) ->
%     {atomic, Location} = mnesia:transaction( fun() ->
%     	NextID = get_locid(),
%     	New = #location{ id = NextID,
% 	                   name = Name },
% 	    mnesia:write( New ),
% 	    New
%     end),
%     {ok, Location}.

% get_location(Id) ->
%     {atomic, Location} = mnesia:transaction( fun() ->
%     	case mnesia:read( location, Id, read ) of
% 			[Item] -> Item;
% 			[] -> []
% 		end
%     end),
%     {ok, Location}.

% update_location(Id, Name) ->
%     {atomic, Location} = mnesia:transaction( fun() ->
%     	New = #location{ id = Id,
% 	                   name = Name },
% 	    mnesia:write( New ),
% 	    New
%     end),
%     {ok, Location}.

% delete_location(Id) ->
%     {ok, Location} = get_location(Id),
%     mnesia:transaction( fun() ->
%                             mnesia:delete_object(Location)
%                         end).

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% %%% el_link CRUD OPERATIONS %%%
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% add_link(EmpID, LocID) ->
%     {atomic, Link} = mnesia:transaction( fun() ->
%     	New = #el_link{ employee_id = EmpID,
% 	                   location_id = LocID },
% 	    mnesia:write( New ),
% 	    New
%     end),
%     {ok, Link}.

% delete_link(EmpID, LocID) ->
%     mnesia:transaction( fun() ->
%                             mnesia:delete_object(#el_link{ employee_id = EmpID,
%                             							   location_id = LocID })
%                         end).

% employee_location_links(EmpID) ->
%     mnesia:transaction(
%       fun() -> qlc:e(
%         qlc:q( [ get_location( L#el_link.location_id ) ||
%           L <- mnesia:table(el_link),
%           L#el_link.employee_id == EmpID
%           ])
%         )
%       end
%     ).

% location_employee_links(LocID) ->
%     mnesia:transaction(
%       fun() -> qlc:e(
%         qlc:q( [ get_employee( L#el_link.employee_id ) ||
%           L <- mnesia:table(el_link),
%           L#el_link.location_id == LocID
%           ])
%         )
%       end
%     ).