%%% RESTapi Records

%% Employees
-record(employee, {
		id,			% Employee ID number
		first,		% First Name
		middle, 	% Middle Name
		last,   	% Last Name
		created,	% (epoch UTC) Microseconds
		modified	% (epoch UTC) Microseconds
	}).

%% Locations
-record(location, {
		id,			% Location ID
		name		% Name of location
	}).

%% el_link - used for creating associations between employees and locations
-record(el_link, {
		employee_id,
		location_id
	}).

%% ids - used for tracking id numbers
-record(id, {
		type,
		value
	}).