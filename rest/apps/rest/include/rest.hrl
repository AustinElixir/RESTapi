%%% RESTapi Records

%% Employees
-record(employees, {
		id,			% Employee ID number
		first,		% First Name
		middle, 	% Middle Name
		last,   	% Last Name
		created,	% (epoch UTC)
		modified	% (epoch UTC)
	}).

%% Locations
-record(locations, {
		id,			% Location ID
		name,		% Name of location
		longitude,	% Longitude
		latitude	% Latitude
	}).

%% el_link - used for creating associations between employees and locations
-record(el_link, {
		employee_id,
		location_id
	}).