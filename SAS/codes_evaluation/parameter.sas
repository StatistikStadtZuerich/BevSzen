/*this year*/
	%let this_year = %sysfunc(year(%sysfunc(today())));
	%put &this_year.;

