/*population by scenario/version and year: model*/
	proc sql;
	   create table pop_scen_y_model as 
	   select versionartcd, 
	          year, 
	            round(sum(pop), &round_pop.) as pop_model
	      from pop_model
	      group by versionartcd,
	               year
	      order by versionartcd,
	               year;
	quit;

