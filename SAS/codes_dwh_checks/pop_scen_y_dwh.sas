/*population by scenario/version and year: dwh*/
	/*future values only (since from the model only future values*/
	proc sql;
	   create table pop_scen_y_dwh as 
	   select publjahr, 
	          versionartcd, 
	          versionartlang, 
	          jahr, 
	            round(sum(anzbestwir), &round_pop.) as pop_dwh
	      from pop_dwh
	      where (publjahr = &current_year.) and (basisszenariencd = '2')
	      group by publjahr,
	               versionartcd,
	               versionartlang,
	               jahr
	      order by versionartcd,
	               jahr;
	quit;






