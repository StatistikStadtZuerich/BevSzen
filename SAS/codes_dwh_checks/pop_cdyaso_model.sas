/*population: model*/

	/*c: scenario/version*/
	/*d: district*/
	/*y: year*/
	/*a: age (ten year age classes)*/
	/*s: sex*/
	/*o: origin*/

	proc sql;
	   create table pop_cdyaso_model as 
	   select versionartcd, 
	          year label = 'Jahr' as Jahr, 
	          quarcd, 
	          alterv10cd, 
	          sexcd, 
	          herkunftcd, 
		        round(sum(pop), &round_pop.) as pop_model
	      from pop_model_vari
	      group by versionartcd,
	               year,
	               quarcd,
	               alterv10cd,
	               sexcd,
	               herkunftcd
	      order by versionartcd,
	               year,
	               quarcd,
	               alterv10cd,
	               sexcd,
	               herkunftcd;
	quit;


