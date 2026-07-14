/*population: dwh*/

	/*future values only (since from the model only future values)*/

	/*c: scenario/version*/
	/*d: district*/
	/*y: year*/
	/*a: age (ten year age classes)*/
	/*s: sex*/
	/*o: origin*/

	proc sql;
	   create table pop_cdyaso_dwh as 
	   select publjahr, 
	          basisszenariencd, 
	          basisszenarienlang, 
	          versionartcd, 
	          versionartlang, 
	          jahr, 
	          alterv10cd, 
	          alterv10lang, 
	          sexcd, 
	          sexlang, 
	          herkunftcd, 
	          herkunftlang, 
	          quarcd, 
	          quarlang, 
		            round(sum(anzbestwir), &round_pop.) as pop_dwh
	      from pop_dwh
	      where (publjahr = &current_year.) and (strip(basisszenariencd) = '2')
	      group by publjahr,
	               basisszenariencd,
	               basisszenarienlang,
	               versionartcd,
	               versionartlang,
	               jahr,
	               alterv10cd,
	               alterv10lang,
	               sexcd,
	               sexlang,
	               herkunftcd,
	               herkunftlang,
	               quarcd,
	               quarlang
	      order by versionartcd,
	               jahr,
	               alterv10cd,
	               sexcd,
	               herkunftcd,
	               quarcd;
	quit;




