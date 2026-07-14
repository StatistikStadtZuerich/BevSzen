/*population: open data*/

	/*c: scenario/version*/
	/*d: district*/
	/*y: year*/
	/*a: age (ten year age classes)*/
	/*s: sex*/
	/*o: origin*/

	proc sql;
	   create table pop_cdyaso_od as 
	   select versionartcd, 
	          stichtagdatjahr label = 'Jahr' as Jahr, 
	          quarcd, 
	          alterv10cd, 
	          sexcd, 
	          herkunftcd, 
		        round(sum(anzbestwir), &round_pop.) as pop_od
	      from pop_od_vari
	      group by versionartcd,
	               stichtagdatjahr,
	               quarcd,
	               alterv10cd,
	               sexcd,
	               herkunftcd
	      order by versionartcd,
	               stichtagdatjahr,
	               quarcd,
	               alterv10cd,
	               herkunftcd;
	quit;

