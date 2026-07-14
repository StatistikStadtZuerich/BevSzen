/*population by cdyaso from dwh*/

	/*c: scenario/version*/
	/*d: district*/
	/*y: year*/
	/*a: age (ten year age classes)*/
	/*s: sex*/
	/*o: origin*/

	/*only the three scenarios (not the birth versions)*/
	/*why? on open data only the scenarios and not the birth versions*/

	/*here also with past data*/
	/*why? on open data also the past data is available*/

	proc sql;
	   create table pop_cdyaso_dwh_scen1 as 
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
		        sum(anzbestwir) as pop_sum
	      from pop_dwh
	      where (publjahr = &current_year.) and (strip(versionartcd) in ('1', '2', '3'))
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


/*round the same way as the open data had been rounded*/
	/*(to avoid comparing apples and oranges)*/
		data pop_cdyaso_dwh_scen;
			set pop_cdyaso_dwh_scen1;
			if strip(basisszenariencd)='1' then pop_dwh = pop_sum;
			else pop_dwh = round(sum(pop_sum), &round_od.);
		run;


/*housekeeping*/
	proc datasets noprint;
		delete pop_cdyaso_dwh_scen1;
	run;


