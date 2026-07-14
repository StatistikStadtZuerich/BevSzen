/*publication year: transpose*/

/*suggestion: not too detailed*/
/*otherwise lost in 'noise'*/
/*keep most important variables: year, version, district, age*/

/*aggregate the population*/
	proc sql;
	   create table pop_agg as 
	   select publjahr, 
	          jahr, 
	          versionartcd, 
	          versionartlang, 
	          quarcd, 
	          quarlang, 
	          alterv10cd, 
	          alterv10lang, 
	            (sum(anzbestwir)) as AnzBestWir
	      from pop_dwh
	      group by publjahr,
	               jahr,
	               versionartcd,
	               versionartlang,
	               quarcd,
	               quarlang,
	               alterv10cd,
	               alterv10lang
	      order by versionartcd,
	               jahr,
	               quarcd,
	               alterv10cd,
					publjahr;
	quit;


/*current and previous year*/
/*why?two strings ('current', 'previous') are a bit easier to handle than two macro variable (&current_year., &previous_year.)*/
	data curr_prev;
		length publication $200;
		set pop_agg;
		if publjahr = &current_year. then publication = 'current';
		else if publjahr = &previous_year. then publication = 'previous';
		drop PublJahr;
	run;

				 
/*transpose*/
	proc transpose data=curr_prev out=pop_transposed (drop = _NAME_);
		by versionartcd versionartlang jahr quarcd quarlang 
			alterv10cd alterv10lang;
		id publication;
		var AnzBestWir;
	run;


/*replace missing by zero*/
	proc stdize data=pop_transposed out=pop_transp reponly missing=0;
	run;

        
/*housekeeping*/
	proc datasets noprint;
		delete pop_agg curr_prev pop_transposed;
	run;
