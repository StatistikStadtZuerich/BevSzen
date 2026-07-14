/*Left join*/
	proc sql;
	   create table temp_dwh_model_join as 
	   select t1.*,  
	          t2.pop_model
	      from pop_cdyaso_dwh t1
	           left join pop_cdyaso_model t2 on (t1.versionartcd = t2.versionartcd) and 
					(t1.jahr = t2.jahr) and (t1.quarcd = t2.quarcd) and (t1.alterv10cd = t2.alterv10cd) and
				    (t1.sexcd = t2.sexcd) and (t1.herkunftcd = t2.herkunftcd)
	      order by t1.versionartcd,
	               t1.jahr,
	               t1.quarcd,
	               t1.alterv10cd,
	               t1.sexcd,
	               t1.herkunftcd;
	quit;

	%check_zeilenzahl(dat1=pop_cdyaso_dwh, dat2=temp_dwh_model_join);



/*absolute difference: dwh vs. model*/
	data dwh_model_diff;
		set temp_dwh_model_join;
		abs_diff = abs(pop_dwh - pop_model);
	run;


/*check: absolute difference always zero?*/
	title1 "absolute difference between dwh and model: should be zero (for all assesed scenarios/versions)";
	proc tabulate
		data=dwh_model_diff;
		var abs_diff;
		class versionartcd /	order=unformatted missing;
		class versionartlang /	order=unformatted missing;
		table versionartcd*versionartlang*sum, abs_diff;
	run;
	title;


/*housekeeping*/
	proc datasets noprint;
		delete temp_dwh_model_join;
	run;


