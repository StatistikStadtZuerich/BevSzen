/*compare: current to previous*/
	data compare;
		length diff_percent 8.0;
		set pop_transp;
		if previous = 0 then diff_percent = .;
		else diff_percent = round((current - previous) / previous * 100, &round_percent.);
	run;

	title1 "total (rows)";
	proc tabulate
		data=compare;
		table n;
	run;
	title;


/*differences: not zero and not missing, and more people than the threshold*/
	%put &thres_people.;

	proc sql;
	   create table differ as 
	   select versionartcd, 
	          versionartlang, 
	          jahr, 
	          quarcd, 
	          quarlang, 
	          alterv10cd, 
	          alterv10lang, 
	          previous, 
	          current, 
	          diff_percent
	      from compare
	      where (diff_percent ^= 0) and (diff_percent ^= .) and (previous >= &thres_people.)
	      order by diff_percent;
	quit;

	title1 "differences (rows)";
	proc tabulate
		data=differ;
		table n;
	run;
	title;


/*plot: edcf*/
	proc univariate data=differ;
	    var diff_percent;
	    cdfplot diff_percent;
	run;

/*plot: difference vs. previous*/
	proc sgplot data=differ;
	    scatter x=previous y=diff_percent / transparency=0.4;
	    refline 0 / axis=y lineattrs=(color=red pattern=shortdash thickness=2);
	    xaxis label="previous";
	    yaxis label="difference in percent";
	run;


/*'extreme' differences*/
	data extreme;
		set differ;
		where (diff_percent >= &thres_percent.) or 
			(diff_percent <= (-1) * &thres_percent.);
	run;


