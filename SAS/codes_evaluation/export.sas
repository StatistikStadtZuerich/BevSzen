/*export: population*/
	proc export data=pop
		outfile="&pop_path."
		dbms=csv
		replace;
	run;

/*export: processes*/
	proc export data=prc
		outfile="&prc_path."
		dbms=csv
		replace;
	run;

