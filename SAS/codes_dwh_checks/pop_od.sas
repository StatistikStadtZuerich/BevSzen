/*import population data from open data*/
filename tmpcsv TEMP;
 
proc http
	out=tmpcsv url="&path_pop_od." method="get"
	proxyhost="proxy-dev.szh.loc" proxyport=8080 proxyusername="SSZ-DevProxy"
	proxypassword="{SAS002}5E38660B076C1B15015683F82557DD7F248D452A24F543F111D2106436DF8463";
run;
 

proc import datafile=tmpcsv dbms=csv out=pop_od replace;
	guessingrows=max;
	delimiter=','; 
run;
 
filename tmpcsv CLEAR;



/*checks*/
	proc tabulate
		data=pop_od;
		class versionartsort /	order=unformatted missing;
		class versionartlang /	order=unformatted missing;
		table versionartsort*versionartlang, n;
	run;

	proc tabulate
		data=pop_od;
		class AlterV10sort /	order=unformatted missing;
		class AlterV10lang /	order=unformatted missing;
		table AlterV10sort*AlterV10lang, n;
	run;

	proc tabulate
		data=pop_od;
		class sexsort /	order=unformatted missing;
		class sexlang /	order=unformatted missing;
		table sexsort*sexlang, n;
	run;

	proc tabulate
		data=pop_od;
		class herkunftsort / order=unformatted missing;
		class herkunftlang / order=unformatted missing;
		table herkunftsort*herkunftlang, n;
	run;

	proc tabulate
		data=pop_od;
		class quarsort / order=unformatted missing;
		class quarlang / order=unformatted missing;
		table quarsort*quarlang, n;
	run;

