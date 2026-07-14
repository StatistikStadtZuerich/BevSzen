/*Check if the current year exists in the data*/

	%macro check_year_exists(data=, curr_year=, vari=PublJahr);

		/*maximum year in the data*/
			proc sql noprint;
			     select max(&vari.) into :max_year trimmed
			     from &data.;
			quit;

		/*output note (is the current year in the dwh data?)*/
			%if &curr_year.=&max_year. %then %do;
				%put EVERYTHING OK (year &curr_year. exists in variable &vari. of dataset &data.);
			%end;
		    %else %do;
		        %put NOT OK (year &curr_year. does NOT exist in variable &vari. of dataset &data.);
		    %end;

	%mend;

