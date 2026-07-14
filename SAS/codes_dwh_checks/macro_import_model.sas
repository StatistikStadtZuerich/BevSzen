/*Macro: import model data*/
	%macro import_model_data(path=, scen_cd=, out=);

		/*import*/
			proc import datafile="&path."
				out=import_temp dbms=csv replace; 
				getnames=yes; 
				guessingrows=max;
			run;

		/*scenario*/
			data &out.;
				length VersionArtCd $200;
				set import_temp;
				VersionArtCd = &scen_cd.;
			run;

		/*houskeeping*/
			proc datasets noprint;
				delete import_temp;
			run;
			

	%mend import_model_data;





