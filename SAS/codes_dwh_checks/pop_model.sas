/*import population from model data*/

	/*why not in another macro? scenarios and versions can change (e.g. birth versione, or BZO revision)*/
	/*codes: see proc tabulate after dwh import*/

	%import_model_data(path=&model_lower., 			scen_cd='1', out=imp_1);
	%import_model_data(path=&model_middle., 		scen_cd='2', out=imp_2);
	%import_model_data(path=&model_upper., 			scen_cd='3', out=imp_3);
	%import_model_data(path=&model_birth_lower., 	scen_cd='4', out=imp_4);
	%import_model_data(path=&model_birth_upper., 	scen_cd='5', out=imp_5);


/*append to one dataset*/
	data pop_model;
		set imp_:;
	run;

/*housekeeping*/
	proc datasets noprint;
		delete imp_:;
	run;








