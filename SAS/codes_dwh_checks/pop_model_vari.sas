/*model: variables as in dwh*/
	data pop_model_vari; 
		length AlterV10Cd $200 SexCd $200 HerkunftCd $200 QuarCd $200;
		set pop_model;
		AlterV10Cd = put(age, age_.);
		SexCd = put(strip(sex), $sex_.);
		HerkunftCd = put(strip(origin), $origin_.);
		QuarCd = put(strip(district), $district_.);
	run;

/*check: variables ok?*/
	proc tabulate
		data=pop_model_vari;
		class sexcd /	order=unformatted missing;
		class sex /	order=unformatted missing;
		table sexcd*sex, n;
	run;

	proc tabulate
		data=pop_model_vari;
		class HerkunftCd /	order=unformatted missing;
		class origin /	order=unformatted missing;
		table HerkunftCd*origin, n;
	run;

	proc tabulate
		data=pop_model_vari;
		class QuarCd /	order=unformatted missing;
		class district / order=unformatted missing;
		table QuarCd*district, n;
	run;

	proc tabulate
		data=pop_model_vari;
		class alterv10cd /	order=unformatted missing;
		class age /	order=unformatted missing;
		table alterv10cd*age, n;
	run;

