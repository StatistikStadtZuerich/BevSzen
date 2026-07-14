/*check: tables*/
	proc freq data=pop_dwh;
		tables _character_ / missing;
	run;

	proc means data=pop_dwh n mean std min max nmiss;
	run;

	proc freq data=car_dwh;
		tables _character_ / missing;
	run;

	proc means data=car_dwh n mean std min max nmiss;
	run;

	proc freq data=apa_dwh;
		tables _character_ / missing;
	run;

	proc means data=apa_dwh n mean std min max nmiss;
	run;

/*there are negative values for bruttogeschossfläche*/
/*these are correct for kareb model but are set to zero for the population projections*/
	data neg_areas;
		set car_dwh;
		where BruttoGeschFlaeche <= 0;
	run;