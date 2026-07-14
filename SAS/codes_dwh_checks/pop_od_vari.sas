/*open data: variables as in dwh*/
	data pop_od_vari; 
		length VersionArtCd $200 AlterV10Cd $200 SexCd $200 HerkunftCd $200 QuarCd $200;
		set pop_od;
		VersionArtCd = put(VersionArtSort, z1.); 
		AlterV10Cd = put(AlterV10Sort, age_od_.);
		SexCd = put(SexSort, z1.); 
		HerkunftCd = put(HerkunftSort, z1.); 
		QuarCd = put(QuarSort, z3.); 
	run;


/*check: variables ok?*/
	proc tabulate
		data=pop_od_vari;
		class VersionArtCd /	order=unformatted missing;
		class VersionArtSort /	order=unformatted missing;
		table VersionArtCd*VersionArtSort, n;
	run;

	proc tabulate
		data=pop_od_vari;
		class alterv10sort /	order=unformatted missing;
		class alterv10cd /	order=unformatted missing;
		class alterv10lang /	order=unformatted missing;
		table alterv10sort*alterv10cd*alterv10lang, n;
	run;

	proc tabulate
		data=pop_od_vari;
		class sexcd /	order=unformatted missing;
		class sexsort /	order=unformatted missing;
		class sexlang /	order=unformatted missing;
		table sexsort*sexcd*sexlang, n;
	run;

	proc tabulate
		data=pop_od_vari;
		class herkunftcd /	order=unformatted missing;
		class herkunftsort /	order=unformatted missing;
		class herkunftlang /	order=unformatted missing;
		table herkunftsort*herkunftcd*herkunftlang, n;
	run;

	proc tabulate
		data=pop_od_vari;
		class QuarCd /	order=unformatted missing;
		class QuarSort /	order=unformatted missing;
		class QuarLang /	order=unformatted missing;
		table QuarSort*QuarCd*QuarLang, n;
	run;

