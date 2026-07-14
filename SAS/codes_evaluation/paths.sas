/*output paths (population and processes, i.e. births, deaths, imigration, emigration, naturalization)*/
	%let out_path = \\szh.loc\ssz\strengvertraulich\Szen\BevSzen\&this_year.\2_Daten\1_Input\;
	%let pop_path = &out_path.dwh_scen_pop.csv;
	%let prc_path = &out_path.dwh_scen_prc.csv;

	%put &pop_path.;
	%put &prc_path.;



