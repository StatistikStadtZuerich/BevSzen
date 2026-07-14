/*population on open data*/
	%let path_pop_od = https://data.stadt-zuerich.ch/dataset/bev_szenarien_od3440/download/BEV344OD3440.csv;


/*population: model output*/

	/*main path*/
		%let model_path = \\szh.loc\ssz\strengvertraulich\Szen\BevSzen\&current_year.\2_Daten\5_Outputs\;

	/*scenarios*/
		%let model_lower = &model_path.lower\population_future.csv;
		%let model_middle = &model_path.middle\population_future.csv;
		%let model_upper = &model_path.upper\population_future.csv;

	/*birth versions*/
		%let model_birth_lower = &model_path.middle_birth_lower\population_future.csv;
		%let model_birth_upper = &model_path.middle_birth_upper\population_future.csv;




