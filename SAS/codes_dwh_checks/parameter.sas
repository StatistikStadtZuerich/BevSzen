/*current year (latest data the dwh)*/
	/*why not automatically? latest year in dwh does not have to be this year (e.g. when check is executed in May)*/
	%let current_year = 2026;

/*previous year (second latest data in the dwh)*/
	/*why not automatically? does not have to be the year before 'current_year' (e.g. when popscen every two years)*/
	%let previous_year = 2025;

/*round (for population checks)*/
	%let round_pop = 0.01;

/*round (used in open data)*/
	%let round_od = 10;

/*round: percent*/
	%let round_percent = 0.1;

/*threshold: people (compare previous and current)*/
	%let thres_people = 20;

/*threshold: percent (compare previous and current)*/
	%let thres_percent = 20;

