/*Check missings*/
	/*desired outcome: 0 missing observations for each variable*/

/*population in dwh*/
	%count_missing_numeric(data=pop_dwh, out=miss_pop_dwh_num);
	%count_missing_char(data=pop_dwh, out=miss_pop_dwh_char);

/*capacity and reserves in dwh*/
	%count_missing_numeric(data=car_dwh, out=miss_car_dwh_num);
	%count_missing_char(data=car_dwh, out=miss_car_dwh_char);

/*apartments in dwh*/
	%count_missing_numeric(data=apa_dwh, out=miss_apa_dwh_num);
	%count_missing_char(data=apa_dwh, out=miss_apa_dwh_char);

