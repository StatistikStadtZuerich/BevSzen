/*formats*/
	proc format;
		/*age code as in dwh*/
		value age_
			0 - 9 = "0"
			10 - 19 = "10"
			20 - 29 = "20"
			30 - 39 = "30"
			40 - 49 = "40"
			50 - 59 = "50"
			60 - 69 = "60"
			70 - 79 = "70"
			80 - 89 = "80"
			90 - 99 = "90"
			100 - high = "100";
		value age_od_
			1 = "0"
			2 = "10"
			3 = "20"
			4 = "30"
			5 = "40"
			6 = "50"
			7 = "60"
			8 = "70"
			9 = "80"
			10 = "90"
			11 = "100";
		value $sex_
			"male" = "1"
			"female" = "2";
		value $origin_
			"Swiss" = "1"
			"foreign" = "2";
		value $district_
			"Kreis 1" = "010"
			"Wollishofen" = "021"
			"Leimbach" = "023"
			"Enge" = "024"
			"Alt-Wiedikon" = "031"
			"Friesenberg" = "033"
			"Sihlfeld" = "034"
			"Werd" = "041"
			"Langstrasse" = "042"
			"Hard" = "044"
			"Gewerbeschule" = "051"
			"Escher Wyss" = "052"
			"Unterstrass" = "061"
			"Oberstrass" = "063"
			"Fluntern" = "071"
			"Hottingen" = "072"
			"Hirslanden" = "073"
			"Witikon" = "074"
			"Seefeld" = "081"
			"Muehlebach" = "082"
			"Weinegg" = "083"
			"Albisrieden" = "091"
			"Altstetten" = "092"
			"Hoengg" = "101"
			"Wipkingen" = "102"
			"Affoltern" = "111"
			"Oerlikon" = "115"
			"Seebach" = "119"
			"Saatlen" = "121"
			"Schwamendingen-Mitte" = "122"
			"Hirzenbach" = "123";
	run;
	


