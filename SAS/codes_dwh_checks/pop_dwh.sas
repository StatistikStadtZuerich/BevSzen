/*import populaton data from dwh*/



/* assign the library using the INFOMAPS library engine */

sysecho "Zuweisen der Bibliothek für den Zugriff auf Information Map";
%macro SetDisplayOrder;
%if %sysevalf(&sysver>=9.4) %then displayorder=folder;
%mend SetDisplayOrder;

libname _egimle sasioime
	 mappath="/InformationMaps/Szenarien/SZE Bevölkerungsszenario BEV"
	 aggregate=yes
	 metacredentials=no
	 PRESERVE_MAP_NAMES=YES
	 %SetDisplayOrder;
/* NOTE: when using this LIBNAME statement in a batch environment,  */
/* you might need to add metadata host and credentials information. */


data pop_dwh (label='Ausgewählte Daten von SZE Bevölkerungsszenario BEV');
	sysecho "Extrahieren von Daten aus der Information Map";
	length 
		HerkunftLang $ 255
		HerkunftCd $ 255
		SexLang $ 255
		SexCd $ 255
		AlterV10Lang $ 255
		AlterV10Cd $ 255
		AnzBestWir 8
		AnzGebuWir 8
		AnzZuzuWir 8
		AnzWezuWir 8
		AnzEinbWir 8
		AnzSterWir 8
		QuarLang $ 255
		QuarCd $ 255
		KreisLang $ 255
		KreisCd $ 255
		VersionArtCd $ 200
		VersionArtLang $ 200
		Jahr 8
		BasisSzenarienLang $ 255
		BasisSzenarienCd $ 255
		PublJahr 8
		;
	label 
		HerkunftLang="Herkunft (lang)"  /* Herkunft (lang) */
		HerkunftCd="Herkunft (Code)"  /* Herkunft (Code) */
		SexLang="Geschlecht (lang)"  /* Geschlecht (lang) */
		SexCd="Geschlecht (Code)"  /* Geschlecht (Code) */
		AlterV10lang="Alter in 10-Jahres-Klassen"  /* Vollendetes Alter (Num) */
		AlterV10Cd="Alter in 10-Jahres-Klassen (Code)"  /* Vollendetes Alter (Code) */
		AnzBestWir="wirtschaftliche Bevölkerung"  /* wirtschaftliche Bevölkerung */
		AnzGebuWir="Anzahl Geburten wirtschaftlich"  /* Anzahl Geburten wirtschaftlich */
		AnzZuzuWir="Anzahl Zuzüge wirtschaftlich"  /* Anzahl Zuzüge wirtschaftlich */
		AnzWezuWir="Anzahl Wegzüge wirtschaftlich"  /* Anzahl Wegzüge wirtschaftlich */
		AnzEinbWir="Anzahl Einbürgerungen wirtschaftlich"  /* Anzahl Einbürgerungen wirtschaftlich */
		AnzSterWir="Anzahl Sterbefälle wirtschaftlich"  /* Anzahl Sterbefälle wirtschaftlich */
		QuarLang="Stadtquartier (lang)"  /* Stadtquartier (lang) */
		QuarCd="Stadtquartier (Code)"  /* Stadtquartier (Code) */
		KreisLang="Stadtkreis (lang)"  /* Stadtkreis (lang) */
		KreisCd="Stadtkreis (Code)"  /* Stadtkreis (Code) */
		VersionArtCd="Art des Szenarios (Code)"  /* Art des Szenarios (Code) */
		VersionArtLang="Art des Szenarios (lang)"  /* Art des Szenarios (lang) */
		Jahr=" "  /* Jahr */
		BasisSzenarienLang="Basiswerte oder Szenarienwerte (lang)"  /* Basiswerte oder Szenarienwerte (lang) */
		BasisSzenarienCd="Basiswerte oder Szenarienwerte (Code)"  /* Basiswerte oder Szenarienwerte (Code) */
		;
	
	set _egimle."SZE Bevölkerungsszenario BEV"n 
		(keep=
			HerkunftLang
			HerkunftCd
			SexLang
			SexCd
			AlterV10lang
			AlterV10Cd
			AnzBestWir
			AnzGebuWir
			AnzZuzuWir
			AnzWezuWir
			AnzEinbWir
			AnzSterWir
			QuarLang
			QuarCd
			KreisLang
			KreisCd
			VersionArtCd
			VersionArtLang
			Jahr
			BasisSzenarienLang
			BasisSzenarienCd 
			PublJahr
		 /* default EXPCOLUMNLEN is 32 */ 
		  filter=(PublJahr >= &previous_year.) 
		 
		 );
	
run;

/* clear the libname when complete */
libname _egimle clear;


/*Checks*/
	title1 "scenarios, publication years";
	proc tabulate
		data=pop_dwh;
		class versionartcd /	order=unformatted missing;
		class versionartlang /	order=unformatted missing;
		class publjahr /	order=unformatted missing;
		table versionartcd*versionartlang*n, publjahr;
	run;
	title;

	title1 "age classes (code and name variable)";
	proc tabulate
		data=pop_dwh;
		class alterv10lang /	order=unformatted missing;
		class alterv10cd /	order=unformatted missing;
		class publjahr /	order=unformatted missing;
		table alterv10cd*alterv10lang*n, publjahr;
	run;
	title;

	title1 "sex (code and name variable)";
	proc tabulate
		data=pop_dwh;
		class SexLang /	order=unformatted missing;
		class SexCd /	order=unformatted missing;
		class publjahr /	order=unformatted missing;
		table SexCd*SexLang*n, publjahr;
	run;
	title;

	title1 "district (code and name variable)";
	proc tabulate
		data=pop_dwh;
		class QuarLang /	order=unformatted missing;
		class QuarCd /	order=unformatted missing;
		class publjahr /	order=unformatted missing;
		table QuarCd*QuarLang*n, publjahr;
	run;
	title;
