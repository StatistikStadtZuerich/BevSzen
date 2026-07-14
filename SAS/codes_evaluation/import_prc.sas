/*import processes (prc)*/

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


data prc (label='Ausgewählte Daten von SZE Bevölkerungsszenario BEV');
	sysecho "Extrahieren von Daten aus der Information Map";
	length 
		PublJahr 8
		Jahr 8
		BasisSzenarienCd $ 255
		BasisSzenarienLang $ 255
		VersionArtCd $ 200
		VersionArtLang $ 200
		SexCd $ 255
		HerkunftCd $ 255
		QuarCd $ 255
		QuarLang $ 255
		AnzGebuWir 8
		AnzSterWir 8
		AnzZuzuWir 8
		AnzWezuWir 8
		AnzEinbWir 8
		;
	label 
		PublJahr="Publikationsjahr"  /* Publikationsjahr */
		Jahr=" "  /* Jahr */
		BasisSzenarienCd="Basiswerte oder Szenarienwerte (Code)"  /* Basiswerte oder Szenarienwerte (Code) */
		BasisSzenarienLang="Basiswerte oder Szenarienwerte (lang)"  /* Basiswerte oder Szenarienwerte (lang) */
		VersionArtCd="Art des Szenarios (Code)"  /* Art des Szenarios (Code) */
		VersionArtLang="Art des Szenarios (lang)"  /* Art des Szenarios (lang) */
		SexCd="Geschlecht (Code)"  /* Geschlecht (Code) */
		HerkunftCd="Herkunft (Code)"  /* Herkunft (Code) */
		QuarCd="Stadtquartier (Code)"  /* Stadtquartier (Code) */
		QuarLang="Stadtquartier (lang)"  /* Stadtquartier (lang) */
		AnzGebuWir="Anzahl Geburten wirtschaftlich"  /* Anzahl Geburten wirtschaftlich */
		AnzSterWir="Anzahl Sterbefälle wirtschaftlich"  /* Anzahl Sterbefälle wirtschaftlich */
		AnzZuzuWir="Anzahl Zuzüge wirtschaftlich"  /* Anzahl Zuzüge wirtschaftlich */
		AnzWezuWir="Anzahl Wegzüge wirtschaftlich"  /* Anzahl Wegzüge wirtschaftlich */
		AnzEinbWir="Anzahl Einbürgerungen wirtschaftlich"  /* Anzahl Einbürgerungen wirtschaftlich */
		;
	
	set _egimle."SZE Bevölkerungsszenario BEV"n 
		(keep=
			PublJahr
			Jahr
			BasisSzenarienCd
			BasisSzenarienLang
			VersionArtCd
			VersionArtLang
			SexCd
			HerkunftCd
			QuarCd
			QuarLang
			AnzGebuWir
			AnzSterWir
			AnzZuzuWir
			AnzWezuWir
			AnzEinbWir 
		 /* default EXPCOLUMNLEN is 32 */ 
		 filter=((BasisSzenarienCd = "2")) 
		 
		 );
	
run;

/* clear the libname when complete */
libname _egimle clear;



/*Checks*/
	proc tabulate
		data=prc;
		var anzgebuwir anzsterwir anzzuzuwir anzwezuwir anzeinbwir;
		class publjahr /	order=unformatted missing;
		table publjahr*sum, anzgebuwir anzsterwir anzzuzuwir anzwezuwir anzeinbwir;
	run;

	proc tabulate
		data=work.prc;
		var anzgebuwir anzsterwir anzzuzuwir anzwezuwir anzeinbwir;
		class versionartcd /	order=unformatted missing;
		class versionartlang /	order=unformatted missing;
		class publjahr /	order=formatted descending missing;
		table publjahr, versionartcd*versionartlang*sum,
			anzgebuwir anzsterwir anzzuzuwir anzwezuwir anzeinbwir;
	run;

