/*apartment (apa) data from dwh*/


/* assign the library using the INFOMAPS library engine */

sysecho "Zuweisen der Bibliothek für den Zugriff auf Information Map";
%macro SetDisplayOrder;
%if %sysevalf(&sysver>=9.4) %then displayorder=folder;
%mend SetDisplayOrder;

libname _egimle sasioime
	 mappath="/InformationMaps/Szenarien/SZE Bevölkerungsszenario WOHNEN"
	 aggregate=yes
	 metacredentials=no
	 PRESERVE_MAP_NAMES=YES
	 %SetDisplayOrder;
/* NOTE: when using this LIBNAME statement in a batch environment,  */
/* you might need to add metadata host and credentials information. */


data apa_dwh (label='Ausgewählte Daten von SZE Bevölkerungsszenario WOHNEN');
	sysecho "Extrahieren von Daten aus der Information Map";
	length 
		Jahr 8
		PublJahr 8
		BasisSzenarienCd $ 255
		WohnungsflProPers 8
		PersProWhg 8
		VersionArtCd $ 200
		QuarCd $ 255
		QuarLang $ 255
		KreisCd $ 255
		KreisLang $ 255
		EigentumGrundstkCd $ 255
		EigentumGrundstkLang $ 255
		BruttoGeschFlaeche 8
		;
	label 
		Jahr=" "  /* Jahr */
		PublJahr="Publikationsjahr"  /* Publikationsjahr */
		BasisSzenarienCd="Basiswerte oder Szenarienwerte (Code)"  /* Basiswerte oder Szenarienwerte (Code) */
		WohnungsflProPers="Wohnungsfläche in m2 pro Person"  /* Wohnungsfläche in m2 pro Person */
		PersProWhg="Personenzahl pro Wohnung"  /* Personenzahl pro Wohnung */
		VersionArtCd="Art des Szenarios (Code)"  /* Art des Szenarios (Code) */
		QuarCd="Stadtquartier (Code)"  /* Stadtquartier (Code) */
		QuarLang="Stadtquartier (lang)"  /* Stadtquartier (lang) */
		KreisCd="Stadtkreis (Code)"  /* Stadtkreis (Code) */
		KreisLang="Stadtkreis (lang)"  /* Stadtkreis (lang) */
		EigentumGrundstkCd="Eigentümerart des Grundstücks (Code)"  /* Eigentümerart des Grundstücks (Code) */
		EigentumGrundstkLang="Eigentümerart des Grundstücks (lang)"  /* Eigentümerart des Grundstücks (lang) */
		BruttoGeschFlaeche="Bruttogeschossfläche"  /* Bruttogeschossfläche */
		;
	
	set _egimle."SZE Bevölkerungsszenario WOHNEN"n 
		(keep=
			Jahr
			PublJahr
			BasisSzenarienCd
			WohnungsflProPers
			PersProWhg
			VersionArtCd
			QuarCd
			QuarLang
			KreisCd
			KreisLang
			EigentumGrundstkCd
			EigentumGrundstkLang
			BruttoGeschFlaeche 
		 /* default EXPCOLUMNLEN is 32 */ 
		  filter=(PublJahr >= &previous_year.) 
		 
		 );
	
run;

/* clear the libname when complete */
libname _egimle clear;