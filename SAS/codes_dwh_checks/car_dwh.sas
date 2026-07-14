/*capacity and reserves (car) from dwh*/


/* assign the library using the INFOMAPS library engine */

sysecho "Zuweisen der Bibliothek für den Zugriff auf Information Map";
%macro SetDisplayOrder;
%if %sysevalf(&sysver>=9.4) %then displayorder=folder;
%mend SetDisplayOrder;

libname _egimle sasioime
	 mappath="/InformationMaps/Szenarien/SZE Bevölkerungsszenario KAREB"
	 aggregate=yes
	 metacredentials=no
	 PRESERVE_MAP_NAMES=YES
	 %SetDisplayOrder;
/* NOTE: when using this LIBNAME statement in a batch environment,  */
/* you might need to add metadata host and credentials information. */


data car_dwh (label='Ausgewählte Daten von SZE Bevölkerungsszenario KAREB');
	sysecho "Extrahieren von Daten aus der Information Map";
	length 
		PublJahr 8
		QuarCd $ 255
		QuarLang $ 255
		KreisCd $ 255
		KreisLang $ 255
		FilterJahr 8
		EigentumGrundstkCd $ 255
		EigentumGrundstkLang $ 255
		BereichCd $ 200
		BereichLang $ 200
		ArealCd $ 200
		ArealLang $ 200
		WohnanteilCd $ 200
		WohnanteilLang $ 200
		BruttoGeschFlaeche 8
		;
	label 
		PublJahr="Publikationsjahr"  /* Publikationsjahr */
		QuarCd="Stadtquartier (Code)"  /* Stadtquartier (Code) */
		QuarLang="Stadtquartier (lang)"  /* Stadtquartier (lang) */
		KreisCd="Stadtkreis (Code)"  /* Stadtkreis (Code) */
		KreisLang="Stadtkreis (lang)"  /* Stadtkreis (lang) */
		FilterJahr="Jahresfilter"  /* Jahresfilter */
		EigentumGrundstkCd="Eigentümerart des Grundstücks (Code)"  /* Eigentümerart des Grundstücks (Code) */
		EigentumGrundstkLang="Eigentümerart des Grundstücks (lang)"  /* Eigentümerart des Grundstücks (lang) */
		BereichCd="Bereich (Code)"  /* Bereich (Code) */
		BereichLang="Bereich (lang)"  /* Bereich (lang) */
		ArealCd="Arealüberbauungen (Code)"  /* Arealüberbauungen (Code) */
		ArealLang="Arealüberbauungen (lang)"  /* Arealüberbauungen (lang) */
		WohnanteilCd="Wohnanteil (Code)"  /* Wohnanteil (Code) */
		WohnanteilLang="Wohnanteil (lang)"  /* Wohnanteil (lang) */
		BruttoGeschFlaeche="Bruttogeschossfläche"  /* Bruttogeschossfläche */
		;
	
	set _egimle."SZE Bevölkerungsszenario KAREB"n 
		(keep=
			PublJahr
			QuarCd
			QuarLang
			KreisCd
			KreisLang
			FilterJahr
			EigentumGrundstkCd
			EigentumGrundstkLang
			BereichCd
			BereichLang
			ArealCd
			ArealLang
			WohnanteilCd
			WohnanteilLang
			BruttoGeschFlaeche 
		 /* default EXPCOLUMNLEN is 32 */ 
		   filter=(PublJahr >= &previous_year.) 
		 
		 );
	
run;

/* clear the libname when complete */
libname _egimle clear;
