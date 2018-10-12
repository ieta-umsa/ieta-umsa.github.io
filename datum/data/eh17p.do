* Stata-commands to read and label eh17p.dat

capture infile /*
*/long folio     byte nro       str1 depto     byte area      byte s02a_02   byte s02a_03   byte s02a_04a /*
*/byte s02a_04b  int s02a_04c  byte s02a_05   int s02a_06a  int s02a_06b  int s02a_06c  int s02a_07_1/*
*/int s02a_07_2 int s02a_07_3 byte s02a_08   byte s02a_10   byte s03a_01a  long s03a_01b  str43 s03a_01b2/*
*/str6 s03a_01b2_codstr4 s03a_01c  byte s03a_02   str80 s03a_02e  int s03a_03   byte s03a_03a  byte s03a_04  /*
*/int s03a_04npiocbyte s04a_01a  byte s04a_01b  str54 s04a_01e  byte s04a_02a  byte s04a_02b  str46 s04a_02e /*
*/byte s04a_03a  byte s04a_03b  byte s04a_03c  byte s04a_03d  byte s04a_03e  byte s04a_03f  byte s04a_03g /*
*/byte s04a_04a  byte s04a_04b  str49 s04a_04e  long s04a_05a  long s04a_05b  long s04a_05c  int s04a_05d /*
*/long s04a_05e  byte s04a_06a  byte s04a_06b  byte s04a_06c  byte s04a_06d  byte s04a_06e  byte s04a_06f /*
*/byte s04a_06g  byte s04a_07a  byte s04a_07b  byte s04a_07c  byte s04a_07d  byte s04a_07e  byte s04a_07f /*
*/byte s04a_07g  byte s04a_08a  byte s04a_08b  byte s04a_09   byte s04b_11a  byte s04b_11b  byte s04b_12  /*
*/byte s04b_13   byte s04b_14a  int s04b_14b  byte s04b_15   str37 s04b_15e  byte s04b_16   str50 s04b_16e /*
*/byte s04b_17   str39 s04b_17e  byte s04b_18   str50 s04b_18e  byte s04b_19   byte s04b_20a1 byte s04b_20a2/*
*/byte s04b_20b  byte s04b_21a  byte s04b_21b  byte s04c_22   byte s04c_23   byte s04d_24   byte s04d_25  /*
*/byte s04d_26   byte s04d_27a  byte s04d_27b  byte s04e_28a  byte s04e_28b  byte s04e_29a  byte s04e_29b /*
*/byte s04e_30a  byte s04e_30b  str54 s04e_30c  byte s04e_31a  byte s04e_31b  byte s04e_31c  byte s04e_31d /*
*/byte s04e_31e  byte s04e_31f  byte s04e_32a  byte s04e_32b  byte s04e_33a  byte s04e_33b  byte s04f_34  /*
*/byte s04f_35a  byte s04f_35b  byte s04f_35c  str52 s04f_35e  byte s05a_01   byte s05a_01a  byte s05a_02a /*
*/byte s05a_02b  byte s05a_03a  byte s05a_03b  str83 s05a_04   str5 s05a_04_codbyte s05a_05   byte s05a_05a /*
*/byte s05a_06a  byte s05a_06b  byte s05a_07a  byte s05a_07b  byte s05a_08   byte s05a_09   byte s05b_10  /*
*/byte s05b_11   byte s05b_11a  byte s05c_13   byte s05c_14a  byte s05c_14b  byte s05c_15a  byte s05c_15b /*
*/byte s05c_16aa byte s05c_16ab str46 s05c_16e  byte s05c_17a  byte s05c_17b  byte s05c_17c  byte s05c_17d /*
*/byte s05c_17e  byte s05c_17f  byte s05c_17g  byte s05c_17h  byte s05c_17i  byte s05c_17j  byte s05c_17k /*
*/byte s05c_17l  byte s06a_01   byte s06a_02   byte s06a_03   byte s06a_04   byte s06a_05   byte s06a_06aa/*
*/byte s06a_06ab byte s06a_06ac str47 s06a_06e  byte s06a_07   byte s06a_08a  byte s06a_08b  byte s06a_09  /*
*/str44 s06a_09e  byte s06a_10   str83 s06a_10e  str87 s06b_11a  str5 s06b_11a_codstr92 s06b_11b  str91 s06b_12a /*
*/str5 s06b_12a_codstr90 s06b_12b  str60 s06b_13   byte s06b_14a  byte s06b_14b  byte s06b_15a  byte s06b_15b /*
*/byte s06b_16   byte s06b_17   byte s06b_18   byte s06b_19   byte s06b_19a  byte s06b_20   str50 s06b_20e /*
*/int s06b_21   byte s06b_21a  byte s06b_21b  double s06b_22   byte s06b_23aa byte s06b_23ab byte s06b_23a1/*
*/byte s06b_23a2 double s06c_25a  byte s06c_25b  int s06c_26a  long s06c_26b  double s06c_27aa byte s06c_27ab/*
*/int s06c_27ba byte s06c_27bb byte s06c_28a  byte s06c_28a1 byte s06c_28b  byte s06c_29a  byte s06c_29b /*
*/byte s06c_30a  byte s06c_30a1 int s06c_30a2 byte s06c_30b  byte s06c_30b1 int s06c_30b2 byte s06c_30c /*
*/byte s06c_30c1 int s06c_30c2 byte s06c_30d  byte s06c_30d1 int s06c_30d2 byte s06c_30e  byte s06c_30e1/*
*/int s06c_30e2 long s06d_31a  byte s06d_31b  double s06d_32aa byte s06d_32ab long s06d_32ba byte s06d_32bb/*
*/double s06d_32ca byte s06d_32cb double s06d_32da byte s06d_32db double s06d_32ea byte s06d_32eb double s06d_32fa/*
*/byte s06d_32fb double s06d_32ga byte s06d_32gb double s06d_33a  byte s06d_33b  byte s06e_34a  byte s06e_34b /*
*/str50 s06e_34e  byte s06e_35   byte s06e_36   byte s06e_37   byte s06e_38a  long s06e_38a1 byte s06e_38a2/*
*/byte s06e_38b  long s06e_38b1 byte s06e_38b2 byte s06e_38c  long s06e_38c1 byte s06e_38c2 byte s06e_38d /*
*/long s06e_38d1 byte s06e_38d2 byte s06e_39   str83 s06f_40a  str5 s06f_40a_codstr85 s06f_40b  byte s06f_41  /*
*/byte s06f_42   int s06f_43   byte s06f_43a  double s06f_44   byte s06f_45a  byte s06f_45b  byte s06f_45a1/*
*/byte s06f_45a2 int s06g_47a  byte s06g_47b  byte s06g_48a  int s06g_48a1 byte s06g_48b  int s06g_48b1/*
*/byte s06g_48c  int s06g_48c1 long s06g_49a  byte s06g_49b  double s06g_50aa byte s06g_50ab int s06g_50ba/*
*/byte s06g_50bb int s06g_50ca byte s06g_50cb int s06g_50da byte s06g_50db double s06g_50ea byte s06g_50eb/*
*/int s06g_50fa byte s06g_50fb int s06g_50ga byte s06g_50gb double s06g_51a  byte s06g_51b  byte s06h_52  /*
*/byte s06h_53   byte s06h_54   str44 s06h_54f  byte s06h_55   str80 s06h_55e  byte s06h_56   str83 s06h_56e /*
*/byte s06h_57   byte s06h_58a  byte s06h_58b  int s07a_01a  int s07a_01b  int s07a_01c  int s07a_01d /*
*/byte s07a_01e  int s07a_01e0 byte s07a_01e1 str53 s07a_01e1ebyte s07a_01e2 str50 s07a_01e2eint s07a_02a /*
*/long s07a_02b  long s07a_02c  str53 s07a_02ce long s07a_03a  long s07a_03b  long s07a_03c  long s07a_04a /*
*/int s07a_04b  long s07a_04c  int s07b_05aa byte s07b_05ab int s07b_05ba byte s07b_05bb byte s07c_06  /*
*/byte s07c_07   long s07c_08a  byte s07c_08b  str13 s07c_08e  byte s07c_09   str85 s07c_09e  int s07c_10  /*
*/str2 estrato   str17 upm       int factor    byte cob_op    byte cob_op2digbyte caeb_op   byte cobersalud/*
*/byte hnv_ult_aniobyte quienatenpartobyte dondeatenpartobyte f         byte aoesc     byte niv_ed    byte niv_ed_g1/*
*/byte cmasi     byte pet       byte ocupado   byte cesante   byte aspirante byte desocupadobyte pea      /*
*/byte temporal  byte permanentebyte pei       byte condact   double phrs      double shrs      double tothrs   /*
*/byte totper    double yprilab   double yseclab   double ylab      double ynolab    double yper      double yhog     /*
*/double yhogpc    double z         double zext      byte p0        double p1        double p2        byte pext0    /*
*/double pext1     double pext2     using eh17p.dat

capture label variable folio "Folio" 
capture label variable nro "Numero de persona" 
capture label variable depto "Departamento" 
capture label variable area "Area Urbana Rural" 
capture label variable s02a_02 "�Es hombre o mujer?" 
capture label value s02a_02 s02a_02 
capture label variable s02a_03 "�Cu�ntos a�os cumplidos tiene?" 
capture label variable s02a_04a "�Cu�l es la fecha de su nacimiento?(Dia)" 
capture label variable s02a_04b "�Cu�l es la fecha de su nacimiento?(Mes)" 
capture label variable s02a_04c "�Cu�l es la fecha de su nacimiento?(A�o)" 
capture label variable s02a_05 "�Qu� relaci�n o parentesco tiene �con el jefe o jefa del hogar?" 
capture label value s02a_05 s02a_05 
capture label variable s02a_06a "INDAGUE QUIEN ES EL ESPOSO/A O COMPA�ERO/A, de" 
capture label variable s02a_06b "INDAGUE QUIEN ES EL PADRE/PADRASTRO, de" 
capture label variable s02a_06c "INDAGUE QUIEN ES LA MADRE/MADRASTRA ,de" 
capture label variable s02a_07_1 "�Qu� Idiomas habla, incluidos los de las naciones y pueblos ind�gena originarios" 
capture label value s02a_07_1 s02a_07_1 
capture label variable s02a_07_2 "�Qu� Idiomas habla, incluidos los de las naciones y pueblos ind�gena originarios" 
capture label value s02a_07_2 s02a_07_2 
capture label variable s02a_07_3 "�Qu� Idiomas habla, incluidos los de las naciones y pueblos ind�gena originarios" 
capture label value s02a_07_3 s02a_07_3 
capture label variable s02a_08 "�Cu�l es el idioma o lengua en el que aprendi� a hablar en su ni�ez?" 
capture label value s02a_08 s02a_08 
capture label variable s02a_10 "�Cu�l es su estado civil o conyugal actual?" 
capture label value s02a_10 s02a_10 
capture label variable s03a_01a "�D�nde viv�a hace 5 a�os (2012)?" 
capture label value s03a_01a s03a_01a 
capture label variable s03a_01b "Especifique el Departamento - Provincia - Municipio �d�nde viv�a hace 5 a�os (20" 
capture label value s03a_01b s03a_01b 
capture label variable s03a_01b2 "Especif�que la CIUDAD O COMUNIDAD" 
capture label variable s03a_01b2_cod "" 
capture label variable s03a_01c "Especifique el pa�s donde viv�a hace 5 a�os" 
capture label variable s03a_02 "�Cu�l fue la raz�n por la que dej� ese lugar?" 
capture label value s03a_02 s03a_02 
capture label variable s03a_02e "OTRA RAZON?(Especifique)" 
capture label variable s03a_03 "�Desde que a�o vive aqu�?" 
capture label variable s03a_03a "�Desde que mes vive aqu�?" 
capture label variable s03a_04 "Como boliviana o boliviano �A que naci�n o pueblo ind�gena originario o campesin" 
capture label value s03a_04 s03a_04 
capture label variable s03a_04npioc "�A cu�l naci�n o pueblo ind�gena originario o campesino o afro boliviano pertene" 
capture label value s03a_04npioc s03a_04npioc 
capture label variable s04a_01a "� Tiene alguna enfermedad cr�nica que haya sido diagnosticada 1�" 
capture label value s04a_01a s04a_01a 
capture label variable s04a_01b "� Tiene alguna enfermedad cr�nica que haya sido diagnosticada 2�" 
capture label value s04a_01b s04a_01b 
capture label variable s04a_01e "� Tiene alguna enfermedad cr�nica que haya sido diagnosticada como: Otra enferme" 
capture label variable s04a_02a "En los �ltimos 12 meses, �se ha enfermado  1�" 
capture label value s04a_02a s04a_02a 
capture label variable s04a_02b "En los �ltimos 12 meses, �se ha enfermado  2�" 
capture label value s04a_02b s04a_02b 
capture label variable s04a_02e "En los �ltimos 12 meses, �se ha enfermado  de otra enfermedad?: (Especifique)" 
capture label variable s04a_03a "En los �ltimos 12 meses, por problemas de salud, �acudi� o se atendi� en�A. Caja" 
capture label value s04a_03a s04a_03a 
capture label variable s04a_03b "En los �ltimos 12 meses, por problemas de salud, �acudi� o se atendi� en�B. Esta" 
capture label value s04a_03b s04a_03b 
capture label variable s04a_03c "En los �ltimos 12 meses, por problemas de salud, �acudi� o se atendi� en�C. Esta" 
capture label value s04a_03c s04a_03c 
capture label variable s04a_03d "En los �ltimos 12 meses, por problemas de salud, �acudi� o se atendi� en�D. Su d" 
capture label value s04a_03d s04a_03d 
capture label variable s04a_03e "En los �ltimos 12 meses, por problemas de salud, �acudi� o se atendi� en�E. Su d" 
capture label value s04a_03e s04a_03e 
capture label variable s04a_03f "En los �ltimos 12 meses, por problemas de salud, �acudi� o se atendi� en�F. M�di" 
capture label value s04a_03f s04a_03f 
capture label variable s04a_03g "En los �ltimos 12 meses, por problemas de salud, �acudi� o se atendi� en�G. La f" 
capture label value s04a_03g s04a_03g 
capture label variable s04a_04a "�Est� registrada/o o afiliada/o a alguno de los siguientes seguros de salud? 1�" 
capture label value s04a_04a s04a_04a 
capture label variable s04a_04b "�Est� registrada/o o afiliada/o a alguno de los siguientes seguros de salud? 2�" 
capture label value s04a_04b s04a_04b 
capture label variable s04a_04e "�Est� registrada/o o afiliada/o a alguno de los siguientes seguros de salud? Otr" 
capture label variable s04a_05a "�En los �ltimos 12 meses, cu�nto tuvo que pagar el hogar por los cuidados de sal" 
capture label variable s04a_05b "�En los �ltimos 12 meses, cu�nto tuvo que pagar el hogar por los cuidados de sal" 
capture label variable s04a_05c "�En los �ltimos 12 meses, cu�nto tuvo que pagar el hogar por los cuidados de sal" 
capture label variable s04a_05d "�En los �ltimos 12 meses, cu�nto tuvo que pagar el hogar por los cuidados de sal" 
capture label variable s04a_05e "�En los �ltimos 12 meses, cu�nto tuvo que pagar el hogar por los cuidados de sal" 
capture label variable s04a_06a "�Tiene�alguna dificultad permanente que limite o impida ver a�n con los anteojos" 
capture label value s04a_06a s04a_06a 
capture label variable s04a_06b "�Tiene�alguna dificultad permanente que limite o impida oir a�n cuando utiliza a" 
capture label value s04a_06b s04a_06b 
capture label variable s04a_06c "�Tiene�alguna dificultad permanente que limite o impida hablar comunicarse o con" 
capture label value s04a_06c s04a_06c 
capture label variable s04a_06d "�Tiene�alguna dificultad permanente que limite o impida caminar o subir gradas?" 
capture label value s04a_06d s04a_06d 
capture label variable s04a_06e "�Tiene�alguna dificultad permanente que limite o impida aprender, recordar o con" 
capture label value s04a_06e s04a_06e 
capture label variable s04a_06f "�Tiene�alguna dificultad permanente que limite o impida vestirse, ba�arse o come" 
capture label value s04a_06f s04a_06f 
capture label variable s04a_06g "�Tiene�alguna enfermedad mental que limite o afecte sus actividades diarias?Si t" 
capture label value s04a_06g s04a_06g 
capture label variable s04a_07a "�El origen de esta dificultad es de (A. ver a�n con los anteojos o lentes puesto" 
capture label value s04a_07a s04a_07a 
capture label variable s04a_07b " �El origen de esta dificultad es de (B. oir a�n cuando utiliza audifonos?)" 
capture label value s04a_07b s04a_07b 
capture label variable s04a_07c " �El origen de esta dificultad es de (C. hablar comunicarse o conversar?)" 
capture label value s04a_07c s04a_07c 
capture label variable s04a_07d " �El origen de esta dificultad es de (D.caminar o subir gradas?)" 
capture label value s04a_07d s04a_07d 
capture label variable s04a_07e " �El origen de esta dificultad es de (E. aprender, recordar o concentrarse?)" 
capture label value s04a_07e s04a_07e 
capture label variable s04a_07f " �El origen de esta dificultad es de (F. vestirse, ba�arse o comer?)" 
capture label value s04a_07f s04a_07f 
capture label variable s04a_07g " �El origen de esta dificultad es de (G. enfermedad mental que afecte sus activi" 
capture label value s04a_07g s04a_07g 
capture label variable s04a_08a "�Tiene Carnet de Persona con Discapacidad otorgado por el Ministerio de Salud?" 
capture label value s04a_08a s04a_08a 
capture label variable s04a_08b "�Cual es el grado de discapacidad que presenta?" 
capture label value s04a_08b s04a_08b 
capture label variable s04a_09 "�En los �ltimos 12 meses, recibi� la Renta Solidaria de 1,000 Bs.?" 
capture label value s04a_09 s04a_09 
capture label variable s04b_11a "�Est� o estuvo alguna vez embarazada?" 
capture label value s04b_11a s04b_11a 
capture label variable s04b_11b "�Cu�ntas veces? (incluyendo el actual embarazo)" 
capture label variable s04b_12 "�Cu�ntas hijas e hijos nacidos vivos ha tenido? (aunque despu�s hayan muerto)" 
capture label variable s04b_13 "De ellos �Cu�ntos est�n vivos actualmente?" 
capture label variable s04b_14a "�En qu� Mes naci� su �ltima hija o hijo nacido vivo? (aunque despu�s haya muerto" 
capture label variable s04b_14b "�En qu� A�o naci� su �ltima hija o hijo nacido vivo? (aunque despu�s haya muerto" 
capture label variable s04b_15 "�Qui�n atendi� su �ltimo parto?" 
capture label value s04b_15 s04b_15 
capture label variable s04b_15e "�Qui�n atendi� su �ltimo parto? Otra persona (Especifique)" 
capture label variable s04b_16 "�Por qu� no se hizo atender por m�dico/enfermera?" 
capture label value s04b_16 s04b_16 
capture label variable s04b_16e "�Por qu� no se hizo atender por m�dico/enfermera? Otro (Especifique)" 
capture label variable s04b_17 "�D�nde fue atendido el parto de su �ltimo/a hijo/a nacido vivo?" 
capture label value s04b_17 s04b_17 
capture label variable s04b_17e "�D�nde fue atendido el parto de su �ltimo/a hijo/a nacido vivo? Otro (Especifiqu" 
capture label variable s04b_18 "�La atenci�n de su �ltimo parto estuvo cubierta por alguno de los siguientes seg" 
capture label value s04b_18 s04b_18 
capture label variable s04b_18e "�La atenci�n de su �ltimo parto estuvo cubierta por alguno de los siguientes seg" 
capture label variable s04b_19 "En su �ltimo embarazo, �se ha inscrito al Bono Juana Azurduy (BJA)?" 
capture label value s04b_19 s04b_19 
capture label variable s04b_20a1 "En los �ltimos 12 meses, �cobr� usted el Bono Juana Azurduy por: Controles prena" 
capture label value s04b_20a1 s04b_20a1 
capture label variable s04b_20a2 " En los �ltimos 12 meses, �cobr� usted el Bono Juana Azurduy por: Controles pren" 
capture label variable s04b_20b "En los �ltimos 12 meses, �cobr� usted el Bono Juana Azurduy por: El parto y prim" 
capture label value s04b_20b s04b_20b 
capture label variable s04b_21a "En su �ltimo embarazo, �recibi� el Subsidio Universal Prenatal del Estado?" 
capture label value s04b_21a s04b_21a 
capture label variable s04b_21b "En los �ltimos 12 meses, � cuantos meses recibio el Subsidio Universal Prenatal" 
capture label variable s04c_22 "Durante este a�o, �asiste o asisti� a alg�n centro infantil que no sea pre kinde" 
capture label value s04c_22 s04c_22 
capture label variable s04c_23 "�A que tipo de establecimiento asiste o asisti� ?" 
capture label value s04c_23 s04c_23 
capture label variable s04d_24 "�En las �ltimas dos semanas,  tuvo diarrea?" 
capture label value s04d_24 s04d_24 
capture label variable s04d_25 "�En las �ltimas dos semanas  tuvo tos, resfr�o o gripe, bronquitis o pulmon�a?" 
capture label value s04d_25 s04d_25 
capture label variable s04d_26 "�Alguna vez inscribieron a  al Bono Juana Azurduy?" 
capture label value s04d_26 s04d_26 
capture label variable s04d_27a "En los �ltimos 12 meses, �cobr� usted el Bono Juana Azurduy por los controles in" 
capture label value s04d_27a s04d_27a 
capture label variable s04d_27b "En los �ltimos 12 meses, �cobr� usted el Bono Juana Azurduy por los controles in" 
capture label variable s04e_28a "�En el trabajo o en sus tareas cotidianas requiere/necesita realizar una activid" 
capture label value s04e_28a s04e_28a 
capture label variable s04e_28b "�En una semana t�pica �cuantos d�as realiza dicha actividad?" 
capture label variable s04e_29a "�Para trasladarse al trabajo/realizar alguna actividad, camina o usa una bicicle" 
capture label value s04e_29a s04e_29a 
capture label variable s04e_29b "�En una semana t�pica �cuantos d�as realiza dicha actividad?" 
capture label variable s04e_30a "�En su tiempo libre �practica�alg�n deporte o actividad f�sica en al menos 30 mi" 
capture label value s04e_30a s04e_30a 
capture label variable s04e_30b "�En una semana t�pica �cuantos d�as realiza dicha actividad?" 
capture label variable s04e_30c "�Qu� actividad f�sica o deporte practica?" 
capture label variable s04e_31a "�Qu� tipos de espacios o instalaciones utiliza�para hacer deporte o actividad f�" 
capture label value s04e_31a s04e_31a 
capture label variable s04e_31b "�Qu� tipos de espacios o instalaciones utiliza�para hacer deporte o actividad f�" 
capture label value s04e_31b s04e_31b 
capture label variable s04e_31c "�Qu� tipos de espacios o instalaciones utiliza�para hacer deporte o actividad f�" 
capture label value s04e_31c s04e_31c 
capture label variable s04e_31d "�Qu� tipos de espacios o instalaciones utiliza�para hacer deporte o actividad f�" 
capture label value s04e_31d s04e_31d 
capture label variable s04e_31e "�Qu� tipos de espacios o instalaciones utiliza�para hacer deporte o actividad f�" 
capture label value s04e_31e s04e_31e 
capture label variable s04e_31f "�Qu� tipos de espacios o instalaciones utiliza�para hacer deporte o actividad f�" 
capture label value s04e_31f s04e_31f 
capture label variable s04e_32a "�Durante los ultimos 12 meses��ha fumado cigarrillos?" 
capture label value s04e_32a s04e_32a 
capture label variable s04e_32b "�Con que frecuencia ha fumado cigarrillos�?" 
capture label value s04e_32b s04e_32b 
capture label variable s04e_33a "�Durante los �ltimos 12 meses� ha consumido bebidas alcoh�licas?" 
capture label value s04e_33a s04e_33a 
capture label variable s04e_33b "�Con que frecuencia ha consumido bebidas alcoholicas�?" 
capture label value s04e_33b s04e_33b 
capture label variable s04f_34 "�Qu� tan segura(o) se siente��caminando sola(o) cerca de su vivienda cuando es d" 
capture label value s04f_34 s04f_34 
capture label variable s04f_35a "�En los �ltimos 12 meses,��ha sido victima de: 1�" 
capture label value s04f_35a s04f_35a 
capture label variable s04f_35b "�En los �ltimos 12 meses,��ha sido victima de: 2�" 
capture label value s04f_35b s04f_35b 
capture label variable s04f_35c "�En los �ltimos 12 meses,��ha sido victima de: 3�" 
capture label value s04f_35c s04f_35c 
capture label variable s04f_35e "�En los ultimos 12 meses,��ha sido victima de:(Especifique)" 
capture label variable s05a_01 "�Sabe leer y escribir?" 
capture label value s05a_01 s05a_01 
capture label variable s05a_01a "�Es��capaz de sumar o multiplicar n�meros, ya sea en papel o mentalmente?" 
capture label value s05a_01a s05a_01a 
capture label variable s05a_02a "�Cu�l fue el NIVEL Y CURSO m�s alto de instrucci�n que aprob�?" 
capture label value s05a_02a s05a_02a 
capture label variable s05a_02b "Ingrese el Curso o Grado" 
capture label variable s05a_03a "Para ingresar a ese nivel �Cu�l fue el NIVEL Y CURSO anterior de educaci�n que a" 
capture label value s05a_03a s05a_03a 
capture label variable s05a_03b "Ingrese el Curso o Grado" 
capture label variable s05a_04 "�Cu�l es su t�tulo profesional o t�cnico obtenido?" 
capture label variable s05a_04_cod "" 
capture label variable s05a_05 "Durante este a�o, �se inscribi� o matricul� en alg�n curso o grado de educaci�n" 
capture label value s05a_05 s05a_05 
capture label variable s05a_05a "�Por qu� raz�n no se inscribi�/matricul�? (Indague la raz�n m�s importante)" 
capture label value s05a_05a s05a_05a 
capture label variable s05a_06a "�A qu� NIVEL Y CURSO de educaci�n escolar, alternativa,  superior o postgrado se" 
capture label value s05a_06a s05a_06a 
capture label variable s05a_06b "Ingrese el Curso o Grado" 
capture label variable s05a_07a "En esta gesti�n, �recibi� Desayuno/Almuerzo o merienda escolar?" 
capture label value s05a_07a s05a_07a 
capture label variable s05a_07b "��Cuantos meses aproximadamente?" 
capture label variable s05a_08 "�Recibi� el Bono Juancito Pinto el a�o pasado (2016)?" 
capture label value s05a_08 s05a_08 
capture label variable s05a_09 "El establecimiento en el que se matricul� es:" 
capture label value s05a_09 s05a_09 
capture label variable s05b_10 "Actualmente, �asiste al nivel y curso al que se matricul� este 2017?" 
capture label value s05b_10 s05b_10 
capture label variable s05b_11 "�Por qu� raz�n no asiste actualmente? (Indague la raz�n m�s importante)" 
capture label value s05b_11 s05b_11 
capture label variable s05b_11a "Actualmente, �Se encuentra en per�odo de vacaci�n, descanso pedag�gico o receso?" 
capture label value s05b_11a s05b_11a 
capture label variable s05c_13 "�En los �ltimos 3 meses,��ha utilizado tel�fono celular?" 
capture label value s05c_13 s05c_13 
capture label variable s05c_14a "�En los �ltimos 3 meses,��ha utilizado computadora (de escritorio, laptop, table" 
capture label value s05c_14a s05c_14a 
capture label variable s05c_14b "�Con qu� frecuencia?" 
capture label value s05c_14b s05c_14b 
capture label variable s05c_15a "�En los �ltimos 3 meses,��ha utilizado internet en cualquier lugar?" 
capture label value s05c_15a s05c_15a 
capture label variable s05c_15b "�Con que frecuencia?" 
capture label value s05c_15b s05c_15b 
capture label variable s05c_16aa "�D�nde�� ha utilizado internet en los �ltimos 3 meses?� 1�" 
capture label value s05c_16aa s05c_16aa 
capture label variable s05c_16ab "�D�nde�� ha utilizado internet en los �ltimos 3 meses?� 2�" 
capture label value s05c_16ab s05c_16ab 
capture label variable s05c_16e "�D�nde ha utilizado usted internet en los �ltimos 3 meses? (Especifique)" 
capture label variable s05c_17a "�Para cu�l de las siguientes actividades�us� internet en los �ltimos 3 meses (de" 
capture label value s05c_17a s05c_17a 
capture label variable s05c_17b "�Para cu�l de las siguientes actividades�us� internet en los �ltimos 3 meses (de" 
capture label value s05c_17b s05c_17b 
capture label variable s05c_17c "�Para cu�l de las siguientes actividades�us� internet en los �ltimos 3 meses (de" 
capture label value s05c_17c s05c_17c 
capture label variable s05c_17d "�Para cu�l de las siguientes actividades�us� internet en los �ltimos 3 meses (de" 
capture label value s05c_17d s05c_17d 
capture label variable s05c_17e "�Para cu�l de las siguientes actividades�us� internet en los �ltimos 3 meses (de" 
capture label value s05c_17e s05c_17e 
capture label variable s05c_17f "�Para cu�l de las siguientes actividades�us� internet en los �ltimos 3 meses (de" 
capture label value s05c_17f s05c_17f 
capture label variable s05c_17g "�Para cu�l de las siguientes actividades�us� internet en los �ltimos 3 meses (de" 
capture label value s05c_17g s05c_17g 
capture label variable s05c_17h "�Para cu�l de las siguientes actividades�us� internet en los �ltimos 3 meses (de" 
capture label value s05c_17h s05c_17h 
capture label variable s05c_17i "�Para cu�l de las siguientes actividades�us� internet en los �ltimos 3 meses (de" 
capture label value s05c_17i s05c_17i 
capture label variable s05c_17j "�Para cu�l de las siguientes actividades�us� internet en los �ltimos 3 meses (de" 
capture label value s05c_17j s05c_17j 
capture label variable s05c_17k "�Para cu�l de las siguientes actividades�us� internet en los �ltimos 3 meses (de" 
capture label value s05c_17k s05c_17k 
capture label variable s05c_17l "�Para cu�l de las siguientes actividades�us� internet en los �ltimos 3 meses (de" 
capture label value s05c_17l s05c_17l 
capture label variable s06a_01 "Durante la semana pasada, �trabaj� al menos una hora?" 
capture label value s06a_01 s06a_01 
capture label variable s06a_02 "Durante la semana pasada, dedic� al menos una hora a:" 
capture label value s06a_02 s06a_02 
capture label variable s06a_03 "�La semana pasada, tuvo alg�n empleo, negocio o empresa propia en la cual no tra" 
capture label value s06a_03 s06a_03 
capture label variable s06a_04 "�La semana pasada quer�a usted trabajar y estaba disponible para hacerlo?" 
capture label value s06a_04 s06a_04 
capture label variable s06a_05 "Durante las �ltimas cuatro semanas �busc� trabajo o hizo gestiones para establec" 
capture label value s06a_05 s06a_05 
capture label variable s06a_06aa "�Qu� gestiones hizo para buscar trabajo o establecer un negocio propio?�1�" 
capture label value s06a_06aa s06a_06aa 
capture label variable s06a_06ab "�Qu� gestiones hizo para buscar trabajo o establecer un negocio propio?�2�" 
capture label value s06a_06ab s06a_06ab 
capture label variable s06a_06ac "�Qu� gestiones hizo para buscar trabajo o establecer un negocio propio?�3�" 
capture label value s06a_06ac s06a_06ac 
capture label variable s06a_06e "�Qu� gestiones hizo para buscar trabajo o establecer un negocio propio?(Especifi" 
capture label variable s06a_07 "�Ha trabajado alguna vez anteriormente?" 
capture label value s06a_07 s06a_07 
capture label variable s06a_08a "�Hace cu�nto tiempo que no trabaja o que no ha realizado alguna actividad para g" 
capture label variable s06a_08b "�Hace cu�nto tiempo que no trabaja o que no ha realizado alguna actividad para g" 
capture label value s06a_08b s06a_08b 
capture label variable s06a_09 "Es usted:" 
capture label value s06a_09 s06a_09 
capture label variable s06a_09e "Es usted: (Especifique)" 
capture label variable s06a_10 "�Por qu� no busc� trabajo?" 
capture label value s06a_10 s06a_10 
capture label variable s06a_10e "�Por qu� no busc� trabajo? (Especifique)" 
capture label variable s06b_11a "Durante la semana pasada, �cu�l fue su ocupaci�n principal?" 
capture label variable s06b_11a_cod "" 
capture label variable s06b_11b "�Qu� tareas realiza o qu� funciones desempe�a?" 
capture label variable s06b_12a "�Cu�l es la actividad econ�mica principal del establecimiento, negocio, instituc" 
capture label variable s06b_12a_cod "" 
capture label variable s06b_12b "�Principalmente, qu� produce, comercializa o servicio presta, el establecimiento" 
capture label variable s06b_13 " �Cu�l es el nombre de la empresa, instituci�n, negocio o lugar donde trabaja?" 
capture label variable s06b_14a "�Hace cu�nto tiempo trabaja en esta empresa, instituci�n, negocio o lugar? 1�" 
capture label variable s06b_14b "�Hace cu�nto tiempo trabaja en esta empresa, instituci�n, negocio o lugar? 2�" 
capture label value s06b_14b s06b_14b 
capture label variable s06b_15a "�Hace cu�nto tiempo desempe�a esta ocupaci�n en esa empresa, instituci�n, negoci" 
capture label variable s06b_15b "�Hace cu�nto tiempo desempe�a esta ocupaci�n en esa empresa, instituci�n, negoci" 
capture label value s06b_15b s06b_15b 
capture label variable s06b_16 "En esta ocupaci�n usted trabaja como:" 
capture label value s06b_16 s06b_16 
capture label variable s06b_17 "En este trabajo, usted�" 
capture label value s06b_17 s06b_17 
capture label variable s06b_18 "La administraci�n de la empresa, instituci�n, negocio o lugar donde trabaja es.." 
capture label value s06b_18 s06b_18 
capture label variable s06b_19 "La empresa, negocio, taller o establecimiento donde trabaja, �cuenta con NIT (N�" 
capture label value s06b_19 s06b_19 
capture label variable s06b_19a "En esta empresa, negocio, taller o establecimiento, donde trabaja:" 
capture label value s06b_19a s06b_19a 
capture label variable s06b_20 "�D�nde realiza sus labores?" 
capture label value s06b_20 s06b_20 
capture label variable s06b_20e "�D�nde realiza sus labores? (Especifique)" 
capture label variable s06b_21 "�Cu�ntas personas trabajan en la empresa, instituci�n o lugar donde trabaja, inc" 
capture label variable s06b_21a "En todo el pa�s�Cu�ntas personas trabajan en esa empresa, institucion o lugar do" 
capture label value s06b_21a s06b_21a 
capture label variable s06b_21b "�En este trabajo en los �ltimos 12 meses recibi� capacitaci�n laboral?" 
capture label value s06b_21b s06b_21b 
capture label variable s06b_22 "�Cu�ntos d�as a la semana trabaja regularmente en su ocupaci�n?" 
capture label variable s06b_23aa "�Cu�ntas horas en promedio trabaja al d�a en su ocupaci�n? Horas:" 
capture label variable s06b_23ab "�Cu�ntas horas en promedio trabaja al d�a en su ocupaci�n? Minutos:" 
capture label variable s06b_23a1 "�Cu�ntas horas en promedio m�s o menos al d�a que las habituales trabaj� efectiv" 
capture label value s06b_23a1 s06b_23a1 
capture label variable s06b_23a2 "�Cu�ntas horas en promedio m�s o menos al d�a que las habituales trabaj� efectiv" 
capture label variable s06c_25a "�Cu�nto es su salario l�quido, excluyendo los descuentos de ley (AFP, IVA)? (Mon" 
capture label variable s06c_25b "�Cu�nto es su salario l�quido, excluyendo los descuentos de ley (AFP, IVA)? (Mon" 
capture label value s06c_25b s06c_25b 
capture label variable s06c_26a "Durante los �ltimos 12 meses, �recibi� usted pagos por: � Bono o prima de produc" 
capture label variable s06c_26b "Durante los �ltimos 12 meses, �recibi� usted pagos por: �Aguinaldo?�(Monto en Bs" 
capture label variable s06c_27aa "�Durante los �ltimos doce meses, recibi� usted pagos en efectivo por: Comisiones" 
capture label value s06c_27aa s06c_27aa 
capture label variable s06c_27ab "�Durante los �ltimos doce meses, recibi� usted pagos en efectivo por: Comisiones" 
capture label value s06c_27ab s06c_27ab 
capture label variable s06c_27ba "�Durante los �ltimos doce meses, recibi� usted pagos en efectivo por: Horas Extr" 
capture label value s06c_27ba s06c_27ba 
capture label variable s06c_27bb "�Durante los �ltimos doce meses, recibi� usted pagos en efectivo por: Horas Extr" 
capture label value s06c_27bb s06c_27bb 
capture label variable s06c_28a "Durante los �ltimos doce meses, �recibi� usted: Subsidio prenatal o de lactancia" 
capture label value s06c_28a s06c_28a 
capture label variable s06c_28a1 "Durante los �ltimos doce meses, �recibi� usted: Subsidio prenatal o de lactancia" 
capture label variable s06c_28b "Durante los �ltimos doce meses, �recibi� usted: Bono de natalidad?" 
capture label value s06c_28b s06c_28b 
capture label variable s06c_29a "�En su actual ocupaci�n Ud. Recibe o recibir� los siguientes beneficios: Vacacio" 
capture label value s06c_29a s06c_29a 
capture label variable s06c_29b "�En su actual ocupaci�n Ud. Recibe o recibir� los siguientes beneficios: Seguro" 
capture label value s06c_29b s06c_29b 
capture label variable s06c_30a "Adem�s de los ingresos recibidos en dinero por su trabajo, en los �ltimos doce m" 
capture label value s06c_30a s06c_30a 
capture label variable s06c_30a1 "Adem�s de los ingresos recibidos en dinero por su trabajo, en los �ltimos doce m" 
capture label value s06c_30a1 s06c_30a1 
capture label variable s06c_30a2 " Adem�s de los ingresos recibidos en dinero por su trabajo, en los �ltimos doce" 
capture label variable s06c_30b "Adem�s de los ingresos recibidos en dinero por su trabajo, en los �ltimos doce m" 
capture label value s06c_30b s06c_30b 
capture label variable s06c_30b1 "Adem�s de los ingresos recibidos en dinero por su trabajo, en los �ltimos doce m" 
capture label value s06c_30b1 s06c_30b1 
capture label variable s06c_30b2 "Adem�s de los ingresos recibidos en dinero por su trabajo, en los �ltimos doce m" 
capture label variable s06c_30c "Adem�s de los ingresos recibidos en dinero por su trabajo, en los �ltimos doce m" 
capture label value s06c_30c s06c_30c 
capture label variable s06c_30c1 "Adem�s de los ingresos recibidos en dinero por su trabajo, en los �ltimos doce m" 
capture label value s06c_30c1 s06c_30c1 
capture label variable s06c_30c2 " Adem�s de los ingresos recibidos en dinero por su trabajo, en los �ltimos doce" 
capture label variable s06c_30d "Adem�s de los ingresos recibidos en dinero por su trabajo, en los �ltimos doce m" 
capture label value s06c_30d s06c_30d 
capture label variable s06c_30d1 "Adem�s de los ingresos recibidos en dinero por su trabajo, en los �ltimos doce m" 
capture label value s06c_30d1 s06c_30d1 
capture label variable s06c_30d2 " Adem�s de los ingresos recibidos en dinero por su trabajo, en los �ltimos doce" 
capture label variable s06c_30e "Adem�s de los ingresos recibidos en dinero por su trabajo, en los �ltimos doce m" 
capture label value s06c_30e s06c_30e 
capture label variable s06c_30e1 "Adem�s de los ingresos recibidos en dinero por su trabajo, en los �ltimos doce m" 
capture label value s06c_30e1 s06c_30e1 
capture label variable s06c_30e2 "Adem�s de los ingresos recibidos en dinero por su trabajo, en los �ltimos doce m" 
capture label variable s06d_31a "�Cu�nto es su�ingreso total�en su ocupaci�n principal? 1�" 
capture label variable s06d_31b "�Cu�nto es su�ingreso total�en su ocupaci�n principal? 2�" 
capture label value s06d_31b s06d_31b 
capture label variable s06d_32aa "�Del ingreso total declarado en la pregunta anterior, cu�nto utiliza o guarda pa" 
capture label variable s06d_32ab "�Del ingreso total declarado en la pregunta anterior, cu�nto utiliza o guarda pa" 
capture label value s06d_32ab s06d_32ab 
capture label variable s06d_32ba "��Del�ingreso total declarado en la pregunta anterior, cu�nto utiliza o guarda p" 
capture label variable s06d_32bb "��Del�ingreso total declarado en la pregunta anterior, cu�nto utiliza o guarda p" 
capture label value s06d_32bb s06d_32bb 
capture label variable s06d_32ca "��Del ingreso total declarado en la pregunta anterior, cu�nto utiliza o guarda p" 
capture label variable s06d_32cb "��Del ingreso total declarado en la pregunta anterior, cu�nto utiliza o guarda p" 
capture label value s06d_32cb s06d_32cb 
capture label variable s06d_32da "�Del ingreso total declarado en la pregunta anterior, cu�nto utiliza o guarda pa" 
capture label variable s06d_32db "�Del ingreso total declarado en la pregunta anterior, cu�nto utiliza o guarda pa" 
capture label value s06d_32db s06d_32db 
capture label variable s06d_32ea "��Del ingreso total declarado en la pregunta anterior, cu�nto utiliza o guarda p" 
capture label variable s06d_32eb "��Del ingreso total declarado en la pregunta anterior, cu�nto utiliza o guarda p" 
capture label value s06d_32eb s06d_32eb 
capture label variable s06d_32fa "��Del ingreso total declarado en la pregunta anterior, cu�nto utiliza o guarda p" 
capture label variable s06d_32fb "��Del ingreso total declarado en la pregunta anterior, cu�nto utiliza o guarda p" 
capture label value s06d_32fb s06d_32fb 
capture label variable s06d_32ga "��Del ingreso total declarado en la pregunta anterior, cu�nto utiliza o guarda p" 
capture label variable s06d_32gb "��Del ingreso total declarado en la pregunta anterior, cu�nto utiliza o guarda p" 
capture label value s06d_32gb s06d_32gb 
capture label variable s06d_33a "Una vez descontadas todas sus obligaciones (sueldos, salarios, compra de materia" 
capture label variable s06d_33b "Una vez descontadas todas sus obligaciones (sueldos, salarios, compra de materia" 
capture label value s06d_33b s06d_33b 
capture label variable s06e_34a "�Cu�l fue el origen del capital necesario para iniciar este negocio, empresa o a" 
capture label value s06e_34a s06e_34a 
capture label variable s06e_34b "�Cu�l fue el origen del capital necesario para iniciar este negocio, empresa o a" 
capture label value s06e_34b s06e_34b 
capture label variable s06e_34e "�Cu�l fue el origen del capital necesario para iniciar este negocio, empresa o a" 
capture label variable s06e_35 "�Durante los �ltimos 12 meses, obtuvo alg�n pr�stamo de dinero para que su negoc" 
capture label value s06e_35 s06e_35 
capture label variable s06e_36 "Desde que inici� este negocio, empresa o actividad, sus ingresos��" 
capture label value s06e_36 s06e_36 
capture label variable s06e_37 "Considerando la situaci�n actual de su negocio, empresa o actividad, �Qu� expect" 
capture label value s06e_37 s06e_37 
capture label variable s06e_38a "�Su negocio, empresa o actividad dispone de �Maquinarias o equipos propios?" 
capture label value s06e_38a s06e_38a 
capture label variable s06e_38a1 "�Su negocio, empresa o actividad dispone de Maquinarias o equipos propios? �Cu�n" 
capture label variable s06e_38a2 "�Su negocio, empresa o actividad dispone de Maquinarias o equipos propios? �Cu�n" 
capture label value s06e_38a2 s06e_38a2 
capture label variable s06e_38b "�Su negocio, empresa o actividad dispone de �Mercader�a?" 
capture label value s06e_38b s06e_38b 
capture label variable s06e_38b1 "�Su negocio, empresa o actividad dispone de Mercader�a? �Cu�nto cree que cuesten" 
capture label variable s06e_38b2 "�Su negocio, empresa o actividad dispone de �Mercader�a? �Cu�nto cree que cueste" 
capture label value s06e_38b2 s06e_38b2 
capture label variable s06e_38c "�Su negocio, empresa o actividad dispone de Local (es) y/o establecimiento (s) p" 
capture label value s06e_38c s06e_38c 
capture label variable s06e_38c1 "�Su negocio, empresa o actividad dispone de Local y/o establecimiento(s) propio(" 
capture label variable s06e_38c2 "�Su negocio, empresa o actividad dispone de Local y/o establecimiento(s) propio(" 
capture label value s06e_38c2 s06e_38c2 
capture label variable s06e_38d "�Su negocio, empresa o actividad dispone de Veh�culo(s) propio(s) de uso exclusi" 
capture label value s06e_38d s06e_38d 
capture label variable s06e_38d1 "�Su negocio, empresa o actividad dispone de �Veh�culo(s) propio(s) de uso exclus" 
capture label variable s06e_38d2 "�Su negocio, empresa o actividad dispone de Veh�culo(s) propio(s) de uso exclusi" 
capture label value s06e_38d2 s06e_38d2 
capture label variable s06e_39 "Adem�s de la actividad mencionada anteriormente, �realiz� otro trabajo durante l" 
capture label value s06e_39 s06e_39 
capture label variable s06f_40a "�Cu�l es la actividad econ�mica principal de �ste otro establecimiento, negocio," 
capture label variable s06f_40a_cod "" 
capture label variable s06f_40b "�Principalmente, qu� produce, comercializa o servicio presta, el establecimiento" 
capture label variable s06f_41 "En esta ocupaci�n usted trabaja como:" 
capture label value s06f_41 s06f_41 
capture label variable s06f_42 "La administraci�n de la empresa, instituci�n, negocio o lugar donde trabaja es�" 
capture label value s06f_42 s06f_42 
capture label variable s06f_43 "�Cu�ntas personas trabajan en la empresa, instituci�n o lugar donde trabaja, inc" 
capture label variable s06f_43a "En todo el pa�s, �Cuantas personas trabajan en la empresa, instituci�n o lugar d" 
capture label value s06f_43a s06f_43a 
capture label variable s06f_44 "�Cu�ntos d�as trabaj� la semana anterior?" 
capture label variable s06f_45a "�Cu�ntas horas promedio al d�a trabaj� la semana anterior? Horas:" 
capture label variable s06f_45b "�Cu�ntas horas promedio al d�a trabaj� la semana anterior? Minutos:" 
capture label variable s06f_45a1 "�Cu�ntas horas en promedio m�s o menos al d�a que las habituales trabaj� efectiv" 
capture label value s06f_45a1 s06f_45a1 
capture label variable s06f_45a2 "�Cu�ntas horas en promedio m�s o menos al d�a que las habituales trabaj� efectiv" 
capture label variable s06g_47a "Cu�nto es su salario l�quido en �sta otra ocupaci�n, excluyendo los descuentos d" 
capture label variable s06g_47b "Cu�nto es su salario l�quido en �sta otra ocupaci�n, excluyendo los descuentos d" 
capture label value s06g_47b s06g_47b 
capture label variable s06g_48a "Durante los �ltimos doce meses, ha recibido:��Pago por horas extras, bono o prim" 
capture label value s06g_48a s06g_48a 
capture label variable s06g_48a1 "Durante los �ltimos doce meses, ha recibido: �Pago por horas extras, bono o prim" 
capture label variable s06g_48b "Durante los �ltimos doce meses, ha recibido: �Alimentos, transporte, vestimenta?" 
capture label value s06g_48b s06g_48b 
capture label variable s06g_48b1 "Durante los �ltimos doce meses, ha recibido: � Alimentos, transporte, vestimenta" 
capture label variable s06g_48c "Durante los �ltimos doce meses, ha recibido: Vivienda, alojamiento, otros?" 
capture label value s06g_48c s06g_48c 
capture label variable s06g_48c1 "Durante los �ltimos doce meses, ha recibido:� �Vivienda, alojamiento, otros?��(M" 
capture label variable s06g_49a "Cu�nto es su ingreso total en �sta otra ocupaci�n?�(Monto en Bs) 1�" 
capture label variable s06g_49b "Cu�nto es su ingreso total en �sta otra ocupaci�n?�(Monto en Bs) 2�" 
capture label value s06g_49b s06g_49b 
capture label variable s06g_50aa "Del ingreso total declarado en la pregunta anterior, cu�nto utiliza o guarda par" 
capture label variable s06g_50ab "Del ingreso total declarado en la pregunta anterior, cu�nto utiliza o guarda par" 
capture label value s06g_50ab s06g_50ab 
capture label variable s06g_50ba "Del�ingreso total declarado en la pregunta anterior, cu�nto utiliza o guarda par" 
capture label variable s06g_50bb "Del�ingreso total declarado en la pregunta anterior, cu�nto utiliza o guarda par" 
capture label value s06g_50bb s06g_50bb 
capture label variable s06g_50ca "�Del ingreso total declarado en la pregunta anterior, cu�nto utiliza o guarda pa" 
capture label variable s06g_50cb "�Del ingreso total declarado en la pregunta anterior, cu�nto utiliza o guarda pa" 
capture label value s06g_50cb s06g_50cb 
capture label variable s06g_50da "Del ingreso total declarado en la pregunta anterior, cu�nto utiliza o guarda par" 
capture label variable s06g_50db "Del ingreso total declarado en la pregunta anterior, cu�nto utiliza o guarda par" 
capture label value s06g_50db s06g_50db 
capture label variable s06g_50ea "Del ingreso total declarado en la pregunta anterior, cu�nto utiliza o guarda par" 
capture label variable s06g_50eb "Del ingreso total declarado en la pregunta anterior, cu�nto utiliza o guarda par" 
capture label value s06g_50eb s06g_50eb 
capture label variable s06g_50fa "�Del ingreso total declarado en la pregunta anterior, cu�nto utiliza o guarda pa" 
capture label variable s06g_50fb "�Del ingreso total declarado en la pregunta anterior, cu�nto utiliza o guarda pa" 
capture label value s06g_50fb s06g_50fb 
capture label variable s06g_50ga "�Del ingreso total declarado en la pregunta anterior, cu�nto utiliza o guarda pa" 
capture label variable s06g_50gb "�Del ingreso total declarado en la pregunta anterior, cu�nto utiliza o guarda pa" 
capture label value s06g_50gb s06g_50gb 
capture label variable s06g_51a "Una vez pagadas todas sus obligaciones (sueldos, salarios, compra de materiales," 
capture label variable s06g_51b "Una vez pagadas todas sus obligaciones (sueldos, salarios, compra de materiales," 
capture label value s06g_51b s06g_51b 
capture label variable s06h_52 "�Desea trabajar m�s horas de las que trabaj� la semana pasada?" 
capture label value s06h_52 s06h_52 
capture label variable s06h_53 "�Est� disponible para trabajar m�s horas?" 
capture label value s06h_53 s06h_53 
capture label variable s06h_54 "�Qu� gesti�n hizo para trabajar m�s horas?�" 
capture label value s06h_54 s06h_54 
capture label variable s06h_54f "�Qu� gesti�n hizo para trabajar m�s horas?Especifique." 
capture label variable s06h_55 "�Por qu� no trabaj� m�s horas a la semana?" 
capture label value s06h_55 s06h_55 
capture label variable s06h_55e "�Por qu� no trabaj� m�s horas a la semana? Otro Especifique" 
capture label variable s06h_56 "�Cu�l fue el motivo por el que dej� de trabajar en su �ltima ocupaci�n?Seleccion" 
capture label value s06h_56 s06h_56 
capture label variable s06h_56e "�Cu�l fue el motivo por el que dej� de trabajar en su �ltima ocupaci�n?Otro (Esp" 
capture label variable s06h_57 "�Ha trabajado alguna vez anteriormente?" 
capture label value s06h_57 s06h_57 
capture label variable s06h_58a "��Est� usted afiliado a: � Gremio, sindicato�o asociaci�n�laboral?" 
capture label value s06h_58a s06h_58a 
capture label variable s06h_58b "�Esta usted afiliado a: �AFP�(Administradora�de� Fondos de�Pensiones)?" 
capture label value s06h_58b s06h_58b 
capture label variable s07a_01a "Recibe usted ingresos (rentas) mensuales por: �Jubilaci�n (vejez)?" 
capture label variable s07a_01b "Recibe usted ingresos (rentas) mensuales por: �Benem�rito?" 
capture label variable s07a_01c "Recibe usted ingresos (rentas) mensuales por: �Invalidez?�(Monto en Bs)" 
capture label variable s07a_01d "Recibe usted ingresos (rentas) mensuales por: �Viudez, orfandad?(Monto en Bs)" 
capture label variable s07a_01e "Recibe usted ingresos (rentas) mensuales por: Renta Dignidad?" 
capture label value s07a_01e s07a_01e 
capture label variable s07a_01e0 "Recibe usted ingresos (rentas) mensuales por: Renta Dignidad?�(Monto en Bs)" 
capture label variable s07a_01e1 "La mayor parte del dinero de la Renta Dignidad lo gast� en:" 
capture label value s07a_01e1 s07a_01e1 
capture label variable s07a_01e1e "La mayor parte del dinero de la Renta Dignidad lo gast� en: Otros (Especifique)" 
capture label variable s07a_01e2 "�Por qu� no recibi� la Renta Dignidad?" 
capture label value s07a_01e2 s07a_01e2 
capture label variable s07a_01e2e "�Por qu� no recibi� la Renta Dignidad? Otro (Especifique)" 
capture label variable s07a_02a "Adem�s de los ingresos mencionados, recibe usted ingresos (rentas) mensuales por" 
capture label variable s07a_02b "Adem�s de los ingresos mencionados, recibe usted ingresos (rentas) mensuales por" 
capture label variable s07a_02c "Adem�s de los ingresos mencionados, recibe usted ingresos (rentas) mensuales por" 
capture label variable s07a_02ce "Adem�s de los ingresos mencionados, recibe usted ingresos (rentas) mensuales por" 
capture label variable s07a_03a "Durante los �ltimos doce meses, �recibi� usted: (en montos anuales) Alquileres d" 
capture label variable s07a_03b "Durante los �ltimos doce meses, �recibi� usted: (en montos anuales) Dividendos," 
capture label variable s07a_03c "Durante los �ltimos doce meses, �recibi� usted: (en montos anuales) Alquiler de" 
capture label variable s07a_04a "Adem�s de los ingresos mencionados anteriormente, durante los �ltimos doce meses" 
capture label variable s07a_04b "Adem�s de los ingresos mencionados anteriormente, durante los �ltimos doce meses" 
capture label variable s07a_04c "Adem�s de los ingresos mencionados anteriormente, durante los �ltimos doce meses" 
capture label variable s07b_05aa "En los �ltimos doce meses, �recibi� usted�Asistencia familiar por divorcio o sep" 
capture label value s07b_05aa s07b_05aa 
capture label variable s07b_05ab "En los �ltimos doce meses, �recibi� usted�Asistencia familiar por divorcio o sep" 
capture label value s07b_05ab s07b_05ab 
capture label variable s07b_05ba "En los �ltimos doce meses, �recibi� usted�Dinero, alimentos o regalos de otras p" 
capture label value s07b_05ba s07b_05ba 
capture label variable s07b_05bb "En los �ltimos doce meses, �recibi� usted�Dinero, alimentos o regalos de otras p" 
capture label value s07b_05bb s07b_05bb 
capture label variable s07c_06 "En los �ltimos 12 meses, �recibi� , dinero o encomiendas de otras personas que r" 
capture label value s07c_06 s07c_06 
capture label variable s07c_07 "�Con que frecuencia recibi� el dinero o encomiendas mencionadas?" 
capture label value s07c_07 s07c_07 
capture label variable s07c_08a "Si recibi� dinero, �cu�l es el monto y en qu� moneda lo recibi�?" 
capture label variable s07c_08b "Si recibi� dinero, �cu�l es el monto y en qu� moneda lo recibi�?" 
capture label value s07c_08b s07c_08b 
capture label variable s07c_08e "Si recibi� dinero, �cu�l es el monto y en qu� moneda lo recibi�?(Especifique)" 
capture label variable s07c_09 "Si recibi� dinero, la mayor parte lo gast� en:" 
capture label value s07c_09 s07c_09 
capture label variable s07c_09e "Si recibi� dinero, la mayor parte lo gast� en:(Especifique)" 
capture label variable s07c_10 "Si recibi� en especie, valorar en Bolivianos.�(Monto en Bs)" 
capture label variable estrato "Estrato estad�stico geogr�fico" 
capture label variable upm "Unidad Primaria de Muestreo" 
capture label variable factor "Factor de expansi�n" 
capture label variable cob_op "Grupo Ocupacional ocupaci�n principal" 
capture label value cob_op cob_op 
capture label variable cob_op2dig "Grupo Ocupacional ocupaci�n principal" 
capture label variable caeb_op "Clasificaci�n de Actividad Econ�mica de Bolivia Ocupacion principal" 
capture label value caeb_op caeb_op 
capture label variable cobersalud "Cobertura Seguro de Salud" 
capture label value cobersalud cobersalud 
capture label variable hnv_ult_anio "Hijos nacidos vivos ultimo a�o" 
capture label value hnv_ult_anio hnv_ult_anio 
capture label variable quienatenparto "Quien atendio parto" 
capture label value quienatenparto quienatenparto 
capture label variable dondeatenparto "Donde atendio parto" 
capture label value dondeatenparto dondeatenparto 
capture label variable f "A�os de estudio previos" 
capture label variable aoesc "A�os de estudio" 
capture label variable niv_ed "Niv edu detallado" 
capture label value niv_ed niv_ed 
capture label variable niv_ed_g1 "Niv edu genera1 1" 
capture label value niv_ed_g1 niv_ed_g1 
capture label variable cmasi "Condici�n de Matriculaci�n y Asistencia" 
capture label value cmasi cmasi 
capture label variable pet "Poblaci�n en edad de trabajar" 
capture label value pet pet 
capture label variable ocupado "Poblaci�n Ocupada" 
capture label value ocupado ocupado 
capture label variable cesante "Poblaci�n Desocupada Cesante" 
capture label value cesante cesante 
capture label variable aspirante "Poblaci�n Desocupada Aspirante" 
capture label value aspirante aspirante 
capture label variable desocupado "Poblaci�n Desocupada" 
capture label value desocupado desocupado 
capture label variable pea "Poblaci�n Activa" 
capture label value pea pea 
capture label variable temporal "Poblaci�n Inactiva Temporal" 
capture label value temporal temporal 
capture label variable permanente "Poblaci�n Inactiva Permanente" 
capture label value permanente permanente 
capture label variable pei "Poblaci�n Inactiva" 
capture label value pei pei 
capture label variable condact "Condici�n de Actividad Ocupacion Principal" 
capture label value condact condact 
capture label variable phrs "Horas trabajadas a la semana Ocupaci�n Principal" 
capture label variable shrs "Horas trabajadas a la semana Ocupaci�n Secundaria" 
capture label variable tothrs "Horas trabajadas a la semana" 
capture label variable totper "Total de personas" 
capture label variable yprilab "Ingreso laboral Ocupaci�n Principal (Bs/Mes)" 
capture label variable yseclab "Ingreso laboral Ocupaci�n Secundaria (Bs/Mes)" 
capture label variable ylab "Ingreso laboral (Bs/Mes)" 
capture label variable ynolab "Ingreso no laboral (Bs/Mes)" 
capture label variable yper "Ingreso Personal (Bs/Mes)" 
capture label variable yhog "Ingreso del Hogar (Bs/Mes)" 
capture label variable yhogpc "Ingreso Perc�pita del Hogar (Bs/Mes)" 
capture label variable z "L�nea de pobreza (bs/persona/mes)" 
capture label variable zext "L�nea de pobreza extrema o de indigencia (bs/persona/mes)" 
capture label variable p0 "Pobreza por Ingreso" 
capture label value p0 p0 
capture label variable p1 "Brecha de pobreza por ingreso" 
capture label variable p2 "Magnitud de pobreza por ingreso" 
capture label variable pext0 "Pobreza extrema o indigencia por ingreso" 
capture label value pext0 pext0 
capture label variable pext1 "Brecha de pobreza extrema por ingreso" 
capture label variable pext2 "Magnitud de pobreza extrema por ingreso" 
label define pext0 0 `"No pobre extremo"', modify
label define pext0 1 `"Pobre extremo"', modify
label define p0 0 `"No Pobre"', modify
label define p0 1 `"Pobre"', modify
label define condact 0 `"pent"', modify
label define condact 1 `"p_ocupado"', modify
label define condact 2 `"p_cesante"', modify
label define condact 3 `"p_aspirante"', modify
label define condact 4 `"p_temporal"', modify
label define condact 5 `"p_permanente"', modify
label define pei 0 `"No"', modify
label define pei 1 `"Si"', modify
label define permanente 0 `"No"', modify
label define permanente 1 `"Si"', modify
label define temporal 0 `"No"', modify
label define temporal 1 `"Si"', modify
label define pea 0 `"No"', modify
label define pea 1 `"Si"', modify
label define desocupado 0 `"No"', modify
label define desocupado 1 `"Si"', modify
label define aspirante 0 `"No"', modify
label define aspirante 1 `"Si"', modify
label define cesante 0 `"No"', modify
label define cesante 1 `"Si"', modify
label define ocupado 0 `"No"', modify
label define ocupado 1 `"Si"', modify
label define pet 0 `"pent"', modify
label define pet 1 `"pet"', modify
label define pet 9 `"Sin especificar"', modify
label define cmasi 1 `"NO matriculado"', modify
label define cmasi 2 `"Asiste"', modify
label define cmasi 3 `"No asiste"', modify
label define niv_ed_g1 0 `"Ninguno"', modify
label define niv_ed_g1 1 `"Primaria"', modify
label define niv_ed_g1 2 `"Secundaria"', modify
label define niv_ed_g1 3 `"Superior"', modify
label define niv_ed_g1 5 `"Otros"', modify
label define niv_ed 0 `"Ninguno"', modify
label define niv_ed 1 `"Primaria incompleta"', modify
label define niv_ed 2 `"Primaria completa"', modify
label define niv_ed 3 `"Secundaria incompleta"', modify
label define niv_ed 4 `"Secundaria completa"', modify
label define niv_ed 5 `"Superior"', modify
label define niv_ed 7 `"Otros"', modify
label define dondeatenparto 1 `"Establecimientos de salud"', modify
label define dondeatenparto 2 `"Domicilio"', modify
label define dondeatenparto 3 `"Otro lugar"', modify
label define quienatenparto 1 `"Atenci�n Institucional"', modify
label define quienatenparto 2 `"Atenci�n Partera"', modify
label define quienatenparto 3 `"M�dico Tradicional"', modify
label define quienatenparto 4 `"Otro"', modify
label define hnv_ult_anio 0 `"No tuvo"', modify
label define hnv_ult_anio 1 `"Tuvo un hijo nacido vivo"', modify
label define cobersalud 1 `"P�blico"', modify
label define cobersalud 2 `"Privado"', modify
label define cobersalud 3 `"Otro"', modify
label define cobersalud 4 `"Ninguno"', modify
label define caeb_op 0 `"Agricultura,Ganader�a,Caza,Pesca y Silvicultura"', modify
label define caeb_op 1 `"Explotaci�n de Minas y Canteras"', modify
label define caeb_op 2 `"Industria Manufacturera"', modify
label define caeb_op 3 `"Suministro de electricidad,gas,vapor y aire acondicionado"', modify
label define caeb_op 4 `"Suministro de agua, evac. de aguas res,gesti�n de desechos"', modify
label define caeb_op 5 `"Construcci�n"', modify
label define caeb_op 6 `"Venta por mayor y menor,reparaci�n de automotores"', modify
label define caeb_op 7 `"Transporte y Almacenamiento"', modify
label define caeb_op 8 `"Actividades de alojamiento y servicio de comidas"', modify
label define caeb_op 9 `"Informaciones y Comunicaciones"', modify
label define caeb_op 10 `"Intermediaci�n Financiera y Seguros"', modify
label define caeb_op 11 `"Actividades inmobiliarias"', modify
label define caeb_op 12 `"Servicios Profesionales y T�cnicos"', modify
label define caeb_op 13 `"Actividades de Servicios Administrativos y de Apoyo"', modify
label define caeb_op 14 `"Adm. P�blica, Defensa y Seguridad Social"', modify
label define caeb_op 15 `"Servicios de Educaci�n"', modify
label define caeb_op 16 `"Servicios de Salud y Asistencia Social"', modify
label define caeb_op 17 `"Actividades artisticas,entretenimiento y recreativas"', modify
label define caeb_op 18 `"Otras actividades de servicios"', modify
label define caeb_op 19 `"Actividades de Hogares Privados"', modify
label define caeb_op 20 `"Servicio de Organismos Extraterritoriales"', modify
label define caeb_op 99 `"Sin especificar"', modify
label define cob_op 0 `"Fuerzas Armadas"', modify
label define cob_op 1 `"Directivos Adm. P�blica y Empresas"', modify
label define cob_op 2 `"Profesionales cientificos e intelectuales"', modify
label define cob_op 3 `"T�cnicos de Nivel Medio"', modify
label define cob_op 4 `"Empleados de oficina"', modify
label define cob_op 5 `"Trabajadores de Servicio y Vendedores"', modify
label define cob_op 6 `"Trabajadores en Agricultura, Pecuaria,Pesca y otros"', modify
label define cob_op 7 `"Trabajadores de la Construcci�n, Ind. Manufacturera y Otros"', modify
label define cob_op 8 `"Operadores de Instalaciones y Maquinarias"', modify
label define cob_op 9 `"Trabajadores No Calificados"', modify
label define cob_op 99 `"Sin especificar"', modify
label define s07c_09 1 `"1.Construcci�n o compra de vivienda"', modify
label define s07c_09 2 `"2.Ampliaci�n o reparaci�n de la vivienda"', modify
label define s07c_09 3 `"3.Insumos, materiales, etc., para su actividad econ�mica"', modify
label define s07c_09 4 `"4.Equipamiento del hogar"', modify
label define s07c_09 5 `"5.Salud"', modify
label define s07c_09 6 `"6. Alimentaci�n"', modify
label define s07c_09 7 `"7. Otros (Especifique)"', modify
label define s07c_08b 1 `"1.Bolivianos"', modify
label define s07c_08b 2 `"2.Euros"', modify
label define s07c_08b 3 `"3.D�lares"', modify
label define s07c_08b 4 `"4.Pesos argentinos"', modify
label define s07c_08b 5 `"5.Reales"', modify
label define s07c_08b 6 `"6.Pesos chilenos"', modify
label define s07c_08b 7 `"7.Otro (Especifique)"', modify
label define s07c_07 2 `"2.Semanal"', modify
label define s07c_07 3 `"3.Quincenal"', modify
label define s07c_07 4 `"4.Mensual"', modify
label define s07c_07 5 `"5.Bimestral"', modify
label define s07c_07 6 `"6.Trimestral"', modify
label define s07c_07 7 `"7.Semestral"', modify
label define s07c_07 8 `"8.Anual"', modify
label define s07c_06 1 `"1. Si"', modify
label define s07c_06 2 `"2. No"', modify
label define s07b_05bb 2 `"2.Semanal"', modify
label define s07b_05bb 3 `"3.Quincenal"', modify
label define s07b_05bb 4 `"4.Mensual"', modify
label define s07b_05bb 5 `"5.Bimestral"', modify
label define s07b_05bb 6 `"6.Trimestral"', modify
label define s07b_05bb 7 `"7.Semestral"', modify
label define s07b_05bb 8 `"8.Anual"', modify
label define s07b_05bb 9 `"9. Ninguno"', modify
label define s07b_05ba 2 `"2.Semanal"', modify
label define s07b_05ba 3 `"3.Quincenal"', modify
label define s07b_05ba 4 `"4.Mensual"', modify
label define s07b_05ba 5 `"5.Bimestral"', modify
label define s07b_05ba 6 `"6.Trimestral"', modify
label define s07b_05ba 7 `"7.Semestral"', modify
label define s07b_05ba 8 `"8.Anual"', modify
label define s07b_05ba 9 `"9. Ninguno"', modify
label define s07b_05ab 2 `"2.Semanal"', modify
label define s07b_05ab 3 `"3.Quincenal"', modify
label define s07b_05ab 4 `"4.Mensual"', modify
label define s07b_05ab 5 `"5.Bimestral"', modify
label define s07b_05ab 6 `"6.Trimestral"', modify
label define s07b_05ab 7 `"7.Semestral"', modify
label define s07b_05ab 8 `"8.Anual"', modify
label define s07b_05ab 9 `"9. Ninguno"', modify
label define s07b_05aa 2 `"2.Semanal"', modify
label define s07b_05aa 3 `"3.Quincenal"', modify
label define s07b_05aa 4 `"4.Mensual"', modify
label define s07b_05aa 5 `"5.Bimestral"', modify
label define s07b_05aa 6 `"6.Trimestral"', modify
label define s07b_05aa 7 `"7.Semestral"', modify
label define s07b_05aa 8 `"8.Anual"', modify
label define s07b_05aa 9 `"9. Ninguno"', modify
label define s07a_01e2 1 `"1.No contaba con documentos (CN o CI) ni con testigos de la comunidad"', modify
label define s07a_01e2 2 `"2.No conoc�a la fecha u hora del pago"', modify
label define s07a_01e2 3 `"3.Estaba enfermo"', modify
label define s07a_01e2 4 `"4.Tuvo que viajar"', modify
label define s07a_01e2 5 `"5.No le corresponde por su edad"', modify
label define s07a_01e2 6 `"6.No realizo el tramite de inscripci�n"', modify
label define s07a_01e2 7 `"7. Todav�a trabaja y percibe ingresos del Tesoro General de la Naci�n (TGN)"', modify
label define s07a_01e2 8 `"8.Otro (Especifique)"', modify
label define s07a_01e1 1 `"1.Alimentos para el hogar"', modify
label define s07a_01e1 2 `"2.Ropa y calzados"', modify
label define s07a_01e1 3 `"3.Educaci�n"', modify
label define s07a_01e1 4 `"4.Salud"', modify
label define s07a_01e1 5 `"5.Vivienda"', modify
label define s07a_01e1 6 `"6.Ahorro"', modify
label define s07a_01e1 7 `"7.Otros (Especifique)"', modify
label define s07a_01e 1 `"1. Si"', modify
label define s07a_01e 2 `"2. No"', modify
label define s06h_58b 1 `"1. Si"', modify
label define s06h_58b 2 `"2. No"', modify
label define s06h_58a 1 `"1. Si"', modify
label define s06h_58a 2 `"2. No"', modify
label define s06h_57 1 `"1. Si"', modify
label define s06h_57 2 `"2. No"', modify
label define s06h_56 1 `"1.RENUNCIA"', modify
label define s06h_56 2 `"2.DESPIDO"', modify
label define s06h_56 3 `"3.FIN DE CONTRATO"', modify
label define s06h_56 4 `"4.POR FALTA DE CLIENTES, PEDIDO O FINANCIAMIENTO"', modify
label define s06h_56 5 `"5.FALTA DE MATERIALES, INSUMOS,MAQUINARIA,TIERRA"', modify
label define s06h_56 6 `"6.POR ESTUDIOS"', modify
label define s06h_56 7 `"7.POR ENFERMEDAD"', modify
label define s06h_56 8 `"8.POR MOTIVOS PERSONALES (Especifique)"', modify
label define s06h_56 9 `"9.OTRO (Especifique)"', modify
label define s06h_55 1 `"1.NO ENCUENTRA OTRO TRABAJO"', modify
label define s06h_55 2 `"2.POR FALTA DE CLIENTES, PEDIDOS O FINANCIAMINETO"', modify
label define s06h_55 3 `"3.FALTA DE MATERIALES, INSUMOS, MAQUINARIA, TIERRA"', modify
label define s06h_55 4 `"4.POR ENFERMEDAD"', modify
label define s06h_55 5 `"5.POR MOTIVOS PERSONALES O FAMILIARES (Especifique)"', modify
label define s06h_55 6 `"6.OTRO (Especifique)"', modify
label define s06h_54 1 `"1.Consult� con empleadores"', modify
label define s06h_54 2 `"2.Puso o contest� anuncios/dej� su CV"', modify
label define s06h_54 3 `"3.Acudi� a la bolsa de trabajo"', modify
label define s06h_54 4 `"4.Consult� con amigos, parientes o personas conocidas"', modify
label define s06h_54 5 `"5.Trat� de conseguir capital, clientes, etc."', modify
label define s06h_54 6 `"6.Realiz� consultas continuas a peri�dicos"', modify
label define s06h_54 7 `"7.Otro (Especifique)"', modify
label define s06h_53 1 `"1. Si"', modify
label define s06h_53 2 `"2. No"', modify
label define s06h_52 1 `"1. Si"', modify
label define s06h_52 2 `"2. No"', modify
label define s06g_51b 1 `"1. Diario"', modify
label define s06g_51b 2 `"2.Semanal"', modify
label define s06g_51b 3 `"3.Quincenal"', modify
label define s06g_51b 4 `"4.Mensual"', modify
label define s06g_51b 5 `"5.Bimestral"', modify
label define s06g_51b 6 `"6.Trimestral"', modify
label define s06g_51b 7 `"7.Semestral"', modify
label define s06g_51b 8 `"8.Anual"', modify
label define s06g_50gb 1 `"1. Diario"', modify
label define s06g_50gb 2 `"2.Semanal"', modify
label define s06g_50gb 3 `"3.Quincenal"', modify
label define s06g_50gb 4 `"4.Mensual"', modify
label define s06g_50gb 5 `"5.Bimestral"', modify
label define s06g_50gb 6 `"6.Trimestral"', modify
label define s06g_50gb 7 `"7.Semestral"', modify
label define s06g_50gb 8 `"8.Anual"', modify
label define s06g_50gb 9 `"9. Ninguno"', modify
label define s06g_50fb 1 `"1. Diario"', modify
label define s06g_50fb 2 `"2.Semanal"', modify
label define s06g_50fb 3 `"3.Quincenal"', modify
label define s06g_50fb 4 `"4.Mensual"', modify
label define s06g_50fb 5 `"5.Bimestral"', modify
label define s06g_50fb 6 `"6.Trimestral"', modify
label define s06g_50fb 7 `"7.Semestral"', modify
label define s06g_50fb 8 `"8.Anual"', modify
label define s06g_50fb 9 `"9. Ninguno"', modify
label define s06g_50eb 1 `"1. Diario"', modify
label define s06g_50eb 2 `"2.Semanal"', modify
label define s06g_50eb 3 `"3.Quincenal"', modify
label define s06g_50eb 4 `"4.Mensual"', modify
label define s06g_50eb 5 `"5.Bimestral"', modify
label define s06g_50eb 6 `"6.Trimestral"', modify
label define s06g_50eb 7 `"7.Semestral"', modify
label define s06g_50eb 8 `"8.Anual"', modify
label define s06g_50eb 9 `"9. Ninguno"', modify
label define s06g_50db 1 `"1. Diario"', modify
label define s06g_50db 2 `"2.Semanal"', modify
label define s06g_50db 3 `"3.Quincenal"', modify
label define s06g_50db 4 `"4.Mensual"', modify
label define s06g_50db 5 `"5.Bimestral"', modify
label define s06g_50db 6 `"6.Trimestral"', modify
label define s06g_50db 7 `"7.Semestral"', modify
label define s06g_50db 8 `"8.Anual"', modify
label define s06g_50db 9 `"9. Ninguno"', modify
label define s06g_50cb 1 `"1. Diario"', modify
label define s06g_50cb 2 `"2.Semanal"', modify
label define s06g_50cb 3 `"3.Quincenal"', modify
label define s06g_50cb 4 `"4.Mensual"', modify
label define s06g_50cb 5 `"5.Bimestral"', modify
label define s06g_50cb 6 `"6.Trimestral"', modify
label define s06g_50cb 7 `"7.Semestral"', modify
label define s06g_50cb 8 `"8.Anual"', modify
label define s06g_50cb 9 `"9. Ninguno"', modify
label define s06g_50bb 1 `"1. Diario"', modify
label define s06g_50bb 2 `"2.Semanal"', modify
label define s06g_50bb 3 `"3.Quincenal"', modify
label define s06g_50bb 4 `"4.Mensual"', modify
label define s06g_50bb 5 `"5.Bimestral"', modify
label define s06g_50bb 6 `"6.Trimestral"', modify
label define s06g_50bb 7 `"7.Semestral"', modify
label define s06g_50bb 8 `"8.Anual"', modify
label define s06g_50bb 9 `"9. Ninguno"', modify
label define s06g_50ab 1 `"1. Diario"', modify
label define s06g_50ab 2 `"2.Semanal"', modify
label define s06g_50ab 3 `"3.Quincenal"', modify
label define s06g_50ab 4 `"4.Mensual"', modify
label define s06g_50ab 5 `"5.Bimestral"', modify
label define s06g_50ab 6 `"6.Trimestral"', modify
label define s06g_50ab 7 `"7.Semestral"', modify
label define s06g_50ab 8 `"8.Anual"', modify
label define s06g_50ab 9 `"9. Ninguno"', modify
label define s06g_49b 1 `"1. Diario"', modify
label define s06g_49b 2 `"2.Semanal"', modify
label define s06g_49b 3 `"3.Quincenal"', modify
label define s06g_49b 4 `"4.Mensual"', modify
label define s06g_49b 5 `"5.Bimestral"', modify
label define s06g_49b 6 `"6.Trimestral"', modify
label define s06g_49b 7 `"7.Semestral"', modify
label define s06g_49b 8 `"8.Anual"', modify
label define s06g_48c 1 `"1. Si"', modify
label define s06g_48c 2 `"2. No"', modify
label define s06g_48b 1 `"1. Si"', modify
label define s06g_48b 2 `"2. No"', modify
label define s06g_48a 1 `"1. Si"', modify
label define s06g_48a 2 `"2. No"', modify
label define s06g_47b 1 `"1. Diario"', modify
label define s06g_47b 2 `"2.Semanal"', modify
label define s06g_47b 3 `"3.Quincenal"', modify
label define s06g_47b 4 `"4.Mensual"', modify
label define s06g_47b 5 `"5.Bimestral"', modify
label define s06g_47b 6 `"6.Trimestral"', modify
label define s06g_47b 7 `"7.Semestral"', modify
label define s06g_47b 8 `"8.Anual"', modify
label define s06g_47b 9 `"9. Ninguno"', modify
label define s06f_45a1 1 `"1. horas menos"', modify
label define s06f_45a1 2 `"2. horas m�s"', modify
label define s06f_45a1 3 `"3. Trabaj� las mismas horas que habitualmente trabaja"', modify
label define s06f_43a 1 `"1.S�lo 1 persona (el entrevistado)"', modify
label define s06f_43a 2 `"2.De 2 a 4 personas"', modify
label define s06f_43a 3 `"3.De 5 a 10 personas"', modify
label define s06f_43a 4 `"4.De 11 a 14 personas"', modify
label define s06f_43a 5 `"5.De 15 a 19 personas"', modify
label define s06f_43a 6 `"6.De 20 a 49 personas"', modify
label define s06f_43a 7 `"7.De 50 o m�s personas"', modify
label define s06f_42 1 `"1.�Administraci�n P�blica?"', modify
label define s06f_42 2 `"2.�Empresa P�blica (estrat�gica)?"', modify
label define s06f_42 3 `"3.�Privada (Empresa mediana o grande)?"', modify
label define s06f_42 4 `"4.�Privada (Negocio familiar, micro o peque�a empresa)?"', modify
label define s06f_42 5 `"5.ONG (Organizaci�n no Gubernamental) y otras sin fines de lucro"', modify
label define s06f_42 6 `"6.Organismos internacionales, embajadas"', modify
label define s06f_41 1 `"1.Obrero/a"', modify
label define s06f_41 2 `"2.Empleado/a"', modify
label define s06f_41 3 `"3.Trabajador/a por cuenta propia"', modify
label define s06f_41 4 `"4.Patr�n/a, socio/a o empleador/a que s� recibe salario"', modify
label define s06f_41 5 `"5.Patr�n/a, socio/a o empleador/a que no recibe salario"', modify
label define s06f_41 6 `"6.Cooperativista de producci�n"', modify
label define s06f_41 7 `"7.Trabajador/a familiar o aprendiz sin remuneraci�n"', modify
label define s06f_41 8 `"8.Empleada/o del hogar"', modify
label define s06e_39 1 `"1. Si"', modify
label define s06e_39 2 `"2. No"', modify
label define s06e_38d2 1 `"1.BS"', modify
label define s06e_38d2 2 `"2."', modify
label define s06e_38d 1 `"1. Si"', modify
label define s06e_38d 2 `"2. No"', modify
label define s06e_38c2 1 `"1.BS"', modify
label define s06e_38c2 2 `"2."', modify
label define s06e_38c 1 `"1. Si"', modify
label define s06e_38c 2 `"2. No"', modify
label define s06e_38b2 1 `"1.BS"', modify
label define s06e_38b2 2 `"2."', modify
label define s06e_38b 1 `"1. Si"', modify
label define s06e_38b 2 `"2. No"', modify
label define s06e_38a2 1 `"1.BS"', modify
label define s06e_38a2 2 `"2."', modify
label define s06e_38a 1 `"1. Si"', modify
label define s06e_38a 2 `"2. No"', modify
label define s06e_37 1 `"1.Ampliar el negocio, empresa o actividad"', modify
label define s06e_37 2 `"2.Continuar con el negocio, empresa o actividad, en el mismo nivel"', modify
label define s06e_37 3 `"3.Abandonar el negocio, empresa o actividad"', modify
label define s06e_36 1 `"1.�Han aumentado?"', modify
label define s06e_36 2 `"2.�Se han mantenido igual?"', modify
label define s06e_36 3 `"3.�Han disminuido?"', modify
label define s06e_35 1 `"1.SI, DEL BANCO"', modify
label define s06e_35 2 `"2.SI, DE UNA FINANCIERA"', modify
label define s06e_35 3 `"3.SI, DE UNA CASA DE PR�STAMO"', modify
label define s06e_35 4 `"4.SI, DE PARIENTES O AMIGOS"', modify
label define s06e_35 5 `"5.SI, DE OTRO LUGAR"', modify
label define s06e_35 6 `"6.NO OBTUVO NING�N PR�STAMO"', modify
label define s06e_34b 1 `"1. Indemnizaci�n recibida"', modify
label define s06e_34b 2 `"2. Herencia"', modify
label define s06e_34b 3 `"3. Venta de bienes o inmuebles"', modify
label define s06e_34b 4 `"4. Ahorros propios"', modify
label define s06e_34b 5 `"5. En sociedad con otras personas"', modify
label define s06e_34b 6 `"6. Pr�stamos de parientes o amigos"', modify
label define s06e_34b 7 `"7. Pr�stamo bancario"', modify
label define s06e_34b 8 `"8. Ninguno"', modify
label define s06e_34b 9 `"9. Otro (especifique)"', modify
label define s06e_34a 1 `"1.Indemnizaci�n recibida"', modify
label define s06e_34a 2 `"2.Herencia"', modify
label define s06e_34a 3 `"3.Venta de bienes o inmuebles"', modify
label define s06e_34a 4 `"4.Ahorros propios"', modify
label define s06e_34a 5 `"5.En sociedad con otras personas"', modify
label define s06e_34a 6 `"6.Pr�stamos de parientes o amigos"', modify
label define s06e_34a 7 `"7.Pr�stamo bancario"', modify
label define s06e_34a 8 `"8.Ninguno"', modify
label define s06e_34a 9 `"9. Otro (Especifique)"', modify
label define s06d_33b 1 `"1. Diario"', modify
label define s06d_33b 2 `"2.Semanal"', modify
label define s06d_33b 3 `"3.Quincenal"', modify
label define s06d_33b 4 `"4.Mensual"', modify
label define s06d_33b 5 `"5.Bimestral"', modify
label define s06d_33b 6 `"6.Trimestral"', modify
label define s06d_33b 7 `"7.Semestral"', modify
label define s06d_33b 8 `"8.Anual"', modify
label define s06d_33b 9 `"9. Ninguno"', modify
label define s06d_32gb 1 `"1.Diario"', modify
label define s06d_32gb 2 `"2.Semanal"', modify
label define s06d_32gb 3 `"3.Quincenal"', modify
label define s06d_32gb 4 `"4.Mensual"', modify
label define s06d_32gb 5 `"5.Bimestral"', modify
label define s06d_32gb 6 `"6.Trimestral"', modify
label define s06d_32gb 7 `"7.Semestral"', modify
label define s06d_32gb 8 `"8.Anual"', modify
label define s06d_32fb 1 `"1.Diario"', modify
label define s06d_32fb 2 `"2.Semanal"', modify
label define s06d_32fb 3 `"3.Quincenal"', modify
label define s06d_32fb 4 `"4.Mensual"', modify
label define s06d_32fb 5 `"5.Bimestral"', modify
label define s06d_32fb 6 `"6.Trimestral"', modify
label define s06d_32fb 7 `"7.Semestral"', modify
label define s06d_32fb 8 `"8.Anual"', modify
label define s06d_32eb 1 `"1.Diario"', modify
label define s06d_32eb 2 `"2.Semanal"', modify
label define s06d_32eb 3 `"3.Quincenal"', modify
label define s06d_32eb 4 `"4.Mensual"', modify
label define s06d_32eb 5 `"5.Bimestral"', modify
label define s06d_32eb 6 `"6.Trimestral"', modify
label define s06d_32eb 7 `"7.Semestral"', modify
label define s06d_32eb 8 `"8.Anual"', modify
label define s06d_32db 1 `"1.Diario"', modify
label define s06d_32db 2 `"2.Semanal"', modify
label define s06d_32db 3 `"3.Quincenal"', modify
label define s06d_32db 4 `"4.Mensual"', modify
label define s06d_32db 5 `"5.Bimestral"', modify
label define s06d_32db 6 `"6.Trimestral"', modify
label define s06d_32db 7 `"7.Semestral"', modify
label define s06d_32db 8 `"8.Anual"', modify
label define s06d_32cb 1 `"1.Diario"', modify
label define s06d_32cb 2 `"2.Semanal"', modify
label define s06d_32cb 3 `"3.Quincenal"', modify
label define s06d_32cb 4 `"4.Mensual"', modify
label define s06d_32cb 5 `"5.Bimestral"', modify
label define s06d_32cb 6 `"6.Trimestral"', modify
label define s06d_32cb 7 `"7.Semestral"', modify
label define s06d_32cb 8 `"8.Anual"', modify
label define s06d_32bb 1 `"1. Diario"', modify
label define s06d_32bb 2 `"2.Semanal"', modify
label define s06d_32bb 3 `"3.Quincenal"', modify
label define s06d_32bb 4 `"4.Mensual"', modify
label define s06d_32bb 5 `"5.Bimestral"', modify
label define s06d_32bb 6 `"6.Trimestral"', modify
label define s06d_32bb 7 `"7.Semestral"', modify
label define s06d_32bb 8 `"8.Anual"', modify
label define s06d_32bb 9 `"9. Ninguno"', modify
label define s06d_32ab 1 `"1. Diario"', modify
label define s06d_32ab 2 `"2.Semanal"', modify
label define s06d_32ab 3 `"3.Quincenal"', modify
label define s06d_32ab 4 `"4.Mensual"', modify
label define s06d_32ab 5 `"5.Bimestral"', modify
label define s06d_32ab 6 `"6.Trimestral"', modify
label define s06d_32ab 7 `"7.Semestral"', modify
label define s06d_32ab 8 `"8.Anual"', modify
label define s06d_32ab 9 `"9. Ninguno"', modify
label define s06d_31b 1 `"1. Diario"', modify
label define s06d_31b 2 `"2.Semanal"', modify
label define s06d_31b 3 `"3.Quincenal"', modify
label define s06d_31b 4 `"4.Mensual"', modify
label define s06d_31b 5 `"5.Bimestral"', modify
label define s06d_31b 6 `"6.Trimestral"', modify
label define s06d_31b 7 `"7.Semestral"', modify
label define s06d_31b 8 `"8.Anual"', modify
label define s06d_31b 9 `"9. Ninguno"', modify
label define s06c_30e1 1 `"1. Diario"', modify
label define s06c_30e1 2 `"2.Semanal"', modify
label define s06c_30e1 3 `"3.Quincenal"', modify
label define s06c_30e1 4 `"4.Mensual"', modify
label define s06c_30e1 5 `"5.Bimestral"', modify
label define s06c_30e1 6 `"6.Trimestral"', modify
label define s06c_30e1 7 `"7.Semestral"', modify
label define s06c_30e1 8 `"8.Anual"', modify
label define s06c_30e 1 `"1. Si"', modify
label define s06c_30e 2 `"2. No"', modify
label define s06c_30d1 1 `"1. Diario"', modify
label define s06c_30d1 2 `"2.Semanal"', modify
label define s06c_30d1 3 `"3.Quincenal"', modify
label define s06c_30d1 4 `"4.Mensual"', modify
label define s06c_30d1 5 `"5.imestral"', modify
label define s06c_30d1 6 `"6.Trimestral"', modify
label define s06c_30d1 7 `"7.Semestral"', modify
label define s06c_30d1 8 `"8.Anual"', modify
label define s06c_30d 1 `"1. Si"', modify
label define s06c_30d 2 `"2. No"', modify
label define s06c_30c1 1 `"1. Diario"', modify
label define s06c_30c1 2 `"2.Semanal"', modify
label define s06c_30c1 3 `"3.Quincenal"', modify
label define s06c_30c1 4 `"4.Mensual"', modify
label define s06c_30c1 5 `"5.Bimestral"', modify
label define s06c_30c1 6 `"6.Trimestral"', modify
label define s06c_30c1 7 `"7.Semestral"', modify
label define s06c_30c1 8 `"8.Anual"', modify
label define s06c_30c 1 `"1. Si"', modify
label define s06c_30c 2 `"2. No"', modify
label define s06c_30b1 1 `"1. Diario"', modify
label define s06c_30b1 2 `"2.Semanal"', modify
label define s06c_30b1 3 `"3.Quincenal"', modify
label define s06c_30b1 4 `"4.Mensual"', modify
label define s06c_30b1 5 `"5.Bimestral"', modify
label define s06c_30b1 6 `"6.Trimestral"', modify
label define s06c_30b1 7 `"7.Semestral"', modify
label define s06c_30b1 8 `"8.Anual"', modify
label define s06c_30b 1 `"1. Si"', modify
label define s06c_30b 2 `"2. No"', modify
label define s06c_30a1 1 `"1. Diario"', modify
label define s06c_30a1 2 `"2.Semanal"', modify
label define s06c_30a1 3 `"3.Quincenal"', modify
label define s06c_30a1 4 `"4.Mensual"', modify
label define s06c_30a1 5 `"5.Bimestral"', modify
label define s06c_30a1 6 `"6.Trimestral"', modify
label define s06c_30a1 7 `"7.Semestral"', modify
label define s06c_30a1 8 `"8.Anual"', modify
label define s06c_30a 1 `"1. Si"', modify
label define s06c_30a 2 `"2. No"', modify
label define s06c_29b 1 `"1. Si"', modify
label define s06c_29b 2 `"2. No"', modify
label define s06c_29a 1 `"1. Si"', modify
label define s06c_29a 2 `"2. No"', modify
label define s06c_28b 1 `"1. Si"', modify
label define s06c_28b 2 `"2. No"', modify
label define s06c_28a 1 `"1. Si"', modify
label define s06c_28a 2 `"2. No"', modify
label define s06c_27bb 1 `"1. Diario"', modify
label define s06c_27bb 2 `"2. Semanal"', modify
label define s06c_27bb 3 `"3. Quincenal"', modify
label define s06c_27bb 4 `"4. Mensual"', modify
label define s06c_27bb 5 `"5. Bimestral"', modify
label define s06c_27bb 6 `"6. Trimestral"', modify
label define s06c_27bb 7 `"7. Semestral"', modify
label define s06c_27bb 8 `"8. Anual"', modify
label define s06c_27bb 9 `"9. Ninguno"', modify
label define s06c_27ba 1 `"1. Diario"', modify
label define s06c_27ba 2 `"2. Semanal"', modify
label define s06c_27ba 3 `"3. Quincenal"', modify
label define s06c_27ba 4 `"4. Mensual"', modify
label define s06c_27ba 5 `"5. Bimestral"', modify
label define s06c_27ba 6 `"6. Trimestral"', modify
label define s06c_27ba 7 `"7. Semestral"', modify
label define s06c_27ba 8 `"8. Anual"', modify
label define s06c_27ba 9 `"9. Ninguno"', modify
label define s06c_27ab 1 `"1. Diario"', modify
label define s06c_27ab 2 `"2. Semanal"', modify
label define s06c_27ab 3 `"3. Quincenal"', modify
label define s06c_27ab 4 `"4. Mensual"', modify
label define s06c_27ab 5 `"5. Bimestral"', modify
label define s06c_27ab 6 `"6. Trimestral"', modify
label define s06c_27ab 7 `"7. Semestral"', modify
label define s06c_27ab 8 `"8. Anual"', modify
label define s06c_27ab 9 `"9. Ninguno"', modify
label define s06c_27aa 1 `"1. Diario"', modify
label define s06c_27aa 2 `"2. Semanal"', modify
label define s06c_27aa 3 `"3. Quincenal"', modify
label define s06c_27aa 4 `"4. Mensual"', modify
label define s06c_27aa 5 `"5. Bimestral"', modify
label define s06c_27aa 6 `"6. Trimestral"', modify
label define s06c_27aa 7 `"7. Semestral"', modify
label define s06c_27aa 8 `"8. Anual"', modify
label define s06c_27aa 9 `"9. Ninguno"', modify
label define s06c_25b 1 `"1. Diario"', modify
label define s06c_25b 2 `"2. Semanal"', modify
label define s06c_25b 3 `"3. Quincenal"', modify
label define s06c_25b 4 `"4. Mensual"', modify
label define s06c_25b 5 `"5. Bimestral"', modify
label define s06c_25b 6 `"6. Trimestral"', modify
label define s06c_25b 7 `"7. Semestral"', modify
label define s06c_25b 8 `"8. Anual"', modify
label define s06b_23a1 1 `"1. horas menos"', modify
label define s06b_23a1 2 `"2. horas m�s"', modify
label define s06b_23a1 3 `"3. Trabaj� las mismas horas que habitualmente trabaja"', modify
label define s06b_21b 1 `"1. Si"', modify
label define s06b_21b 2 `"2. No"', modify
label define s06b_21a 1 `"1.S�lo 1 persona (el entrevistado)"', modify
label define s06b_21a 2 `"2.De 2 a 4 personas"', modify
label define s06b_21a 3 `"3.De 5 a 10 personas"', modify
label define s06b_21a 4 `"4.De 11 a 14 personas"', modify
label define s06b_21a 5 `"5.De 15 a 19 personas"', modify
label define s06b_21a 6 `"6.De 20 a 49 personas"', modify
label define s06b_21a 7 `"7.De 50 o m�s personas"', modify
label define s06b_20 1 `"1.En su vivienda particular"', modify
label define s06b_20 2 `"2.Local o terreno exclusivo"', modify
label define s06b_20 3 `"3.Puesto m�vil"', modify
label define s06b_20 4 `"4.En quiosco o puesto fijo"', modify
label define s06b_20 5 `"5.Veh�culo"', modify
label define s06b_20 6 `"6.Servicios a domicilio"', modify
label define s06b_20 7 `"7.Ambulante"', modify
label define s06b_20 8 `"8.Ambulante de preventa"', modify
label define s06b_20 9 `"9.Otro  (Especif�que)"', modify
label define s06b_19a 1 `"1.Se llevan libros de contabilidad o se acude a  los servicios de un contador"', modify
label define s06b_19a 2 `"2.S�lo se utiliza un cuaderno personal para llevar las cuentas"', modify
label define s06b_19a 3 `"3.No se lleva registro contable"', modify
label define s06b_19a 4 `"4.No sabe"', modify
label define s06b_19 1 `"1.Si, en r�gimen general"', modify
label define s06b_19 2 `"2.Si, en r�gimen simplificado"', modify
label define s06b_19 3 `"3.No tiene/En proceso"', modify
label define s06b_19 4 `"4.No sabe"', modify
label define s06b_18 1 `"1.�Administraci�n P�blica?"', modify
label define s06b_18 2 `"2.�Empresa P�blica (estrat�gica)?"', modify
label define s06b_18 3 `"3.�Privada (Empresa mediana o grande)?"', modify
label define s06b_18 4 `"4.�Privada (Negocio familiar, micro o peque�a empresa)?"', modify
label define s06b_18 5 `"5.ONG  (Organizaci�n no Gubernamental) y otras sin fines de lucro"', modify
label define s06b_18 6 `"6.Organismos internacionales, embajadas"', modify
label define s06b_17 1 `"1.�Firm� contrato con  fecha de vencimiento o t�rmino?"', modify
label define s06b_17 2 `"2.�No firm� contrato pero tiene compromiso por obra o trabajo terminado?"', modify
label define s06b_17 3 `"3.�No firmo contrato pero tiene un acuerdo verbal?"', modify
label define s06b_17 4 `"4.�Es personal de planta con �tem?"', modify
label define s06b_17 5 `"5.No firm� contrato"', modify
label define s06b_16 1 `"1.Obrero/a"', modify
label define s06b_16 2 `"2.Empleado/a"', modify
label define s06b_16 3 `"3.Trabajador/a por cuenta propia"', modify
label define s06b_16 4 `"4.Patr�n/a, socio/a o empleador/a  que s� recibe salario"', modify
label define s06b_16 5 `"5.Patr�n/a, socio/a o empleador/a  que no recibe salario"', modify
label define s06b_16 6 `"6.Cooperativista de  producci�n"', modify
label define s06b_16 7 `"7.Trabajador/a familiar o aprend�z sin remuneraci�n"', modify
label define s06b_16 8 `"8.Empleada/o del hogar"', modify
label define s06b_15b 2 `"2. Semana"', modify
label define s06b_15b 4 `"4. Mes"', modify
label define s06b_15b 8 `"8. A�o"', modify
label define s06b_14b 2 `"2. Semana"', modify
label define s06b_14b 4 `"4. Mes"', modify
label define s06b_14b 8 `"8. A�o"', modify
label define s06a_10 1 `"1.TIENE TRABAJO ASEGURADO QUE  COMENZAR� EN MENOS DE  CUATRO SEMANAS"', modify
label define s06a_10 2 `"2.BUSC� ANTES Y ESPERA  RESPUESTA"', modify
label define s06a_10 3 `"3.NO CREE PODER ENCONTRAR   TRABAJO"', modify
label define s06a_10 4 `"4.SE CANS� DE BUSCAR TRABAJO"', modify
label define s06a_10 5 `"5.ESPERA PERIODO DE MAYOR  ACTIVIDAD"', modify
label define s06a_10 6 `"6.POR QUE EST� ESTUDIANDO"', modify
label define s06a_10 7 `"7.POR VEJEZ/ JUBILACI�N"', modify
label define s06a_10 8 `"8.CORTA EDAD"', modify
label define s06a_10 9 `"9.POR ENFERMEDAD/ ACCIDENTE/ DISCAPACIDAD"', modify
label define s06a_10 10 `"10.NO NECESITA TRABAJAR"', modify
label define s06a_10 11 `"11.LABORES DE CASA/ EMBARAZO/  CUIDADO DE NI�OS/AS"', modify
label define s06a_10 12 `"12.ESPERA TEMPORADA DE COSECHA   O TRABAJO"', modify
label define s06a_10 13 `"13.POR OTRAS CAUSAS (Especifique)"', modify
label define s06a_09 1 `"1.�Estudiante?"', modify
label define s06a_09 2 `"2.�Ama de casa o  responsable de los quehaceres del hogar?"', modify
label define s06a_09 3 `"3.�Jubilado o benem�rito?"', modify
label define s06a_09 4 `"4.�Enfermo o discapacitado?"', modify
label define s06a_09 5 `"5.�Persona de edad avanzada?"', modify
label define s06a_09 6 `"6.�Otro? (Especifique)"', modify
label define s06a_08b 2 `"2. Semana"', modify
label define s06a_08b 4 `"4. Mes"', modify
label define s06a_08b 8 `"8. A�o"', modify
label define s06a_07 1 `"1. Si"', modify
label define s06a_07 2 `"2. No"', modify
label define s06a_06ac 1 `"1.Consult� con empleadores"', modify
label define s06a_06ac 2 `"2.Asisti� a una entrevista de trabajo"', modify
label define s06a_06ac 3 `"3.Puso o contest� anuncios"', modify
label define s06a_06ac 4 `"4. Acudio a la bolsa de trabajo del gobierno"', modify
label define s06a_06ac 5 `"5.Consult� en internet"', modify
label define s06a_06ac 6 `"6.Consult� con amigos, parientes o personas conocidas"', modify
label define s06a_06ac 7 `"7.Trat� de conseguir capital, clientes, etc."', modify
label define s06a_06ac 8 `"8.Realiz� consultas continuas a peri�dicos"', modify
label define s06a_06ac 9 `"9.Coloc� su curriculum vitae en alguna de las redes sociales"', modify
label define s06a_06ac 10 `"10.Otro (Especifique)"', modify
label define s06a_06ab 1 `"1.Consult� con empleadores"', modify
label define s06a_06ab 2 `"2.Asisti� a una entrevista de trabajo"', modify
label define s06a_06ab 3 `"3.Puso o contest� anuncios"', modify
label define s06a_06ab 4 `"4. Acudio a la bolsa de trabajo del gobierno"', modify
label define s06a_06ab 5 `"5.Consult� en internet"', modify
label define s06a_06ab 6 `"6.Consult� con amigos, parientes o personas conocidas"', modify
label define s06a_06ab 7 `"7.Trat� de conseguir capital, clientes, etc."', modify
label define s06a_06ab 8 `"8.Realiz� consultas continuas a peri�dicos"', modify
label define s06a_06ab 9 `"9.Coloc� su curriculum vitae en alguna de las redes sociales"', modify
label define s06a_06ab 10 `"10.Otro (Especifique)"', modify
label define s06a_06aa 1 `"1.Consult� con empleadores"', modify
label define s06a_06aa 2 `"2.Asisti� a una entrevista de trabajo"', modify
label define s06a_06aa 3 `"3.Puso o contest� anuncios"', modify
label define s06a_06aa 4 `"4. Acudio a la bolsa de trabajo del gobierno"', modify
label define s06a_06aa 5 `"5.Consult� en internet"', modify
label define s06a_06aa 6 `"6.Consult� con amigos, parientes o personas conocidas"', modify
label define s06a_06aa 7 `"7.Trat� de conseguir capital, clientes, etc."', modify
label define s06a_06aa 8 `"8.Realiz� consultas continuas a peri�dicos"', modify
label define s06a_06aa 9 `"9.Coloc� su curriculum vitae en alguna de las redes sociales"', modify
label define s06a_06aa 10 `"10.Otro (Especifique)"', modify
label define s06a_05 1 `"1. Si"', modify
label define s06a_05 2 `"2. No"', modify
label define s06a_04 1 `"1. Si"', modify
label define s06a_04 2 `"2. No"', modify
label define s06a_03 1 `"1.Vacaciones o permisos?"', modify
label define s06a_03 2 `"2.Licencia de maternidad?"', modify
label define s06a_03 3 `"3.Enfermedad o accidente?"', modify
label define s06a_03 4 `"4.Falta de materiales o clientes?"', modify
label define s06a_03 5 `"5.Temporada baja?"', modify
label define s06a_03 6 `"6.Huelga, paro o conflicto laboral?"', modify
label define s06a_03 7 `"7.Mal tiempo?"', modify
label define s06a_03 8 `"8.Estar suspendido?"', modify
label define s06a_03 9 `"9.Problemas personales o familiares?"', modify
label define s06a_03 10 `"10.NINGUNO"', modify
label define s06a_02 1 `"1.�Trabajar en cultivos agr�colas o en la crianza de animales para consumo del h"', modify
label define s06a_02 2 `"2.�Trabajar en cultivos agr�colas o en la crianza de animales para vender?"', modify
label define s06a_02 3 `"3.�Atender o ayudar en alg�n negocio propio o familiar?"', modify
label define s06a_02 4 `"4.�Vender en la calle en un puesto o como ambulante?"', modify
label define s06a_02 5 `"5.�Preparar alimentos, hilar, tejer, coser u otras actividades para la venta?"', modify
label define s06a_02 6 `"6.�Prestar servicios a otras personas por remuneraci�n (lavar ropa ajena, cortar"', modify
label define s06a_02 7 `"7.�Realizar alguna otra actividad por la cual gan� dinero?"', modify
label define s06a_02 8 `"8.NINGUNA ACTIVIDAD"', modify
label define s06a_01 1 `"1. Si"', modify
label define s06a_01 2 `"2. No"', modify
label define s05c_17l 1 `"1. Si"', modify
label define s05c_17l 2 `"2. No"', modify
label define s05c_17k 1 `"1. Si"', modify
label define s05c_17k 2 `"2. No"', modify
label define s05c_17j 1 `"1. Si"', modify
label define s05c_17j 2 `"2. No"', modify
label define s05c_17i 1 `"1. Si"', modify
label define s05c_17i 2 `"2. No"', modify
label define s05c_17h 1 `"1. Si"', modify
label define s05c_17h 2 `"2. No"', modify
label define s05c_17g 1 `"1. Si"', modify
label define s05c_17g 2 `"2. No"', modify
label define s05c_17f 1 `"1. Si"', modify
label define s05c_17f 2 `"2. No"', modify
label define s05c_17e 1 `"1. Si"', modify
label define s05c_17e 2 `"2. No"', modify
label define s05c_17d 1 `"1. Si"', modify
label define s05c_17d 2 `"2. No"', modify
label define s05c_17c 1 `"1. Si"', modify
label define s05c_17c 2 `"2. No"', modify
label define s05c_17b 1 `"1. Si"', modify
label define s05c_17b 2 `"2. No"', modify
label define s05c_17a 1 `"1. Si"', modify
label define s05c_17a 2 `"2. No"', modify
label define s05c_16ab 1 `"1. En el Hogar?"', modify
label define s05c_16ab 2 `"2. En el Trabajo?"', modify
label define s05c_16ab 3 `"3. En el lugar de estudio?"', modify
label define s05c_16ab 4 `"4. En el hogar de otra persona ?"', modify
label define s05c_16ab 5 `"5. En un lugar comunitario con    acceso a internet?"', modify
label define s05c_16ab 6 `"6.En un local de acceso comercial  a internet (caf� Internet) ?"', modify
label define s05c_16ab 7 `"7. A trav�s de un tel�fono  celular m�vil?"', modify
label define s05c_16ab 8 `"8. A trav�s de otros dispositivos  de acceso m�vil.?"', modify
label define s05c_16ab 9 `"9. Otro (especifique)"', modify
label define s05c_16aa 1 `"1. En el Hogar?"', modify
label define s05c_16aa 2 `"2. En el Trabajo?"', modify
label define s05c_16aa 3 `"3. En el lugar de estudio?"', modify
label define s05c_16aa 4 `"4. En el hogar de otra persona ?"', modify
label define s05c_16aa 5 `"5. En un lugar comunitario con    acceso a internet?"', modify
label define s05c_16aa 6 `"6.En un local de acceso comercial  a internet (caf� Internet) ?"', modify
label define s05c_16aa 7 `"7. A trav�s de un tel�fono  celular m�vil?"', modify
label define s05c_16aa 8 `"8. A trav�s de otros dispositivos  de acceso m�vil.?"', modify
label define s05c_16aa 9 `"9. Otro (especifique)"', modify
label define s05c_15b 1 `"1. Al menos una vez al d�a"', modify
label define s05c_15b 2 `"2. Al menos una vez por semana, pero no todos los  d�as."', modify
label define s05c_15b 3 `"3. Menos de una vez por   semana."', modify
label define s05c_15a 1 `"1. Si"', modify
label define s05c_15a 2 `"2. No"', modify
label define s05c_14b 1 `"1. Al menos una vez al d�a"', modify
label define s05c_14b 2 `"2. Al menos una vez por semana, pero no todos los  d�as."', modify
label define s05c_14b 3 `"3. Menos de una vez por   semana."', modify
label define s05c_14a 1 `"1. Si"', modify
label define s05c_14a 2 `"2. No"', modify
label define s05c_13 1 `"1. Si"', modify
label define s05c_13 2 `"2. No"', modify
label define s05b_11a 1 `"1. Si"', modify
label define s05b_11a 2 `"2. No"', modify
label define s05b_11 1 `"1.VACACI�N (DESCANSO PEDAG�GICO/RECESO)"', modify
label define s05b_11 2 `"2.CULMIN� SUS ESTUDIOS"', modify
label define s05b_11 3 `"3.POR ENFERMEDAD/ACCIDENTE/DISCAPACIDAD"', modify
label define s05b_11 4 `"4.POR EMBARAZO"', modify
label define s05b_11 5 `"5.FALT� DINERO PARA COMPRAR �TILES,ETC."', modify
label define s05b_11 6 `"6.EL ESTABLECIMIENTO ES DISTANTE"', modify
label define s05b_11 7 `"7.EL ESTABLECIMIENTO NO ES SEGURO"', modify
label define s05b_11 8 `"8.LA ENSE�ANZA NO ES BUENA/ADECUADA O NO SE APRENDE NADA"', modify
label define s05b_11 9 `"9.FALTA INTER�S (de la persona no matriculada)"', modify
label define s05b_11 10 `"10.POR REALIZAR LABORES DE CASA/CUIDADO DE NI�OS/AS"', modify
label define s05b_11 11 `"11.POR AYUDAR EN EL NEGOCIO/ACTIVIDAD DE MI FAMILIA"', modify
label define s05b_11 12 `"12.POR TRABAJO (EXCLUYENDO 11)"', modify
label define s05b_11 13 `"13.POR APRENDER UN OFICIO (de manera emp�rica)"', modify
label define s05b_11 14 `"14.OTRO"', modify
label define s05b_10 1 `"1.ASISTE"', modify
label define s05b_10 2 `"2.NO ASISTE"', modify
label define s05a_09 1 `"1.Fiscal / P�blico/Convenio"', modify
label define s05a_09 2 `"2.Particular/privado"', modify
label define s05a_08 1 `"1. Si"', modify
label define s05a_08 2 `"2. No"', modify
label define s05a_07a 1 `"1. Si"', modify
label define s05a_07a 2 `"2. No"', modify
label define s05a_06a 12 `"12.CURSO DE ALFABETIZACI�N"', modify
label define s05a_06a 13 `"13.EDUCACI�N INICIAL O PRE ESCOLAR (PRE-KINDER/KINDER) - SISTEMA ESCOLAR ACTUAL"', modify
label define s05a_06a 41 `"41.PRIMARIA (1 A 6 A�OS) - SISTEMA ESCOLAR ACTUAL"', modify
label define s05a_06a 42 `"42.SECUNDARIA (1 A 6 A�OS) - SISTEMA ESCOLAR ACTUAL"', modify
label define s05a_06a 61 `"61.EDUCACI�N JUVENIL ALTERNATIVA (EJA) - EDUCACI�N ALTERNATIVA Y ESPECIAL"', modify
label define s05a_06a 62 `"62.EDUCACI�N PRIMARIA DE ADULTOS (EPA) - EDUCACI�N ALTERNATIVA Y ESPECIAL"', modify
label define s05a_06a 63 `"63.EDUCACI�N SECUNDARIA DE ADULTOS (ESA) - EDUCACI�N ALTERNATIVA Y ESPECIAL"', modify
label define s05a_06a 64 `"64.PROGRAMA NACIONAL DE POST ALFABETIZACI�N - EDUCACI�N ALTERNATIVA Y ESPECIAL"', modify
label define s05a_06a 65 `"65.EDUCACI�N ESPECIAL - EDUCACI�N ALTERNATIVA Y ESPECIAL"', modify
label define s05a_06a 71 `"71.NORMAL (ESCUELA SUP- DE FORMACI�N DE  MAESTROS - EDUCACI�N SUPERIOR"', modify
label define s05a_06a 72 `"72.UNIVERSIDAD - EDUCACI�N SUPERIOR"', modify
label define s05a_06a 73 `"73.POSTGRADO DIPLOMADO - EDUCACI�N SUPERIOR"', modify
label define s05a_06a 74 `"74.POSTGRADO MAESTR�A - EDUCACI�N SUPERIOR"', modify
label define s05a_06a 75 `"75.POSTGRADO DOCTORADO - EDUCACI�N SUPERIOR"', modify
label define s05a_06a 76 `"76.T�CNICO DE UNIVERSIDAD - EDUCACI�N SUPERIOR"', modify
label define s05a_06a 77 `"77.T�CNICO DE INSTITUTO TECNICO /TECNOLOGICO (Duraci�n mayor o igual a 2 a�os) -"', modify
label define s05a_06a 78 `"78. FORMACI�N SUPERIOR ART�STICA - EDUCACI�N SUPERIOR"', modify
label define s05a_06a 79 `"79.INSTITUTOS DE FORMACI�N MILITAR Y POLICIAL - EDUCACI�N SUPERIOR"', modify
label define s05a_06a 80 `"80. EDUCACI�N T�CNICA DE ADULTOS (ETA) - EDUCACI�N SUPERIOR"', modify
label define s05a_06a 81 `"81.OTROS CURSOS  (Duraci�n menor a 2 a�os)  - EDUCACI�N SUPERIOR"', modify
label define s05a_05a 1 `"1.CULMIN� SUS ESTUDIOS"', modify
label define s05a_05a 2 `"2.POR ENFERMEDAD/ACCIDENTE/DISCAPACIDAD"', modify
label define s05a_05a 3 `"3.POR EMBARAZO"', modify
label define s05a_05a 4 `"4.FALT� DINERO PARA COMPRAR �TILES,ETC."', modify
label define s05a_05a 5 `"5.EL ESTABLECIMIENTO ES DISTANTE"', modify
label define s05a_05a 6 `"6.EL ESTABLECIMIENTO NO ES SEGURO"', modify
label define s05a_05a 7 `"7.LA ENSE�ANZA NO ES BUENA/ADECUADA O NO SE APRENDE NADA"', modify
label define s05a_05a 8 `"8.FALTA INTER�S (de la persona no matriculada)"', modify
label define s05a_05a 9 `"9.POR REALIZAR LABORES DE CASA/CUIDADO DE NI�OS/AS"', modify
label define s05a_05a 10 `"10.POR AYUDAR EN EL NEGOCIO/ACTIVIDAD DE MI FAMILIA"', modify
label define s05a_05a 11 `"11.POR TRABAJO (EXCLUYENDO 10)"', modify
label define s05a_05a 12 `"12.POR APRENDER UN OFICIO (de manera emp�rica)"', modify
label define s05a_05a 13 `"13.LA EDUCACI�N NO ES IMPORTANTE"', modify
label define s05a_05a 14 `"14.OTRO"', modify
label define s05a_05 1 `"1. Si"', modify
label define s05a_05 2 `"2. No"', modify
label define s05a_03a 11 `"11.NINGUNO"', modify
label define s05a_03a 21 `"21.B�SICO (1 A 5 A�OS) - SISTEMA ESCOLAR ANTIGUO"', modify
label define s05a_03a 22 `"22.INTERMEDIO (1 A 3 A�OS) - SISTEMA ESCOLAR ANTIGUO"', modify
label define s05a_03a 23 `"23.MEDIO (1 A 4 A�OS) - SISTEMA ESCOLAR ANTIGUO"', modify
label define s05a_03a 31 `"31.PRIMARIA (1 A 8 A�OS) - SISTEMA ESCOLAR ANTERIOR"', modify
label define s05a_03a 32 `"32.SECUNDARIA (1 A 4 A�OS) - SISTEMA ESCOLAR ANTERIOR"', modify
label define s05a_03a 41 `"41.PRIMARIA (1 A 6 A�OS) - SISTEMA ESCOLAR ACTUAL"', modify
label define s05a_03a 42 `"42.SECUNDARIA (1 A 6 A�OS) - SISTEMA ESCOLAR ACTUAL"', modify
label define s05a_03a 51 `"51.EDUCACI�N B�SICA  DE   ADULTOS (EBA) - EDUACI�N DE ADULTOS (SISTEMA ANTIGUO)"', modify
label define s05a_03a 52 `"52.CENTRO DE EDUCACI�N MEDIA  DE ADULTOS (CEMA) - EDUCACI�N DE ADULTOS (SISTEMA"', modify
label define s05a_03a 61 `"61.EDUCACI�N JUVENIL ALTERNATIVA (EJA) - EDUCACI�N ALTERNATIVA Y ESPECIAL"', modify
label define s05a_03a 62 `"62.EDUCACI�N PRIMARIA DE ADULTOS (EPA) - EDUCACI�N ALTERNATIVA Y ESPECIAL"', modify
label define s05a_03a 63 `"63.EDUCACI�N SECUNDARIA DE ADULTOS (ESA) - EDUCACI�N ALTERNATIVA Y ESPECIAL"', modify
label define s05a_03a 64 `"64.PROGRAMA NACIONAL DE POST ALFABETIZACI�N - EDUCACI�N ALTERNATIVA Y ESPECIAL"', modify
label define s05a_03a 65 `"65.EDUCACI�N  ESPECIAL - EDUCACI�N ALTERNATIVA Y ESPECIAL"', modify
label define s05a_02a 11 `"11.NINGUNO"', modify
label define s05a_02a 12 `"12.CURSO DE ALFABETIZACI�N"', modify
label define s05a_02a 13 `"13.EDUCACI�N INICIAL O PRE-ESCOLAR (PRE KINDER/KINDER)"', modify
label define s05a_02a 21 `"21.B�SICO (1 A 5 A�OS) - SISTEMA ESCOLAR ANTIGUO"', modify
label define s05a_02a 22 `"22.INTERMEDIO (1 A 3 A�OS) - SISTEMA ESCOLAR ANTIGUO"', modify
label define s05a_02a 23 `"23.MEDIO (1 A 4 A�OS) - SISTEMA ESCOLAR ANTIGUO"', modify
label define s05a_02a 31 `"31.PRIMARIA (1 A 8 A�OS) - SISTEMA ESCOLAR ANTERIOR"', modify
label define s05a_02a 32 `"32.SECUNDARIA (1 A 4 A�OS) - SISTEMA ESCOLAR ANTERIOR"', modify
label define s05a_02a 41 `"41.PRIMARIA (1 A 6 A�OS) - SISTEMA ESCOLAR ACTUAL"', modify
label define s05a_02a 42 `"42.SECUNDARIA (1 A 6 A�OS) - SISTEMA ESCOLAR ACTUAL"', modify
label define s05a_02a 51 `"51.EDUCACI�N B�SICA  DE   ADULTOS (EBA) - EDUCACI�N DE ADULTOS (SISTEMA ANTIGUO)"', modify
label define s05a_02a 52 `"52.CENTRO DE EDUCACI�N MEDIA  DE ADULTOS (CEMA) - EDUCACI�N DE ADULTOS (SISTEMA"', modify
label define s05a_02a 61 `"61.EDUCACI�N JUVENIL ALTERNATIVA (EJA) - EDUCACI�N ALTERNATIVA Y ESPECIAL"', modify
label define s05a_02a 62 `"62.EDUCACI�N PRIMARIA DE ADULTOS (EPA) - EDUCACI�N ALTERNATIVA Y ESPECIAL"', modify
label define s05a_02a 63 `"63.EDUCACI�N SECUNDARIA DE ADULTOS (ESA) - EDUCACI�N ALTERNATIVA Y ESPECIAL"', modify
label define s05a_02a 64 `"64.PROGRAMA NACIONAL DE POST ALFABETIZACI�N - EDUCACI�N ALTERNATIVA Y ESPECIAL"', modify
label define s05a_02a 65 `"65.EDUCACI�N  ESPECIAL - EDUCACI�N ALTERNATIVA Y ESPECIAL"', modify
label define s05a_02a 71 `"71.NORMAL (ESCUELA SUP. DE FORMACI�N DE  MAESTROS) - EDUCACI�N SUPERIOR"', modify
label define s05a_02a 72 `"72.UNIVERSIDAD - EDUCACI�N SUPERIOR"', modify
label define s05a_02a 73 `"73.POSTGRADO DIPLOMADO - EDUCACI�N SUPERIOR"', modify
label define s05a_02a 74 `"74.POSTGRADO MAESTR�A - EDUCACI�N SUPERIOR"', modify
label define s05a_02a 75 `"75.POSTGRADO DOCTORADO - EDUCACI�N SUPERIOR"', modify
label define s05a_02a 76 `"76.T�CNICO DE UNIVERSIDAD - EDUCACI�N SUPERIOR"', modify
label define s05a_02a 77 `"77.T�CNICO DE INSTITUTO T�CNICO E INSTITUTO TECNOLOG�GICO  (Duraci�n mayor o igu"', modify
label define s05a_02a 78 `"78.FORMACION SUPERIOR ART�STICA"', modify
label define s05a_02a 79 `"79. INSTITUTOS DE FORMACI�N MILITAR Y POLICIAL - EDUCACI�N SUPERIOR"', modify
label define s05a_02a 80 `"80. EDUCACI�N T�CNICA DE ADULTOS (ETA) - EDUCACI�N SUPERIOR"', modify
label define s05a_02a 81 `"81.OTROS CURSOS  (Duraci�n menor a 2 a�os) - EDUCACI�N SUPERIOR"', modify
label define s05a_01a 1 `"1. Si"', modify
label define s05a_01a 2 `"2. No"', modify
label define s05a_01 1 `"1. Si"', modify
label define s05a_01 2 `"2. No"', modify
label define s04f_35c 1 `"1.Atraco (Robo a personas) en la v�a p�blica"', modify
label define s04f_35c 2 `"2.Robo en su vivienda o negocio"', modify
label define s04f_35c 3 `"3.Robo de veh�culo o accesorios"', modify
label define s04f_35c 4 `"4.Lesiones graves o gravisimas ocasionadas por otra persona"', modify
label define s04f_35c 5 `"5.Violaci�n o abuso sexual"', modify
label define s04f_35c 6 `"6.Estafa o abuso de confianza"', modify
label define s04f_35c 7 `"7.Despojo de su inmueble"', modify
label define s04f_35c 8 `" 8.Trata de personas"', modify
label define s04f_35c 9 `"9.Extorsi�n"', modify
label define s04f_35c 10 `" 10.Secuestro"', modify
label define s04f_35c 11 `"11. Otros hechos delictivos (ESPECIFIQUE)"', modify
label define s04f_35c 12 `"12.Ninguno"', modify
label define s04f_35b 1 `"1.Atraco (Robo a personas) en la v�a p�blica"', modify
label define s04f_35b 2 `"2.Robo en su vivienda o negocio"', modify
label define s04f_35b 3 `"3.Robo de veh�culo o accesorios"', modify
label define s04f_35b 4 `"4.Lesiones graves o gravisimas ocasionadas por otra persona"', modify
label define s04f_35b 5 `"5.Violaci�n o abuso sexual"', modify
label define s04f_35b 6 `"6.Estafa o abuso de confianza"', modify
label define s04f_35b 7 `"7.Despojo de su inmueble"', modify
label define s04f_35b 8 `" 8.Trata de personas"', modify
label define s04f_35b 9 `"9.Extorsi�n"', modify
label define s04f_35b 10 `" 10.Secuestro"', modify
label define s04f_35b 11 `"11. Otros hechos delictivos (ESPECIFIQUE)"', modify
label define s04f_35b 12 `"12.Ninguno"', modify
label define s04f_35a 1 `"1.Atraco (Robo a personas) en la v�a p�blica"', modify
label define s04f_35a 2 `"2.Robo en su vivienda o negocio"', modify
label define s04f_35a 3 `"3.Robo de veh�culo o accesorios"', modify
label define s04f_35a 4 `"4.Lesiones graves o gravisimas ocasionadas por otra persona"', modify
label define s04f_35a 5 `"5.Violaci�n o abuso sexual"', modify
label define s04f_35a 6 `"6.Estafa o abuso de confianza"', modify
label define s04f_35a 7 `"7.Despojo de su inmueble"', modify
label define s04f_35a 8 `" 8.Trata de personas"', modify
label define s04f_35a 9 `"9.Extorsi�n"', modify
label define s04f_35a 10 `" 10.Secuestro"', modify
label define s04f_35a 11 `"11. Otros hechos delictivos (ESPECIFIQUE)"', modify
label define s04f_35a 12 `"12.Ninguno"', modify
label define s04f_34 1 `"1.Muy inseguro"', modify
label define s04f_34 2 `"2.Inseguro"', modify
label define s04f_34 3 `"3.Seguro"', modify
label define s04f_34 4 `"4.Muy seguro"', modify
label define s04e_33b 1 `"1.Diariamente"', modify
label define s04e_33b 2 `"2.Una o dos veces por semana"', modify
label define s04e_33b 3 `"3.Una o dos veces por mes"', modify
label define s04e_33b 4 `"4. Ocasionalmente"', modify
label define s04e_33a 1 `"1. Si"', modify
label define s04e_33a 2 `"2. No"', modify
label define s04e_32b 1 `"1.Diariamente"', modify
label define s04e_32b 2 `"2.Una o dos veces por semana"', modify
label define s04e_32b 3 `"3.Una o dos veces por mes"', modify
label define s04e_32b 4 `"4. Ocasionalmente"', modify
label define s04e_32a 1 `"1. Si"', modify
label define s04e_32a 2 `"2. No"', modify
label define s04e_31f 1 `"1. Si"', modify
label define s04e_31f 2 `"2. No"', modify
label define s04e_31e 1 `"1. Si"', modify
label define s04e_31e 2 `"2. No"', modify
label define s04e_31d 1 `"1. Si"', modify
label define s04e_31d 2 `"2. No"', modify
label define s04e_31c 1 `"1. Si"', modify
label define s04e_31c 2 `"2. No"', modify
label define s04e_31b 1 `"1. Si"', modify
label define s04e_31b 2 `"2. No"', modify
label define s04e_31a 1 `"1. Si"', modify
label define s04e_31a 2 `"2. No"', modify
label define s04e_30a 1 `"1. Si"', modify
label define s04e_30a 2 `"2. No"', modify
label define s04e_29a 1 `"1. Si"', modify
label define s04e_29a 2 `"2. No"', modify
label define s04e_28a 1 `"1. Si"', modify
label define s04e_28a 2 `"2. No"', modify
label define s04d_27a 1 `"1. Si"', modify
label define s04d_27a 2 `"2. No"', modify
label define s04d_26 1 `"1. Si"', modify
label define s04d_26 2 `"2. No"', modify
label define s04d_25 1 `"1. Si"', modify
label define s04d_25 2 `"2. No"', modify
label define s04d_24 1 `"1. Si"', modify
label define s04d_24 2 `"2. No"', modify
label define s04c_23 1 `"1.Centro Infantil P�blico /Fiscal o de Convenio"', modify
label define s04c_23 2 `"2.Centro Infantil Privado"', modify
label define s04c_22 1 `"1. Si"', modify
label define s04c_22 2 `"2. No"', modify
label define s04b_21a 1 `"1. Si"', modify
label define s04b_21a 2 `"2. No"', modify
label define s04b_20b 1 `"1. Si"', modify
label define s04b_20b 2 `"2. No"', modify
label define s04b_20a1 1 `"1. Si"', modify
label define s04b_20a1 2 `"2. No"', modify
label define s04b_19 1 `"1. Si"', modify
label define s04b_19 2 `"2. No"', modify
label define s04b_18 1 `"1.Prestaciones del Servicio de Salud Integral (ex SUMI) ?"', modify
label define s04b_18 2 `"2.Seguro de Salud del Gobierno Aut�nomo Departamental o Municipal?"', modify
label define s04b_18 3 `"3.Caja de Salud?"', modify
label define s04b_18 4 `"4.Seguro Privado?"', modify
label define s04b_18 5 `"5.Otro (Especifique)"', modify
label define s04b_18 6 `"6.Ninguno?"', modify
label define s04b_17 1 `"1.ESTABLECIMIENTO DE SALUD DE LAS CAJAS (CNS, PETROLERA, CNC, SEG. UNIVERSITARIO"', modify
label define s04b_17 2 `"2.ESTABLECIMIENTO DE SALUD P�BLICO ATENDIDO S�LO POR ENFERMERA /AUXILIAR DE ENFE"', modify
label define s04b_17 3 `"3.ESTABLECIMIENTO DE SALUD P�BLICO, ATENDIDO POR UNO O M�S M�DICOS"', modify
label define s04b_17 4 `"4.ESTABLECIMIENTO DE SALUD PRIVADO, ATENDIDO POR UNO O M�S M�DICOS"', modify
label define s04b_17 5 `"5.DOMICILIO, ATENDIDO POR EL PROGRAMA-MI SALUD"', modify
label define s04b_17 6 `"6.DOMICILIO, ATENDIDO POR OTROS"', modify
label define s04b_17 7 `"7.OTRO (ESPECIFIQUE)"', modify
label define s04b_16 1 `"1.EL ESTABLECIMIENTO DE SALUD EST� MUY LEJOS O ES INACCESIBLE"', modify
label define s04b_16 2 `"2.CUESTA MUCHO DINERO LLEGAR AL ESTABLECIMIENTO DE SALUD"', modify
label define s04b_16 3 `"3.DESCONFIANZA EN EL SERVICIO"', modify
label define s04b_16 4 `"4.SU PAREJA NO LE PERMITE"', modify
label define s04b_16 5 `"5.CONF�A M�S EN LA MATRONA/PARTERA O ALG�N FAMILIAR"', modify
label define s04b_16 6 `"6.SE PIERDE MUCHO TIEMPO"', modify
label define s04b_16 7 `"7.OTRO (ESPECIFIQUE)"', modify
label define s04b_15 1 `"1.M�DICO"', modify
label define s04b_15 2 `"2.ENFERMERA/AUX.  DE  ENFERMER�A"', modify
label define s04b_15 3 `"3.RESPONSABLE O  PROMOTOR DE SALUD/AGENTE  COMUNITARIO EN SALUD"', modify
label define s04b_15 4 `"4.PARTERA O MATRONA"', modify
label define s04b_15 5 `"5.M�DICO TRADICIONAL /CURANDERO/NATURISTA"', modify
label define s04b_15 6 `"6.UN FAMILIAR"', modify
label define s04b_15 7 `"7.USTED MISMA"', modify
label define s04b_15 8 `"8.OTRA PERSONA (ESPECIFIQUE)"', modify
label define s04b_11a 1 `"1.Si, actualmente embarazada"', modify
label define s04b_11a 2 `"2.Si, estuvo embarazada"', modify
label define s04b_11a 3 `"3.No"', modify
label define s04a_09 1 `"1. Si"', modify
label define s04a_09 2 `"2. No"', modify
label define s04a_08b 1 `"1.Leve"', modify
label define s04a_08b 2 `"2.Moderado"', modify
label define s04a_08b 3 `"3.Grave"', modify
label define s04a_08b 4 `"4.Muy grave"', modify
label define s04a_08a 1 `"1. Si"', modify
label define s04a_08a 2 `"2. No"', modify
label define s04a_07g 1 `"1. Nacimiento?"', modify
label define s04a_07g 2 `"2. Enfermedad?"', modify
label define s04a_07g 3 `"3. Accidente?"', modify
label define s04a_07g 4 `"4. Edad avanzada?"', modify
label define s04a_07g 5 `"5. Por otra causa?"', modify
label define s04a_07f 1 `"1. Nacimiento?"', modify
label define s04a_07f 2 `"2. Enfermedad?"', modify
label define s04a_07f 3 `"3. Accidente?"', modify
label define s04a_07f 4 `"4. Edad avanzada?"', modify
label define s04a_07f 5 `"5. Por otra causa?"', modify
label define s04a_07e 1 `"1. Nacimiento?"', modify
label define s04a_07e 2 `"2. Enfermedad?"', modify
label define s04a_07e 3 `"3. Accidente?"', modify
label define s04a_07e 4 `"4. Edad avanzada?"', modify
label define s04a_07e 5 `"5. Por otra causa?"', modify
label define s04a_07d 1 `"1. Nacimiento?"', modify
label define s04a_07d 2 `"2. Enfermedad?"', modify
label define s04a_07d 3 `"3. Accidente?"', modify
label define s04a_07d 4 `"4. Edad avanzada?"', modify
label define s04a_07d 5 `"5. Por otra causa?"', modify
label define s04a_07c 1 `"1. Nacimiento?"', modify
label define s04a_07c 2 `"2. Enfermedad?"', modify
label define s04a_07c 3 `"3. Accidente?"', modify
label define s04a_07c 4 `"4. Edad avanzada?"', modify
label define s04a_07c 5 `"5. Por otra causa?"', modify
label define s04a_07b 1 `"1. Nacimiento?"', modify
label define s04a_07b 2 `"2. Enfermedad?"', modify
label define s04a_07b 3 `"3. Accidente?"', modify
label define s04a_07b 4 `"4. Edad avanzada?"', modify
label define s04a_07b 5 `"5. Por otra causa?"', modify
label define s04a_07a 1 `"1.Nacimiento?"', modify
label define s04a_07a 2 `"2.Enfermedad?"', modify
label define s04a_07a 3 `"3.Accidente?"', modify
label define s04a_07a 4 `"4.Edad avanzada ?"', modify
label define s04a_07a 5 `"5.Por otra causa?"', modify
label define s04a_06g 1 `"1.Ninguna dificultad"', modify
label define s04a_06g 2 `"2.Si, algo de dificultad"', modify
label define s04a_06g 3 `"3.Si,  mucha dificultad"', modify
label define s04a_06g 4 `"4.No puedo hacerlo"', modify
label define s04a_06f 1 `"1.Ninguna dificultad"', modify
label define s04a_06f 2 `"2.Si, algo de dificultad"', modify
label define s04a_06f 3 `"3.Si,  mucha dificultad"', modify
label define s04a_06f 4 `"4.No puedo hacerlo"', modify
label define s04a_06e 1 `"1.Ninguna dificultad"', modify
label define s04a_06e 2 `"2.Si, algo de dificultad"', modify
label define s04a_06e 3 `"3.Si,  mucha dificultad"', modify
label define s04a_06e 4 `"4.No puedo hacerlo"', modify
label define s04a_06d 1 `"1.Ninguna dificultad"', modify
label define s04a_06d 2 `"2.Si, algo de dificultad"', modify
label define s04a_06d 3 `"3.Si,  mucha dificultad"', modify
label define s04a_06d 4 `"4.No puedo hacerlo"', modify
label define s04a_06c 1 `"1.Ninguna dificultad"', modify
label define s04a_06c 2 `"2.Si, algo de dificultad"', modify
label define s04a_06c 3 `"3.Si,  mucha dificultad"', modify
label define s04a_06c 4 `"4.No puedo hacerlo"', modify
label define s04a_06b 1 `"1.Ninguna dificultad"', modify
label define s04a_06b 2 `"2.Si, algo de dificultad"', modify
label define s04a_06b 3 `"3.Si,  mucha dificultad"', modify
label define s04a_06b 4 `"4.No puedo hacerlo"', modify
label define s04a_06a 1 `"1.Ninguna dificultad"', modify
label define s04a_06a 2 `"2.Si, algo de dificultad"', modify
label define s04a_06a 3 `"3.Si,  mucha dificultad"', modify
label define s04a_06a 4 `"4.No puedo hacerlo"', modify
label define s04a_04b 1 `"1. Prestaciones del Servicio  de Salud Integral (antes SUMI y SPAM)"', modify
label define s04a_04b 2 `"2. CNS (Caja Nacional de Salud)/Caja de la Banca Privada/Banca Estatal/COSSMIL/S"', modify
label define s04a_04b 3 `"3. Seguros de Salud del Gobierno Aut�nomo Departamental o Municipal"', modify
label define s04a_04b 4 `"4. Seguros privados"', modify
label define s04a_04b 5 `"5. Otro(Especifique)"', modify
label define s04a_04b 6 `"6. Ninguno"', modify
label define s04a_04a 1 `"1. Prestaciones del Servicio  de Salud Integral (antes SUMI y SPAM)"', modify
label define s04a_04a 2 `"2. CNS (Caja Nacional de Salud)/Caja de la Banca Privada/Banca Estatal/COSSMIL/S"', modify
label define s04a_04a 3 `"3. Seguros de Salud del Gobierno Aut�nomo Departamental o Municipal"', modify
label define s04a_04a 4 `"4. Seguros privados"', modify
label define s04a_04a 5 `"5. Otro(Especifique)"', modify
label define s04a_04a 6 `"6. Ninguno"', modify
label define s04a_03g 1 `"1. Si"', modify
label define s04a_03g 2 `"2. No"', modify
label define s04a_03f 1 `"1. Si"', modify
label define s04a_03f 2 `"2. No"', modify
label define s04a_03e 1 `"1. Si"', modify
label define s04a_03e 2 `"2. No"', modify
label define s04a_03d 1 `"1. Si"', modify
label define s04a_03d 2 `"2. No"', modify
label define s04a_03c 1 `"1. Si"', modify
label define s04a_03c 2 `"2. No"', modify
label define s04a_03b 1 `"1. Si"', modify
label define s04a_03b 2 `"2. No"', modify
label define s04a_03a 1 `"1. Si"', modify
label define s04a_03a 2 `"2. No"', modify
label define s04a_02b 1 `"1. Malaria?"', modify
label define s04a_02b 2 `"2. Dengue?"', modify
label define s04a_02b 3 `"3. Leishmaniasis?"', modify
label define s04a_02b 4 `"4. Chikungunya?"', modify
label define s04a_02b 5 `"5. Zika?"', modify
label define s04a_02b 6 `"6. Gripe A(H1N1) u otra influenza?"', modify
label define s04a_02b 7 `"7. Otras (Especifique)"', modify
label define s04a_02b 8 `"8. Ninguna?"', modify
label define s04a_02a 1 `"1. Malaria?"', modify
label define s04a_02a 2 `"2. Dengue?"', modify
label define s04a_02a 3 `"3. Leishmaniasis?"', modify
label define s04a_02a 4 `"4. Chikungunya?"', modify
label define s04a_02a 5 `"5. Zika?"', modify
label define s04a_02a 6 `"6. Gripe A(H1N1) u otra influenza?"', modify
label define s04a_02a 7 `"7. Otras (Especifique)"', modify
label define s04a_02a 8 `"8. Ninguna?"', modify
label define s04a_01b 1 `"1. Diabetes?"', modify
label define s04a_01b 2 `"2. C�ncer?"', modify
label define s04a_01b 3 `"3. Enfermedad renal?"', modify
label define s04a_01b 4 `"4. Enfermedad del coraz�n?"', modify
label define s04a_01b 5 `"5. Tuberculosis?"', modify
label define s04a_01b 6 `"6. Chagas?"', modify
label define s04a_01b 7 `"7. Reumatismo/Artritis/Artrosis/Osteoporosis?"', modify
label define s04a_01b 8 `" 8. Enfermedad del h�gado?"', modify
label define s04a_01b 9 `"9. Hipertensi�n arterial?"', modify
label define s04a_01b 10 `"10. Otra enfermedad cr�nica (especifique)"', modify
label define s04a_01b 11 `"11. Ninguna?"', modify
label define s04a_01a 1 `"1. Diabetes?"', modify
label define s04a_01a 2 `"2. C�ncer?"', modify
label define s04a_01a 3 `"3. Enfermedad renal?"', modify
label define s04a_01a 4 `"4. Enfermedad del coraz�n?"', modify
label define s04a_01a 5 `"5. Tuberculosis?"', modify
label define s04a_01a 6 `"6. Chagas?"', modify
label define s04a_01a 7 `"7. Reumatismo/Artritis/Artrosis/Osteoporosis?"', modify
label define s04a_01a 8 `" 8. Enfermedad del h�gado?"', modify
label define s04a_01a 9 `"9. Hipertensi�n arterial?"', modify
label define s04a_01a 10 `"10. Otra enfermedad cr�nica (especifique)"', modify
label define s04a_01a 11 `"11. Ninguna?"', modify
label define s03a_04npioc 1 `"Afroboliviano"', modify
label define s03a_04npioc 2 `"Araona"', modify
label define s03a_04npioc 3 `"Aymara"', modify
label define s03a_04npioc 4 `"Ayoreo"', modify
label define s03a_04npioc 5 `"Baure"', modify
label define s03a_04npioc 6 `"Canichana"', modify
label define s03a_04npioc 7 `"Cavine�o"', modify
label define s03a_04npioc 8 `"Cayubaba"', modify
label define s03a_04npioc 9 `"Chacobo"', modify
label define s03a_04npioc 11 `"Chiquitano"', modify
label define s03a_04npioc 12 `"Esse Ejja"', modify
label define s03a_04npioc 13 `"Guaran�"', modify
label define s03a_04npioc 14 `"Guarasugwe"', modify
label define s03a_04npioc 15 `"Guarayo"', modify
label define s03a_04npioc 16 `"Itonama"', modify
label define s03a_04npioc 17 `"Joaquiniano"', modify
label define s03a_04npioc 18 `"Kallawaya"', modify
label define s03a_04npioc 19 `"Leco"', modify
label define s03a_04npioc 20 `"Machiner�"', modify
label define s03a_04npioc 21 `"Maropa"', modify
label define s03a_04npioc 22 `"Moje�o"', modify
label define s03a_04npioc 23 `"Mor�"', modify
label define s03a_04npioc 24 `"Moset�n"', modify
label define s03a_04npioc 25 `"Movima"', modify
label define s03a_04npioc 26 `"Murato"', modify
label define s03a_04npioc 27 `"Pacahuara"', modify
label define s03a_04npioc 28 `"Quechua"', modify
label define s03a_04npioc 29 `"Sirion�"', modify
label define s03a_04npioc 30 `"Tacana"', modify
label define s03a_04npioc 31 `"Tapiete"', modify
label define s03a_04npioc 32 `"Tsimane Chiman"', modify
label define s03a_04npioc 33 `"Weenayek"', modify
label define s03a_04npioc 34 `"Yaminahua"', modify
label define s03a_04npioc 35 `"Yuki"', modify
label define s03a_04npioc 36 `"Yuracar�"', modify
label define s03a_04npioc 37 `"Yuracar� - Moje�o"', modify
label define s03a_04npioc 39 `"Jacha carangas"', modify
label define s03a_04npioc 40 `"Uru Chipayas"', modify
label define s03a_04npioc 42 `"Chichas"', modify
label define s03a_04npioc 43 `"Mestizo"', modify
label define s03a_04npioc 44 `"Yampara"', modify
label define s03a_04npioc 45 `"Qhara qhara"', modify
label define s03a_04npioc 46 `"Chiriguano"', modify
label define s03a_04npioc 47 `"Killacas"', modify
label define s03a_04npioc 49 `"Lipez"', modify
label define s03a_04npioc 50 `"Monkox"', modify
label define s03a_04npioc 52 `"Jatun Killacas"', modify
label define s03a_04npioc 54 `"Poroma"', modify
label define s03a_04npioc 56 `"Suyu sura"', modify
label define s03a_04npioc 57 `"Pucara"', modify
label define s03a_04npioc 60 `"Suyu Chuwi"', modify
label define s03a_04npioc 65 `"Calcha"', modify
label define s03a_04npioc 66 `"Chullpas"', modify
label define s03a_04npioc 69 `"Ayllu Yura"', modify
label define s03a_04npioc 71 `"Ayllu Porco"', modify
label define s03a_04npioc 72 `"Suyu Charcas"', modify
label define s03a_04npioc 73 `"Ayllu Kacachaca"', modify
label define s03a_04npioc 74 `"Ayllu Jukumani"', modify
label define s03a_04npioc 75 `"Ayllu Jila"', modify
label define s03a_04npioc 76 `"Ayllu Jalka"', modify
label define s03a_04npioc 78 `"Urus"', modify
label define s03a_04npioc 79 `"Quila Quila"', modify
label define s03a_04npioc 81 `"Chaqui"', modify
label define s03a_04npioc 83 `"Tinquipaya"', modify
label define s03a_04npioc 84 `"Moro moro"', modify
label define s03a_04npioc 85 `"Pati Pati"', modify
label define s03a_04npioc 87 `"San Juan"', modify
label define s03a_04npioc 90 `"Pojpo"', modify
label define s03a_04npioc 92 `"Larecaja"', modify
label define s03a_04npioc 93 `"Qollas"', modify
label define s03a_04npioc 102 `"Jach`a Pacajaqui"', modify
label define s03a_04npioc 105 `"Yapacan�"', modify
label define s03a_04npioc 106 `"Qhapaq Uma Suyu"', modify
label define s03a_04npioc 108 `"Quillacas"', modify
label define s03a_04npioc 109 `"Condo"', modify
label define s03a_04npioc 112 `"Challapata"', modify
label define s03a_04npioc 113 `"Lagunillas"', modify
label define s03a_04npioc 114 `"Urmiri de Quillacas"', modify
label define s03a_04npioc 116 `"Pampa Aullagas"', modify
label define s03a_04npioc 117 `"Salinas"', modify
label define s03a_04npioc 118 `"suari"', modify
label define s03a_04npioc 119 `"Aroma"', modify
label define s03a_04npioc 120 `"Ucumasi"', modify
label define s03a_04npioc 124 `"Totora Marka"', modify
label define s03a_04npioc 125 `"Choquecota"', modify
label define s03a_04npioc 126 `"Curahuara de Carangas"', modify
label define s03a_04npioc 127 `"Turco"', modify
label define s03a_04npioc 128 `"suaylla Marka"', modify
label define s03a_04npioc 130 `"Corque"', modify
label define s03a_04npioc 131 `"suachacalla"', modify
label define s03a_04npioc 132 `"Andamarca"', modify
label define s03a_04npioc 133 `"Orinoca"', modify
label define s03a_04npioc 135 `"Sabaya"', modify
label define s03a_04npioc 137 `"Nacion Coroma"', modify
label define s03a_04npioc 140 `"Menonita"', modify
label define s03a_04npioc 141 `"Tobas"', modify
label define s03a_04npioc 143 `"Akarapis"', modify
label define s03a_04npioc 145 `"Mojocoya"', modify
label define s03a_04npioc 146 `"Charazani"', modify
label define s03a_04npioc 147 `"Layme"', modify
label define s03a_04npioc 148 `"Pojos"', modify
label define s03a_04npioc 149 `"Belen"', modify
label define s03a_04npioc 345 `"Uchupiamonas"', modify
label define s03a_04npioc 350 `"Pukina"', modify
label define s03a_04npioc 355 `"Charagua"', modify
label define s03a_04npioc 356 `"Jes�s de Machaca"', modify
label define s03a_04npioc 357 `"Chayanta"', modify
label define s03a_04npioc 358 `"Okinawa"', modify
label define s03a_04npioc 410 `"Ayllu Kharacha"', modify
label define s03a_04npioc 412 `"IOC sin descripci�n precisa"', modify
label define s03a_04npioc 451 `"Mataco"', modify
label define s03a_04npioc 454 `"Trinitario"', modify
label define s03a_04npioc 455 `"Ignaciano"', modify
label define s03a_04npioc 456 `"Loretano"', modify
label define s03a_04npioc 457 `"Javeriano"', modify
label define s03a_04npioc 458 `"Reyesano"', modify
label define s03a_04npioc 460 `"Originario"', modify
label define s03a_04npioc 461 `"Ind�gena u originario no especificado"', modify
label define s03a_04npioc 463 `"Besiro"', modify
label define s03a_04npioc 466 `"Uru-ito"', modify
label define s03a_04npioc 600 `"Intercultural"', modify
label define s03a_04npioc 700 `"Campesino"', modify
label define s03a_04npioc 996 `"Indigena"', modify
label define s03a_04npioc 997 `"La Descripci�n no corresponde al nombre de un pueblo"', modify
label define s03a_04npioc 999 `"Omisi�n"', modify
label define s03a_04 1 `"1. Pertenece"', modify
label define s03a_04 2 `"2. No pertenece"', modify
label define s03a_04 3 `"3. No soy boliviana o boliviano"', modify
label define s03a_02 1 `"1. B�SQUEDA DE TRABAJO"', modify
label define s03a_02 2 `"2. TRASLADO DE TRABAJO"', modify
label define s03a_02 3 `"3. EDUCACI�N"', modify
label define s03a_02 4 `"4. SALUD"', modify
label define s03a_02 5 `"5. RAZ�N FAMILIAR"', modify
label define s03a_02 6 `"6. OTRA RAZ�N (especifique)"', modify
label define s03a_01b 10101 `"Chuquisaca - Oropeza - Sucre"', modify
label define s03a_01b 10102 `"Chuquisaca - Oropeza - Yotala"', modify
label define s03a_01b 10103 `"Chuquisaca - Oropeza - Poroma"', modify
label define s03a_01b 10201 `"Chuquisaca - Azurduy - Azurduy"', modify
label define s03a_01b 10202 `"Chuquisaca - Azurduy - Tarvita"', modify
label define s03a_01b 10301 `"Chuquisaca - Zuda�ez - Zud��ez"', modify
label define s03a_01b 10302 `"Chuquisaca - Zuda�ez - Presto"', modify
label define s03a_01b 10303 `"Chuquisaca - Zuda�ez - Mojocoya"', modify
label define s03a_01b 10304 `"Chuquisaca - Zuda�ez - Icla"', modify
label define s03a_01b 10401 `"Chuquisaca - Tomina - Padilla"', modify
label define s03a_01b 10402 `"Chuquisaca - Tomina - Tomina"', modify
label define s03a_01b 10403 `"Chuquisaca - Tomina - Sopachuy"', modify
label define s03a_01b 10404 `"Chuquisaca - Tomina - Villa Alcal�"', modify
label define s03a_01b 10405 `"Chuquisaca - Tomina - El Villar"', modify
label define s03a_01b 10501 `"Chuquisaca - Hernando Siles - Monteagudo"', modify
label define s03a_01b 10502 `"Chuquisaca - Hernando Siles - Huacareta"', modify
label define s03a_01b 10601 `"Chuquisaca - Yamparaez - Tarabuco"', modify
label define s03a_01b 10602 `"Chuquisaca - Yamparaez - Yampar�ez"', modify
label define s03a_01b 10701 `"Chuquisaca - Nor Cinti - Camargo"', modify
label define s03a_01b 10702 `"Chuquisaca - Nor Cinti - San Lucas"', modify
label define s03a_01b 10703 `"Chuquisaca - Nor Cinti - Incahuasi"', modify
label define s03a_01b 10704 `"Chuquisaca - Nor Cinti - Villa Charcas"', modify
label define s03a_01b 10801 `"Chuquisaca - Belisario Boeto - Villa Serrano"', modify
label define s03a_01b 10901 `"Chuquisaca - Sud Cinti - Villa Abecia"', modify
label define s03a_01b 10902 `"Chuquisaca - Sud Cinti - Culpina"', modify
label define s03a_01b 10903 `"Chuquisaca - Sud Cinti - Las Carreras"', modify
label define s03a_01b 11001 `"Chuquisaca - Luis Calvo - Muyupampa"', modify
label define s03a_01b 11002 `"Chuquisaca - Luis Calvo - Huacaya"', modify
label define s03a_01b 11003 `"Chuquisaca - Luis Calvo - Macharet�"', modify
label define s03a_01b 20101 `"La Paz - Murillo - La Paz"', modify
label define s03a_01b 20102 `"La Paz - Murillo - Palca"', modify
label define s03a_01b 20103 `"La Paz - Murillo - Mecapaca"', modify
label define s03a_01b 20104 `"La Paz - Murillo - Achocalla"', modify
label define s03a_01b 20105 `"La Paz - Murillo - El Alto"', modify
label define s03a_01b 20201 `"La Paz - Omasuyos - Achacachi"', modify
label define s03a_01b 20202 `"La Paz - Omasuyos - Ancoraimes"', modify
label define s03a_01b 20203 `"La Paz - Omasuyos - Chua Cocani"', modify
label define s03a_01b 20204 `"La Paz - Omasuyos - Huarina"', modify
label define s03a_01b 20205 `"La Paz - Omasuyos - Santiago de Huata"', modify
label define s03a_01b 20206 `"La Paz - Omasuyos - Huatajata"', modify
label define s03a_01b 20301 `"La Paz - Pacajes - Corocoro"', modify
label define s03a_01b 20302 `"La Paz - Pacajes - Caquiaviri"', modify
label define s03a_01b 20303 `"La Paz - Pacajes - Calacoto"', modify
label define s03a_01b 20304 `"La Paz - Pacajes - Comanche"', modify
label define s03a_01b 20305 `"La Paz - Pacajes - Chara�a"', modify
label define s03a_01b 20306 `"La Paz - Pacajes - Waldo Ballivi�n"', modify
label define s03a_01b 20307 `"La Paz - Pacajes - Nazacara de Pacajes"', modify
label define s03a_01b 20308 `"La Paz - Pacajes - Callapa"', modify
label define s03a_01b 20401 `"La Paz - Camacho - Puerto Acosta"', modify
label define s03a_01b 20402 `"La Paz - Camacho - Mocomoco"', modify
label define s03a_01b 20403 `"La Paz - Camacho - Puerto Carabuco"', modify
label define s03a_01b 20404 `"La Paz - Camacho - Humanata"', modify
label define s03a_01b 20405 `"La Paz - Camacho - Escoma"', modify
label define s03a_01b 20501 `"La Paz - Mu�ecas - Chuma"', modify
label define s03a_01b 20502 `"La Paz - Mu�ecas - Ayata"', modify
label define s03a_01b 20503 `"La Paz - Mu�ecas - Aucapata"', modify
label define s03a_01b 20601 `"La Paz - Larecaja - Sorata"', modify
label define s03a_01b 20602 `"La Paz - Larecaja - Guanay"', modify
label define s03a_01b 20603 `"La Paz - Larecaja - Tacacoma"', modify
label define s03a_01b 20604 `"La Paz - Larecaja - Quiabaya"', modify
label define s03a_01b 20605 `"La Paz - Larecaja - Combaya"', modify
label define s03a_01b 20606 `"La Paz - Larecaja - Tipuani"', modify
label define s03a_01b 20607 `"La Paz - Larecaja - Mapiri"', modify
label define s03a_01b 20608 `"La Paz - Larecaja - Teoponte"', modify
label define s03a_01b 20701 `"La Paz - Franz Tamayo - Apolo"', modify
label define s03a_01b 20702 `"La Paz - Franz Tamayo - Pelechuco"', modify
label define s03a_01b 20801 `"La Paz - Ingavi - Viacha"', modify
label define s03a_01b 20802 `"La Paz - Ingavi - Guaqui"', modify
label define s03a_01b 20803 `"La Paz - Ingavi - Tiahuanacu"', modify
label define s03a_01b 20804 `"La Paz - Ingavi - Desaguadero"', modify
label define s03a_01b 20805 `"La Paz - Ingavi - San Andr�s de Machaca"', modify
label define s03a_01b 20806 `"La Paz - Ingavi - Jes�s de Machaca"', modify
label define s03a_01b 20807 `"La Paz - Ingavi - Taraco"', modify
label define s03a_01b 20901 `"La Paz - Loayza - Luribay"', modify
label define s03a_01b 20902 `"La Paz - Loayza - Sapahaqui"', modify
label define s03a_01b 20903 `"La Paz - Loayza - Yaco"', modify
label define s03a_01b 20904 `"La Paz - Loayza - Malla"', modify
label define s03a_01b 20905 `"La Paz - Loayza - Cairoma"', modify
label define s03a_01b 21001 `"La Paz - Inquisivi - Inquisivi"', modify
label define s03a_01b 21002 `"La Paz - Inquisivi - Quime"', modify
label define s03a_01b 21003 `"La Paz - Inquisivi - Cajuata"', modify
label define s03a_01b 21004 `"La Paz - Inquisivi - Colquiri"', modify
label define s03a_01b 21005 `"La Paz - Inquisivi - Ichoca"', modify
label define s03a_01b 21006 `"La Paz - Inquisivi - Villa Libertad Licoma"', modify
label define s03a_01b 21101 `"La Paz - Sud Yungas - Chulumani"', modify
label define s03a_01b 21102 `"La Paz - Sud Yungas - Irupana"', modify
label define s03a_01b 21103 `"La Paz - Sud Yungas - Yanacachi"', modify
label define s03a_01b 21104 `"La Paz - Sud Yungas - Palos Blancos"', modify
label define s03a_01b 21105 `"La Paz - Sud Yungas - La Asunta"', modify
label define s03a_01b 21201 `"La Paz - Los Andes - Pucarani"', modify
label define s03a_01b 21202 `"La Paz - Los Andes - Laja"', modify
label define s03a_01b 21203 `"La Paz - Los Andes - Batallas"', modify
label define s03a_01b 21204 `"La Paz - Los Andes - Puerto P�rez"', modify
label define s03a_01b 21301 `"La Paz - Aroma - Sica Sica"', modify
label define s03a_01b 21302 `"La Paz - Aroma - Umala"', modify
label define s03a_01b 21303 `"La Paz - Aroma - Ayo Ayo"', modify
label define s03a_01b 21304 `"La Paz - Aroma - Calamarca"', modify
label define s03a_01b 21305 `"La Paz - Aroma - Patacamaya"', modify
label define s03a_01b 21306 `"La Paz - Aroma - Colquencha"', modify
label define s03a_01b 21307 `"La Paz - Aroma - Collana"', modify
label define s03a_01b 21401 `"La Paz - Nor Yungas - Coroico"', modify
label define s03a_01b 21402 `"La Paz - Nor Yungas - Coripata"', modify
label define s03a_01b 21501 `"La Paz - Abel Iturralde - Ixiamas"', modify
label define s03a_01b 21502 `"La Paz - Abel Iturralde - San Buenaventura"', modify
label define s03a_01b 21601 `"La Paz - Bautista Saavedra - Charazani"', modify
label define s03a_01b 21602 `"La Paz - Bautista Saavedra - Curva"', modify
label define s03a_01b 21701 `"La Paz - Manco Kapac - Copacabana"', modify
label define s03a_01b 21702 `"La Paz - Manco Kapac - San Pedro de Tiquina"', modify
label define s03a_01b 21703 `"La Paz - Manco Kapac - Tito Yupanqui"', modify
label define s03a_01b 21801 `"La Paz - Gualberto Villarroel - San Pedro de Curahuara"', modify
label define s03a_01b 21802 `"La Paz - Gualberto Villarroel - Papel Pampa"', modify
label define s03a_01b 21803 `"La Paz - Gualberto Villarroel - Chacarilla"', modify
label define s03a_01b 21901 `"La Paz - Jos� Manuel Pando - Santiago de Machaca"', modify
label define s03a_01b 21902 `"La Paz - Jos� Manuel Pando - Catacora"', modify
label define s03a_01b 22001 `"La Paz - Caranavi - Caranavi"', modify
label define s03a_01b 22002 `"La Paz - Caranavi - Alto Beni"', modify
label define s03a_01b 30101 `"Cochabamba - Cercado - Cochabamba"', modify
label define s03a_01b 30201 `"Cochabamba - Campero - Aiquile"', modify
label define s03a_01b 30202 `"Cochabamba - Campero - Pasorapa"', modify
label define s03a_01b 30203 `"Cochabamba - Campero - Omereque"', modify
label define s03a_01b 30301 `"Cochabamba - Ayopaya - Independencia"', modify
label define s03a_01b 30302 `"Cochabamba - Ayopaya - Morochata"', modify
label define s03a_01b 30303 `"Cochabamba - Ayopaya - Cocapata"', modify
label define s03a_01b 30401 `"Cochabamba - Esteban Arce - Tarata"', modify
label define s03a_01b 30402 `"Cochabamba - Esteban Arce - Anzaldo"', modify
label define s03a_01b 30403 `"Cochabamba - Esteban Arce - Arbieto"', modify
label define s03a_01b 30404 `"Cochabamba - Esteban Arce - Sacabamba"', modify
label define s03a_01b 30501 `"Cochabamba - Arani - Arani"', modify
label define s03a_01b 30502 `"Cochabamba - Arani - Vacas"', modify
label define s03a_01b 30601 `"Cochabamba - Arque - Arque"', modify
label define s03a_01b 30602 `"Cochabamba - Arque - Tacopaya"', modify
label define s03a_01b 30701 `"Cochabamba - Capinota - Capinota"', modify
label define s03a_01b 30702 `"Cochabamba - Capinota - Santiv��ez"', modify
label define s03a_01b 30703 `"Cochabamba - Capinota - Sicaya"', modify
label define s03a_01b 30801 `"Cochabamba - German Jord�n - Cliza"', modify
label define s03a_01b 30802 `"Cochabamba - German Jord�n - Toco"', modify
label define s03a_01b 30803 `"Cochabamba - German Jord�n - Tolata"', modify
label define s03a_01b 30901 `"Cochabamba - Quillacollo - Quillacollo"', modify
label define s03a_01b 30902 `"Cochabamba - Quillacollo - Sipesipe"', modify
label define s03a_01b 30903 `"Cochabamba - Quillacollo - Tiquipaya"', modify
label define s03a_01b 30904 `"Cochabamba - Quillacollo - Vinto"', modify
label define s03a_01b 30905 `"Cochabamba - Quillacollo - Colcapirhua"', modify
label define s03a_01b 31001 `"Cochabamba - Chapare - Sacaba"', modify
label define s03a_01b 31002 `"Cochabamba - Chapare - Colomi"', modify
label define s03a_01b 31003 `"Cochabamba - Chapare - Villa Tunari"', modify
label define s03a_01b 31101 `"Cochabamba - Tapacar� - Tapacar�"', modify
label define s03a_01b 31201 `"Cochabamba - Carrasco - Totora"', modify
label define s03a_01b 31202 `"Cochabamba - Carrasco - Pojo"', modify
label define s03a_01b 31203 `"Cochabamba - Carrasco - Pocona"', modify
label define s03a_01b 31204 `"Cochabamba - Carrasco - Chimor�"', modify
label define s03a_01b 31205 `"Cochabamba - Carrasco - Puerto Villarroel"', modify
label define s03a_01b 31206 `"Cochabamba - Carrasco - Entre R�os"', modify
label define s03a_01b 31301 `"Cochabamba - Mizque - Mizque"', modify
label define s03a_01b 31302 `"Cochabamba - Mizque - Vila Vila"', modify
label define s03a_01b 31303 `"Cochabamba - Mizque - Alalay"', modify
label define s03a_01b 31401 `"Cochabamba - Punata - Punata"', modify
label define s03a_01b 31402 `"Cochabamba - Punata - Villa Rivero"', modify
label define s03a_01b 31403 `"Cochabamba - Punata - San Benito"', modify
label define s03a_01b 31404 `"Cochabamba - Punata - Tacachi"', modify
label define s03a_01b 31405 `"Cochabamba - Punata - Cuchumuela"', modify
label define s03a_01b 31501 `"Cochabamba - Bol�var - Bol�var"', modify
label define s03a_01b 31601 `"Cochabamba - Tiraque - Tiraque"', modify
label define s03a_01b 31602 `"Cochabamba - Tiraque - Shinahota"', modify
label define s03a_01b 40101 `"Oruro - Cercado - Oruro"', modify
label define s03a_01b 40102 `"Oruro - Cercado - Caracollo"', modify
label define s03a_01b 40103 `"Oruro - Cercado - El Choro"', modify
label define s03a_01b 40104 `"Soracachi - Paria - Pari"', modify
label define s03a_01b 40201 `"Oruro - Eduardo Avaroa - Challapata"', modify
label define s03a_01b 40202 `"Oruro - Eduardo Avaroa - Quillacas"', modify
label define s03a_01b 40301 `"Oruro - Carangas - Corque"', modify
label define s03a_01b 40302 `"Oruro - Carangas - Choque Cota"', modify
label define s03a_01b 40401 `"Oruro - Sajama - Curahuara de Carangas"', modify
label define s03a_01b 40402 `"Oruro - Sajama - Turco"', modify
label define s03a_01b 40501 `"Oruro - Litoral de Atacama - Huachacalla"', modify
label define s03a_01b 40502 `"Oruro - Litoral de Atacama - Escara"', modify
label define s03a_01b 40503 `"Oruro - Litoral de Atacama - Cruz de Machacamarca"', modify
label define s03a_01b 40504 `"Oruro - Litoral de Atacama - Yunguyo de Litoral"', modify
label define s03a_01b 40505 `"Oruro - Litoral de Atacama - Esmeralda"', modify
label define s03a_01b 40601 `"Oruro - Poopo - Poop�"', modify
label define s03a_01b 40602 `"Oruro - Poopo - Paz�a"', modify
label define s03a_01b 40603 `"Oruro - Poopo - Antequera"', modify
label define s03a_01b 40701 `"Oruro - Pantaleon Dalence - Huanuni"', modify
label define s03a_01b 40702 `"Oruro - Pantaleon Dalence - Machacamarca"', modify
label define s03a_01b 40801 `"Oruro - Ladislao Cabrera - Salinas de Garci Mendoza"', modify
label define s03a_01b 40802 `"Oruro - Ladislao Cabrera - Pampa Aullagas"', modify
label define s03a_01b 40901 `"Oruro - Atahuallpa - Sabaya"', modify
label define s03a_01b 40902 `"Oruro - Atahuallpa - Coipasa"', modify
label define s03a_01b 40903 `"Oruro - Atahuallpa - Chipaya"', modify
label define s03a_01b 41001 `"Oruro - Saucari - Toledo"', modify
label define s03a_01b 41101 `"Oruro - Tomas Barron - Eucaliptus"', modify
label define s03a_01b 41201 `"Oruro - Sud Carangas - Santiago de Andamarca"', modify
label define s03a_01b 41202 `"Oruro - Sud Carangas - Bel�n de Andamarca"', modify
label define s03a_01b 41301 `"Oruro - San Pedro de Totora - San Pedro de Totora"', modify
label define s03a_01b 41401 `"Oruro - Sebasti�n Pagador - Huari"', modify
label define s03a_01b 41501 `"Oruro - Puerto de Mejillones - La Rivera"', modify
label define s03a_01b 41502 `"Oruro - Puerto de Mejillones - Todos Santos"', modify
label define s03a_01b 41503 `"Oruro - Puerto de Mejillones - Carangas"', modify
label define s03a_01b 41601 `"Oruro - Nor Carangas - Huayllamarca"', modify
label define s03a_01b 50101 `"Potos� - Tomas Frias - Potos�"', modify
label define s03a_01b 50102 `"Potos� - Tomas Frias - Tinguipaya"', modify
label define s03a_01b 50103 `"Potos� - Tomas Frias - Yocalla"', modify
label define s03a_01b 50104 `"Potos� - Tomas Frias - Urmiri"', modify
label define s03a_01b 50201 `"Potos� - Rafael Bustillo - Unc�a"', modify
label define s03a_01b 50202 `"Potos� - Rafael Bustillo - Chayanta"', modify
label define s03a_01b 50203 `"Potos� - Rafael Bustillo - Llallagua"', modify
label define s03a_01b 50204 `"Potos� - Rafael Bustillo - Chuquihuta"', modify
label define s03a_01b 50301 `"Potos� - Cornelio Saavedra - Betanzos"', modify
label define s03a_01b 50302 `"Potos� - Cornelio Saavedra - Chaqu�"', modify
label define s03a_01b 50303 `"Potos� - Cornelio Saavedra - Tacobamba"', modify
label define s03a_01b 50401 `"Potos� - Chayanta - Colquechaca"', modify
label define s03a_01b 50402 `"Potos� - Chayanta - Ravelo"', modify
label define s03a_01b 50403 `"Potos� - Chayanta - Pocoata"', modify
label define s03a_01b 50404 `"Potos� - Chayanta - Ocur�"', modify
label define s03a_01b 50501 `"Potos� - Charcas - San Pedro de Buena Vista"', modify
label define s03a_01b 50502 `"Potos� - Charcas - Toro Toro"', modify
label define s03a_01b 50601 `"Potos� - Nor Chichas - Cotagaita"', modify
label define s03a_01b 50602 `"Potos� - Nor Chichas - Vitichi"', modify
label define s03a_01b 50701 `"Potos� - Alonso de Iba�ez - Villa de Sacaca"', modify
label define s03a_01b 50702 `"Potos� - Alonso de Iba�ez - Caripuyo"', modify
label define s03a_01b 50801 `"Potos� - Sur Chichas - Tupiza"', modify
label define s03a_01b 50802 `"Potos� - Sur Chichas - Atocha"', modify
label define s03a_01b 50901 `"Potos� - Nor L�pez - Colcha"', modify
label define s03a_01b 50902 `"Potos� - Nor L�pez - San Pedro de Quemes"', modify
label define s03a_01b 51001 `"Potos� - Sur L�pez - San Pablo de L�pez"', modify
label define s03a_01b 51002 `"Potos� - Sur L�pez - Mojinete"', modify
label define s03a_01b 51003 `"Potos� - Sur L�pez - San Antonio de Esmoruco"', modify
label define s03a_01b 51101 `"Potos� - Jos� Maria Linares - Puna"', modify
label define s03a_01b 51102 `"Potos� - Jos� Maria Linares - Caiza"', modify
label define s03a_01b 51103 `"Potos� - Jos� Maria Linares - Ckochas"', modify
label define s03a_01b 51201 `"Potos� - Antonio Quijarro - Uyuni"', modify
label define s03a_01b 51202 `"Potos� - Antonio Quijarro - Tomave"', modify
label define s03a_01b 51203 `"Potos� - Antonio Quijarro - Porco"', modify
label define s03a_01b 51301 `"Potos� - Bernardino Bilbao Rioja - Arampampa"', modify
label define s03a_01b 51302 `"Potos� - Bernardino Bilbao Rioja - Acasio"', modify
label define s03a_01b 51401 `"Potos� - Daniel Campos - Llica"', modify
label define s03a_01b 51402 `"Potos� - Daniel Campos - Tahua"', modify
label define s03a_01b 51501 `"Potos� - Modesto Omiste - Villaz�n"', modify
label define s03a_01b 51601 `"Potos� - Enrique Baldivieso - San Agust�n"', modify
label define s03a_01b 60101 `"Tarija - Cercado - Tarija"', modify
label define s03a_01b 60201 `"Tarija - Aniceto Arce - Padcaya"', modify
label define s03a_01b 60202 `"Tarija - Aniceto Arce - Bermejo"', modify
label define s03a_01b 60301 `"Tarija - Gran Chaco - Yacuiba"', modify
label define s03a_01b 60302 `"Tarija - Gran Chaco - Carapar�"', modify
label define s03a_01b 60303 `"Tarija - Gran Chaco - Villamontes"', modify
label define s03a_01b 60401 `"Tarija - Aviles - Uriondo"', modify
label define s03a_01b 60402 `"Tarija - Aviles - Yunchar�"', modify
label define s03a_01b 60501 `"Tarija - M�ndez - Villa San Lorenzo"', modify
label define s03a_01b 60502 `"Tarija - M�ndez - El Puente"', modify
label define s03a_01b 60601 `"Tarija - Burnet OConnor - Entre R�os"', modify
label define s03a_01b 70101 `"Santa Cruz - Andr�s Iba�ez - Santa Cruz de la Sierra"', modify
label define s03a_01b 70102 `"Santa Cruz - Andr�s Iba�ez - Cotoca"', modify
label define s03a_01b 70103 `"Santa Cruz - Andr�s Iba�ez - Porongo"', modify
label define s03a_01b 70104 `"Santa Cruz - Andr�s Iba�ez - La Guardia"', modify
label define s03a_01b 70105 `"Santa Cruz - Andr�s Iba�ez - El Torno"', modify
label define s03a_01b 70201 `"Santa Cruz - Warnes - Warnes"', modify
label define s03a_01b 70202 `"Santa Cruz - Warnes - Okinawa Uno"', modify
label define s03a_01b 70301 `"Santa Cruz - Velasco - San Ignacio de Velasco"', modify
label define s03a_01b 70302 `"Santa Cruz - Velasco - San Miguel de Velasco"', modify
label define s03a_01b 70303 `"Santa Cruz - Velasco - San Rafael"', modify
label define s03a_01b 70401 `"Santa Cruz - Ichilo - Buena Vista"', modify
label define s03a_01b 70402 `"Santa Cruz - Ichilo - San Carlos"', modify
label define s03a_01b 70403 `"Santa Cruz - Ichilo - Yapacan�"', modify
label define s03a_01b 70404 `"Santa Cruz - Ichilo - San Juan de Yapacan�"', modify
label define s03a_01b 70501 `"Santa Cruz - Chiquitos - San Jos� de Chiquitos"', modify
label define s03a_01b 70502 `"Santa Cruz - Chiquitos - Pail�n"', modify
label define s03a_01b 70503 `"Santa Cruz - Chiquitos - Robor�"', modify
label define s03a_01b 70601 `"Santa Cruz - Sara - Portachuelo"', modify
label define s03a_01b 70602 `"Santa Cruz - Sara - Santa Rosa del Sara"', modify
label define s03a_01b 70603 `"Santa Cruz - Sara - Colpa B�lgica"', modify
label define s03a_01b 70701 `"Santa Cruz - Cordillera - Lagunillas"', modify
label define s03a_01b 70702 `"Santa Cruz - Cordillera - Charagua"', modify
label define s03a_01b 70703 `"Santa Cruz - Cordillera - Cabezas"', modify
label define s03a_01b 70704 `"Santa Cruz - Cordillera - Cuevo"', modify
label define s03a_01b 70705 `"Santa Cruz - Cordillera - Guti�rrez"', modify
label define s03a_01b 70706 `"Santa Cruz - Cordillera - Camiri"', modify
label define s03a_01b 70707 `"Santa Cruz - Cordillera - Boyuibe"', modify
label define s03a_01b 70801 `"Santa Cruz - Vallegrande - Vallegrande"', modify
label define s03a_01b 70802 `"Santa Cruz - Vallegrande - Trigal"', modify
label define s03a_01b 70803 `"Santa Cruz - Vallegrande - Moro Moro"', modify
label define s03a_01b 70804 `"Santa Cruz - Vallegrande - Postrer Valle"', modify
label define s03a_01b 70805 `"Santa Cruz - Vallegrande - Pucar�"', modify
label define s03a_01b 70901 `"Santa Cruz - Florida - Samaipata"', modify
label define s03a_01b 70902 `"Santa Cruz - Florida - Pampa Grande"', modify
label define s03a_01b 70903 `"Santa Cruz - Florida - Mairana"', modify
label define s03a_01b 70904 `"Santa Cruz - Florida - Quirusillas"', modify
label define s03a_01b 71001 `"Santa Cruz - Obispo Santiestevan - Montero"', modify
label define s03a_01b 71002 `"Santa Cruz - Obispo Santiestevan - Gral. Saavedra"', modify
label define s03a_01b 71003 `"Santa Cruz - Obispo Santiestevan - Mineros"', modify
label define s03a_01b 71004 `"Santa Cruz - Obispo Santiestevan - Fern�ndez Alonso"', modify
label define s03a_01b 71005 `"Santa Cruz - Obispo Santiestevan - San Pedro"', modify
label define s03a_01b 71101 `"Santa Cruz - �uflo de Ch�vez - Concepci�n"', modify
label define s03a_01b 71102 `"Santa Cruz - �uflo de Ch�vez - San Javier"', modify
label define s03a_01b 71103 `"Santa Cruz - �uflo de Ch�vez - San Ram�n"', modify
label define s03a_01b 71104 `"Santa Cruz - �uflo de Ch�vez - San Juli�n"', modify
label define s03a_01b 71105 `"Santa Cruz - �uflo de Ch�vez - San Antonio de Lomer�o"', modify
label define s03a_01b 71106 `"Santa Cruz - �uflo de Ch�vez - Cuatro Ca�adas"', modify
label define s03a_01b 71201 `"Santa Cruz - Angel Sandoval - San Mat�as"', modify
label define s03a_01b 71301 `"Santa Cruz - Manuel Maria Caballero - Comarapa"', modify
label define s03a_01b 71302 `"Santa Cruz - Manuel Maria Caballero - Saipina"', modify
label define s03a_01b 71401 `"Santa Cruz - German Busch - Puerto Su�rez"', modify
label define s03a_01b 71402 `"Santa Cruz - German Busch - Puerto Quijarro"', modify
label define s03a_01b 71403 `"Santa Cruz - German Busch - Carmen Rivero T�rrez"', modify
label define s03a_01b 71501 `"Santa Cruz - Guarayos - Ascensi�n de Guarayos"', modify
label define s03a_01b 71502 `"Santa Cruz - Guarayos - Urubich�"', modify
label define s03a_01b 71503 `"Santa Cruz - Guarayos - El Puente"', modify
label define s03a_01b 80101 `"Beni - Cercado - Trinidad"', modify
label define s03a_01b 80102 `"Beni - Cercado - San Javier"', modify
label define s03a_01b 80201 `"Beni - Vaca Diez - Riberalta"', modify
label define s03a_01b 80202 `"Beni - Vaca Diez - Guayaramer�n"', modify
label define s03a_01b 80301 `"Beni - Jos� Ballivi�n - Reyes"', modify
label define s03a_01b 80302 `"Beni - Jos� Ballivi�n - San Borja"', modify
label define s03a_01b 80303 `"Beni - Jos� Ballivi�n - Santa Rosa"', modify
label define s03a_01b 80304 `"Beni - Jos� Ballivi�n - Rurrenabaque"', modify
label define s03a_01b 80401 `"Beni - Yacuma - Santa Ana de Yacuma"', modify
label define s03a_01b 80402 `"Beni - Yacuma - Exaltaci�n"', modify
label define s03a_01b 80501 `"Beni - Moxos - San Ignacio"', modify
label define s03a_01b 80601 `"Beni - Marban - Loreto"', modify
label define s03a_01b 80602 `"Beni - Marban - San Andr�s"', modify
label define s03a_01b 80701 `"Beni - Mamore - San Joaqu�n"', modify
label define s03a_01b 80702 `"Beni - Mamore - San Ram�n"', modify
label define s03a_01b 80703 `"Beni - Mamore - Puerto Siles"', modify
label define s03a_01b 80801 `"Beni - Itenez - Magdalena"', modify
label define s03a_01b 80802 `"Beni - Itenez - Baures"', modify
label define s03a_01b 80803 `"Beni - Itenez - Huacaraje"', modify
label define s03a_01b 90101 `"Pando - Nicol�s Su�rez - Cobija"', modify
label define s03a_01b 90102 `"Pando - Nicol�s Su�rez - Porvenir"', modify
label define s03a_01b 90103 `"Pando - Nicol�s Su�rez - Bolpebra"', modify
label define s03a_01b 90104 `"Pando - Nicol�s Su�rez - Bella Flor"', modify
label define s03a_01b 90201 `"Pando - Manuripi - Puerto Rico"', modify
label define s03a_01b 90202 `"Pando - Manuripi - San Pedro"', modify
label define s03a_01b 90203 `"Pando - Manuripi - Filadelfia"', modify
label define s03a_01b 90301 `"Pando - Madre de Dios - Puerto Gonzalo Moreno"', modify
label define s03a_01b 90302 `"Pando - Madre de Dios - San Lorenzo"', modify
label define s03a_01b 90303 `"Pando - Madre de Dios - Sena"', modify
label define s03a_01b 90401 `"Pando - Abuna - Santa Rosa"', modify
label define s03a_01b 90402 `"Pando - Abuna - Ingavi"', modify
label define s03a_01b 90501 `"Pando - Federico Rom�n - Nueva Esperanza"', modify
label define s03a_01b 90502 `"Pando - Federico Rom�n - Villa Nueva (Loma Alta)"', modify
label define s03a_01b 90503 `"Pando - Federico Rom�n - Santos Mercado"', modify
label define s03a_01a 1 `"1. Aqu�"', modify
label define s03a_01a 2 `"2. En otro lugar del pa�s"', modify
label define s03a_01a 3 `"3. En el exterior"', modify
label define s03a_01a 4 `"4. A�n no hab�a nacido"', modify
label define s02a_10 1 `"1. SOLTERO/A"', modify
label define s02a_10 2 `"2. CASADO/A"', modify
label define s02a_10 3 `"3. CONVIVIENTE O CONCUBINO/A"', modify
label define s02a_10 4 `"4. SEPARADO/A"', modify
label define s02a_10 5 `"5. DIVORCIADO/A"', modify
label define s02a_10 6 `"6. VIUDO/A"', modify
label define s02a_08 1 `"ARAONA"', modify
label define s02a_08 2 `"AYMARA"', modify
label define s02a_08 3 `"BAURE"', modify
label define s02a_08 4 `"B�SIRO"', modify
label define s02a_08 5 `"CANICHANA"', modify
label define s02a_08 6 `"CASTELLANO"', modify
label define s02a_08 7 `"CAVINE�O"', modify
label define s02a_08 8 `"CAYUBABA"', modify
label define s02a_08 9 `"CH�COBO"', modify
label define s02a_08 10 `"CHIMAN"', modify
label define s02a_08 11 `"ESE EJJA"', modify
label define s02a_08 12 `"GUARANI"', modify
label define s02a_08 13 `"GUARASUAWE"', modify
label define s02a_08 14 `"GUARAYU"', modify
label define s02a_08 15 `"ITONAMA"', modify
label define s02a_08 16 `"LECO"', modify
label define s02a_08 17 `"MACHAJUYAI-KALLAWAYA"', modify
label define s02a_08 18 `"MACHINERI"', modify
label define s02a_08 19 `"MAROPA"', modify
label define s02a_08 20 `"MOJE�O IGNACIANO"', modify
label define s02a_08 21 `"MOJE�O TRINITARIO"', modify
label define s02a_08 22 `"MOR�"', modify
label define s02a_08 23 `"MOSETEN"', modify
label define s02a_08 24 `"MOVIMA"', modify
label define s02a_08 25 `"PACAWARA"', modify
label define s02a_08 26 `"PUQUINA"', modify
label define s02a_08 27 `"QUECHUA"', modify
label define s02a_08 28 `"SIRION�"', modify
label define s02a_08 29 `"TACANA"', modify
label define s02a_08 30 `"TAPIETE"', modify
label define s02a_08 31 `"TOROMONA"', modify
label define s02a_08 32 `"URUCHIPAYA"', modify
label define s02a_08 33 `"WEENHAYEK"', modify
label define s02a_08 34 `"YAMINAWA"', modify
label define s02a_08 35 `"YUKI"', modify
label define s02a_08 36 `"YURACARE"', modify
label define s02a_08 37 `"ZAMUCO"', modify
label define s02a_08 38 `"IDIOMA EXTRANJERO"', modify
label define s02a_08 39 `"OTRO IDIOMA NACIONAL"', modify
label define s02a_08 40 `"ALBAN�S"', modify
label define s02a_08 41 `"ALEMAN"', modify
label define s02a_08 42 `"ARABE"', modify
label define s02a_08 43 `"BULGARO"', modify
label define s02a_08 44 `"CATALAN"', modify
label define s02a_08 45 `"CHINO"', modify
label define s02a_08 46 `"COREANO"', modify
label define s02a_08 47 `"CROATA"', modify
label define s02a_08 48 `"DANES"', modify
label define s02a_08 49 `"ESCOS�S"', modify
label define s02a_08 50 `"FINLAND�S"', modify
label define s02a_08 51 `"FRANCES"', modify
label define s02a_08 52 `"sOLANDES"', modify
label define s02a_08 53 `"s�NGARO"', modify
label define s02a_08 54 `"INGLES"', modify
label define s02a_08 55 `"ITALIANO"', modify
label define s02a_08 56 `"JAPONES"', modify
label define s02a_08 57 `"NORUEGO"', modify
label define s02a_08 58 `"PORTUGUES"', modify
label define s02a_08 59 `"RUMANO"', modify
label define s02a_08 60 `"RUSO"', modify
label define s02a_08 61 `"SERBIO"', modify
label define s02a_08 62 `"SUECO"', modify
label define s02a_08 63 `"TAILAND�S"', modify
label define s02a_08 64 `"TURCO"', modify
label define s02a_08 65 `"UCRANIANO"', modify
label define s02a_08 66 `"VASCO"', modify
label define s02a_08 67 `"VIETNAM�S"', modify
label define s02a_08 70 `"QUINAMAYA"', modify
label define s02a_08 71 `"Qom (Toba)"', modify
label define s02a_08 995 `"B. NO PUEDE HABLAR"', modify
label define s02a_08 996 `"A. NO HABLA A�N"', modify
label define s02a_08 997 `"NO CORRESPONDE A IDIOMA"', modify
label define s02a_08 998 `"NO SABE"', modify
label define s02a_08 999 `"OMISI�N"', modify
label define s02a_07_3 1 `"ARAONA"', modify
label define s02a_07_3 2 `"AYMARA"', modify
label define s02a_07_3 3 `"BAURE"', modify
label define s02a_07_3 4 `"B�SIRO"', modify
label define s02a_07_3 5 `"CANICHANA"', modify
label define s02a_07_3 6 `"CASTELLANO"', modify
label define s02a_07_3 7 `"CAVINE�O"', modify
label define s02a_07_3 8 `"CAYUBABA"', modify
label define s02a_07_3 9 `"CH�COBO"', modify
label define s02a_07_3 10 `"CHIMAN"', modify
label define s02a_07_3 11 `"ESE EJJA"', modify
label define s02a_07_3 12 `"GUARANI"', modify
label define s02a_07_3 13 `"GUARASUAWE"', modify
label define s02a_07_3 14 `"GUARAYU"', modify
label define s02a_07_3 15 `"ITONAMA"', modify
label define s02a_07_3 16 `"LECO"', modify
label define s02a_07_3 17 `"MACHAJUYAI-KALLAWAYA"', modify
label define s02a_07_3 18 `"MACHINERI"', modify
label define s02a_07_3 19 `"MAROPA"', modify
label define s02a_07_3 20 `"MOJE�O IGNACIANO"', modify
label define s02a_07_3 21 `"MOJE�O TRINITARIO"', modify
label define s02a_07_3 22 `"MOR�"', modify
label define s02a_07_3 23 `"MOSETEN"', modify
label define s02a_07_3 24 `"MOVIMA"', modify
label define s02a_07_3 25 `"PACAWARA"', modify
label define s02a_07_3 26 `"PUQUINA"', modify
label define s02a_07_3 27 `"QUECHUA"', modify
label define s02a_07_3 28 `"SIRION�"', modify
label define s02a_07_3 29 `"TACANA"', modify
label define s02a_07_3 30 `"TAPIETE"', modify
label define s02a_07_3 31 `"TOROMONA"', modify
label define s02a_07_3 32 `"URUCHIPAYA"', modify
label define s02a_07_3 33 `"WEENHAYEK"', modify
label define s02a_07_3 34 `"YAMINAWA"', modify
label define s02a_07_3 35 `"YUKI"', modify
label define s02a_07_3 36 `"YURACARE"', modify
label define s02a_07_3 37 `"ZAMUCO"', modify
label define s02a_07_3 38 `"IDIOMA EXTRANJERO"', modify
label define s02a_07_3 39 `"OTRO IDIOMA NACIONAL"', modify
label define s02a_07_3 40 `"ALBAN�S"', modify
label define s02a_07_3 41 `"ALEMAN"', modify
label define s02a_07_3 42 `"ARABE"', modify
label define s02a_07_3 43 `"BULGARO"', modify
label define s02a_07_3 44 `"CATALAN"', modify
label define s02a_07_3 45 `"CHINO"', modify
label define s02a_07_3 46 `"COREANO"', modify
label define s02a_07_3 47 `"CROATA"', modify
label define s02a_07_3 48 `"DANES"', modify
label define s02a_07_3 49 `"ESCOS�S"', modify
label define s02a_07_3 50 `"FINLAND�S"', modify
label define s02a_07_3 51 `"FRANCES"', modify
label define s02a_07_3 52 `"sOLANDES"', modify
label define s02a_07_3 53 `"s�NGARO"', modify
label define s02a_07_3 54 `"INGLES"', modify
label define s02a_07_3 55 `"ITALIANO"', modify
label define s02a_07_3 56 `"JAPONES"', modify
label define s02a_07_3 57 `"NORUEGO"', modify
label define s02a_07_3 58 `"PORTUGUES"', modify
label define s02a_07_3 59 `"RUMANO"', modify
label define s02a_07_3 60 `"RUSO"', modify
label define s02a_07_3 61 `"SERBIO"', modify
label define s02a_07_3 62 `"SUECO"', modify
label define s02a_07_3 63 `"TAILAND�S"', modify
label define s02a_07_3 64 `"TURCO"', modify
label define s02a_07_3 65 `"UCRANIANO"', modify
label define s02a_07_3 66 `"VASCO"', modify
label define s02a_07_3 67 `"VIETNAM�S"', modify
label define s02a_07_3 70 `"QUINAMAYA"', modify
label define s02a_07_3 71 `"Qom (Toba)"', modify
label define s02a_07_3 995 `"B. NO PUEDE HABLAR"', modify
label define s02a_07_3 996 `"A. NO HABLA A�N"', modify
label define s02a_07_3 997 `"NO CORRESPONDE A IDIOMA"', modify
label define s02a_07_3 998 `"NO SABE"', modify
label define s02a_07_3 999 `"OMISI�N"', modify
label define s02a_07_2 1 `"ARAONA"', modify
label define s02a_07_2 2 `"AYMARA"', modify
label define s02a_07_2 3 `"BAURE"', modify
label define s02a_07_2 4 `"B�SIRO"', modify
label define s02a_07_2 5 `"CANICHANA"', modify
label define s02a_07_2 6 `"CASTELLANO"', modify
label define s02a_07_2 7 `"CAVINE�O"', modify
label define s02a_07_2 8 `"CAYUBABA"', modify
label define s02a_07_2 9 `"CH�COBO"', modify
label define s02a_07_2 10 `"CHIMAN"', modify
label define s02a_07_2 11 `"ESE EJJA"', modify
label define s02a_07_2 12 `"GUARANI"', modify
label define s02a_07_2 13 `"GUARASUAWE"', modify
label define s02a_07_2 14 `"GUARAYU"', modify
label define s02a_07_2 15 `"ITONAMA"', modify
label define s02a_07_2 16 `"LECO"', modify
label define s02a_07_2 17 `"MACHAJUYAI-KALLAWAYA"', modify
label define s02a_07_2 18 `"MACHINERI"', modify
label define s02a_07_2 19 `"MAROPA"', modify
label define s02a_07_2 20 `"MOJE�O IGNACIANO"', modify
label define s02a_07_2 21 `"MOJE�O TRINITARIO"', modify
label define s02a_07_2 22 `"MOR�"', modify
label define s02a_07_2 23 `"MOSETEN"', modify
label define s02a_07_2 24 `"MOVIMA"', modify
label define s02a_07_2 25 `"PACAWARA"', modify
label define s02a_07_2 26 `"PUQUINA"', modify
label define s02a_07_2 27 `"QUECHUA"', modify
label define s02a_07_2 28 `"SIRION�"', modify
label define s02a_07_2 29 `"TACANA"', modify
label define s02a_07_2 30 `"TAPIETE"', modify
label define s02a_07_2 31 `"TOROMONA"', modify
label define s02a_07_2 32 `"URUCHIPAYA"', modify
label define s02a_07_2 33 `"WEENHAYEK"', modify
label define s02a_07_2 34 `"YAMINAWA"', modify
label define s02a_07_2 35 `"YUKI"', modify
label define s02a_07_2 36 `"YURACARE"', modify
label define s02a_07_2 37 `"ZAMUCO"', modify
label define s02a_07_2 38 `"IDIOMA EXTRANJERO"', modify
label define s02a_07_2 39 `"OTRO IDIOMA NACIONAL"', modify
label define s02a_07_2 40 `"ALBAN�S"', modify
label define s02a_07_2 41 `"ALEMAN"', modify
label define s02a_07_2 42 `"ARABE"', modify
label define s02a_07_2 43 `"BULGARO"', modify
label define s02a_07_2 44 `"CATALAN"', modify
label define s02a_07_2 45 `"CHINO"', modify
label define s02a_07_2 46 `"COREANO"', modify
label define s02a_07_2 47 `"CROATA"', modify
label define s02a_07_2 48 `"DANES"', modify
label define s02a_07_2 49 `"ESCOS�S"', modify
label define s02a_07_2 50 `"FINLAND�S"', modify
label define s02a_07_2 51 `"FRANCES"', modify
label define s02a_07_2 52 `"sOLANDES"', modify
label define s02a_07_2 53 `"s�NGARO"', modify
label define s02a_07_2 54 `"INGLES"', modify
label define s02a_07_2 55 `"ITALIANO"', modify
label define s02a_07_2 56 `"JAPONES"', modify
label define s02a_07_2 57 `"NORUEGO"', modify
label define s02a_07_2 58 `"PORTUGUES"', modify
label define s02a_07_2 59 `"RUMANO"', modify
label define s02a_07_2 60 `"RUSO"', modify
label define s02a_07_2 61 `"SERBIO"', modify
label define s02a_07_2 62 `"SUECO"', modify
label define s02a_07_2 63 `"TAILAND�S"', modify
label define s02a_07_2 64 `"TURCO"', modify
label define s02a_07_2 65 `"UCRANIANO"', modify
label define s02a_07_2 66 `"VASCO"', modify
label define s02a_07_2 67 `"VIETNAM�S"', modify
label define s02a_07_2 70 `"QUINAMAYA"', modify
label define s02a_07_2 71 `"Qom (Toba)"', modify
label define s02a_07_2 995 `"B. NO PUEDE HABLAR"', modify
label define s02a_07_2 996 `"A. NO HABLA A�N"', modify
label define s02a_07_2 997 `"NO CORRESPONDE A IDIOMA"', modify
label define s02a_07_2 998 `"NO SABE"', modify
label define s02a_07_2 999 `"OMISI�N"', modify
label define s02a_07_1 1 `"ARAONA"', modify
label define s02a_07_1 2 `"AYMARA"', modify
label define s02a_07_1 3 `"BAURE"', modify
label define s02a_07_1 4 `"B�SIRO"', modify
label define s02a_07_1 5 `"CANICHANA"', modify
label define s02a_07_1 6 `"CASTELLANO"', modify
label define s02a_07_1 7 `"CAVINE�O"', modify
label define s02a_07_1 8 `"CAYUBABA"', modify
label define s02a_07_1 9 `"CH�COBO"', modify
label define s02a_07_1 10 `"CHIMAN"', modify
label define s02a_07_1 11 `"ESE EJJA"', modify
label define s02a_07_1 12 `"GUARANI"', modify
label define s02a_07_1 13 `"GUARASUAWE"', modify
label define s02a_07_1 14 `"GUARAYU"', modify
label define s02a_07_1 15 `"ITONAMA"', modify
label define s02a_07_1 16 `"LECO"', modify
label define s02a_07_1 17 `"MACHAJUYAI-KALLAWAYA"', modify
label define s02a_07_1 18 `"MACHINERI"', modify
label define s02a_07_1 19 `"MAROPA"', modify
label define s02a_07_1 20 `"MOJE�O IGNACIANO"', modify
label define s02a_07_1 21 `"MOJE�O TRINITARIO"', modify
label define s02a_07_1 22 `"MOR�"', modify
label define s02a_07_1 23 `"MOSETEN"', modify
label define s02a_07_1 24 `"MOVIMA"', modify
label define s02a_07_1 25 `"PACAWARA"', modify
label define s02a_07_1 26 `"PUQUINA"', modify
label define s02a_07_1 27 `"QUECHUA"', modify
label define s02a_07_1 28 `"SIRION�"', modify
label define s02a_07_1 29 `"TACANA"', modify
label define s02a_07_1 30 `"TAPIETE"', modify
label define s02a_07_1 31 `"TOROMONA"', modify
label define s02a_07_1 32 `"URUCHIPAYA"', modify
label define s02a_07_1 33 `"WEENHAYEK"', modify
label define s02a_07_1 34 `"YAMINAWA"', modify
label define s02a_07_1 35 `"YUKI"', modify
label define s02a_07_1 36 `"YURACARE"', modify
label define s02a_07_1 37 `"ZAMUCO"', modify
label define s02a_07_1 38 `"IDIOMA EXTRANJERO"', modify
label define s02a_07_1 39 `"OTRO IDIOMA NACIONAL"', modify
label define s02a_07_1 40 `"ALBAN�S"', modify
label define s02a_07_1 41 `"ALEMAN"', modify
label define s02a_07_1 42 `"ARABE"', modify
label define s02a_07_1 43 `"BULGARO"', modify
label define s02a_07_1 44 `"CATALAN"', modify
label define s02a_07_1 45 `"CHINO"', modify
label define s02a_07_1 46 `"COREANO"', modify
label define s02a_07_1 47 `"CROATA"', modify
label define s02a_07_1 48 `"DANES"', modify
label define s02a_07_1 49 `"ESCOS�S"', modify
label define s02a_07_1 50 `"FINLAND�S"', modify
label define s02a_07_1 51 `"FRANCES"', modify
label define s02a_07_1 52 `"sOLANDES"', modify
label define s02a_07_1 53 `"s�NGARO"', modify
label define s02a_07_1 54 `"INGLES"', modify
label define s02a_07_1 55 `"ITALIANO"', modify
label define s02a_07_1 56 `"JAPONES"', modify
label define s02a_07_1 57 `"NORUEGO"', modify
label define s02a_07_1 58 `"PORTUGUES"', modify
label define s02a_07_1 59 `"RUMANO"', modify
label define s02a_07_1 60 `"RUSO"', modify
label define s02a_07_1 61 `"SERBIO"', modify
label define s02a_07_1 62 `"SUECO"', modify
label define s02a_07_1 63 `"TAILAND�S"', modify
label define s02a_07_1 64 `"TURCO"', modify
label define s02a_07_1 65 `"UCRANIANO"', modify
label define s02a_07_1 66 `"VASCO"', modify
label define s02a_07_1 67 `"VIETNAM�S"', modify
label define s02a_07_1 70 `"QUINAMAYA"', modify
label define s02a_07_1 71 `"Qom (Toba)"', modify
label define s02a_07_1 995 `"B. NO PUEDE HABLAR"', modify
label define s02a_07_1 996 `"A. NO HABLA A�N"', modify
label define s02a_07_1 997 `"NO CORRESPONDE A IDIOMA"', modify
label define s02a_07_1 998 `"NO SABE"', modify
label define s02a_07_1 999 `"OMISI�N"', modify
label define s02a_05 1 `"1.JEFE O JEFA DEL HOGAR"', modify
label define s02a_05 2 `"2.ESPOSA/O O CONVIVIENTE"', modify
label define s02a_05 3 `"3.HIJO/A O ENTENADO/A"', modify
label define s02a_05 4 `"4.YERNO O NUERA"', modify
label define s02a_05 5 `"5.HERMANO/A O CU�ADO/A"', modify
label define s02a_05 6 `"6.PADRES"', modify
label define s02a_05 7 `"7.SUEGROS"', modify
label define s02a_05 8 `"8.NIETO/NIETA"', modify
label define s02a_05 9 `"9.OTRO PARIENTE"', modify
label define s02a_05 10 `"10.OTRO QUE NO ES PARIENTE"', modify
label define s02a_05 11 `"11.EMPLEADA/O DEL HOGAR   CAMA ADENTRO"', modify
label define s02a_05 12 `"12.PARIENTE DE LA  EMPLEADA/O DEL HOGAR"', modify
label define s02a_02 1 `"1.Hombre"', modify
label define s02a_02 2 `"2.Mujer"', modify