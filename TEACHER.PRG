******************************************************************************
**               PROGRAMA TEACHER PARA APRENDIZAJE DE INGLES                **
******************************************************************************
SET EXACT OFF
SET SCOREBOARD OFF
SET BELL OFF
SET DELETED ON
SET DATE TO BRITISH
Borrados=.F.               && Indica si hay trminos borrados
PrimeraVez=.T.             && Indicar  si es la primera vez que se entra en fase de preguntas
InicioSesion=SECONDS()     && Momento en que se inicia la sesiขn de trabajo
Lpp=64                     && Lกneas por p gina de impresiขn
TopNivel=13                && M ximo nง de niveles de dificultad admitidos
IE=.T.                     && Tipo de pregunta Ingls a Espaคol
EI=.F.                     && Tipo de pregunta Espaคol a Ingles
DECLARE Pregs[TopNivel,7]  && PREGUNTAS.          1..Frec. en dกas,
			   && 2..Todas I->E,      3..Todas E->I,   
			   && 4..Propuestas I->E, 5..Propuestas E->I,
			   && 6..Elegidas I->E,   7..Elegidas E->I
* Declaraciขn de frecuencias de pregunta segฃn nivel
Pregs[ 1,1]=1     && Diario
Pregs[ 2,1]=3     && 3 dกas
Pregs[ 3,1]=5     && Semanal
Pregs[ 4,1]=12    && 2 semanas
Pregs[ 5,1]=19    && 3 semanas
Pregs[ 6,1]=26    && Mensual
Pregs[ 7,1]=40    && mes y medio (6 semanas)
Pregs[ 8,1]=54    && 2 meses
Pregs[ 9,1]=82    && 3 meses
Pregs[10,1]=110   && 4 meses
Pregs[11,1]=166   && 6 meses
Pregs[12,1]=222   && 8 meses
Pregs[13,1]=338   && Anual
USE DICCI.DBF NEW 
SET INDEX TO INGLES,NIVDATIE,NIVDATEI
USE TABLA NEW            && Tabla de posiciones de registros
DO WHILE .T.
   SET DEVICE TO SCREEN
   SELECT DICCI
   SET ORDER TO 2     && NIVDATIE
   CLEAR SCREEN
   @  0,0  TO  3,79 DOUBLE
   @  1,15 SAY "TEACHER r3.0(แ) para el estudio de Ingls"
   @  2,15 SAY "       1993. Alejandro Alonso Puig"
   @  4,0  SAY "BASE DE DATOS DICCI.DBF: "+LTRIM(STR(RECCOUNT()))+" Terminos."
   @  7,5  SAY "MENU DE OPCIONES"
   @  8,0  TO 16,79
   @ 10,10 SAY "1 .. Introducir trminos"
   @ 11,10 SAY "2 .. Preguntar trminos"
   @ 12,10 SAY "3 .. Localizar/Alterar trminos"
   @ 13,10 SAY "4 .. Listar Base de Datos"
   @ 14,10 SAY "S .. Salir del programa"
   @ 17,0
   WAIT "Opciขn elegida: " TO Opcion
   @  7,0 CLEAR
   DO CASE
      CASE Opcion="1"  && *********** INTRODUCCION DE NUEVOS TERMINOS ***
	 SET ORDER TO 1  && Ingles
	 Salir=.F.
	 DO WHILE .NOT. Salir
	    @  7,0 CLEAR
	    @ 23,5 SAY "Introducir  @  para salir"
	    GO TOP
	    TerIng=SPACE(LEN(DICCI->Ingles))
	    TerEsp=SPACE(LEN(DICCI->Espanol))
	    @ 10,0 TO 12,79
	    @ 10,5 SAY " Termino Ingles "
	    DO WHILE TerIng=" "
	       @ 11,2 GET TerIng PICTURE REPLICATE("!",LEN(DICCI->INGLES))
	       READ
	    ENDDO
	    IF (LTRIM(RTRIM(TerIng))="@")
	       Salir=.T.
	    ELSE 
	       Existente=.F.
	       SEEK(TerIng)
	       IF FOUND()
		  * Termino existente 
		  Existente=.T.
		  TerEsp=DICCI->Espanol
	       ENDIF
	       @ 13,0 TO 15,79
	       @ 13,5 SAY " Termino Espaคol "
	       @ 14,2 GET TerEsp  PICTURE REPLICATE("!",LEN(DICCI->ESPANOL))
	       READ
	       DO WHILE TerEsp=" "
		  @ 14,2 GET TerEsp  PICTURE REPLICATE("!",LEN(DICCI->ESPANOL))
		  READ
	       ENDDO
	       IF (LTRIM(RTRIM(TerEsp))="@")
		  Salir=.T.
	       ELSE
		  IF Existente
		     REPLACE Espanol WITH TerEsp
		  ELSE   
		     APPEND BLANK
		     REPLACE Ingles WITH TerIng
		     REPLACE Espanol WITH TerEsp
		     REPLACE NivIE WITH 1
		     REPLACE NivEI WITH 1
		     REPLACE DateIE WITH DATE()
		     REPLACE DateEI WITH DATE()
		     @  4,25  SAY LTRIM(STR(RECCOUNT()))+" Terminos.    "
		     IF .NOT.PrimeraVez
			Pregs[NivIE,2]=Pregs[NivIE,2]+1
			Pregs[NivEI,3]=Pregs[NivEI,3]+1
		     ENDIF
		  ENDIF
	       ENDIF
	    ENDIF
	 ENDDO
      CASE Opcion="2"   && ******************* PREGUNTAR TERMINOS ****
	IF PrimeraVez
	   PrimeraVez=.F.
	   * Se rellena la matriz Pregs[] 
	   Indice=1
	   DO WHILE Indice<=TopNivel
	      Pregs[Indice,2]=0 && Todas I->E
	      Pregs[Indice,3]=0 && Todas E->I
	      Pregs[Indice,4]=0 && Propuestos I->E
	      Pregs[Indice,5]=0 && Propuestos E->I
	      Indice=Indice+1
	   ENDDO 
	   SELECT DICCI
	   SET ORDER TO 0   && Orden natural (prueba de velocidad)
	   GO TOP
	   * C lculo de traducciones totales y propuestas por niveles
	   @  6,0  CLEAR
	   @ 10,18 SAY "CARGANDO INFORMACION DE LA BASE DE DATOS"
	   @ 12,15 SAY "0%"
	   @ 12,39 SAY "50%"
	   @ 12,63 SAY "100%"
	   @ 13,14 TO 15,66
	   @ 23, 5 SAY "ESC .. Detener carga."
	   PunTabla=0
	   IndPorc=RECCOUNT()/50
	   Dot=1
	   DO WHILE (.NOT.EOF())
	      IF INKEY()=27  && Se pulsข ESC
		 PrimerVez=.T.
		 EXIT
	      ENDIF
	      Pregs[NivIE,2]=Pregs[NivIE,2]+1
	      IF DATE() - DateIE >= Pregs[NivIE,1]
		 Pregs[NivIE,4]=Pregs[NivIE,4]+1
	      ENDIF
	      Pregs[NivEI,3]=Pregs[NivEI,3]+1
	      IF DATE() - DateEI >= Pregs[NivEI,1]
		 Pregs[NivEI,5]=Pregs[NivEI,5]+1
	      ENDIF
	      SKIP 1
	      PunTabla=PunTabla+1
	      DO WHILE PunTabla>=Dot*IndPorc
		 @ 14,14+Dot SAY "Û"
		 Dot=Dot+1
	      ENDDO
	   ENDDO
	ENDIF
	* Presentar esquema para elecciขn de preguntas a realizar
	CLEAR SCREEN
	TEXT
       ษอออออออัอออออออออหอออออออออออออหอออออออออออออป     ษอออออออออออออป
       บ       ณ         บ    TODAS    บ PROPUESTAS. บ     บ  ELEGIDAS.  บ
       บ NIVEL ณ FRECUEN บ I->E ณ E->I บ I->E ณ E->I บ     บ I->E ณ E->I บ
       ฬอออออออุอออออออออฮออออออุออออออฮออออออุออออออน     ฬออออออุออออออน
       บ   1   ณ         บ      ณ      บ      ณ      บ.....บ      ณ      บ
       บ   2   ณ         บ      ณ      บ      ณ      บ.....บ      ณ      บ
       บ   3   ณ         บ      ณ      บ      ณ      บ.....บ      ณ      บ
       บ   4   ณ         บ      ณ      บ      ณ      บ.....บ      ณ      บ
       บ   5   ณ         บ      ณ      บ      ณ      บ.....บ      ณ      บ
       บ   6   ณ         บ      ณ      บ      ณ      บ.....บ      ณ      บ
       บ   7   ณ         บ      ณ      บ      ณ      บ.....บ      ณ      บ
       บ   8   ณ         บ      ณ      บ      ณ      บ.....บ      ณ      บ
       บ   9   ณ         บ      ณ      บ      ณ      บ.....บ      ณ      บ
       บ  10   ณ         บ      ณ      บ      ณ      บ.....บ      ณ      บ
       บ  11   ณ         บ      ณ      บ      ณ      บ.....บ      ณ      บ
       บ  12   ณ         บ      ณ      บ      ณ      บ.....บ      ณ      บ
       บ  13   ณ         บ      ณ      บ      ณ      บ.....บ      ณ      บ
       ศอออออออฯอออออออออฮออออออุออออออฮออออออุออออออน     ฬออออออุออออออน
ÿÿÿÿÿÿÿÿÿÿÿÿSUBTOTALES ->บ      ณ      บ      ณ      บ     บ      ณ      บ
ÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿฬออออออฯออออออฮออออออฯออออออน     ฬออออออฯออออออน
ÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿTOTALES ->บ             บ             บ     บ             บ
ÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿศอออออออออออออสอออออออออออออผ     ศอออออออออออออผ
	ENDTEXT
	Indice=1
	STORE 0 TO SubTodasIE,SubTodasEI
	STORE 0 TO SubPropIE, SubPropEI
	DO WHILE Indice<=TopNivel
	   @ Indice+4,19 SAY Pregs[Indice,1] PICTURE "999"
	   @ Indice+4,27 SAY Pregs[Indice,2] PICTURE "9999"
	   @ Indice+4,34 SAY Pregs[Indice,3] PICTURE "9999"
	   @ Indice+4,41 SAY Pregs[Indice,4] PICTURE "9999"
	   @ Indice+4,48 SAY Pregs[Indice,5] PICTURE "9999"
	   Pregs[Indice,6]=Pregs[Indice,4] && Por defecto
	   Pregs[Indice,7]=Pregs[Indice,5] && Por defecto
	   SubTodasIE=SubTodasIE+Pregs[Indice,2]
	   SubTodasEI=SubTodasEI+Pregs[Indice,3]
	   SubPropIE =SubPropIE +Pregs[Indice,4]
	   SubPropEI =SubPropEI +Pregs[Indice,5]
	   Indice=Indice+1
	ENDDO
	TotTodas  =SubTodasIE+SubTodasEI
	TotProp   =SubPropIE +SubPropEI 
	@ 19,27 SAY SubTodasIE PICTURE "9999"
	@ 19,34 SAY SubTodasEI PICTURE "9999"
	@ 19,41 SAY SubPropIE  PICTURE "9999"
	@ 19,48 SAY SubPropEI  PICTURE "9999"
	@ 21,31 SAY TotTodas   PICTURE "9999"
	@ 21,45 SAY TotProp    PICTURE "9999"
	Correcto="N"
	DO WHILE Correcto$"nN"
	   Indice=1
	   STORE 0 TO SubElegIE, SubElegEI, TotEleg
	   DO WHILE Indice<=TopNivel
	      @ Indice+4,61 GET Pregs[Indice,6] PICTURE "9999"
	      @ Indice+4,68 GET Pregs[Indice,7] PICTURE "9999"
	      Indice=Indice+1
	   ENDDO
	   READ
	   Indice=1
	   DO WHILE Indice<=TopNivel
	      SubElegIE=SubElegIE+Pregs[Indice,6]
	      SubElegEI=SubElegEI+Pregs[Indice,7]
	      Indice=Indice+1
	   ENDDO
	   TotEleg=SubElegIE+SubElegEI
	   @ 19,61 SAY SubElegIE  PICTURE "9999"
	   @ 19,68 SAY SubElegEI  PICTURE "9999"
	   @ 21,65 SAY TotEleg    PICTURE "9999"
	   @ 23, 8 SAY "จDatos Correctos? (S/N/F)  " GET Correcto ;
		       PICTURE "!" VALID Correcto$"SNF"
	   READ
	   @ 23, 8 CLEAR
	ENDDO
	PunNivel=1
	QuedanTot=TotEleg && Total de preguntas a hacer (todos los niveles)
	CLEAR SCREEN
	@  0,0  TO  3,79 DOUBLE
	@  1,15 SAY "TEACHER r3.0(แ) para el estudio de Ingls"
	@  2,15 SAY "       1993. Alejandro Alonso Puig"
	Abortar=.F.
	DO WHILE .NOT.(PunNivel>TopNivel)
	   IF Correcto$"fF"  &&Volver al menu principal
	      EXIT
	   ENDIF
	   Preguntas=Pregs[PunNivel,6]+Pregs[PunNivel,7] && a hacer en este nivel
	   IF Preguntas>0  && Hay preguntas que hacer en este nivel
	      SET EXACT OFF
	      *** Carga de la Tabla ***
	      SELECT Tabla
	      ZAP
	      @ 7,0 CLEAR
	      @ 10,14 SAY "CREANDO TABLA DE PREGUNTAS PARA EL NIVEL "+LTRIM(STR(PunNivel))
	      @ 12,15 SAY "0%"
	      @ 12,39 SAY "50%"
	      @ 12,63 SAY "100%"
	      @ 13,14 TO 15,66
	      @ 23, 5 SAY "ESC .. Detener carga."
	      PunTabla=0
	      IndPorc=Preguntas/50
	      Dot=1
	      IF Pregs[PunNivel,6]>0     && Hay preguntas del tipo IE
		 TipoPreg=IE
		 SELECT DICCI
		 SET ORDER TO 2  && NivDatIE
		 GO TOP
		 SEEK STR(PunNivel,2)
		 SELECT TABLA
		 Indice=1  && Contador de preguntas cargadas
		 DO WHILE Indice<=Pregs[PunNivel,6]
		    PunTabla=PunTabla+1
		    DO WHILE PunTabla>=Dot*IndPorc
		       @ 14,14+Dot SAY "Û"
		       Dot=Dot+1
		    ENDDO
		    IF INKEY()=27
		       Abortar=.T.
		       EXIT
		    ENDIF
		    APPEND BLANK
		    REPLACE Registro WITH DICCI->(RECNO())
		    REPLACE Tipo WITH TipoPreg
		    SKIP 1 ALIAS DICCI
		    Indice=Indice+1
		 ENDDO
		 IF Abortar
		    EXIT
		 ENDIF
	      ENDIF
	      IF Pregs[PunNivel,7]>0     && Hay preguntas del tipo EI
		 TipoPreg=EI
		 SELECT DICCI
		 SET ORDER TO 3  && NivDatEI
		 GO TOP
		 SEEK STR(PunNivel,2)
		 SELECT TABLA
		 Indice=1  && Contador de preguntas cargadas
		 DO WHILE Indice<=Pregs[PunNivel,7]
		    PunTabla=PunTabla+1
		    DO WHILE PunTabla>=Dot*IndPorc
		       @ 14,14+Dot SAY "Û"
		       Dot=Dot+1
		    ENDDO
		    IF INKEY()=27
		       Abortar=.T.
		       EXIT
		    ENDIF
		    APPEND BLANK
		    REPLACE Registro WITH DICCI->(RECNO())
		    REPLACE Tipo WITH TipoPreg
		    SKIP 1 ALIAS DICCI
		    Indice=Indice+1
		 ENDDO
		 IF Abortar
		    EXIT
		 ENDIF
	      ENDIF
	      SET EXACT ON
	      SELECT DICCI
	      ***** CARGA TABLA FINALIZADA, COMIENZA FASE DE PREGUNTAS ****
	      Quedan=Preguntas   && Por preguntar en este nivel
	      DO WHILE Quedan>0  && Bucle de preguntas del nivel PunNivel
		 @  5,0 CLEAR
		 @  5,0 SAY "NIVEL DE DIFICULTAD..........................: "+LTRIM(STR(PunNivel))+"     (Frecuencia: "+STR(Pregs[PunNivel,1],3)+" dกas.)"
		 @  6,0 SAY "Preguntas TOTALES que quedan por hacer.......: "+LTRIM(STR(QuedanTot))
		 @  7,0 SAY "Preguntas que quedan por hacer en este nivel.: "+LTRIM(STR(Quedan))
		 Rand=VAL("0"+SUBSTR(STR(SECONDS()),8,3)) && Simulaciขn aleatorio
		 Posicion=INT(Rand*Quedan+1)
		 SELECT TABLA
		 GO Posicion 
		 SELECT DICCI
		 GO TABLA->(Registro)
		 @ 10,0 TO 12,79
		 IF TABLA->Tipo=IE  && Pregunta de Ingls a Espaคol
		    Tiempo=DATE()-DateIE  && Tiempo pasado desde anterior vez
		    IF TABLA->Repetido=.T.
		       @ 18,0 SAY "REPETICION DE PREGUNTA"
		    ELSE
		       @ 18,0 SAY "Pregunta realizada hace "+STR(Tiempo,3)+" dกas"
		    ENDIF
		    IF (NivIE<TopNivel).AND.(Tiempo>=Pregs[NivIE+1,1]).AND.(TABLA->Repetido=.F.)
		       @ 20,0 TO 22,79
		       @ 21,5 SAY "ATENCION, Si se acierta a la primera, cambiarla a nivel m s f cil"
		    ENDIF
		    @ 10,5 SAY " Termino Ingls "
		    @ 11,2 SAY DICCI->INGLES
		    @ 13,0 TO 16,79
		    @ 13,5 SAY " Significado Espaคol "
		    Resp=SPACE(LEN(DICCI->ESPANOL))
		    @ 14,2 GET Resp PICTURE REPLICATE("!",LEN(DICCI->ESPANOL))
		    READ
		    IF UPPER(Resp)=UPPER(DICCI->ESPANOL)
		       @ 15,20 SAY "===== OK. RESPUESTA EXACTA ====="
		    ELSE
		       @ 15,2  SAY DICCI->ESPANOL
		    ENDIF
		 ELSE && Pregunta de Espaคol a Ingls
		    Tiempo=DATE()-DateEI  && Tiempo pasado desde anterior vez
		    IF TABLA->Repetido=.T.
		       @ 18,0 SAY "REPETICION DE PREGUNTA"
		    ELSE
		       @ 18,0 SAY "Pregunta realizada hace "+STR(Tiempo,3)+" dกas"
		    ENDIF
		    IF (NivEI<TopNivel).AND.(Tiempo>=Pregs[NivEI+1,1]).AND.(TABLA->Repetido=.F.)
		       @ 20,0 TO 22,79
		       @ 21,5 SAY "ATENCION, Si se acierta a la primera, cambiarla a nivel m s f cil"
		    ENDIF
		    @ 10,5 SAY " Termino Espaคol "
		    @ 11,2 SAY DICCI->Espanol
		    @ 13,0 TO 16,79
		    @ 13,5 SAY " Significado Ingls "
		    Resp=SPACE(LEN(DICCI->Ingles))
		    @ 14,2 GET Resp PICTURE REPLICATE("!",LEN(DICCI->Ingles))
		    READ
		    IF UPPER(Resp)=UPPER(DICCI->Ingles)
		       @ 15,20 SAY "===== OK. RESPUESTA EXACTA ====="
		    ELSE
		       @ 15,2  SAY DICCI->Ingles
		    ENDIF
		 ENDIF
		 @ 19,0 CLEAR
		 @ 19,0 TO 22,79 DOUBLE
		 @ 19,5 SAY "OPCIONES"
		 @ 20,5 SAY "R..Repetir,   M..Modificar,   B..Borrar,   S..Salir,   INTRO..Pasar"
		 IF (PunNivel<TopNivel).AND.(TABLA->Repetido=.F.)
		    @ 21,5  SAY "+..Cambiar a nivel m s f cil"
		 ENDIF
		 IF PunNivel>1          
		 @ 21,40 SAY "-..Cambiar a nivel menos f cil"
		 ENDIF
		 WAIT "" TO Resp
		 DO CASE
		    CASE Resp$"rR"  && REPETIR TERMINO
		       TABLA->Repetido=.T.
		       IF TABLA->Tipo=IE
			  REPLACE DateIE WITH DATE()
		       ELSE
			  REPLACE DateEI WITH DATE()
		       ENDIF
		    CASE Resp$"+"  && CAMBIAR A NIVEL MAS FACIL
		       IF (PunNivel<TopNivel).AND.(TABLA->Repetido=.F.)
			  IF TABLA->Tipo=IE
			     DICCI->NivIE=DICCI->NivIE+1
			     REPLACE DateIE WITH DATE()
			     Pregs[PunNivel  ,2]=Pregs[PunNivel  ,2]-1
			     Pregs[PunNivel+1,2]=Pregs[PunNivel+1,2]+1
			     Pregs[PunNivel  ,4]=Pregs[PunNivel  ,4]-1
			  ELSE
			     DICCI->NivEI=DICCI->NivEI+1
			     REPLACE DateEI WITH DATE()
			     Pregs[PunNivel  ,3]=Pregs[PunNivel  ,3]-1
			     Pregs[PunNivel+1,3]=Pregs[PunNivel+1,3]+1
			     Pregs[PunNivel  ,5]=Pregs[PunNivel  ,5]-1
			  ENDIF
			  SELECT TABLA
			  GO Quedan 
			  Dato1A=TABLA->Registro
			  Dato1B=TABLA->Tipo
			  Dato1C=TABLA->Repetido
			  GO Posicion 
			  REPLACE Registro WITH Dato1A
			  REPLACE Tipo     WITH Dato1B
			  REPLACE Repetido WITH Dato1C
			  Quedan=Quedan-1
			  QuedanTot=QuedanTot-1
			  SELECT DICCI
		       ELSE && No se puede incrementar nivel. Se pasa.
			  SELECT TABLA
			  IF Tipo=IE
			     DICCI->DateIE=DATE()
			     Pregs[PunNivel  ,4]=Pregs[PunNivel  ,4]-1
			  ELSE
			     DICCI->DateEI=DATE()
			     Pregs[PunNivel  ,5]=Pregs[PunNivel  ,5]-1
			  ENDIF
			  GO Quedan 
			  Dato1A=TABLA->Registro
			  Dato1B=TABLA->Tipo
			  Dato1C=TABLA->Repetido
			  GO Posicion 
			  REPLACE Registro WITH Dato1A
			  REPLACE Tipo     WITH Dato1B
			  REPLACE Repetido WITH Dato1C
			  Quedan=Quedan-1
			  QuedanTot=QuedanTot-1
			  SELECT DICCI
		       ENDIF
		    CASE Resp$"-"  && CAMBIAR A NIVEL MENOS FACIL
		       IF PunNivel>1
			  IF TABLA->Tipo=IE
			     DICCI->NivIE=DICCI->NivIE-1
			     REPLACE DateIE WITH DATE()
			     Pregs[PunNivel  ,2]=Pregs[PunNivel  ,2]-1
			     Pregs[PunNivel-1,2]=Pregs[PunNivel-1,2]+1
			     Pregs[PunNivel  ,4]=Pregs[PunNivel  ,4]-1
			  ELSE
			     DICCI->NivEI=DICCI->NivEI-1
			     REPLACE DateEI WITH DATE()
			     Pregs[PunNivel  ,3]=Pregs[PunNivel  ,3]-1
			     Pregs[PunNivel-1,3]=Pregs[PunNivel-1,3]+1
			     Pregs[PunNivel  ,5]=Pregs[PunNivel  ,5]-1
			  ENDIF
			  SELECT TABLA
			  GO Quedan 
			  Dato1A=TABLA->Registro
			  Dato1B=TABLA->Tipo
			  Dato1C=TABLA->Repetido
			  GO Posicion 
			  REPLACE Registro WITH Dato1A
			  REPLACE Tipo     WITH Dato1B
			  REPLACE Repetido WITH Dato1C
			  Quedan=Quedan-1
			  QuedanTot=QuedanTot-1
			  SELECT DICCI
		       ELSE && No se puede decrementar nivel. Se pasa
			  SELECT TABLA
			  IF Tipo=IE
			     DICCI->DateIE=DATE()
			     Pregs[PunNivel  ,4]=Pregs[PunNivel  ,4]-1
			  ELSE
			     DICCI->DateEI=DATE()
			     Pregs[PunNivel  ,5]=Pregs[PunNivel  ,5]-1
			  ENDIF
			  GO Quedan 
			  Dato1A=TABLA->Registro
			  Dato1B=TABLA->Tipo
			  Dato1C=TABLA->Repetido
			  GO Posicion 
			  REPLACE Registro WITH Dato1A
			  REPLACE Tipo     WITH Dato1B
			  REPLACE Repetido WITH Dato1C
			  Quedan=Quedan-1
			  QuedanTot=QuedanTot-1
			  SELECT DICCI
		       ENDIF
		    CASE Resp$"mM"  && MODIFICAR
		       IF TABLA->Tipo=IE
			  @ 11,2 GET DICCI->Ingles PICTURE REPLICATE("!",LEN(DICCI->Ingles))
			  @ 13,0 CLEAR
			  @ 13,0 TO 15,79
			  @ 14,2 GET DICCI->Espanol PICTURE REPLICATE("!",LEN(DICCI->Espanol))
			  READ
		       ELSE
			  @ 11,2 GET DICCI->Espanol PICTURE REPLICATE("!",LEN(DICCI->Espanol))
			  @ 13,0 CLEAR
			  @ 13,0 TO 15,79
			  @ 14,2 GET DICCI->Ingles  PICTURE REPLICATE("!",LEN(DICCI->Ingles ))
			  READ
		       ENDIF
		       TABLA->Repetido=.F.
		    CASE Resp$"bB"  && BORRAR
		       Correcto="N"
		       @ 19,0 CLEAR
		       @ 19,0 TO 22,79
		       @ 20,5 SAY "ATENCION, se ha elegido BORRAR el trmino actual de la base de datos"
		       @ 21,5 SAY "Confirme la elecciขn.           BORRAR TERMINO (S/N): " GET Correcto PICTURE "!" VALID Correcto$"SN"
		       READ
		       IF Correcto="S"
			  SELECT DICCI
			  DELETE
			  Borrados=.T.
			  SELECT TABLA
			  GO Quedan 
			  Dato1A=TABLA->Registro
			  Dato1B=TABLA->Tipo
			  Dato1C=TABLA->Repetido
			  GO Posicion 
			  REPLACE Registro WITH Dato1A
			  REPLACE Tipo     WITH Dato1B
			  REPLACE Repetido WITH Dato1C
			  Quedan=Quedan-1
			  QuedanTot=QuedanTot-1
			  IF TABLA->Tipo=IE
			     Pregs[PunNivel  ,2]=Pregs[PunNivel  ,2]-1
			     Pregs[PunNivel  ,4]=Pregs[PunNivel  ,4]-1
			  ELSE
			     Pregs[PunNivel  ,3]=Pregs[PunNivel  ,3]-1
			     Pregs[PunNivel  ,5]=Pregs[PunNivel  ,5]-1
			  ENDIF
			  SELECT DICCI
		       ENDIF
		    CASE Resp$"sS"  && SALIR
		       QuedanTot=QuedanTot-Quedan && se quitan las de este nivel
		       EXIT
		    OTHERWISE  && opcion pasar por defecto
		       SELECT TABLA
		       IF Tipo=IE
			  DICCI->DateIE=DATE()
			  Pregs[PunNivel  ,4]=Pregs[PunNivel  ,4]-1
		       ELSE
			  DICCI->DateEI=DATE()
			  Pregs[PunNivel  ,5]=Pregs[PunNivel  ,5]-1
		       ENDIF
		       GO Quedan 
		       Dato1A=TABLA->Registro
		       Dato1B=TABLA->Tipo
		       Dato1C=TABLA->Repetido
		       GO Posicion 
		       REPLACE Registro WITH Dato1A
		       REPLACE Tipo     WITH Dato1B
		       REPLACE Repetido WITH Dato1C
		       Quedan=Quedan-1
		       QuedanTot=QuedanTot-1
		       SELECT DICCI
		 ENDCASE
	      ENDDO  && Fin del bucle de preguntas del nivel PunNivel
	   ENDIF
	   PunNivel=PunNivel+1
	ENDDO
      CASE Opcion="3"  && ************** LOCALIZAR/ALTERAR TERMINOS ***
      CASE Opcion="4"  &&*************** LISTAR BASE DE DATOS *********
	SET ORDER TO 1    && Ingles
	@  9,0 TO 12,79
	@ 10,15 SAY "SE VA A IMPRIMIR LA BASE DE DATOS ENTERA (";
		    +LTRIM(STR(INT(RECCOUNT()/(Lpp-2))))+" P ginas)"
	@ 11,15 SAY "             PREPARE LA IMPRESORA"
	@ 15,0 TO 18,79
	@ 15,5 SAY " OPCIONES "
	@ 16,15 SAY "I .. Imprimir"
	@ 17,15 SAY "S .. Salir"
	@ 19,0
	WAIT "Elija opciขn " TO Resp
	DO CASE
	   CASE Resp$"iI"
	      @ 15,0 CLEAR
	      @ 17,30 SAY "ESC ... Abortar impresiขn"
	      @ 18,30 SAY "P   ... Pausa en impresiขn"
	      SET DEVICE TO PRINTER
	      Pagina=0
	      Abortar=.F.
	      DO WHILE (.NOT. EOF()) .AND. (.NOT.Abortar)
		 Pagina=Pagina+1
		 @ 0,0  SAY DATE()
		 @ 0,40 SAY "P gina:"+LTRIM(STR(Pagina))
		 @ 0,70 SAY SUBSTR(DICCI->INGLES,1,10)
		 Linea=1
		 @ 0,0  SAY CHR(15) 
		 @ 1,0  SAY REPLICATE("อ",134)
		 DO WHILE Linea<Lpp .AND..NOT.EOF()
		    Linea=Linea+1
		    @ Linea,0 SAY SUBSTR(TRIM(DICCI->INGLES)+REPLICATE(".",65),1,65);
				 +SUBSTR(DICCI->ESPANOL,1,65)
		    SKIP
		    IF INKEY()=27
		       Abortar=.T.
		       EXIT
		    ENDIF
		 ENDDO
		 @ Lpp,0  SAY CHR(18) 
	      ENDDO
	      EJECT
	   OTHERWISE
	ENDCASE
      CASE Opcion$"Ss"
	 EXIT
   ENDCASE
ENDDO
IF Borrados=.T.
   @ 5,0 CLEAR
   @ 19,0 TO 21,79
   @ 20,30 SAY "BORRANDO TERMINOS"
   SELECT DICCI
   PACK
ENDIF   
SELECT TABLA
ZAP
CLOSE DATABASES
CLEAR SCREEN
TEXT
Final de la ejecuciขn de TEACHER r3.0(แ)
   (C).1993  Alejandro Alonso Puig


Have a good day..


ENDTEXT
Minutos=INT((SECONDS()-InicioSesion)/60)
? "Duraciขn de la sesiขn: "+LTRIM(RTRIM(STR(Minutos)))+" minutos"
WAIT
? " "
RETURN
