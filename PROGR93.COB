       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROGR93.
      *AUTHOR. GUSTAVO ARAUJO NARCISO.
      **************************************
      * MAINTENANCE OF THE REGISTRATION OF CARS   *
      **************************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
             DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT VEHREG ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS PLATE
                    FILE STATUS  IS ST-ERRO
                    ALTERNATE RECORD KEY IS NAMEP WITH DUPLICATES.
      *
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD VEHREG
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "VEHREG.DAT".
       01 REGVEHI.
          03 PLATE.
             05 LETTERP      PIC X(03).
             05 NUMP     PIC 9(04).
          03 NAMEP         PIC X(30).
          03 BRAND         PIC 9(02).
          03 MODEL        PIC X(20).
          03 COR           PIC X(01).
          03 ANOF          PIC 9(04).
      *
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
       77 W-CONT        PIC 9(06) VALUE ZEROS.
       77 W-OPCAO       PIC X(01) VALUE SPACES.
       77 W-ACT         PIC 9(02) VALUE ZEROS.
       77 MENS          PIC X(50) VALUE SPACES.
       77 CLEAR         PIC X(50) VALUE SPACES.
       01 ST-ERRO       PIC X(02) VALUE "00".
       01 W-SEL         PIC 9(01) VALUE ZEROS.

       01 TXTBRAND      PIC X(15) VALUE SPACES.
       01 IND           PIC 9(02) VALUE ZEROS.


       01 TABCORX.
          03 FILLER     PIC X(15) VALUE "WWHITE".
          03 FILLER     PIC X(15) VALUE "SSILVER".
          03 FILLER     PIC X(15) VALUE "BBLACK".
          03 FILLER     PIC X(15) VALUE "GGREY".
          03 FILLER     PIC X(15) VALUE "UBLUE".
          03 FILLER     PIC X(15) VALUE "YYELLOW".
          03 FILLER     PIC X(15) VALUE "RRED".
          03 FILLER     PIC X(15) VALUE "PPINK".
          03 FILLER     PIC X(15) VALUE "OOTHER".
       01 TABCOR REDEFINES TABCORX.
          03 TBCOR   PIC X(15) OCCURS 9 TIMES.
       01 TXTCOR.
          03 TXTCORCOD PIC X(01) VALUE SPACES.
          03 TXTCORTEXTO PIC X(14) VALUE SPACES.



       01 TABBRANDX.
          03 FILLER     PIC X(15) VALUE " AUDI".
          03 FILLER     PIC X(15) VALUE " PORSCHE".
          03 FILLER     PIC X(15) VALUE " FERRARI".
          03 FILLER     PIC X(15) VALUE " BMW".
          03 FILLER     PIC X(15) VALUE " LAMBORGHINI ".
          03 FILLER     PIC X(15) VALUE " JAGUAR".
          03 FILLER     PIC X(15) VALUE " MERCEDES".
          03 FILLER     PIC X(15) VALUE " ASTON MARTIN".
          03 FILLER     PIC X(15) VALUE " BUGATTI".
       01 TABBRAND REDEFINES TABBRANDX.
          03 TBBRAND   PIC X(15) OCCURS 9 TIMES.


      *-----------------------------------------------------------------
       SCREEN SECTION.
       01  TELA92.
           05  BLANK SCREEN.
           05  LINE 02  COLUMN 01
               VALUE  "                     ***VEHICLE REGISTRATION".
           05  LINE 02  COLUMN 41
               VALUE  "TION ***".
           05  LINE 04  COLUMN 01
               VALUE  "  PLATE:    -".
           05  LINE 07  COLUMN 01
               VALUE  "  OWNER:".
           05  LINE 10  COLUMN 01
               VALUE  "  BRAND:".
           05  LINE 13  COLUMN 01
               VALUE  "  MODEL:".
           05  LINE 16  COLUMN 01
               VALUE  " COLOR: ".
           05  LINE 19  COLUMN 01
               VALUE  "  FABRICATION:".
           05  TLETTERP
               LINE 04  COLUMN 10  PIC X(03)
               USING  LETTERP
               HIGHLIGHT.
           05  TNUMP
               LINE 04  COLUMN 14  PIC 9(04)
               USING  NUMP
               HIGHLIGHT.
           05  TNAMEP
               LINE 07  COLUMN 21  PIC X(30)
               USING  NAMEP
               HIGHLIGHT.
           05  TBRAND
               LINE 10  COLUMN 10  PIC 9(02)
               USING  BRAND
               HIGHLIGHT.
           05  TTXTBRAND
               LINE 10  COLUMN 13  PIC X(15)
               USING  TXTBRAND
               HIGHLIGHT.
           05  TMODEL
               LINE 13  COLUMN 11  PIC X(20)
               USING  MODEL
               HIGHLIGHT.
           05  TCOR
               LINE 16  COLUMN 08  PIC X(01)
               USING  COR
               HIGHLIGHT.
           05  TTXTCOR
               LINE 16  COLUMN 10  PIC X(15)
               USING  TXTCOR
               HIGHLIGHT.
           05  TANOF
               LINE 19  COLUMN 19  PIC 9(04)
               USING  ANOF
               HIGHLIGHT.

       01  TELA92M.
           05  LINE 10  COLUMN 41
               VALUE  "         1 - AUDI".
           05  LINE 11  COLUMN 41
               VALUE  "         2 - PORSCHE".
           05  LINE 12  COLUMN 41
               VALUE  "         3 - FERRARI".
           05  LINE 13  COLUMN 41
               VALUE  "         4 - BMW".
           05  LINE 14  COLUMN 41
               VALUE  "         5 - LAMBORGHINI".
           05  LINE 15  COLUMN 41
               VALUE  "         6 - JAGUAR".
           05  LINE 16  COLUMN 41
               VALUE  "         7 - MERCEDES".
           05  LINE 17  COLUMN 41
               VALUE  "         8 - ASTON MARTIN".
           05  LINE 18  COLUMN 41
               VALUE  "         9 - BUGATTI".

       01  TELA92C.
           05  LINE 10  COLUMN 41
               VALUE  "         W - WHITE".
           05  LINE 11  COLUMN 41
               VALUE  "         S - SILVER".
           05  LINE 12  COLUMN 41
               VALUE  "         B - BLACK".
           05  LINE 13  COLUMN 41
               VALUE  "         G - GREY".
           05  LINE 14  COLUMN 41
               VALUE  "         U - BLUE".
           05  LINE 15  COLUMN 41
               VALUE  "         Y - YELLOW".
           05  LINE 16  COLUMN 41
               VALUE  "         R - RED".
           05  LINE 17  COLUMN 41
               VALUE  "         P - PINK".
           05  LINE 18  COLUMN 41
               VALUE  "         O - OTHER".
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.

      ********** OPEN OR CREATE FILE*************************
       R0.
           OPEN I-O VEHREG
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "30"
                 OPEN OUTPUT VEHREG
                 CLOSE VEHREG
                 MOVE "*** CREATE FILE **" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R0
              ELSE
                 MOVE "ERROR CREATING FILE" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
                 NEXT SENTENCE.

      ********** END *************************
      ********* SHOW SCREEN**********************
       R1.
           MOVE SPACES TO LETTERP NAMEP MODEL COR TXTCOR TXTBRAND
           MOVE ZEROS TO NUMP BRAND ANOF
           DISPLAY TELA92.

      ********* END SHOW SCREEN **********************
       R2.
           ACCEPT TLETTERP
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO ROT-FIM.
           ACCEPT TNUMP

      ***** CHECKS IF A CARD HAS BEEN ENTERED
           IF LETTERP = SPACES
              MOVE "*** ENTER PLATE ***" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R2.

       READ-VEHREG.
           READ VEHREG
           IF ST-ERRO NOT = "23"
             IF ST-ERRO = "00"
                PERFORM R4A
                PERFORM R6A
                DISPLAY TELA92
                GO TO ACE-001

             ELSE
                MOVE "ERROR READING THE FILE" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM
           ELSE
                NEXT SENTENCE.

       R3.
           DISPLAY TELA92
           ACCEPT TNAMEP
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R2.
           IF NAMEP = SPACES
              MOVE "ENTER NAME OF CAR OWNER" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R3.

       R4.
           DISPLAY TELA92M
           ACCEPT TBRAND
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R3.
           IF BRAND = 0
              MOVE "*** CHOOSE THE CAR BRAND ***" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R4.
       R4A.
           MOVE TBBRAND(BRAND) TO TXTBRAND
           DISPLAY TXTBRAND.

           DISPLAY TELA92.

       R5.
           DISPLAY TELA92
           ACCEPT TMODEL
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R4.
           IF MODEL = SPACES
              MOVE "*** TYPE A MODEL ***" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R5.

       R6.
           DISPLAY TELA92C
           MOVE 1 TO IND

           ACCEPT TCOR
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R5.
           IF COR = SPACES
              MOVE "*** CHOOSE CAR COLOR ***" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R6.
       R6A.
           MOVE TBCOR(IND) TO TXTCOR
           IF TXTCORCOD NOT = COR
              ADD 1 TO IND
              IF IND < 10
                 GO TO R6A
              ELSE
                 MOVE "*** INCORRECT COR CODE ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R6
           ELSE
                MOVE TXTCORTEXTO TO TXTCOR
                DISPLAY TTXTCOR.
                DISPLAY TELA92.

       R7.
           MOVE 1 TO IND
           ACCEPT TANOF
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R6.
           IF ANOF = 0
              MOVE "***ENTER YEAR OF CAR MANUFACTURE***" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R7.




       INC-OPC.
                MOVE "Y" TO W-OPCAO
                DISPLAY (23, 40) "CONFIRM (Y/N) : ".
                ACCEPT (23, 57) W-OPCAO WITH UPDATE
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 01 GO TO R7.
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** DATA DECLINED BY THE OPERATOR ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                IF W-OPCAO NOT = "Y" AND "y"
                   MOVE "*** JUST TYPE Y=YES e N=NO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-OPC.
       INC-WR1.
                WRITE REGVEHI
                IF ST-ERRO = "00" OR "02"
                      MOVE "*** RECORDE DATA *** " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO R1.
                IF ST-ERRO = "22"

                  GO TO ALT-RW1
                ELSE
                      MOVE "ERROR IN RECORDING CAR FILE"
                                                       TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM.


      *****************************************
      * ROTINA DE CONSULTA/ALTERACAO/EXCLUSAO *
      *****************************************
      *
       ACE-001.
                DISPLAY (23, 12)
                     "N=NEW RECORD   C=CHANGE   D=DELETE"
                ACCEPT (23, 55) W-OPCAO
                IF W-OPCAO NOT = "N" AND W-OPCAO NOT = "C"
                    AND W-OPCAO NOT = "D" GO TO ACE-001.
                MOVE SPACES TO MENS
                DISPLAY (23, 12) MENS
                IF W-OPCAO = "N"
                   GO TO R1
                ELSE
                   IF W-OPCAO = "C"
                      MOVE 1 TO W-SEL
                      GO TO R3.
      *
       EXC-OPC.
                DISPLAY (23, 40) "DELETE   (Y/N) : ".
                ACCEPT (23, 57) W-OPCAO
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** NO DELETE ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                IF W-OPCAO NOT = "Y" AND "y"
                   MOVE "* JUST TYPE Y=YES  e  N=NO *" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO EXC-OPC.
       EXC-DL1.
                DELETE VEHREG RECORD
                IF ST-ERRO = "00"
                   MOVE "*** DELETED CAR RECORD ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                MOVE "RECORD DELETION ERROR "   TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.
      *
       ALT-OPC.
                DISPLAY (23, 40) "CHANGE  (Y/N) : ".
                ACCEPT (23, 57) W-OPCAO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 01 GO TO R7.
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** NO CHANGE *** " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                IF W-OPCAO NOT = "Y" AND "y"
                   MOVE "*** JUST TYPE Y=YES  e  N=NO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ALT-OPC.
       ALT-RW1.
                REWRITE REGVEHI
                IF ST-ERRO = "00" OR "02"
                   MOVE "*** CHANGE RECORD ***         " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                MOVE "RECORD CHANGE ERROR"   TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.






       ROT-FIM.
           CLOSE VEHREG.
           STOP RUN.

      *---------[ ROTINA DE MENSAGEM ]---------------------
       ROT-MENS.
                MOVE ZEROS TO W-CONT.
       ROT-MENS1.
               DISPLAY (23, 12) MENS.
       ROT-MENS2.
                ADD 1 TO W-CONT
                IF W-CONT < 3000
                   GO TO ROT-MENS2
                ELSE
                   MOVE SPACES TO MENS
                   DISPLAY (23, 12) MENS.
       ROT-MENS-FIM.
                EXIT.
       FIM-ROT-TEMPO.

      *    FILE STATUS
      *    00 = OPERA��O REALIZADO COM SUCESSO
      *    22 = REGISTRO J� CADASTRADO
      *    23 = REGISTRO N�O ENCONTRADO
      *    30 = ARQUIVO N�O ENCONTRADO
