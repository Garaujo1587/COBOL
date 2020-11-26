       IDENTIFICATION DIVISION.
       PROGRAM-ID. MENU.

      *AUTHOR. GUSTAVO ARAUJO NARCISO.
      **************************************
      *                 MENU               *
      **************************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
                    DECIMAL-POINT IS COMMA.
      *-----------------------------------------------------------------
       DATA DIVISION.
      *
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
       01 W-CONT       PIC 9(04) VALUE ZEROS.
       01 W-OPCAO      PIC 9(02) VALUE ZEROS.
       77 W-CONT2       PIC 9(06) VALUE ZEROS.
       01 ST-ERRO      PIC X(02) VALUE "00".
       01 MENS         PIC X(50) VALUE SPACES.
       01 LIMPA        PIC X(50) VALUE SPACES.
       01 W-PROGR      PIC X(07) VALUE SPACES.

      *-----------------------------------------------------------------
       01 TAB-PROGR-R.
           03 FILLER PIC X(07) VALUE "SMP001".
           03 FILLER PIC X(07) VALUE "SMP002".
           03 FILLER PIC X(07) VALUE "SMP003".
           03 FILLER PIC X(07) VALUE "SMP004".
           03 FILLER PIC X(07) VALUE "SMP005".
           03 FILLER PIC X(07) VALUE "SMP040A".
           03 FILLER PIC X(07) VALUE "SMP006".
           03 FILLER PIC X(07) VALUE "CONSULT".
           03 FILLER PIC X(07) VALUE "SMP009".
           03 FILLER PIC X(07) VALUE "SMPCCOM".
           03 FILLER PIC X(07) VALUE "SMP011".
           03 FILLER PIC X(07) VALUE "SMP010".
           03 FILLER PIC X(07) VALUE "PROGR93".

      *
       01 TAB-PROGR   REDEFINES TAB-PROGR-R.
          03 TAB-PROG  PIC X(07) OCCURS 10 TIMES.
      **********************
      *
      ******************
      * SCREEN ROUTINE *
      ******************
      *
       SCREEN SECTION.
       01  SMTMEN.
           05  BLANK SCREEN.
           05  LINE 02  COLUMN 01
               VALUE  "                             CLINICAL SY".
           05  LINE 02  COLUMN 41
               VALUE  "STEM".
           05  LINE 05  COLUMN 01
               VALUE  "                          01 - MEDICAL R".
           05  LINE 05  COLUMN 41
               VALUE  "EGISTRATION".
           05  LINE 06  COLUMN 01
               VALUE  "                          02 - DISEASE R".
           05  LINE 06  COLUMN 41
               VALUE  "EGISTRATION".
           05  LINE 07  COLUMN 01
               VALUE  "                          03 - AGREEMENT".
           05  LINE 07  COLUMN 41
               VALUE  " REGISTRATION".
           05  LINE 08  COLUMN 01
               VALUE  "                          04 - PATIENT R".
           05  LINE 08  COLUMN 41
               VALUE  "EGISTRATION".
           05  LINE 09  COLUMN 01
               VALUE  "                          05 - CEP REGIS".
           05  LINE 09  COLUMN 41
               VALUE  "TRATION".
           05  LINE 10  COLUMN 01
               VALUE  "                          06 - CEP CONSU".
           05  LINE 10  COLUMN 41
               VALUE  "LTATION".
           05  LINE 11  COLUMN 01
               VALUE  "                          07 - REGISTRAT".
           05  LINE 11  COLUMN 41
               VALUE  "ION OF CONSULTATIONS".
           05  LINE 12  COLUMN 01
               VALUE  "                          08 - MEDICAL C".
           05  LINE 12  COLUMN 41
               VALUE  "ONSULTATION".
           05  LINE 13  COLUMN 01
               VALUE  "                          09 - CADCEP RE".
           05  LINE 13  COLUMN 41
               VALUE  "PORT".
           05  LINE 14  COLUMN 01
               VALUE  "                          10 - AGREEMENT".
           05  LINE 14  COLUMN 41
               VALUE  " CONSULTATION".
           05  LINE 15  COLUMN 01
               VALUE  "                          11 - AGREEMENT".
           05  LINE 15  COLUMN 41
               VALUE  " REPORT".
           05  LINE 16  COLUMN 01
               VALUE  "                          12 - DISEASE R".
           05  LINE 16  COLUMN 41
               VALUE  "EPORT".
           05  LINE 17  COLUMN 01
               VALUE  "                          13 - VEHICLE R".
           05  LINE 17  COLUMN 41
               VALUE  "EGISTRATION".
           05  LINE 20  COLUMN 01
               VALUE  "                             OPTIONS :".
           05  LINE 20  COLUMN 41
               VALUE  " ( 00 - CLOSE )".
           05  T-OPCAO
               LINE 20  COLUMN 38  PIC 9(02)
               USING  W-OPCAO
               HIGHLIGHT.
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO.

      *****************************
      * LEVEL 1 SELECTION ROUTINE *
      *****************************
      *
       NIVEL1.
           DISPLAY SMTMEN.

           ACCEPT T-OPCAO.
           IF W-OPCAO = 00
                      GO TO Rfim1.
           IF W-OPCAO > 13
                 MOVE "*** FUNCTION NOT AVAILABLE *** " TO MENS
                 PERFORM RMensa THRU RMensa-FIM
                 GO TO NIVEL1.
           MOVE TAB-PROG(W-OPCAO) TO W-PROGR
           IF W-PROGR = SPACES
                 MOVE "*** FUNCTION NOT IMPLEMENTED *** " TO MENS
                 PERFORM RMensa THRU RMensa-FIM
                 GO TO NIVEL1.
      *
       Rcall.
           CALL W-PROGR ON OVERFLOW
              MOVE "* PROGRAM CALL ERROR *" TO MENS
              PERFORM RMensa THRU RMensa-FIM
              GO TO Rcall1.
           CANCEL W-PROGR.
       Rcall1.
           GO TO NIVEL1.


      *********** MENSAGEM **********************
       RMensa.
                MOVE ZEROS TO W-CONT
                MOVE ZEROS TO W-CONT2.

       RMensa1.
               IF W-CONT2 < 14
                  DISPLAY (24, 12) Mens
               ELSE
                  DISPLAY (24, 12) Limpa
                  MOVE SPACES TO MENS
                  GO TO RMensa-Fim.

       RMensa2.
                ADD 1 TO W-CONT
                IF W-CONT < 1000
                   GO TO RMensa2
                ELSE
                   ADD 1 TO W-CONT2
                   MOVE ZEROS TO W-CONT
                   DISPLAY (24, 12) LIMPA.
       RMensa3.
                ADD 1 TO W-CONT
                IF W-CONT < 1000
                   GO TO RMensa3
                ELSE
                   ADD 1 TO W-CONT2
                   MOVE ZEROS TO W-CONT
                   DISPLAY (24, 12) MENS.
                   GO TO RMensa1.

       RMensa-FIM.
                EXIT.

      ***************** ROTINA FIM ***************
       Rfim.
           EXIT PROGRAM.
       Rfim1.
           DISPLAY (01, 01) ERASE.
           STOP RUN.
