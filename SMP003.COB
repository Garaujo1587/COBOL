       IDENTIFICATION DIVISION.
       PROGRAM-ID. SMP003.
      *AUTHOR. GUSTAVO ARAUJO NARCISO.
      *******************************************
      *MAINTENANCE OF THE AGREEMENT REGISTRATION*
      *******************************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
             DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT CADCONV ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS COD
                    FILE STATUS  IS ST-ERRO
                    ALTERNATE RECORD KEY IS NOUN WITH DUPLICATES
                    ALTERNATE RECORD KEY IS PLAN WITH DUPLICATES.
      *
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD CADCONV
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADCONV.DAT".
       01 REGCONV.
          03 COD          PIC 9(04).
          03 NOUN         PIC X(30).
          03 PLAN         PIC 9(02).

      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
       77 W-CONT        PIC 9(06) VALUE ZEROS.
       77 W-OPCAO       PIC X(01) VALUE SPACES.
       77 W-ACT         PIC 9(02) VALUE ZEROS.
       77 MENS          PIC X(50) VALUE SPACES.
       77 LIMPA         PIC X(50) VALUE SPACES.
       01 ST-ERRO       PIC X(02) VALUE "00".
       01 W-SEL         PIC 9(01) VALUE ZEROS.
       01 TXTTPAMIGO    PIC X(10) VALUE SPACES.
       01 IND           PIC 9(02) VALUE ZEROS.
       01 TEXSEXO       PIC X(12) VALUE SPACES.

       01 TABCONVX.
          03 FILLER     PIC X(30) VALUE "ENFERMARIA REGINAL".
          03 FILLER     PIC X(30) VALUE "ENFERMARIA NACIONAL".
          03 FILLER     PIC X(30) VALUE "ENFERMARIA INTERNACIONAL".
          03 FILLER     PIC X(30) VALUE "APTO PADRAO REGINAL".
          03 FILLER     PIC X(30) VALUE "APTO PADRAO NACIONAL".
          03 FILLER     PIC X(30) VALUE "APTO PADRAO INTERNACIONAL".
          03 FILLER     PIC X(30) VALUE "EMERGENCIA REGIONAL".
          03 FILLER     PIC X(30) VALUE "EMERGENCIA NACIONAL".
          03 FILLER     PIC X(30) VALUE "EMERCENCIA INTERNACIONAL".
          03 FILLER     PIC X(30) VALUE "PLANO GLOBAL".
          03 FILLER     PIC X(30) VALUE "PLANO ESPACIAL".
          03 FILLER     PIC X(30) VALUE "PLANO INTERGALACTICO".

       01 TABCONV REDEFINES TABCONVX.
          03 TBCONV   PIC X(30) OCCURS 12 TIMES.

       01 PLANOTXT    PIC X(30) VALUE SPACES.

      *-----------------------------------------------------------------
       SCREEN SECTION.

       01  TELACONV.
           05  BLANK SCREEN.
           05  LINE 02  COLUMN 01
               VALUE  "--------------------------[ AGREEMENT R".
           05  LINE 02  COLUMN 40
               VALUE  "EGISTRATION ]----------------------------".
           05  LINE 05  COLUMN 01
               VALUE  "     COD:".
           05  LINE 08  COLUMN 01
               VALUE  "     NAME:".
           05  LINE 11  COLUMN 01
               VALUE  "     PLAN:".
           05  TCOD
               LINE 05  COLUMN 14  PIC 9(04)
               USING  COD
               HIGHLIGHT.
           05  TNOME
               LINE 08  COLUMN 12  PIC X(30)
               USING  NOUN
               HIGHLIGHT.
           05  TPLANO
               LINE 11  COLUMN 13  PIC 9(02)
               USING  PLAN
               HIGHLIGHT.
           05  TPLANOTXT
               LINE 11  COLUMN 17  PIC X(30)
               USING  PLANOTXT
               HIGHLIGHT.

       01  SMP003TELA.
           05  LINE 13  COLUMN 01
               VALUE  "----------------------------------------".
           05  LINE 13  COLUMN 41
               VALUE  "----------------------------------------".
           05  LINE 15  COLUMN 01
               VALUE  "   1 - ENFERMARIA REGIONAL".
           05  LINE 15  COLUMN 41
               VALUE  "       7 - EMERGENCIA REGIONAL".
           05  LINE 16  COLUMN 01
               VALUE  "   2 - ENFERMARIA NACIONAL".
           05  LINE 16  COLUMN 41
               VALUE  "       8 - EMERGENCIA NACIONAL".
           05  LINE 17  COLUMN 01
               VALUE  "   3 - ENFERMARIA INTERNACIONACAO".
           05  LINE 17  COLUMN 41
               VALUE  "       9 - EMERGENCIA INTERNACIONAL".
           05  LINE 18  COLUMN 01
               VALUE  "   4 - APTO PADRAO REGIONAL".
           05  LINE 18  COLUMN 41
               VALUE  "      10 - PLANO GLOBAL".
           05  LINE 19  COLUMN 01
               VALUE  "   5 - APTO PADRAO NACIONAL".
           05  LINE 19  COLUMN 41
               VALUE  "      11 - PLANO ESPACIAL".
           05  LINE 20  COLUMN 01
               VALUE  "   6 - APTO PADRAO INTERNACIONAL".
           05  LINE 20  COLUMN 41
               VALUE  "      12 - PLANO INTERGALACTICO".

      *-----------------------------------------------------------------
       PROCEDURE DIVISION.

       R0.
           OPEN I-O CADCONV
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "30"
                 OPEN OUTPUT CADCONV
                 CLOSE CADCONV
                 MOVE "*** CADAMIGO ARCHIVE WAS CREATED **" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R0
              ELSE
                 MOVE "ERROR IN OPENING THE CADMIGO FILE" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
                 NEXT SENTENCE.


       R1.
           MOVE SPACES TO NOUN PLANOTXT
           MOVE ZEROS TO COD PLAN
           DISPLAY TELACONV.

       R2.

           MOVE ZEROS TO COD

           ACCEPT TCOD
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO ROT-FIM.

           IF COD = ZEROS
                MOVE "INVALID CODE" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS2
                GO TO R2.


      *---------------CHECK IF IT ALREADY EXISTS IN THE FILE-----------------*

       LER-CADCID.
           READ CADCONV
           IF ST-ERRO NOT = "23"
             IF ST-ERRO = "00"
                PERFORM R5
                DISPLAY TELACONV
                GO TO ACE-001

             ELSE
                MOVE "ERROR READ CADCONV FILE" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM
           ELSE
                NEXT SENTENCE.

      *----------------------------------------------------------------*

       R3.

           MOVE SPACES TO NOUN

           ACCEPT TNOME
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R2.

           IF NOUN = SPACES
              MOVE "ENTER NAME" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R3.

       R4.
           MOVE ZEROS TO PLAN
           MOVE SPACES TO PLANOTXT
           DISPLAY SMP003TELA.

           ACCEPT TPLANO
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R3.

           IF PLAN = ZEROS
              MOVE "TYPE A VALID PLAN" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R4.

       R5.
           IF PLAN < 13
              MOVE TBCONV(PLAN) TO PLANOTXT
              DISPLAY TPLANOTXT
           ELSE
              GO TO R4.


       INC-OPC.
                MOVE "Y" TO W-OPCAO
                DISPLAY (23, 40) "SAVE (Y/N) : ".
                ACCEPT (23, 57) W-OPCAO WITH UPDATE
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 01 GO TO R4.
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** DATA DECLINED BY THE OPERATOR ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                IF W-OPCAO NOT = "Y" AND "y"
                   MOVE "*** DIGITE APENAS Y=YES OR N=NO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-OPC.
       INC-WR1.
                WRITE REGCONV
                IF ST-ERRO = "00" OR "02"
                      MOVE "*** RECORDED *** " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO R1.
                IF ST-ERRO = "22"

                  GO TO ALT-RW1
                ELSE
                      MOVE "ERROR IN FILE RECORDING"
                                                       TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM.


      *****************************************
      * ROTINA DE CONSULTA/ALTERACAO/EXCLUSAO *
      *****************************************

       ACE-001.
                DISPLAY (23, 12)
                     "N=NEW   C=CHANGE   D=DELETE"
                ACCEPT (23, 55) W-OPCAO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 01
                   GO TO R1.
                IF W-OPCAO NOT = "N" AND W-OPCAO NOT = "C"
                    AND W-OPCAO NOT = "D" GO TO ACE-001.
                MOVE SPACES TO MENS
                DISPLAY (23, 12) MENS
                IF W-OPCAO = "N"
                   GO TO R1
                ELSE
                   IF W-OPCAO = "C"
                      MOVE 1 TO W-SEL
                      GO TO R3
                   ELSE
                      GO TO EXC-OPC.
      *
       EXC-OPC.
                DISPLAY (23, 40) "DELETE   (Y/N) : ".
                ACCEPT (23, 57) W-OPCAO
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** REGISTRATION NOT EXCLUDED ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                IF W-OPCAO NOT = "Y" AND "y"
                   MOVE "* JUST TYPE Y=YES  OR  N=NO *" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO EXC-OPC.
       EXC-DL1.
                DELETE CADCONV RECORD
                IF ST-ERRO = "00"
                   MOVE "*** DELETED ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                MOVE "ERROR EXCLUSION FROM REGISTRATION"   TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.
      *
       ALT-OPC.
                DISPLAY (23, 40) "CHANGE  (Y/N) : ".
                ACCEPT (23, 57) W-OPCAO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 01 GO TO R4.
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** NOT CHANGED INFORMATION *** " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** JUST TYPE Y=YES  OR  N=NO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ALT-OPC.
       ALT-RW1.
                REWRITE REGCONV
                IF ST-ERRO = "00" OR "02"
                   MOVE "*** REGISTRATION CHANGED ***         " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                MOVE "ERROR IN CHANGING THE REGISTRATION"   TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.

       ROT-FIM.
           CLOSE CADCONV.
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
