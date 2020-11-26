       IDENTIFICATION DIVISION.
       PROGRAM-ID. SMP001.
      *AUTHOR. GUSTAVO ARAUJO NARCISO.
      **************************************
      *       MAINTANANCE REGISTER         *
      **************************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
             DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT CADMED ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS CRM
                    FILE STATUS  IS ST-ERRO
                    ALTERNATE RECORD KEY IS NOUN WITH DUPLICATES.
      *
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD CADMED
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADMED.DAT".
       01 REGMED.
          03 CRM           PIC 9(06).
          03 NOUN          PIC X(30).
          03 SPECIALITY    PIC 9(02).
          03 SEX           PIC X(01).
          03 DNASC.
             05 DIANAS     PIC 9(02).
             05 MESNAS     PIC 9(02).
             05 ANONAS     PIC 9(04).
          03 EMAIL         PIC X(30).
          03 PHONE      PIC 9(11).
      *
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
       77 W-CONT        PIC 9(06) VALUE ZEROS.
       77 W-OPCAO       PIC X(01) VALUE SPACES.
       77 W-ACT         PIC 9(02) VALUE ZEROS.
       77 MENS          PIC X(50) VALUE SPACES.
       77 LIMPA         PIC X(50) VALUE SPACES.
       01 ST-ERRO       PIC X(02) VALUE "00".
       01 W-SEL         PIC 9(01) VALUE ZEROS.

       01 TXTESPEC      PIC X(15) VALUE SPACES.
       01 IND           PIC 9(02) VALUE ZEROS.


       01 TABSEXOX.
          03 FILLER     PIC X(11) VALUE "MMALE".
          03 FILLER     PIC X(11) VALUE "FFEMALE".
       01 TABESP REDEFINES TABSEXOX.
          03 TBSEXO   PIC X(11) OCCURS 2 TIMES.
       01 TXTSEXO.
          03 TXTSEXOCOD PIC X(01) VALUE SPACES.
          03 TXTSEXOTEXTO PIC X(10) VALUE SPACES.



       01 TABESPECX.
          03 FILLER     PIC X(15) VALUE "MEDICAL CLINIC".
          03 FILLER     PIC X(15) VALUE "UROLOGY".
          03 FILLER     PIC X(15) VALUE "GYNECOLOGIST".
          03 FILLER     PIC X(15) VALUE "PEDIATRICS".
          03 FILLER     PIC X(15) VALUE "CARDIOLOGIST ".
          03 FILLER     PIC X(15) VALUE "OTHER".
       01 TABESPEC REDEFINES TABESPECX.
          03 TBESPEC   PIC X(15) OCCURS 6 TIMES.


      *-----------------------------------------------------------------
       SCREEN SECTION.
       01  TELAMED.
           05  BLANK SCREEN.
           05  LINE 02  COLUMN 01
               VALUE  "                             MEDICAL REG".
           05  LINE 02  COLUMN 41
               VALUE  "ISTER".
           05  LINE 04  COLUMN 01
               VALUE  "   CRM:".
           05  LINE 07  COLUMN 01
               VALUE  "   NAME:".
           05  LINE 10  COLUMN 01
               VALUE  "   SPECIALITY:".
           05  LINE 13  COLUMN 01
               VALUE  "   SEX:".
           05  LINE 16  COLUMN 01
               VALUE  "   BIRTHDAY:".
           05  LINE 19  COLUMN 01
               VALUE  "   EMAIL:".
           05  LINE 22  COLUMN 01
               VALUE  "   PHONE:".
           05  TCRM
               LINE 04  COLUMN 09  PIC 9(09)
               USING  CRM
               HIGHLIGHT.
           05  TNOME
               LINE 07  COLUMN 10  PIC X(30)
               USING  NOUN
               HIGHLIGHT.
           05  TESPECIALIDADE
               LINE 10  COLUMN 19  PIC X(02)
               USING  SPECIALITY
               HIGHLIGHT.
           05  TTXTESPEC
               LINE 10  COLUMN 22  PIC X(20)
               USING  TXTESPEC
               HIGHLIGHT.
           05  TSEXO
               LINE 13  COLUMN 10  PIC X(01)
               USING  SEX
               HIGHLIGHT.
           05  TTXTSEXO
               LINE 13  COLUMN 12  PIC X(10)
               USING  TXTSEXO
               HIGHLIGHT.
           05  TDNASC
               LINE 16  COLUMN 21  PIC X(08)
               USING  DNASC
               HIGHLIGHT.
           05  TEMAIL
               LINE 19  COLUMN 11  PIC X(30)
               USING  EMAIL
               HIGHLIGHT.
           05  TTELEFONE
               LINE 22  COLUMN 14  PIC X(11)
               USING  PHONE
               HIGHLIGHT.

       01  TELAESP.
           05  LINE 10  COLUMN 41
               VALUE  "         1 - MEDICAL CLINIC".
           05  LINE 11  COLUMN 41
               VALUE  "         2 - UROLOGY".
           05  LINE 12  COLUMN 41
               VALUE  "         3 - GYNECOLOGIST".
           05  LINE 13  COLUMN 41
               VALUE  "         4 - PEDIATRICS".
           05  LINE 14  COLUMN 41
               VALUE  "         5 - CARDIOLOGIST".
           05  LINE 15  COLUMN 41
               VALUE  "         6 - OTHER".

       01  TELASEXO.
           05  LINE 10  COLUMN 41
               VALUE  "         M - MALE".
           05  LINE 11  COLUMN 41
               VALUE  "         F - FEMALE".
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.

      ********** OPEN OR CREATE FILE *************************
       R0.
           OPEN I-O CADMED
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "30"
                 OPEN OUTPUT CADMED
                 CLOSE CADMED
                 MOVE "*** CREATE FILE CADMED **" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R0
              ELSE
                 MOVE "ERROR IN OPEN FILE CADMED" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
                 NEXT SENTENCE.

      ********** END OPEN OR CREATE FILE *************************
      ********* RESET FIELDS AND SHOW SCREEN **********************
       R1.
           MOVE SPACES TO NOUN SEX EMAIL TXTSEXO TXTESPEC
           MOVE ZEROS TO CRM SPECIALITY DNASC PHONE
           DISPLAY TELAMED.

      ********* END RESET FIELDS AND SHOW SCREEN **********************
       R2.
           ACCEPT TCRM
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO ROT-FIM.


      ***** CHECKS IF TYPE CRM
           IF CRM = 0
              MOVE "*** ENTER CRM ***" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R2.

       LER-CADMED.
           READ CADMED
           IF ST-ERRO NOT = "23"
             IF ST-ERRO = "00"
                PERFORM R4A
                PERFORM R5A
                DISPLAY TELAMED
                GO TO ACE-001

             ELSE
                MOVE "ERROR IN READING FILE CADMED" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM
           ELSE
                NEXT SENTENCE.

       R3.
           DISPLAY TELAMED
           ACCEPT TNOME
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R2.
           IF NOUN = SPACES
              MOVE "ENTER NAME DOCTOR" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R3.

       R4.
           DISPLAY TELAMED
           DISPLAY TELAESP
           ACCEPT TESPECIALIDADE
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R3.
           IF SPECIALITY = 0 OR SPECIALITY > 6
              MOVE "*** CHOOSE A SPECIALITY ***" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R4.
       R4A.
           MOVE TBESPEC(SPECIALITY) TO TXTESPEC
           DISPLAY TXTESPEC.

           DISPLAY TELAMED.



       R5.
           DISPLAY TELASEXO
           MOVE 1 TO IND

           ACCEPT TSEXO
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R4.
           IF SEX = SPACES
              MOVE "*** CHOOSE A GENDER ***" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R5.
       R5A.
           MOVE TBSEXO(IND) TO TXTSEXO
           IF TXTSEXOCOD NOT = SEX
              ADD 1 TO IND
              IF IND < 3
                 GO TO R5A
              ELSE
                 MOVE "*** GENDER INVALID ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R5
           ELSE
                MOVE TXTSEXOTEXTO TO TXTSEXO
                DISPLAY TTXTSEXO.
                DISPLAY TELAMED.

       R6.
           DISPLAY TELAMED
           ACCEPT TDNASC
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R5.
           IF DNASC = 0
              MOVE "*** ENTER BIRTHDAY ***" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R6.
           IF MESNAS < 1 OR MESNAS > 12
              MOVE "*** MONTH INVALID ***" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R6.
           IF DIANAS < 1 OR DIANAS > 31
              MOVE "*** DAY INVALID ***" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R6.
           IF ANONAS < 1900 OR ANONAS > 2005
              MOVE "*** YEAR INVALID ***" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R6.


       R7.
           DISPLAY TELAMED
           ACCEPT TEMAIL
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R6.
           IF EMAIL = SPACES
              MOVE "*** ENTER EMAIL ***" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R7.

       R8.
           ACCEPT TTELEFONE
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R7.
           IF PHONE = 0
              MOVE "***ENTER PHONE***" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R8.




       INC-OPC.
                MOVE "Y" TO W-OPCAO
                DISPLAY (23, 40) "SAVE (Y/N) : ".
                ACCEPT (23, 57) W-OPCAO WITH UPDATE
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 01 GO TO R7.
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** DATA DECLINED BY THE OPERATOR ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                IF W-OPCAO NOT = "Y" AND "y"
                   MOVE "*** JUST TYPE Y=YES OR N=NO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-OPC.
       INC-WR1.
                WRITE REGMED
                IF ST-ERRO = "00" OR "02"
                      MOVE "*** RECORDED *** " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO R1.
                IF ST-ERRO = "22"

                  GO TO ALT-RW1
                ELSE
                      MOVE "ERROR IN RECORDING FILE CADMED"
                                                       TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM.


      *****************************************
      * ROTINA DE CONSULTA/ALTERACAO/EXCLUSAO *
      *****************************************
      *
       ACE-001.
                DISPLAY (23, 12)
                     "N=NEW   C=CHANGE   D=DELETE"
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
                   MOVE "*** REGISTER HAS NOT DELETED ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                IF W-OPCAO NOT = "Y" AND "y"
                   MOVE "* JUST TYPE Y=YES  OR  N=NO *" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO EXC-OPC.
       EXC-DL1.
                DELETE CADMED RECORD
                IF ST-ERRO = "00"
                   MOVE "*** REGISTER DELETED ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                MOVE "ERROR IN DELETED REGISTER "   TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.
      *
       ALT-OPC.
                DISPLAY (23, 40) "CHANGE  (Y/N) : ".
                ACCEPT (23, 57) W-OPCAO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 01 GO TO R8.
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** INFORMATION HAS NOT CHANGED *** " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                IF W-OPCAO NOT = "Y" AND "y"
                   MOVE "*** JUST TYPE Y=YES  OR  N=NO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ALT-OPC.
       ALT-RW1.
                REWRITE REGMED
                IF ST-ERRO = "00" OR "02"
                   MOVE "*** CHANGED RECORD ***         " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                MOVE "ERROR IN CHANGING REGISTER CADMED"   TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.






       ROT-FIM.
           CLOSE CADMED.
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
