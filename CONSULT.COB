       IDENTIFICATION DIVISION.
       PROGRAM-ID. CONSULT.
      *AUTHOR. GUSTAVO ARAUJO NARCISO.
      *******************************
      *      MEDICAL APPOINTMENT    *
      *******************************
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
          03 SPEC          PIC 9(02).
          03 SEX           PIC X(01).
          03 BIRTHDAY.
            05 BD          PIC 99.
            05 MD          PIC 99.
            05 YD          PIC 9(04).
          03 EMAIL         PIC X(30).
          03 PHONE.
            05 DDD         PIC 9(02).
            05 NUM         PIC 9(09).
      *
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
       01 W-SEL        PIC 9(01) VALUE ZEROS.
       01 W-CONT       PIC 9(04) VALUE ZEROS.
       01 W-OPCAO      PIC X(01) VALUE SPACES.
       01 ST-ERRO      PIC X(02) VALUE "00".
       01 W-ACT        PIC 9(02) VALUE ZEROS.
       01 MENS         PIC X(50) VALUE SPACES.
       01 LIMPA        PIC X(55) VALUE SPACES.
       01 SOLIC        PIC X(20) VALUE SPACES.
       01 CONLIN       PIC 9(03) VALUE 002.
       01 CEPENTR      PIC 9(09) VALUE ZEROS.
       01 LOGRENTR     PIC X(35) VALUE SPACES.

       01 TABESP.
          03 COLESP    PIC X(30) OCCURS 6 TIMES.
      *-----------------------------------------------------------------
      *
      ******************
      * REPORT SECTION *
      ******************
      *
       01 DET2.
          03 FILLER    PIC X(07) VALUE " CRM : ".
          03 DETCRM    PIC 999999.

       01 DET3.
           03 FILLER    PIC X(09) VALUE " NAME : ".
           03 DETNOME   PIC X(40) VALUE SPACES.

       01 DET4.
           03 FILLER    PIC X(14) VALUE " SPECIALITY : ".
           03 DETESP    PIC 99.

       01 DET5.
          03 FILLER    PIC X(14) VALUE " BIRTHDAY : ".
          03 DETDIA    PIC 99.
          03 FILLER    PIC X(01) VALUE "/".
          03 DETMES    PIC 99.
          03 FILLER    PIC X(01) VALUE "/".
          03 DETANO    PIC 9999.

       01 DET6.
          03 FILLER    PIC X(09) VALUE " EMAIL : ".
          03 DETEMAIL  PIC X(30) VALUE SPACES.

       01 DET7.
          03 FILLER    PIC X(12) VALUE " PHONE : ".
          03 DETDDD    PIC 99.
          03 FILLER    PIC X(01) VALUE SPACES.
          03 DETTEL    PIC 999999999.

       01 DET8.
          03 FILLER    PIC X(10) VALUE " GENDER : ".
          03 DETSEXO   PIC X(01) VALUE SPACES.
          03 FILLER    PIC X(01) VALUE SPACES.
          03 DETDESC   PIC X(15) VALUE SPACES.

      ******************
      * SCREEN ROUTINE *
      ******************
      *
       SCREEN SECTION.

       01  TELAMED.
           05  BLANK SCREEN.
           05  LINE 02  COLUMN 07
               VALUE  "                    MEDICAL APPOINTMENT".
           05  LINE 23  COLUMN 01
               VALUE  "  MENSSAGE :".

           05  LINE 04  COLUMN 01
               VALUE  " CRM: ".

           05  LINE 04  COLUMN 14
               VALUE  " NAME: ".

           05  TCRM
               LINE 04  COLUMN 07  PIC 9(06)
               USING  CRM
               HIGHLIGHT.

           05  TNOUN
               LINE 04  COLUMN 21  PIC X(30)
               USING  NOUN
               HIGHLIGHT.

      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO.
      *
       INC-OP1.
           OPEN INPUT  CADMED
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "30"
                 MOVE "FILE NOT EXISTS" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM2
              ELSE
                 MOVE "ERROR IN OPEN FILE"  TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM2
           ELSE
               NEXT SENTENCE.

      *
       INC-001.
           MOVE ZEROS TO CRM
           MOVE SPACES TO NOUN
           DISPLAY TELAMED.
       INC-001A.
           DISPLAY (23, 13)
            "SOLIC.=> F1=CLOSE   F2=CONSULT F/NAME."
           ACCEPT TCRM
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 02
                      CLOSE CADMED
                      GO TO ROT-FIM.
           IF W-ACT = 03
                      GO TO INC-001B.
           IF W-ACT > 03
                      MOVE "*** UNDEFINED FUNCTION ***" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-001A.
           START CADMED KEY IS NOT LESS CRM INVALID KEY
                 MOVE "*** END OF PROCESSING ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO INC-001.
           GO TO INC-RD2.
       INC-001B.
           DISPLAY (23, 13)
            "SOLIC.=> F1=CLOSE   F2=CONSULT F/CRM."
           ACCEPT TNOUN
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 02
                      CLOSE CADMED
                      GO TO ROT-FIM.
           IF W-ACT = 03
                      GO TO INC-001A.
           IF W-ACT > 03
                      MOVE "*** UNDEFINED FUNCTION ***" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-001A.
           START CADMED KEY IS NOT LESS NOUN INVALID KEY
                 MOVE "*** END OF PROCESSING ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO INC-001B.
       INC-RD2.
           DISPLAY (23, 13) LIMPA
           READ CADMED NEXT
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "10"
                 MOVE "*** END OF PROCESSING ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO NOV-SOL
              ELSE
                 MOVE "ERROR READING THE FILE"  TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
              NEXT SENTENCE.

       ROT-MONTAR.
           MOVE CRM TO DETCRM
           MOVE NOUN TO DETNOME
           MOVE SPEC TO DETESP
           MOVE BD TO DETDIA
           MOVE MD TO DETMES
           MOVE YD TO DETANO
           MOVE EMAIL TO DETEMAIL
           MOVE SEX TO DETSEXO

           IF SEX = "F"
              MOVE "FEMALE" TO DETDESC
           ELSE
              IF SEX = "M"
                 MOVE "MALE" TO DETDESC
              ELSE
                 MOVE "OTHER" TO DETDESC.

           MOVE DDD TO DETDDD
           MOVE NUM TO DETTEL
           COMPUTE LIN = CONLIN + 4
           DISPLAY (LIN, 01) DET2
           ADD 1 TO CONLIN
           COMPUTE LIN = CONLIN + 4
           DISPLAY (LIN, 01) DET3
           ADD 2 TO CONLIN
           COMPUTE LIN = CONLIN + 3
           DISPLAY (LIN, 01) DET4
           ADD 1 TO CONLIN
           COMPUTE LIN = CONLIN + 4
           DISPLAY (LIN, 01) DET5
           ADD 1 TO CONLIN
           COMPUTE LIN = CONLIN + 4
           DISPLAY (LIN, 01) DET6
           ADD 1 TO CONLIN
           COMPUTE LIN = CONLIN + 4
           DISPLAY (LIN, 01) DET7
           ADD 1 TO CONLIN
           COMPUTE LIN = CONLIN + 4
           DISPLAY (LIN, 01) DET8
           ADD 1 TO CONLIN
           IF CONLIN < 17
                  GO TO INC-RD2.
      *
       ROT-SOL.
           DISPLAY (23, 13) " *** CONTINUE CONSULT :    ***"
           MOVE "Y" TO W-OPCAO
           ACCEPT  (23, 38) W-OPCAO  WITH UPDATE
           IF W-OPCAO = "Y"
                  MOVE 01 TO CONLIN
                  DISPLAY TELAMED
                  GO TO INC-RD2.
           IF W-OPCAO NOT = "N"
                  MOVE "*** TYPE Y=YES   N=NO ***" TO MENS
                  PERFORM ROT-MENS THRU ROT-MENS-FIM
                  GO TO ROT-SOL.
           MOVE 01 TO CONLIN.
           GO TO INC-001.
      *
       NOV-SOL.
           DISPLAY (23, 13) " *** NEW CONSULT :    ***"
           MOVE "Y" TO W-OPCAO
           ACCEPT  (23, 34) W-OPCAO WITH UPDATE
           IF W-OPCAO = "Y"
                  MOVE 01 TO CONLIN
                  GO TO INC-001.
           IF W-OPCAO NOT = "N"
                  MOVE "*** TYPE Y=YES   N=NO ***" TO MENS
                  PERFORM ROT-MENS THRU ROT-MENS-FIM
                  GO TO NOV-SOL.

      *
      **********************
      *     END ROUTINE    *
      **********************
      *
       ROT-FIM.
           DISPLAY (01 01) ERASE.
           CLOSE CADMED.
       ROT-FIM2.
           EXIT PROGRAM.
       ROT-FIM3.
           STOP RUN.
      *
      **********************
      *   MESSAGE ROUTINE  *
      **********************
      *
       ROT-MENS.
           MOVE ZEROS TO W-CONT.
       ROT-MENS1.
           DISPLAY (23, 13) MENS.
       ROT-MENS2.
           ADD 1 TO W-CONT
           IF W-CONT < 3000
              GO TO ROT-MENS2
           ELSE
              DISPLAY (23, 12) LIMPA.
       ROT-MENS-FIM.
           EXIT.
       FIM-ROT-MENS.
      *

      *---------------------*** END OF PROGRAM ***--------------------*
