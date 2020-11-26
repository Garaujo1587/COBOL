       IDENTIFICATION DIVISION.
       PROGRAM-ID. SMP002.
      *AUTHOR. GUSTAVO ARAUJO NARCISO.
      ******************************************
      *   MAINTENANCE OF DISEASE REGISTRATION  *
      ******************************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
             DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT CADCID ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS COD
                    FILE STATUS  IS ST-ERRO
                    ALTERNATE RECORD KEY IS DESCRIPTION WITH DUPLICATES.
      *
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD CADCID
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADCID.DAT".
       01 REGCID.
          03 COD               PIC 9(08).
          03 DESCRIPTION       PIC X(30).
          03 SYMPTOMS          PIC X(50).
          03 TREATMENT         PIC X(50).
          03 TIPO              PIC X(30).
          03 NOME              PIC X(30).

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



      *-----------------------------------------------------------------
       SCREEN SECTION.

       01  CIDTELA.
           05  BLANK SCREEN.
           05  LINE 02  COLUMN 01
               VALUE  "---------------------------[ DISEASE REC".
           05  LINE 02  COLUMN 41
               VALUE  "ORD ]-----------------------------------".
           05  LINE 05  COLUMN 01
               VALUE  "     DISEASE CODE:".
           05  LINE 08  COLUMN 01
               VALUE  "     DESCRIPTION:".
           05  LINE 11  COLUMN 01
               VALUE  "     SYMPTOMS:".
           05  LINE 14 COLUMN 01
               VALUE  "     TREATMENT:".
           05  LINE 17 COLUMN 01
               VALUE  "     TYPE:".
           05  LINE 20 COLUMN 01
               VALUE  "     NAME:".

           05  TCOD
               LINE 05  COLUMN 24  PIC 9(04)
               USING  COD
               HIGHLIGHT.
           05  TDEN
               LINE 08  COLUMN 19  PIC X(30)
               USING  DESCRIPTION
               HIGHLIGHT.
           05  TSIN
               LINE 11  COLUMN 16  PIC X(50)
               USING  SYMPTOMS
               HIGHLIGHT.
           05  TTRAT
               LINE 14  COLUMN 18  PIC X(50)
               USING  TREATMENT
               HIGHLIGHT.
           05  TTIPO
               LINE 17  COLUMN 13  PIC X(30)
               USING  TIPO
               HIGHLIGHT.
           05  TNOME
               LINE 20  COLUMN 13  PIC X(30)
               USING  NOME
               HIGHLIGHT.

      *-----------------------------------------------------------------
       PROCEDURE DIVISION.

       R0.
           OPEN I-O CADCID
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "30"
                 OPEN OUTPUT CADCID
                 CLOSE CADCID
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
           MOVE SPACES TO DESCRIPTION SYMPTOMS TREATMENT TIPO NOME
           MOVE ZEROS TO COD
           DISPLAY CIDTELA.

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
           READ CADCID
           IF ST-ERRO NOT = "23"
             IF ST-ERRO = "00"
                DISPLAY CIDTELA
                GO TO ACE-001

             ELSE
                MOVE "ERROR READING CADCID FILE" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM
           ELSE
                NEXT SENTENCE.

      *----------------------------------------------------------------*

       R3.
           MOVE SPACES TO DESCRIPTION

           ACCEPT TDEN
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R2.

           IF DESCRIPTION = SPACES
              MOVE "TYPE DESCRIPTION OF THE DISEASE" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R3.

       R4.
           MOVE SPACES TO SYMPTOMS

           ACCEPT TSIN
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R3.

           IF SYMPTOMS = SPACES
              MOVE "TYPE THE DISEASE SYMPTOMS" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R4.

       R5.
           MOVE SPACES TO TREATMENT

           ACCEPT TTRAT
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R4.

           IF TREATMENT = SPACES
              MOVE "TYPE DISEASE TREATMENT" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R5.

       R6.
           MOVE SPACES TO TIPO

           ACCEPT TTIPO
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R5.

           IF TIPO = SPACES
              MOVE "TYPE THE DISEASE TYPE" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R6.

       R7.
           MOVE SPACES TO NOME

           ACCEPT TNOME
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R6.

           IF NOME = SPACES
              MOVE "ENTER THE NAME OF THE DISEASE" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R7.

       INC-OPC.
                MOVE "Y" TO W-OPCAO
                DISPLAY (23, 40) "SAVE (Y/N) : ".
                ACCEPT (23, 57) W-OPCAO WITH UPDATE
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 01 GO TO R7.
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** DATA REFUSED BY THE OPERATOR ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                IF W-OPCAO NOT = "Y" AND "y"
                   MOVE "*** JUST TYPE Y=YES OR N=NO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-OPC.
       INC-WR1.
                WRITE REGCID
                IF ST-ERRO = "00" OR "02"
                      MOVE "*** DATA RECORDED *** " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO R1.
                IF ST-ERRO = "22"

                  GO TO ALT-RW1
                ELSE
                      MOVE "ERROR IN FILE RECORDING"
                                                       TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM.


      *************************************************
      * ROUTE OF CONSULTATION / AMENDMENT / EXCLUSION *
      *************************************************
      *
       ACE-001.
                DISPLAY (23, 12)
                     "N=NEW   C=CHANGE D=DELETE"
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
                   MOVE "*** NOT DELETED ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                IF W-OPCAO NOT = "Y" AND "y"
                   MOVE "*JUST TYPE Y=YES  OR  N=NO *" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO EXC-OPC.
       EXC-DL1.
                DELETE CADCID RECORD
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
                IF W-ACT = 01 GO TO R3.
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** NOT CHANGED INFORMATION *** " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                IF W-OPCAO NOT = "Y" AND "y"
                   MOVE "*** JUST TYPE Y=YES  OR  N=NO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ALT-OPC.
       ALT-RW1.
                REWRITE REGCID
                IF ST-ERRO = "00" OR "02"
                   MOVE "*** REGISTRATION CHANGED ***         " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                MOVE "ERROR IN CHANGING THE REGISTRATION"   TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.

       ROT-FIM.
           CLOSE CADCID.
           STOP RUN.

      *---------[ MESSAGE ROUTINE ]---------------------
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
