       IDENTIFICATION DIVISION.
       PROGRAM-ID. SMP0005.
      *AUTHOR. GUSTAVO ARAUJO NARCISO.
      **************************************
      *    MAINTENANCE OF REGISTER CEP     *
      **************************************
      *----------------------------------------------------------------
        ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
             DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT CADCEP ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS COD
                    FILE STATUS  IS ST-ERRO
                    ALTERNATE RECORD KEY IS ENDERECO WITH DUPLICATES.
      *
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD CADCEP
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADCEP.DAT".
       01 REGCEP.
          03 COD           PIC 9(08).
          03 ENDERECO      PIC X(30).
          03 BAIRRO        PIC X(20).
          03 CIDADE        PIC X(20).
          03 UF            PIC X(02).
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

       01 IND           PIC 9(02) VALUE ZEROS.


       01 TABUFX.
          03 FILLER     PIC X(22) VALUE "ACACRE".
          03 FILLER     PIC X(22) VALUE "ALALAGOAS".
          03 FILLER     PIC X(22) VALUE "APAMAPA".
          03 FILLER     PIC X(22) VALUE "AMAMAZONAS".
          03 FILLER     PIC X(22) VALUE "BABAHIA".
          03 FILLER     PIC X(22) VALUE "CECEARA".
          03 FILLER     PIC X(22) VALUE "DFDISTRITO FEDERAL".
          03 FILLER     PIC X(22) VALUE "ESESPIRITO SANTO".
          03 FILLER     PIC X(22) VALUE "GOGOIAS".
          03 FILLER     PIC X(22) VALUE "MAMARANHAO".
          03 FILLER     PIC X(22) VALUE "MTMATO GROSSO".
          03 FILLER     PIC X(22) VALUE "MSMATO GROSSO DO SUL".
          03 FILLER     PIC X(22) VALUE "MGMINAS GERAIS".
          03 FILLER     PIC X(22) VALUE "PAPARA".
          03 FILLER     PIC X(22) VALUE "PBPARAIBA".
          03 FILLER     PIC X(22) VALUE "PRPARANA".
          03 FILLER     PIC X(22) VALUE "PEPERNAMBUCO".
          03 FILLER     PIC X(22) VALUE "PIPIAUI".
          03 FILLER     PIC X(22) VALUE "RJRIO DE JANEIRO".
          03 FILLER     PIC X(22) VALUE "RNRIO GRANDE DO NORTE".
          03 FILLER     PIC X(22) VALUE "RSRIO GRANDE DO SUL".
          03 FILLER     PIC X(22) VALUE "RORONDONIA".
          03 FILLER     PIC X(22) VALUE "RRRORAIMA".
          03 FILLER     PIC X(22) VALUE "SCSANTA CATARINA".
          03 FILLER     PIC X(22) VALUE "SPSAO PAULO".
          03 FILLER     PIC X(22) VALUE "SESERGIPE".
          03 FILLER     PIC X(22) VALUE "TOTOCANTINS".
       01 TABUF REDEFINES TABUFX.
          03 TBUF   PIC X(22) OCCURS 27 TIMES.
       01 TXTUF.
          03 TXTUFCOD PIC X(02) VALUE SPACES.
          03 TXTUFTEXTO PIC X(20) VALUE SPACES.






      *-----------------------------------------------------------------
       SCREEN SECTION.
       01  TELACEP.
           05  BLANK SCREEN.
           05  LINE 02  COLUMN 01
               VALUE  "                              REGISTER ".
           05  LINE 02  COLUMN 41
               VALUE  "CEP".
           05  LINE 05  COLUMN 01
               VALUE  "   CODE:".
           05  LINE 08  COLUMN 01
               VALUE  "   ADDRESS:".
           05  LINE 11  COLUMN 01
               VALUE  "   BURGH:".
           05  LINE 14  COLUMN 01
               VALUE  "   CITY:".
           05  LINE 17  COLUMN 01
               VALUE  "   UF:".
           05  TCOD
               LINE 05  COLUMN 12  PIC 9(08)
               USING  COD
               HIGHLIGHT.
           05  TEND
               LINE 08  COLUMN 14  PIC X(30)
               USING  ENDERECO
               HIGHLIGHT.
           05  TBAIRRO
               LINE 11  COLUMN 12  PIC X(20)
               USING  BAIRRO
               HIGHLIGHT.
           05  TCIDADE
               LINE 14  COLUMN 12  PIC X(20)
               USING  CIDADE
               HIGHLIGHT.
           05  TUF
               LINE 17  COLUMN 08  PIC X(02)
               USING  UF
               HIGHLIGHT.
           05  TTUF
               LINE 17  COLUMN 12  PIC X(20)
               USING  TXTUF
               HIGHLIGHT.

       01  TELAUF.
           05  LINE 07  COLUMN 51
               VALUE  "         AC - ACRE".
           05  LINE 08  COLUMN 51
               VALUE  "         AL - ALAGOAS".
           05  LINE 09  COLUMN 51
               VALUE  "         AP - AMAPA".
           05  LINE 10  COLUMN 51
               VALUE  "         AM - AMAZONAS".
           05  LINE 11  COLUMN 51
               VALUE  "         BA - BAHIA".
           05  LINE 12  COLUMN 51
               VALUE  "         CE - CEARA".
           05  LINE 13  COLUMN 51
               VALUE  "         DF - DISTRITO FEDERAL".
           05  LINE 14  COLUMN 51
               VALUE  "         ES - ESPIRITO SANTO".
           05  LINE 15  COLUMN 51
               VALUE  "         GO - GOIAS".
           05  LINE 16  COLUMN 51
               VALUE  "         MA - MARANHAO".
           05  LINE 17  COLUMN 51
               VALUE  "         MT - MATO GROSSO".
           05  LINE 07  COLUMN 25
               VALUE  "         MS - MATO GROSSO DO SUL".
           05  LINE 08  COLUMN 25
               VALUE  "         MG - MINAS GERAIS".
           05  LINE 09  COLUMN 25
               VALUE  "         PA - PARA".
           05  LINE 10  COLUMN 25
               VALUE  "         PB - PARAIBA".
           05  LINE 11  COLUMN 25
               VALUE  "         PR - PARANA".
           05  LINE 12  COLUMN 25
               VALUE  "         PE - PERNAMBUCO".
           05  LINE 13  COLUMN 25
               VALUE  "         PI - PIAUI".
           05  LINE 14  COLUMN 25
               VALUE  "         RJ - RIO DE JANEIRO".
           05  LINE 15  COLUMN 25
               VALUE  "         RN - RIO GRANDE DO NORTE".
           05  LINE 16  COLUMN 25
               VALUE  "         RS - RIO GRANDE DO SUL".
           05  LINE 17  COLUMN 25
               VALUE  "         RO - RONDONIA".
           05  LINE 18  COLUMN 25
               VALUE  "         RR - RORAIMA".
           05  LINE 19  COLUMN 25
               VALUE  "         SC - SANTA CATARINA".
           05  LINE 20  COLUMN 25
               VALUE  "         SP - SAO PAULO".
           05  LINE 21  COLUMN 25
               VALUE  "         SE - SERGIPE".
           05  LINE 22  COLUMN 25
               VALUE  "         TO - TOCANTINS".
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.

      ********** OPEN OR CREATE FILE *************************
       R0.
           OPEN I-O CADCEP
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "30"
                 OPEN OUTPUT CADCEP
                 CLOSE CADCEP
                 MOVE "*** CREATE FILE CADCEP **" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R0
              ELSE
                 MOVE "ERROR IN OPEN CADCEP" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
                 NEXT SENTENCE.

      ********** END OPEN OR CREATE FILE *************************
      ********* RESET FIELDS AND SHOW SCREEN **********************
       R1.
           MOVE SPACES TO ENDERECO BAIRRO CIDADE UF TXTUF
           MOVE ZEROS TO COD
           DISPLAY TELACEP.

      ********* END RESET FIELDS AND SHOW SCREEN **********************
       R2.
           ACCEPT TCOD
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO ROT-FIM.


      ***** CHECKS IF A COD HAS BEEN ENTERED
           IF COD = 0
              MOVE "*** ENTER COD ***" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R2.

       READ-CADCEP.
           READ CADCEP
           IF ST-ERRO NOT = "23"
             IF ST-ERRO = "00"
                PERFORM R6A
                DISPLAY TELACEP
                GO TO ACE-001

             ELSE
                MOVE "ERROR READING THE FILE" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM
           ELSE
                NEXT SENTENCE.

       R3.
           DISPLAY TELACEP
           ACCEPT TEND
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R2.
           IF ENDERECO = SPACES
              MOVE "ENTER ADDRESS" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R3.

       R4.
           DISPLAY TELACEP
           ACCEPT TBAIRRO
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R3.
           IF ENDERECO = SPACES
              MOVE "ENTER NEIGHBORHOOD" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R4.

       R5.
           DISPLAY TELACEP
           ACCEPT TCIDADE
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R4.
           IF CIDADE = SPACES
              MOVE "*** ENTER CITY ***" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R5.

       R6.
           DISPLAY TELAUF
           MOVE 1 TO IND

           ACCEPT TUF
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R5.
           IF UF = SPACES
              MOVE "*** COD UF ***" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R6.
       R6A.
           MOVE TBUF(IND) TO TXTUF
           IF TXTUFCOD NOT = UF
              ADD 1 TO IND
              IF IND < 28
                 GO TO R6A
              ELSE
                 MOVE "*** INCORRECT COD UF ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R6
           ELSE
                MOVE TXTUFTEXTO TO TXTUF
                DISPLAY TTUF.
                DISPLAY TELACEP.




       INC-OPC.
                MOVE "S" TO W-OPCAO
                DISPLAY (23, 40) "SAVE (Y/N) : ".
                ACCEPT (23, 57) W-OPCAO WITH UPDATE
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 01 GO TO R6.
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** DATA DECLINED BY THE OPERATOR ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                IF W-OPCAO NOT = "Y" AND "y"
                   MOVE "*** JUST TYPE Y=YES OR N=NO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-OPC.
       INC-WR1.
                WRITE REGCEP
                IF ST-ERRO = "00" OR "02"
                      MOVE "*** RECORDED *** " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO R1.
                IF ST-ERRO = "22"

                  GO TO ALT-RW1
                ELSE
                      MOVE "FILE WRITE ERROR"
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
                   MOVE "*** REGISTER NOT DELETE ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                IF W-OPCAO NOT = "Y" AND "y"
                   MOVE "* JUST TYPE Y=YES OR  N=NO *" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO EXC-OPC.
       EXC-DL1.
                DELETE CADCEP RECORD
                IF ST-ERRO = "00"
                   MOVE "*** DELETED ***" TO MENS
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
                IF W-ACT = 01 GO TO R6.
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** NO CHANGE *** " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** JUST TYPE Y=YES  OR  N=NO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ALT-OPC.
       ALT-RW1.
                REWRITE REGCEP
                IF ST-ERRO = "00" OR "02"
                   MOVE "*** CHANGE REGISTER ***         " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                MOVE "ERROR IN CHANGE REGISTER"   TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.






       ROT-FIM.
           CLOSE CADCEP.
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
