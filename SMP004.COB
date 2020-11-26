       IDENTIFICATION DIVISION.
       PROGRAM-ID. SMP004.
      *AUTHOR. GUSTAVO ARAUJO NARCISO.
      ********************************************
      *    MAINTENANCE OF PATIENT REGISTRATION   *
      ********************************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
             DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CADPACI ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS CPF
                    FILE STATUS  IS ST-ERRO
                    ALTERNATE RECORD KEY IS NOUN WITH DUPLICATES.

           SELECT CADCEP ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS CEP
                    FILE STATUS  IS ST-ERRO
                    ALTERNATE RECORD KEY IS ENDERECO
                                   WITH DUPLICATES.

           SELECT CADCONV ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS COD
                    FILE STATUS  IS ST-ERRO
                    ALTERNATE RECORD KEY IS CNOME WITH DUPLICATES
                    ALTERNATE RECORD KEY IS PLANO WITH DUPLICATES.
      *
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD CADPACI
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADPACI.DAT".
       01 REGPACI.
          03 CPF            PIC 9(04).
          03 NOUN           PIC X(30).
          03 NASCIMENTO.
             05 DIA         PIC 9(02).
             05 MES         PIC 9(02).
             05 ANO         PIC 9(04).
          03 SEX           PIC X(01).
          03 GENDER         PIC X(01).
          03 PHONE.
             05 DDD         PIC 9(02).
             05 NUM         PIC 9(09).
          03 EMAIL          PIC X(40).
          03 CEPPACI        PIC 9(08).
          03 NUMLOGRA       PIC 9(04).
          03 COMLOGRA       PIC X(12).
          03 CONVENIO       PIC 9(04).


       FD CADCEP
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADCEP.DAT".
       01 REGCEP.
          03 CEP           PIC 9(08).
          03 ENDERECO      PIC X(30).
          03 BAIRRO        PIC X(20).
          03 CIDADE        PIC X(20).
          03 UF            PIC X(02).

       FD CADCONV
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADCONV.DAT".
       01 REGCONV.
          03 COD          PIC 9(04).
          03 CNOME        PIC X(30).
          03 PLANO        PIC 9(02).


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
       01 BI            PIC 9(01) VALUE ZEROS.

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

       01 TABGENEROX.
          03 FILLER     PIC X(15) VALUE "NNOT DECLARED".
          03 FILLER     PIC X(15) VALUE "HHETEROSSEXUAL".
          03 FILLER     PIC X(15) VALUE "BBISSEXUAL".
          03 FILLER     PIC X(15) VALUE "TRANSSEXUAL".
          03 FILLER     PIC X(15) VALUE "OOTHER".
       01 TABGENERO REDEFINES TABGENEROX.
          03 TBGENERO   PIC X(15) OCCURS 5 TIMES.
       01 TXTGENERO.
          03 TXTCOD PIC X(01) VALUE SPACES.
          03 TXTTEXT PIC X(14) VALUE SPACES.

       01 LIXO PIC 9(03) VALUE ZEROS.

      *-----------------------------------------------------------------
       SCREEN SECTION.

       01  TELAPACIENTE.
           05  BLANK SCREEN.
           05  LINE 01  COLUMN 01
               VALUE  "-------------------------[ PATIENT REGIS".
           05  LINE 01  COLUMN 41
               VALUE  "TRATION ]------------------------------".
           05  LINE 03  COLUMN 01
               VALUE  " CPF:       NAME:".
           05  LINE 03  COLUMN 41
               VALUE  "          BIRTHDAY:".
           05  LINE 05  COLUMN 01
               VALUE  " SEXO:               GENERO:".
           05  LINE 05  COLUMN 41
               VALUE  "        PHONE   : (  ) ".
           05  LINE 07  COLUMN 01
               VALUE  " EMAIL:".
           05  LINE 09  COLUMN 01
               VALUE  "------------------------------[ ADDRESS".
           05  LINE 09  COLUMN 41
               VALUE  " ]--------------------------------------".
           05  LINE 11  COLUMN 01
               VALUE  " CEP:          NEIGHBORHOOD: ".
           05  LINE 11  COLUMN 70
               VALUE  " NUM:".
           05  LINE 13  COLUMN 01
               VALUE  " BURGH:                      CITY:".
           05  LINE 15  COLUMN 01
               VALUE  " FEDERATIVE UNIT:".
           05  LINE 15  COLUMN 50
               VALUE  " COMPLEMENT:".
           05  LINE 17  COLUMN 01
               VALUE  "------------------------------[ AGREEMEN".
           05  LINE 17  COLUMN 41
               VALUE  "T]--------------------------------------".
           05  LINE 19  COLUMN 01
               VALUE  " COD:         NAME:".
           05  LINE 21  COLUMN 01
               VALUE  " PLAN:".
           05  LINE 22  COLUMN 01
               VALUE  "----------------------------------------".
           05  LINE 22  COLUMN 41
               VALUE  "----------------------------------------".
           05  LINE 23  COLUMN 01
               VALUE  " MESSAGE:".
           05  TCPF
               LINE 03  COLUMN 07  PIC 9(04)
               USING  CPF
               HIGHLIGHT.
           05  TNOME
               LINE 03  COLUMN 19  PIC X(30)
               USING  NOUN
               HIGHLIGHT.
           05  TNASCIMENTO
               LINE 03  COLUMN 71  PIC 99/99/9999
               USING  NASCIMENTO
               HIGHLIGHT.
           05  TSEXO
               LINE 05  COLUMN 08  PIC X(01)
               USING  SEX
               HIGHLIGHT.
           05  TSEXOTXT
               LINE 05  COLUMN 10  PIC X(10)
               USING  TEXSEXO
               HIGHLIGHT.
           05  TGENERO
               LINE 05  COLUMN 30  PIC X(01)
               USING  GENDER
               HIGHLIGHT.
           05  TTXTGENERO
               LINE 05  COLUMN 32  PIC X(14)
               USING  TXTGENERO
               HIGHLIGHT.
           05  TDDD
               LINE 05  COLUMN 60  PIC 99
               USING  DDD
               HIGHLIGHT.
           05  TNUM
               LINE 05  COLUMN 64  PIC 9(09)
               USING  NUM
               HIGHLIGHT.
           05  TEMAIL
               LINE 07  COLUMN 09  PIC X(40)
               USING  EMAIL
               HIGHLIGHT.
           05  TCEP
               LINE 11  COLUMN 07  PIC 9(08)
               USING  CEP
               HIGHLIGHT.
           05  TLOGRA
               LINE 11  COLUMN 30  PIC X(40)
               USING  ENDERECO
               HIGHLIGHT.
           05  TNUMLOGRA
               LINE 11  COLUMN 76  PIC 9(04)
               USING  NUMLOGRA
               HIGHLIGHT.
           05  TBAIRRO
               LINE 13  COLUMN 10  PIC X(20)
               USING  BAIRRO
               HIGHLIGHT.
           05  TCIDADE
               LINE 13  COLUMN 39  PIC X(30)
               USING  CIDADE
               HIGHLIGHT.
           05  TUF
               LINE 15  COLUMN 24  PIC X(02)
               USING  UF
               HIGHLIGHT.
           05  TCOMPLEMENTO
               LINE 15  COLUMN 64  PIC X(12)
               USING  COMLOGRA
               HIGHLIGHT.
           05  TTUF
               LINE 15  COLUMN 27  PIC X(20)
               USING  TXTUF
               HIGHLIGHT.
           05  TCODIGO
               LINE 19  COLUMN 10  PIC 9(04)
               USING  COD
               HIGHLIGHT.
           05  TCNOME
               LINE 19  COLUMN 22  PIC X(30)
               USING  CNOME
               HIGHLIGHT.
           05  TPLANO
               LINE 21  COLUMN 09  PIC 9(02)
               USING  PLANO
               HIGHLIGHT.
           05  TXTPLANO
               LINE 21  COLUMN 12  PIC X(30)
               USING  PLANOTXT
               HIGHLIGHT.



      *-----------------------------------------------------------------
       PROCEDURE DIVISION.

       R0.
           OPEN I-O CADPACI
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "30"
                 OPEN OUTPUT CADPACI
                 CLOSE CADPACI
                 MOVE "*** CADPACI ARCHIVE WAS CREATED **" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R0
              ELSE
                 MOVE "ERROR IN OPENING THE CADMIGO FILE" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
                 NEXT SENTENCE.
      *
       R0A.
           OPEN INPUT CADCEP
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "30"
                 MOVE "*** CEP FILE NOT FOUND **" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
              ELSE
                 MOVE "ERROR IN OPENING CEP ZIP FILE " TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
                 NEXT SENTENCE.

       R0B.
           OPEN INPUT CADCONV
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "30"
                 MOVE "*** CADCONV FILE NOT FOUND **" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
              ELSE
                 MOVE "ERROR IN OPENING CEP ZIP FILE " TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
                 NEXT SENTENCE.


       R1.
           MOVE SPACES TO NOUN SEX GENDER EMAIL TXTGENERO TEXSEXO
           MOVE ZEROS TO  NASCIMENTO CPF NUM DDD MES ANO DIA
           MOVE SPACES TO ENDERECO BAIRRO CIDADE UF TXTUF COMLOGRA
           MOVE ZEROS TO CEP CPF NUMLOGRA IND
           MOVE SPACES TO CNOME PLANOTXT TXTUFCOD TXTUFTEXTO COMLOGRA
           MOVE ZEROS TO COD PLANO CEPPACI
           DISPLAY TELAPACIENTE.


       R2.

           MOVE ZEROS TO CPF

           ACCEPT TCPF
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO ROT-FIM.

           IF CPF = ZEROS
                MOVE "INVALID CPF" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS2
                GO TO R2.


      *---------------CHECK IF IT ALREADY EXISTS IN THE FILE-----------------*

       LER-CADPACI.
           READ CADPACI
           IF ST-ERRO NOT = "23"
             IF ST-ERRO = "00"
                MOVE CONVENIO TO COD
                MOVE CEPPACI TO CEP
                PERFORM R5A
                PERFORM R6A
                PERFORM R9A
                PERFORM R12A
                PERFORM R9B
                PERFORM R9C
                PERFORM R13
                DISPLAY TELAPACIENTE
                GO TO ACE-001
             ELSE
                MOVE "ERROR READING CADPACI FILE" TO MENS
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
              MOVE "ENTER A VALID NAME" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R3.

       R4.
           MOVE ZEROS TO NASCIMENTO

           ACCEPT TNASCIMENTO
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R3.

           IF NASCIMENTO = SPACES
              MOVE "TYPE A VALID BIRTH" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R4.

           IF ANO < 1 OR ANO > 2020
              MOVE "TYPE A VALID BIRTH" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R4.

           IF MES < 1 OR MES > 12
              MOVE "TYPE A VALID BIRTH" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R4.


           IF MES = 04 OR MES = 06 OR MES = 09 OR MES = 11
                MOVE "ALL RIGHT" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM

                IF DIA > 30 OR DIA < 1
                   MOVE "TYPE A VALID BIRTH" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R4.


           IF MES = 02

                 DIVIDE ANO BY 4 GIVING LIXO REMAINDER BI

                 IF BI = 0

                    PERFORM R4A

                 ELSE

                    PERFORM R4B


           IF DIA > 31 OR DIA < 1
                MOVE "TYPE A VALID BIRTH" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO R4.

       R5.
           ACCEPT TSEXO.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R4.
       R5A.

           IF SEX = "F"
              MOVE "FEMALE" TO TEXSEXO
           ELSE
              IF SEX = "M"
                 MOVE "MALE" TO TEXSEXO
              ELSE
                 IF SEX = "I"
                    MOVE "UNDEFINED" TO TEXSEXO
                 ELSE
                    MOVE "*TYPE M - MALE OR F - FEMALE*" TO MENS
                    PERFORM ROT-MENS THRU ROT-MENS-FIM
                    GO TO R5.

           DISPLAY TELAPACIENTE.
        R6.
           MOVE 1 TO IND
           ACCEPT TGENERO.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R5.
       R6A.
           MOVE TBGENERO(IND) TO TXTGENERO
           IF TXTCOD NOT = GENDER
              ADD 1 TO IND
              IF IND < 10
                 GO TO R6A
              ELSE
                 MOVE "*** INCORRECT TYPE GENERO ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R6
           ELSE
               MOVE TXTTEXT TO TXTGENERO
               DISPLAY TTXTGENERO.


       R7A.
           MOVE ZEROS TO DDD

           ACCEPT TDDD
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R6.

           MOVE ZEROS TO NUM

           ACCEPT TNUM
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R6.

           IF NUM = ZEROS OR DDD = ZEROS
              MOVE "TYPE A VALID PHONE" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R7A.

       R8.
           MOVE SPACES TO EMAIL

           ACCEPT TEMAIL
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R7A.

           IF EMAIL = SPACES
              MOVE "ENTER A VALID EMAIL ADDRESS" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R8.

       R9.
           MOVE ZEROS TO CEP

           ACCEPT TCEP
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                 DISPLAY TELAPACIENTE
                 GO TO R8.

           IF CEP = 0
                 MOVE "*** CEP NOT INFORMED  ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R9.

       R9A.

           READ CADCEP
           IF ST-ERRO = "23"
                   MOVE "*** CEP DOES NOT EXIST ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R9

           IF ST-ERRO NOT = "00"
                  MOVE "ERROR READING ZIP CERTIFICATE FILE" TO MENS
                  PERFORM ROT-MENS THRU ROT-MENS-FIM
                  GO TO ROT-FIM.


       R9B.

           MOVE 1 TO IND.

       R9C.

           MOVE TBUF(IND) TO TXTUF
           IF TXTUFCOD NOT = UF
              ADD 1 TO IND
              IF IND < 28
                 GO TO R9C
              ELSE
                 MOVE "*** INCORRECT UF ZIP CODE***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R9
           ELSE
                MOVE TXTUFTEXTO TO TXTUF
                DISPLAY TTUF.
                DISPLAY TELAPACIENTE.

           MOVE CEP TO CEPPACI.


       R10.

           MOVE ZEROS TO NUMLOGRA

           ACCEPT TNUMLOGRA
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R9.

           IF NUMLOGRA = ZEROS
              MOVE "ENTER A VALID NUMBER" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R10.

       R11.

           MOVE SPACES TO COMLOGRA

           ACCEPT TCOMPLEMENTO
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R10.

           IF COMLOGRA = SPACES
              MOVE "TYPE A VALID COMPLEMENT" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R11.

       R12.

           MOVE ZEROS TO COD
           MOVE SPACES TO PLANOTXT

           ACCEPT TCODIGO
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                 DISPLAY TELAPACIENTE
                 GO TO R11.

           IF COD = ZEROS
                 MOVE "*** NON-INFORMED CODE  ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R12.

       R12A.
           READ CADCONV
           IF ST-ERRO = "23"
                   MOVE "*** CODE DOES NOT EXIST ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R12.
           IF ST-ERRO NOT = "00"
                  MOVE "ERROR READING FILE REGISTRATION AGREEMENTS"
                      TO MENS
                  PERFORM ROT-MENS THRU ROT-MENS-FIM
                  GO TO ROT-FIM.

           MOVE COD TO CONVENIO.

       R13.

           MOVE TBCONV(PLANO) TO PLANOTXT
           DISPLAY TELAPACIENTE.


       INC-OPC.
                MOVE "Y" TO W-OPCAO
                DISPLAY (23, 40) "SAVE (Y/N) : ".
                ACCEPT (23, 57) W-OPCAO WITH UPDATE
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 01 GO TO R12.
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** DATA DECLINED BY THE OPERATOR ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                IF W-OPCAO NOT = "Y" AND "y"
                   MOVE "*** JUST TYPE Y=YES OR N=NO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-OPC.
       INC-WR1.
                WRITE REGPACI
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

      ******************************************************************

       R4A.

               IF DIA > 29 OR DIA < 1
                  MOVE "TYPE A VALID BIRTH" TO MENS
                  PERFORM ROT-MENS THRU ROT-MENS-FIM
                  GO TO R4.


       R4A-FIM.

       R4B.

               IF DIA > 28 OR DIA < 1
                  MOVE "TYPE A VALID BIRTH" TO MENS
                  PERFORM ROT-MENS THRU ROT-MENS-FIM
                  GO TO R4.


       R4B-FIM.

      *****************************************
      * ROTINA DE CONSULTA/ALTERACAO/EXCLUSAO *
      *****************************************
      *
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
                IF W-OPCAO NOT = "Y" AND "Y"
                   MOVE "* JUST TYPE Y=YES OR N=NO *" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO EXC-OPC.
       EXC-DL1.
                DELETE CADPACI RECORD
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
                IF W-OPCAO NOT = "Y" AND "y"
                   MOVE "*** JUST TYPE Y=YES OR N=NO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ALT-OPC.
       ALT-RW1.
                REWRITE REGPACI
                IF ST-ERRO = "00" OR "02"
                   MOVE "*** REGISTRATION CHANGED ***         " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                MOVE "ERROR IN CHANGING THE REGISTRATION"   TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.

       ROT-FIM.
           CLOSE CADPACI
           CLOSE CADCEP
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
