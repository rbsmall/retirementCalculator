       PROCESS NODLL,NODYNAM,TEST(NOSEP),NOCICS,NOSQL,PGMN(LU),NOSEQ
      *+---------------------------------------------------------------+
      *| TEBUD01                                                       |
      *| UNIT TEST FOR Z/OS: TEST CASE PROGRAM                         |
      *| TEST CASE VERSION: 200                                        |
      *| DATE GENERATED: 03/11/2025 11:33                              |
      *| ID: 36c1277e-614c-4945-8079-014e55608947                      |
      *+---------------------------------------------------------------+
      *+---------------------------------------------------------------+
      *| UNIT TEST FOR Z/OS: TEST_TEST1                                |
      *|     FOR TEST TEST1                                            |
      *| TEST CASE VERSION: 200                                        |
      *+---------------------------------------------------------------+
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 'TEST_TEST1'.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE
           ASSIGN TO 'INPUT'.
       DATA DIVISION.
       FILE SECTION.
       FD INPUT-FILE
           RECORD CONTAINS 80 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE F.
      *  *** INPUT-RECORD : ZUT00000000
       1 ZUT00000000.
      *    *** DATE-AREA : ZUT00000001
         5 ZUT00000001.
      *    *** IN-CCYY : ZUT00000002
         7 ZUT00000002 PIC 9(4).
      *    *** IN-MM : ZUT00000003
         7 ZUT00000003 PIC 9(2).
      *    *** IN-DD : ZUT00000004
         7 ZUT00000004 PIC 9(2).
      *    *** FILLER : ZUT00000005
         5 ZUT00000005 PIC X(72).
       WORKING-STORAGE SECTION.
       01 PROGRAM-NAME   PIC X(8)  VALUE 'EBUD01'.
       01 AZ-CSECT       PIC X(72) VALUE SPACES.
       01 BZ-ASSERT.
         03 MESSAGE-LEN PIC S9(4) COMP-4 VALUE 24.
         03 MESSAGE-TXT PIC X(254) VALUE 'HELLO FROM TEST CALLBACK'.
       01  BZ-P1 PIC S9(9) COMP-4 VALUE 4.
       01  BZ-P2 PIC S9(9) COMP-4 VALUE 2001.
       01  BZ-P3 PIC X(3) VALUE 'AZU'.
       01 BZ-TRACE.
         03 TRACE-LEN       PIC S9(4) COMP-4 VALUE 5.
         03 TRACE-TXT       PIC X(254) VALUE 'TRACE'.
       01 BZUASSRT          PIC X(8) VALUE 'BZUASSRT'.
       01 BZUTRACE          PIC X(8) VALUE 'BZUTRACE'.
       01 BZUGETEP          PIC X(8) VALUE 'BZUGETEP'.
       01 AZ-EP-PTR         USAGE IS POINTER.
       01 AZ-TRACE-PTR      POINTER.
       01 ASSERT-ST.
         03 ASSERT-RC PIC 9(9) BINARY VALUE 4.
         03 ASSERT-TEXT PIC 9(4) BINARY VALUE 0.
       01 AZ-TEST-NAME-LEN       PIC S9(9) COMP-5.
       01 AZ-RC-WORK             PIC S9(4) USAGE BINARY.
       01 AZ-FS-CODE        PIC X(2).
       LOCAL-STORAGE SECTION.
       LINKAGE SECTION.
       01 AZ-TEST                   PIC X(80).
       01 AZ-ARG-LIST.
         03 ARG-LENGTH PIC 9(4) COMP-4.
         03 ARG-DATA PIC X(256).
       01 AZ-INFO-BLOCK.
         COPY EQAITERC.
       01 AZ-PROC-PTR       USAGE IS PROCEDURE-POINTER.
       PROCEDURE DIVISION USING AZ-TEST AZ-ARG-LIST AZ-INFO-BLOCK.
      * START
           DISPLAY 'AZU0000I TEST_TEST1 STARTED...'
           MOVE 0 TO AZ-TEST-NAME-LEN.
           INSPECT AZ-TEST TALLYING AZ-TEST-NAME-LEN FOR
           CHARACTERS BEFORE INITIAL SPACE.
      * INITIALIZE PARAMETER
      * SET AREA ADDRESS TO POINTER
      * SET INPUT VALUE
           MOVE 0 TO RETURN-CODE.
      * OPEN INPUT-FILE FOR INPUT
           OPEN INPUT INPUT-FILE
      * CALL TEST PROGRAM
           DISPLAY 'AZU0000I CALL EBUD01'
           CALL BZUGETEP USING BY REFERENCE PROGRAM-NAME AZ-CSECT
             RETURNING AZ-EP-PTR.
           IF AZ-EP-PTR = NULL THEN
             MOVE 1 TO MESSAGE-LEN OF BZ-ASSERT
             STRING 'UNABLE TO GET THE ENTRY POINT BY BZUGETEP.'
               DELIMITED BY SIZE
               INTO MESSAGE-TXT OF BZ-ASSERT
               WITH POINTER MESSAGE-LEN OF BZ-ASSERT
             END-STRING
             SUBTRACT 1 FROM MESSAGE-LEN OF BZ-ASSERT
             PERFORM THROW-ASSERTION-M
             GOBACK
           END-IF
           SET ADDRESS OF AZ-PROC-PTR TO AZ-EP-PTR.
           CALL AZ-PROC-PTR
           .
      * CLOSE INPUT-FILE
           CLOSE INPUT-FILE
      * EVALUATE OUTPUT VALUE
           MOVE 4 TO RETURN-CODE
      * END
           DISPLAY 'AZU0000I TEST_TEST1 END.'
           GOBACK.
       THROW-ASSERTION-M.
           DISPLAY 'AZU0000I *******************************************
      -    '*************************************'
           DISPLAY 'AZU2001W THE TEST "' AZ-TEST(1:AZ-TEST-NAME-LEN) '"
      -    'FAILED DUE TO AN ASSERTION.'
           DISPLAY 'AZU1101I ' MESSAGE-TXT OF BZ-ASSERT(1:MESSAGE-LEN
           OF BZ-ASSERT)
           DISPLAY 'AZU0000I *******************************************
      -    '*************************************'
           CALL BZUASSRT USING BZ-P1 BZ-P2 BZ-P3 BZ-ASSERT
           EXIT.
       END PROGRAM TEST_TEST1.
      *+---------------------------------------------------------------+
      *| UNIT TEST FOR Z/OS: BZU_TEST                                  |
      *|     CALLBACK DEFINITION FOR TEST                              |
      *| TEST CASE VERSION: 200                                        |
      *+---------------------------------------------------------------+
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 'BZU_TEST'.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 PROGRAM-NAME   PIC X(8)  VALUE 'EBUD01'.
       01 AZ-CSECT       PIC X(72) VALUE SPACES.
       01 BZ-ASSERT.
         03 MESSAGE-LEN PIC S9(4) COMP-4 VALUE 24.
         03 MESSAGE-TXT PIC X(254) VALUE 'HELLO FROM TEST CALLBACK'.
       01  BZ-P1 PIC S9(9) COMP-4 VALUE 4.
       01  BZ-P2 PIC S9(9) COMP-4 VALUE 2001.
       01  BZ-P3 PIC X(3) VALUE 'AZU'.
       01 BZ-TRACE.
         03 TRACE-LEN       PIC S9(4) COMP-4 VALUE 5.
         03 TRACE-TXT       PIC X(254) VALUE 'TRACE'.
       01 BZUASSRT          PIC X(8) VALUE 'BZUASSRT'.
       01 BZUTRACE          PIC X(8) VALUE 'BZUTRACE'.
       01 AZ-TRACE-PTR      POINTER.
       01 ASSERT-ST.
         03 ASSERT-RC PIC 9(9) BINARY VALUE 4.
         03 ASSERT-TEXT PIC 9(4) BINARY VALUE 0.
       01 AZ-TEST-NAME-LEN       PIC S9(9) COMP-5.
       01 AZ-GRP-INDEX        PIC 9(8).
       01 AZ-FLAG-IN          PIC 9(1).
       01 AZ-RECORD-PTR       POINTER.
       01 AZ-RC-WORK          PIC S9(4) USAGE BINARY.
       01 AZ-OUTPUT-COUNT-STR PIC X(5).
       LOCAL-STORAGE SECTION.
       LINKAGE SECTION.
       01 AZ-TEST                   PIC X(80).
       01 AZ-INFO-BLOCK.
          COPY EQAITERC.
       01 AZ-ARG-LIST.
         03 ARG-LENGTH PIC 9(4) COMP-4.
         03 ARG-DATA PIC X(256).
       01 AZ-RECORD-COUNT     PIC 9(5) COMP-5.
       PROCEDURE DIVISION.
      * SET INPUT VALUE
           ENTRY "PGM_INPT_EBUD01" USING AZ-TEST AZ-INFO-BLOCK
           .
           DISPLAY 'AZU0000I PGM_INPT_EBUD01 INPUT VALUES...'.
           MOVE 0 TO RETURN-CODE.
           MOVE 0 TO AZ-TEST-NAME-LEN.
           INSPECT AZ-TEST TALLYING AZ-TEST-NAME-LEN FOR CHARACTERS
             BEFORE INITIAL SPACE.
           EVALUATE AZ-TEST(1:AZ-TEST-NAME-LEN)
           WHEN SPACE
             CONTINUE
           WHEN OTHER
             CONTINUE
           END-EVALUATE.
           PERFORM TEARDOWN.
           GOBACK.
      * EVALUATE OUTPUT VALUE
           ENTRY "PGM_OUTP_EBUD01" USING AZ-TEST AZ-INFO-BLOCK
           .
           DISPLAY 'AZU0000I PGM_OUTP_EBUD01 CHECK VALUES...'.
           MOVE 4 TO RETURN-CODE.
           MOVE 0 TO AZ-TEST-NAME-LEN.
           INSPECT AZ-TEST TALLYING AZ-TEST-NAME-LEN FOR CHARACTERS
             BEFORE INITIAL SPACE.
           EVALUATE AZ-TEST(1:AZ-TEST-NAME-LEN)
           WHEN SPACE
             CONTINUE
           WHEN 'TEST1'
             PERFORM CHECK-REC-TEST1
             MOVE 4 TO RETURN-CODE
           WHEN OTHER
             CONTINUE
           END-EVALUATE.
           PERFORM TEARDOWN.
           GOBACK.
       TEARDOWN.
           DISPLAY 'AZU0000I BZU_TEST END.'
           EXIT.
       CHECK-REC-TEST1.
      * CHECK RECORD COUNT FOR TEST1
      * FOR EBUD02
           MOVE 2 TO AZ-GRP-INDEX
           MOVE 0 TO AZ-FLAG-IN
           MOVE RETURN-CODE TO AZ-RC-WORK
           CALL 'GTMEMRC' USING TC-WORK-AREA OF AZ-INFO-BLOCK
             AZ-GRP-INDEX AZ-FLAG-IN AZ-RECORD-PTR
           SET ADDRESS OF AZ-RECORD-COUNT TO AZ-RECORD-PTR
           MOVE AZ-RC-WORK TO RETURN-CODE
           IF AZ-RECORD-COUNT NOT EQUAL 1 THEN
             MOVE 1 TO MESSAGE-LEN OF BZ-ASSERT
             MOVE AZ-RECORD-COUNT TO AZ-OUTPUT-COUNT-STR
             STRING
               'EXPECTED RECORD COUNT IS ''1''. '
               'BUT REAL RECORD COUNT IS ''' AZ-OUTPUT-COUNT-STR ''''
               ' IN EBUD02.'
               DELIMITED BY SIZE INTO MESSAGE-TXT OF BZ-ASSERT
               WITH POINTER MESSAGE-LEN OF BZ-ASSERT
             END-STRING
             SUBTRACT 1 FROM MESSAGE-LEN OF BZ-ASSERT
             PERFORM THROW-ASSERTION-M
           END-IF.
      * FOR EBUD03
           MOVE 3 TO AZ-GRP-INDEX
           MOVE 0 TO AZ-FLAG-IN
           MOVE RETURN-CODE TO AZ-RC-WORK
           CALL 'GTMEMRC' USING TC-WORK-AREA OF AZ-INFO-BLOCK
             AZ-GRP-INDEX AZ-FLAG-IN AZ-RECORD-PTR
           SET ADDRESS OF AZ-RECORD-COUNT TO AZ-RECORD-PTR
           MOVE AZ-RC-WORK TO RETURN-CODE
           IF AZ-RECORD-COUNT NOT EQUAL 1 THEN
             MOVE 1 TO MESSAGE-LEN OF BZ-ASSERT
             MOVE AZ-RECORD-COUNT TO AZ-OUTPUT-COUNT-STR
             STRING
               'EXPECTED RECORD COUNT IS ''1''. '
               'BUT REAL RECORD COUNT IS ''' AZ-OUTPUT-COUNT-STR ''''
               ' IN EBUD03.'
               DELIMITED BY SIZE INTO MESSAGE-TXT OF BZ-ASSERT
               WITH POINTER MESSAGE-LEN OF BZ-ASSERT
             END-STRING
             SUBTRACT 1 FROM MESSAGE-LEN OF BZ-ASSERT
             PERFORM THROW-ASSERTION-M
           END-IF.
           EXIT.
       THROW-ASSERTION-M.
           DISPLAY 'AZU0000I *******************************************
      -    '*************************************'
           DISPLAY 'AZU2001W THE TEST "' AZ-TEST(1:AZ-TEST-NAME-LEN) '"
      -    'FAILED DUE TO AN ASSERTION.'
           DISPLAY 'AZU1101I ' MESSAGE-TXT OF BZ-ASSERT(1:MESSAGE-LEN
           OF BZ-ASSERT)
           DISPLAY 'AZU0000I *******************************************
      -    '*************************************'
           CALL BZUASSRT USING BZ-P1 BZ-P2 BZ-P3 BZ-ASSERT
           EXIT.
       END PROGRAM BZU_TEST.
      *+---------------------------------------------------------------+
      *| UNIT TEST FOR Z/OS: BZU_INIT                                  |
      *|     INITIAL PROCEDURE                                         |
      *| TEST CASE VERSION: 200                                        |
      *+---------------------------------------------------------------+
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 'BZU_INIT'.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 AZ-TEST-NAME-LEN      PIC S9(9) COMP-5.
       01 AZ-TESTCASE-ID        PIC X(36)
           VALUE '36c1277e-614c-4945-8079-014e55608947'.
       LINKAGE SECTION.
       01 AZ-TEST               PIC X(80).
       01 AZ-TEST-ID            PIC X(80).
       01 AZ-INFO-BLOCK.
           COPY EQAITERC.
       PROCEDURE DIVISION USING AZ-TEST
                                AZ-TEST-ID
                                AZ-INFO-BLOCK.
           MOVE 0 TO AZ-TEST-NAME-LEN.
           INSPECT AZ-TEST TALLYING AZ-TEST-NAME-LEN FOR
           CHARACTERS BEFORE INITIAL SPACE.
           DISPLAY 'AZU0000I BZU_INIT: ' AZ-TEST(1:AZ-TEST-NAME-LEN)
           DISPLAY 'AZU0000I TEST CASE VERSION: 200'
           DISPLAY 'AZU0001I FOR TEST RUNNER: latest'
           MOVE AZ-TESTCASE-ID TO AZ-TEST-ID
           GOBACK.
       END PROGRAM BZU_INIT.
      *+---------------------------------------------------------------+
      *| UNIT TEST FOR Z/OS: BZU_TERM                                  |
      *|     TERMINATION PROCEDURE                                     |
      *| TEST CASE VERSION: 200                                        |
      *+---------------------------------------------------------------+
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 'BZU_TERM'.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 AZ-TEST-NAME-LEN      PIC S9(9) COMP-5.
       LINKAGE SECTION.
       01 AZ-TEST               PIC X(80).
       01 AZ-INFO-BLOCK.
           COPY EQAITERC.
       PROCEDURE DIVISION USING AZ-TEST
                                AZ-INFO-BLOCK.
           MOVE 0 TO AZ-TEST-NAME-LEN.
           INSPECT AZ-TEST TALLYING AZ-TEST-NAME-LEN FOR
           CHARACTERS BEFORE INITIAL SPACE.
           DISPLAY 'AZU0000I BZU_TERM: ' AZ-TEST(1:AZ-TEST-NAME-LEN)
           GOBACK.
       END PROGRAM BZU_TERM.
      *+---------------------------------------------------------------+
      *| UNIT TEST FOR Z/OS: EBUD02                                    |
      *|     SUB PROGRAM CALLED FROM PROGRAM UNDER TEST                |
      *| TEST CASE VERSION: 200                                        |
      *+---------------------------------------------------------------+
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 'PGM_EBUD02'.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 BZ-ASSERT.
         03 MESSAGE-LEN PIC S9(4) COMP-4 VALUE 24.
         03 MESSAGE-TXT PIC X(254) VALUE 'HELLO FROM STUB CALLBACK'.
       01  BZ-P1 PIC S9(9) COMP-4 VALUE 4.
       01  BZ-P2 PIC S9(9) COMP-4 VALUE 2001.
       01  BZ-P3 PIC X(3) VALUE 'AZU'.
       01 BZ-TRACE.
         03 TRACE-LEN       PIC S9(4) COMP-4 VALUE 5.
         03 TRACE-TXT       PIC X(254) VALUE 'TRACE'.
       01 BZUASSRT          PIC X(8) VALUE 'BZUASSRT'.
       01 BZUTRACE          PIC X(8) VALUE 'BZUTRACE'.
       01 AZ-TRACE-PTR      POINTER.
       01 AZ-TEST-LEN       PIC S9(8) COMP.
       01 AZ-RECORD.
         03 AZ-RECORD-COUNT-OT PIC 9(5) COMP-5 VALUE 0.
         03 AZ-RECORD-COUNT-IN PIC 9(5) COMP-5 VALUE 0.
       01 AZ-GRP-INDEX      PIC 9(8).
       01 AZ-FLAG-IN        PIC 9(1).
       01 AZ-RECORD-PTR     POINTER.
       01 AZ-RC-WORK        PIC S9(4) USAGE BINARY.
       LOCAL-STORAGE SECTION.
       LINKAGE SECTION.
       01 AZ-TEST                   PIC X(80).
       01 AZ-INFO-BLOCK.
          COPY EQAITERC.
       01 AZ-WK-RECORD-COUNT PIC 9(5) COMP-5.
      *  *** W-EBUD02-LINKAGE-AREA : ZUT0000000A
       1 ZUT0000000A.
      *    *** W-INPUT-DATE : ZUT0000000B
         5 ZUT0000000B.
      *    *** W-CCYY : ZUT0000000C
         10 ZUT0000000C PIC 9(4).
      *    *** W-MM : ZUT0000000D
         10 ZUT0000000D PIC 9(2).
      *    *** W-DD : ZUT0000000E
         10 ZUT0000000E PIC 9(2).
      *    *** W-DAY-DIFFERENCE : ZUT0000000F
         5 ZUT0000000F PIC 9(9).
      *    *** W-EBUD02-PROGRAM-RETCODE : ZUT00000010
         5 ZUT00000010 PIC 9(4).
      *    *** W-EBUD02-REQUEST-SUCCESS : ZUT00000011
         88 ZUT00000011 VALUE 0.
      *
       PROCEDURE DIVISION.
       ENTRY_INPT.
      * ENTRY FOR CHECK OUTPUT VALUE
           ENTRY "PGM_INPT_EBUD02" USING
              AZ-TEST AZ-INFO-BLOCK
           ZUT0000000A.
           DISPLAY 'AZU0000I PGM_INPT_EBUD02 CHECK VALUES...'.
           PERFORM PROC_INPT.
           GOBACK.
      * ENTRY FOR CHECK OUTPUT VALUE WITH CSECT
           ENTRY "PGM_INPT_EBUD01_EBUD02" USING
              AZ-TEST AZ-INFO-BLOCK
           ZUT0000000A.
           DISPLAY 'AZU0000I PGM_INPT_EBUD01_EBUD02 CHECK VALUES...'.
           PERFORM PROC_INPT.
           GOBACK.
       ENTRY_OUTP.
      * ENTRY FOR SET INPUT VALUE
           ENTRY "PGM_OUTP_EBUD02" USING
              AZ-TEST AZ-INFO-BLOCK
           ZUT0000000A.
           DISPLAY 'AZU0000I PGM_OUTP_EBUD02 INPUT VALUES...'.
           PERFORM PROC_OUTP.
           GOBACK.
      * ENTRY FOR SET INPUT VALUE WITH CSECT
           ENTRY "PGM_OUTP_EBUD01_EBUD02" USING
              AZ-TEST AZ-INFO-BLOCK
           ZUT0000000A.
           DISPLAY 'AZU0000I PGM_OUTP_EBUD01_EBUD02 INPUT VALUES...'.
           PERFORM PROC_OUTP.
           GOBACK.
       PROC_INPT.
      * CHECK OUTPUT VALUE
           MOVE 4 TO RETURN-CODE.
           MOVE 0 TO AZ-TEST-LEN.
           INSPECT AZ-TEST TALLYING AZ-TEST-LEN FOR
           CHARACTERS BEFORE INITIAL SPACE.
      * SET AREA ADDRESS TO POINTER
           MOVE ITER OF AZ-INFO-BLOCK TO AZ-RECORD-COUNT-OT
           MOVE 2 TO AZ-GRP-INDEX
           MOVE 0 TO AZ-FLAG-IN
           MOVE RETURN-CODE TO AZ-RC-WORK
           CALL 'GTMEMRC' USING TC-WORK-AREA OF AZ-INFO-BLOCK
             AZ-GRP-INDEX AZ-FLAG-IN AZ-RECORD-PTR
           SET ADDRESS OF AZ-WK-RECORD-COUNT TO AZ-RECORD-PTR
           MOVE AZ-RC-WORK TO RETURN-CODE
           MOVE ITER OF AZ-INFO-BLOCK TO AZ-WK-RECORD-COUNT
           EVALUATE AZ-TEST(1:AZ-TEST-LEN)
           WHEN SPACE
             CONTINUE
           WHEN 'TEST1'
             PERFORM P-OUTPUT-TEST1
           WHEN OTHER
             CONTINUE
           END-EVALUATE.
           PERFORM TEARDOWN.
           EXIT.
       PROC_OUTP.
      * SET INPUT VALUE
           MOVE 0 TO RETURN-CODE.
           MOVE 0 TO AZ-TEST-LEN.
           INSPECT AZ-TEST TALLYING AZ-TEST-LEN FOR
           CHARACTERS BEFORE INITIAL SPACE.
      * SET AREA ADDRESS TO POINTER
           MOVE ITER OF AZ-INFO-BLOCK TO AZ-RECORD-COUNT-IN
           MOVE 2 TO AZ-GRP-INDEX
           MOVE 1 TO AZ-FLAG-IN
           MOVE RETURN-CODE TO AZ-RC-WORK
           CALL 'GTMEMRC' USING TC-WORK-AREA OF AZ-INFO-BLOCK
             AZ-GRP-INDEX AZ-FLAG-IN AZ-RECORD-PTR
           SET ADDRESS OF AZ-WK-RECORD-COUNT TO AZ-RECORD-PTR
           MOVE AZ-RC-WORK TO RETURN-CODE
           MOVE ITER OF AZ-INFO-BLOCK TO AZ-WK-RECORD-COUNT
           EVALUATE AZ-TEST(1:AZ-TEST-LEN)
           WHEN SPACE
             CONTINUE
           WHEN 'TEST1'
             PERFORM P-INPUT-TEST1
           WHEN OTHER
             CONTINUE
           END-EVALUATE.
           PERFORM TEARDOWN.
           EXIT.
       TEARDOWN.
           DISPLAY 'AZU0000I PGM_EBUD02 END.'
           EXIT.
       P-OUTPUT-TEST1.
           IF AZ-RECORD-COUNT-OT = 0 THEN
             CONTINUE
           ELSE
             CONTINUE
           END-IF.
           EXIT.
       P-INPUT-TEST1.
           IF AZ-RECORD-COUNT-IN = 0 THEN
             CONTINUE
           ELSE
             CONTINUE
           END-IF.
           EXIT.
       END PROGRAM 'PGM_EBUD02'.
      *+---------------------------------------------------------------+
      *| UNIT TEST FOR Z/OS: EBUD03                                    |
      *|     SUB PROGRAM CALLED FROM PROGRAM UNDER TEST                |
      *| TEST CASE VERSION: 200                                        |
      *+---------------------------------------------------------------+
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 'PGM_EBUD03'.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 BZ-ASSERT.
         03 MESSAGE-LEN PIC S9(4) COMP-4 VALUE 24.
         03 MESSAGE-TXT PIC X(254) VALUE 'HELLO FROM STUB CALLBACK'.
       01  BZ-P1 PIC S9(9) COMP-4 VALUE 4.
       01  BZ-P2 PIC S9(9) COMP-4 VALUE 2001.
       01  BZ-P3 PIC X(3) VALUE 'AZU'.
       01 BZ-TRACE.
         03 TRACE-LEN       PIC S9(4) COMP-4 VALUE 5.
         03 TRACE-TXT       PIC X(254) VALUE 'TRACE'.
       01 BZUASSRT          PIC X(8) VALUE 'BZUASSRT'.
       01 BZUTRACE          PIC X(8) VALUE 'BZUTRACE'.
       01 AZ-TRACE-PTR      POINTER.
       01 AZ-TEST-LEN       PIC S9(8) COMP.
       01 AZ-RECORD.
         03 AZ-RECORD-COUNT-OT PIC 9(5) COMP-5 VALUE 0.
         03 AZ-RECORD-COUNT-IN PIC 9(5) COMP-5 VALUE 0.
       01 AZ-GRP-INDEX      PIC 9(8).
       01 AZ-FLAG-IN        PIC 9(1).
       01 AZ-RECORD-PTR     POINTER.
       01 AZ-RC-WORK        PIC S9(4) USAGE BINARY.
       LOCAL-STORAGE SECTION.
       LINKAGE SECTION.
       01 AZ-TEST                   PIC X(80).
       01 AZ-INFO-BLOCK.
          COPY EQAITERC.
       01 AZ-WK-RECORD-COUNT PIC 9(5) COMP-5.
      *  *** W-EBUD03-LINKAGE-AREA : ZUT00000012
       1 ZUT00000012.
      *    *** W-RETIREMENT-DATE-IN : ZUT00000013
         5 ZUT00000013.
      *    *** W-RET-YYYY : ZUT00000014
         10 ZUT00000014 PIC X(4).
      *    *** FILLER : ZUT00000015
         10 ZUT00000015 PIC X(1).
      *    *** W-RET-MM : ZUT00000016
         10 ZUT00000016 PIC X(2).
      *    *** W-RET-DD : ZUT00000017
         10 ZUT00000017 PIC X(2).
      *    *** W-RETIREMENT-DATE : ZUT00000018
         5 ZUT00000018 PIC X(80).
      *    *** W-EBUD03-PROGRAM-RETCODE : ZUT00000019
         5 ZUT00000019 PIC 9(4).
      *    *** W-EBUD03-REQUEST-SUCCESS : ZUT0000001A
         88 ZUT0000001A VALUE 0.
      *
       PROCEDURE DIVISION.
       ENTRY_INPT.
      * ENTRY FOR CHECK OUTPUT VALUE
           ENTRY "PGM_INPT_EBUD03" USING
              AZ-TEST AZ-INFO-BLOCK
           ZUT00000012.
           DISPLAY 'AZU0000I PGM_INPT_EBUD03 CHECK VALUES...'.
           PERFORM PROC_INPT.
           GOBACK.
      * ENTRY FOR CHECK OUTPUT VALUE WITH CSECT
           ENTRY "PGM_INPT_EBUD01_EBUD03" USING
              AZ-TEST AZ-INFO-BLOCK
           ZUT00000012.
           DISPLAY 'AZU0000I PGM_INPT_EBUD01_EBUD03 CHECK VALUES...'.
           PERFORM PROC_INPT.
           GOBACK.
       ENTRY_OUTP.
      * ENTRY FOR SET INPUT VALUE
           ENTRY "PGM_OUTP_EBUD03" USING
              AZ-TEST AZ-INFO-BLOCK
           ZUT00000012.
           DISPLAY 'AZU0000I PGM_OUTP_EBUD03 INPUT VALUES...'.
           PERFORM PROC_OUTP.
           GOBACK.
      * ENTRY FOR SET INPUT VALUE WITH CSECT
           ENTRY "PGM_OUTP_EBUD01_EBUD03" USING
              AZ-TEST AZ-INFO-BLOCK
           ZUT00000012.
           DISPLAY 'AZU0000I PGM_OUTP_EBUD01_EBUD03 INPUT VALUES...'.
           PERFORM PROC_OUTP.
           GOBACK.
       PROC_INPT.
      * CHECK OUTPUT VALUE
           MOVE 4 TO RETURN-CODE.
           MOVE 0 TO AZ-TEST-LEN.
           INSPECT AZ-TEST TALLYING AZ-TEST-LEN FOR
           CHARACTERS BEFORE INITIAL SPACE.
      * SET AREA ADDRESS TO POINTER
           MOVE ITER OF AZ-INFO-BLOCK TO AZ-RECORD-COUNT-OT
           MOVE 3 TO AZ-GRP-INDEX
           MOVE 0 TO AZ-FLAG-IN
           MOVE RETURN-CODE TO AZ-RC-WORK
           CALL 'GTMEMRC' USING TC-WORK-AREA OF AZ-INFO-BLOCK
             AZ-GRP-INDEX AZ-FLAG-IN AZ-RECORD-PTR
           SET ADDRESS OF AZ-WK-RECORD-COUNT TO AZ-RECORD-PTR
           MOVE AZ-RC-WORK TO RETURN-CODE
           MOVE ITER OF AZ-INFO-BLOCK TO AZ-WK-RECORD-COUNT
           EVALUATE AZ-TEST(1:AZ-TEST-LEN)
           WHEN SPACE
             CONTINUE
           WHEN 'TEST1'
             PERFORM P-OUTPUT-TEST1
           WHEN OTHER
             CONTINUE
           END-EVALUATE.
           PERFORM TEARDOWN.
           EXIT.
       PROC_OUTP.
      * SET INPUT VALUE
           MOVE 0 TO RETURN-CODE.
           MOVE 0 TO AZ-TEST-LEN.
           INSPECT AZ-TEST TALLYING AZ-TEST-LEN FOR
           CHARACTERS BEFORE INITIAL SPACE.
      * SET AREA ADDRESS TO POINTER
           MOVE ITER OF AZ-INFO-BLOCK TO AZ-RECORD-COUNT-IN
           MOVE 3 TO AZ-GRP-INDEX
           MOVE 1 TO AZ-FLAG-IN
           MOVE RETURN-CODE TO AZ-RC-WORK
           CALL 'GTMEMRC' USING TC-WORK-AREA OF AZ-INFO-BLOCK
             AZ-GRP-INDEX AZ-FLAG-IN AZ-RECORD-PTR
           SET ADDRESS OF AZ-WK-RECORD-COUNT TO AZ-RECORD-PTR
           MOVE AZ-RC-WORK TO RETURN-CODE
           MOVE ITER OF AZ-INFO-BLOCK TO AZ-WK-RECORD-COUNT
           EVALUATE AZ-TEST(1:AZ-TEST-LEN)
           WHEN SPACE
             CONTINUE
           WHEN 'TEST1'
             PERFORM P-INPUT-TEST1
           WHEN OTHER
             CONTINUE
           END-EVALUATE.
           PERFORM TEARDOWN.
           EXIT.
       TEARDOWN.
           DISPLAY 'AZU0000I PGM_EBUD03 END.'
           EXIT.
       P-OUTPUT-TEST1.
           IF AZ-RECORD-COUNT-OT = 0 THEN
             CONTINUE
           ELSE
             CONTINUE
           END-IF.
           EXIT.
       P-INPUT-TEST1.
           IF AZ-RECORD-COUNT-IN = 0 THEN
             CONTINUE
           ELSE
             CONTINUE
           END-IF.
           EXIT.
       END PROGRAM 'PGM_EBUD03'.
      *+---------------------------------------------------------------+
      *| UNIT TEST FOR Z/OS: FILE                                      |
      *|     FILE CALLBACK EXIT POINT                                  |
      *| TEST CASE VERSION: 200                                        |
      *+---------------------------------------------------------------+
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 'FILE_EBUD01'.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       LOCAL-STORAGE SECTION.
       LINKAGE SECTION.
       01 AZ-TEST              PIC X(80).
       01 AZ-INFO-BLOCK.
          COPY EQAITERC.
       01 AZ-INFO-DDNM         PIC X(8).
       01 AZ-INFO-STAT         PIC X(1).
       01 AZ-INFO-COND         PIC X(1).
       01 AZ-ACMDVA            PIC X(4).
       01 AZ-PARM              PIC X(80).
       PROCEDURE DIVISION.
      * CHECK OUTPUT VALUE
      * SET INPUT VALUE
      * QSAM_OUTP_READ_EBUD01.
           ENTRY 'QSAM_OUTP_READ_EBUD01' USING AZ-TEST
             AZ-INFO-BLOCK AZ-INFO-DDNM AZ-INFO-STAT AZ-INFO-COND
             AZ-ACMDVA AZ-PARM.
           DISPLAY 'AZU0000I QSAM_OUTP_READ_EBUD01 INPUT VALUES...'
           IF AZ-INFO-STAT = X'00' THEN
              PERFORM READ-INPUT
           END-IF.
           PERFORM TEARDOWN.
           GOBACK.
      * END
       READ-OUTPUT.
           EXIT.
       READ-INPUT.
      *    FILE "INPUT-FILE" (DD:'INPUT')"
           IF AZ-INFO-DDNM = "'INPUT'" THEN
             CALL 'INPUT-FILE_INPUT_READ' USING AZ-TEST
             AZ-INFO-BLOCK AZ-INFO-DDNM AZ-INFO-STAT AZ-INFO-COND
             AZ-ACMDVA AZ-PARM
           END-IF.
           EXIT.
       WRITE-OUTPUT.
           EXIT.
       WRITE-INPUT.
           EXIT.
       TEARDOWN.
           DISPLAY 'AZU0000I FILE_EBUD01 END.'
           EXIT.
       END PROGRAM 'FILE_EBUD01'.
      *+---------------------------------------------------------------+
      *| UNIT TEST FOR Z/OS: FILE QSAM                                 |
      *|   FILE NAME: INPUT-FILE                                       |
      *| TEST CASE VERSION: 200                                        |
      *+---------------------------------------------------------------+
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 'QSAM_INPUT-FILE_EBUD01'.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 BZ-ASSERT.
         03 MESSAGE-LEN PIC S9(4) COMP-4 VALUE 24.
         03 MESSAGE-TXT PIC X(254) VALUE 'HELLO FROM STUB CALLBACK'.
       01  BZ-P1 PIC S9(9) COMP-4 VALUE 4.
       01  BZ-P2 PIC S9(9) COMP-4 VALUE 2001.
       01  BZ-P3 PIC X(3) VALUE 'AZU'.
       01 BZ-TRACE.
         03 TRACE-LEN       PIC S9(4) COMP-4 VALUE 5.
         03 TRACE-TXT       PIC X(254) VALUE 'TRACE'.
       01 BZUASSRT          PIC X(8) VALUE 'BZUASSRT'.
       01 BZUTRACE          PIC X(8) VALUE 'BZUTRACE'.
       01 AZ-TRACE-PTR      POINTER.
       01 AZ-TEST-LEN       PIC S9(8) COMP.
       01 AZ-RECORD.
         03 AZ-RECORD-COUNT PIC 9(5) COMP-5 VALUE 0.
       01 AZ-GRP-INDEX      PIC 9(8).
       01 AZ-FLAG-IN        PIC 9(1).
       01 AZ-RECORD-PTR     POINTER.
       01 AZ-RC-WORK        PIC S9(4) USAGE BINARY.
       LOCAL-STORAGE SECTION.
       LINKAGE SECTION.
       01 AZ-TEST             PIC X(80).
       01 AZ-INFO-BLOCK.
          COPY EQAITERC.
       01 AZ-INFO-DDNM        PIC X(8).
       01 AZ-INFO-STAT        PIC X(1).
       01 AZ-INFO-COND        PIC X(1).
       01 AZ-ACMDVA           PIC X(4).
       01 AZ-PARM             PIC X(80).
       01 AZ-WK-RECORD-COUNT  PIC 9(5) COMP-5.
      *  *** INPUT-RECORD : ZUT00000000
       1 ZUT00000000.
      *    *** DATE-AREA : ZUT00000001
         5 ZUT00000001.
      *    *** IN-CCYY : ZUT00000002
         7 ZUT00000002 PIC 9(4).
      *    *** IN-MM : ZUT00000003
         7 ZUT00000003 PIC 9(2).
      *    *** IN-DD : ZUT00000004
         7 ZUT00000004 PIC 9(2).
      *    *** FILLER : ZUT00000005
         5 ZUT00000005 PIC X(72).
      *
       PROCEDURE DIVISION.
      * CHECK OUTPUT VALUE
           ENTRY 'INPUT-FILE_OUTPUT_WRITE' USING AZ-TEST
             AZ-INFO-BLOCK AZ-INFO-DDNM AZ-INFO-STAT AZ-INFO-COND
             AZ-ACMDVA AZ-PARM
           DISPLAY 'AZU0000I FILE INPUT-FILE CHECK VALUES...'.
           MOVE 4 TO RETURN-CODE.
           MOVE 0 TO AZ-TEST-LEN.
           INSPECT AZ-TEST TALLYING AZ-TEST-LEN FOR
           CHARACTERS BEFORE INITIAL SPACE.
           SET ADDRESS OF ZUT00000000 TO ADDRESS OF AZ-PARM
      * SET AREA ADDRESS TO POINTER
           MOVE 1 TO AZ-GRP-INDEX
           MOVE 0 TO AZ-FLAG-IN
           MOVE RETURN-CODE TO AZ-RC-WORK
           CALL 'GTMEMRC' USING TC-WORK-AREA OF AZ-INFO-BLOCK
             AZ-GRP-INDEX AZ-FLAG-IN AZ-RECORD-PTR
           SET ADDRESS OF AZ-WK-RECORD-COUNT TO AZ-RECORD-PTR
           MOVE AZ-RC-WORK TO RETURN-CODE
           ADD 1 TO AZ-WK-RECORD-COUNT
           MOVE AZ-WK-RECORD-COUNT TO AZ-RECORD-COUNT
           EVALUATE AZ-TEST(1:AZ-TEST-LEN)
           WHEN SPACE
             CONTINUE
           WHEN 'TEST1'
             PERFORM P-OUTPUT-TEST1
           WHEN OTHER
             CONTINUE
           END-EVALUATE.
           PERFORM TEARDOWN.
           GOBACK.
      * SET INPUT VALUE
           ENTRY 'INPUT-FILE_INPUT_READ' USING AZ-TEST
             AZ-INFO-BLOCK AZ-INFO-DDNM AZ-INFO-STAT AZ-INFO-COND
             AZ-ACMDVA AZ-PARM
           DISPLAY 'AZU0000I FILE INPUT-FILE INPUT VALUES...'.
           MOVE 0 TO RETURN-CODE.
           MOVE 0 TO AZ-TEST-LEN.
           INSPECT AZ-TEST TALLYING AZ-TEST-LEN FOR
           CHARACTERS BEFORE INITIAL SPACE.
           SET ADDRESS OF ZUT00000000 TO ADDRESS OF AZ-PARM
      * SET AREA ADDRESS TO POINTER
           MOVE 1 TO AZ-GRP-INDEX
           MOVE 0 TO AZ-FLAG-IN
           MOVE RETURN-CODE TO AZ-RC-WORK
           CALL 'GTMEMRC' USING TC-WORK-AREA OF AZ-INFO-BLOCK
             AZ-GRP-INDEX AZ-FLAG-IN AZ-RECORD-PTR
           SET ADDRESS OF AZ-WK-RECORD-COUNT TO AZ-RECORD-PTR
           MOVE AZ-RC-WORK TO RETURN-CODE
           ADD 1 TO AZ-WK-RECORD-COUNT
           MOVE AZ-WK-RECORD-COUNT TO AZ-RECORD-COUNT
           EVALUATE AZ-TEST(1:AZ-TEST-LEN)
           WHEN SPACE
             CONTINUE
           WHEN 'TEST1'
             PERFORM P-INPUT-TEST1
           WHEN OTHER
             CONTINUE
           END-EVALUATE.
           PERFORM TEARDOWN.
           GOBACK.
       TEARDOWN.
      *     DISPLAY 'AZU0000I QSAM_INPUT-FILE_EBUD01 END.'
           EXIT.
       P-OUTPUT-TEST1.
           IF AZ-RECORD-COUNT = 0 THEN
             CONTINUE
           ELSE
             CONTINUE
           END-IF.
           EXIT.
       P-INPUT-TEST1.
           IF AZ-RECORD-COUNT = 0 THEN
             CONTINUE
           ELSE
             CONTINUE
           END-IF.
           EXIT.
       END PROGRAM 'QSAM_INPUT-FILE_EBUD01'.
      *+---------------------------------------------------------------+
      *| UNIT TEST FOR Z/OS: GTMEMRC                                   |
      *|     GET DATA AREA FOR RECORD COUNT OF SUBSYSTEM GROUP         |
      *| TEST CASE VERSION: 200                                        |
      *+---------------------------------------------------------------+
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 'GTMEMRC'.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 BZUGTMEM            PIC X(8) VALUE 'BZUGTMEM'.
       01 DATA-SIZE           PIC 9(8) COMP-4.
       LINKAGE SECTION.
       01 AZ-TC-WORK-AREA        PIC X(256).
       01 AZ-GRP-INDEX        PIC 9(8).
       01 AZ-FLAG-IN          PIC 9(1).
       01 AZ-RECORD-PTR       POINTER.
       01 AZ-RECORD-PTR-VALUE
            REDEFINES AZ-RECORD-PTR  PIC S9(9) COMP-5.
       01 DATA-PTR            POINTER.
       01 DATA-PTR-VALUE
            REDEFINES DATA-PTR  PIC S9(9) COMP-5.
       01 DATA-AREA.
         03 RECORD-COUNT-IO OCCURS 3.
           05 RECORD-COUNT-OT PIC 9(5) COMP-5.
           05 RECORD-COUNT-IN PIC 9(5) COMP-5.
       01 WK-RECORD-COUNT     PIC 9(5) COMP-5.
       01 AZ-TEST             PIC X(80).
       01 AZ-INFO-BLOCK.
          COPY EQAITERC.
       PROCEDURE DIVISION USING AZ-TC-WORK-AREA AZ-GRP-INDEX AZ-FLAG-IN
           AZ-RECORD-PTR.
       MAINPROC.
      * ENTRY FOR CALLBACK
           ENTRY "PGM_INPT_GTMEMRC" USING AZ-TEST AZ-INFO-BLOCK
             AZ-TC-WORK-AREA AZ-GRP-INDEX AZ-FLAG-IN AZ-RECORD-PTR.
      * GET DATA AREA
           SET ADDRESS OF DATA-PTR TO ADDRESS OF AZ-TC-WORK-AREA.
           IF DATA-PTR-VALUE = 0 THEN
             COMPUTE DATA-SIZE = LENGTH OF WK-RECORD-COUNT * 2 * 3
             CALL BZUGTMEM USING DATA-SIZE RETURNING DATA-PTR
             SET ADDRESS OF DATA-AREA TO DATA-PTR
             DISPLAY 'AZU0000I AREA ALLOCATED FOR RECORD COUNT:'
           DATA-SIZE
           END-IF
           SET AZ-RECORD-PTR TO DATA-PTR
           COMPUTE AZ-RECORD-PTR-VALUE = AZ-RECORD-PTR-VALUE +
                 LENGTH OF WK-RECORD-COUNT * 2 * (AZ-GRP-INDEX - 1)
           IF AZ-FLAG-IN = 1 THEN
             ADD LENGTH OF WK-RECORD-COUNT TO AZ-RECORD-PTR-VALUE
           END-IF
           SET ADDRESS OF WK-RECORD-COUNT TO AZ-RECORD-PTR
           GOBACK.
       END PROGRAM 'GTMEMRC'.
      *+---------------------------------------------------------------+
      *| UNIT TEST FOR Z/OS: AZU_GENERIC_FILE                          |
      *|   GENERIC FILE CALLBACK EXIT POINT                            |
      *| TEST CASE VERSION: 200                                        |
      *+---------------------------------------------------------------+
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 'AZU_GENERIC_FILE'.
       PROCEDURE DIVISION.
      * QSAM_INPT.
           ENTRY 'QSAM_INPT'.
           DISPLAY 'AZU0000I QSAM_INPT ...'
           MOVE 4 TO RETURN-CODE.
           GOBACK.
      * QSAM_OUTP.
           ENTRY 'QSAM_OUTP'.
           DISPLAY 'AZU0000I QSAM_OUTP ...'
           MOVE 4 TO RETURN-CODE.
           GOBACK.
       END PROGRAM 'AZU_GENERIC_FILE'.
