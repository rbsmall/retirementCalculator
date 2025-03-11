       ID DIVISION.
       PROGRAM-ID. EBUD01.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO 'INPUT'.

       DATA DIVISION.
       FILE SECTION.
       FD  INPUT-FILE
           RECORD CONTAINS 80 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE F.

       01  INPUT-RECORD.
           COPY INDATE.

       WORKING-STORAGE SECTION.
       01  WS-END-OF-FILE     PIC X(1) VALUE 'N'.
           88  EOF            VALUE 'Y'.

       01  W-CALL-PROGRAM       PIC X(8).
       01  W-RETIREMENT-WA      PIC 9(4).

       01  W-EBUD02-LINKAGE-AREA.
           05  W-INPUT-DATE.
               10 W-CCYY  PIC 9(4).
               10 W-MM    PIC 9(2).
               10 W-DD    PIC 9(2).
           05  W-DAY-DIFFERENCE       PIC 9(9).
           05  W-EBUD02-PROGRAM-RETCODE PIC 9(4).
               88 W-EBUD02-REQUEST-SUCCESS VALUE 0.

       01  W-EBUD03-LINKAGE-AREA.
           05  W-RETIREMENT-DATE-IN.
               10 W-RET-YYYY  PIC X(4).
               10 FILLER      PIC X(1) VALUE '/'.
               10 W-RET-MM    PIC X(2).
               10 W-RET-DD    PIC X(2).
           05  W-RETIREMENT-DATE        PIC X(80).
           05  W-EBUD03-PROGRAM-RETCODE PIC 9(4).
               88 W-EBUD03-REQUEST-SUCCESS VALUE 0.

       PROCEDURE DIVISION.

       A000-MAINLINE SECTION.
           OPEN INPUT INPUT-FILE

           PERFORM UNTIL EOF
               READ INPUT-FILE
                   AT END
                       MOVE 'Y' TO WS-END-OF-FILE
                   NOT AT END
                       PERFORM A100-VERIFY-INPUT-DATE
                       PERFORM A200-CALL-DAY-DIFFERENCE-PROG
                       PERFORM A300-CALCULATE-RETIREMENT
               END-READ
           END-PERFORM

           CLOSE INPUT-FILE
           GOBACK.

       A100-VERIFY-INPUT-DATE SECTION.
           MOVE IN-CCYY TO W-CCYY
           MOVE IN-MM   TO W-MM
           MOVE IN-DD   TO W-DD

           DISPLAY 'PROCESSING DATE: ' IN-CCYY '/' IN-MM '/' IN-DD

           IF DATE-AREA NUMERIC
              DISPLAY 'VALID DATE: ' IN-CCYY '/' IN-MM '/' IN-DD
              MOVE 0 TO W-EBUD02-PROGRAM-RETCODE
           ELSE
              DISPLAY 'INVALID DATE: ' DATE-AREA
              MOVE -1 TO W-EBUD02-PROGRAM-RETCODE
           END-IF.

       A200-CALL-DAY-DIFFERENCE-PROG SECTION.
           MOVE 'EBUD02' TO W-CALL-PROGRAM
           MOVE 0        TO W-DAY-DIFFERENCE

           CALL W-CALL-PROGRAM USING W-EBUD02-LINKAGE-AREA

           IF W-EBUD02-REQUEST-SUCCESS
              DISPLAY 'DAYS DIFFERENCE = ' W-DAY-DIFFERENCE
           ELSE
              DISPLAY 'ERROR CALLING ' W-CALL-PROGRAM
              DISPLAY 'RETURN CODE: ' W-EBUD02-PROGRAM-RETCODE
           END-IF.

       A300-CALCULATE-RETIREMENT SECTION.
           IF W-CCYY < 1987
               COMPUTE W-RETIREMENT-WA = W-CCYY + 65
           ELSE
               COMPUTE W-RETIREMENT-WA = W-CCYY + 66
           END-IF

           MOVE W-RETIREMENT-WA TO W-RET-YYYY
           MOVE W-MM            TO W-RET-MM
           MOVE W-DD            TO W-RET-DD
           MOVE SPACES          TO W-RETIREMENT-DATE
           MOVE 0               TO W-EBUD03-PROGRAM-RETCODE

           MOVE 'EBUD03' TO W-CALL-PROGRAM
           CALL W-CALL-PROGRAM USING W-EBUD03-LINKAGE-AREA

           IF W-EBUD03-REQUEST-SUCCESS
              DISPLAY 'RETIREMENT DATE = ' W-RETIREMENT-DATE
           ELSE
              DISPLAY 'ERROR CALLING ' W-CALL-PROGRAM
              DISPLAY 'RETURN CODE: ' W-EBUD03-PROGRAM-RETCODE
           END-IF.