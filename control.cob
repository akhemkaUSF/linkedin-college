       IDENTIFICATION DIVISION.
       PROGRAM-ID. CONTROLFLOW.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUTFILE ASSIGN TO "user_input.txt"
              ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUTPUTFILE ASSIGN TO "output_log.txt"
              ORGANIZATION IS LINE SEQUENTIAL.
           SELECT ACCOUNTS ASSIGN TO "accounts.txt"
              ORGANIZATION IS LINE SEQUENTIAL
              FILE STATUS IS ACC-FS.

       DATA DIVISION.
       FILE SECTION.
       FD  INPUTFILE.
       01  INPUT-REC            PIC X(100).

       FD  OUTPUTFILE.
       01  OUT-REC              PIC X(120).

       FD  ACCOUNTS.
       01  ACCT-REC             PIC X(100).

       WORKING-STORAGE SECTION.
       77  ACC-FS               PIC XX VALUE SPACES.  *> file status for ACCOUNTS

       77  EOF-FLAG             PIC X  VALUE "N".
       77  USERNAME             PIC X(20).
       77  PASSWORD             PIC X(20).
       77  VALID-LOGIN          PIC X  VALUE "N".
       77  ACCT-COUNT           PIC 9  VALUE 0.
       77  SKILL-CHOICE         PIC 9  VALUE 0.
       77  MSG                  PIC X(100).

       *> Fields used when splitting an account line
       77  ACCT-USER            PIC X(20).
       77  ACCT-PASS            PIC X(20).

       *> Password validation helpers
       77  PASSWORD-LEN         PIC 99.
       77  I                    PIC 99.
       77  HAS-UPPER            PIC X  VALUE "N".
       77  HAS-DIGIT            PIC X  VALUE "N".
       77  HAS-SPECIAL          PIC X  VALUE "N".
       77  PASSWORD-VALID       PIC X  VALUE "N".

       PROCEDURE DIVISION.
       MAIN-PARA.
           OPEN INPUT INPUTFILE
           OPEN OUTPUT OUTPUTFILE

           *> Try opening ACCOUNTS for I-O; if it doesn't exist, create it
           OPEN I-O ACCOUNTS
           IF ACC-FS NOT = "00"
              CLOSE ACCOUNTS
              OPEN OUTPUT ACCOUNTS
              CLOSE ACCOUNTS
              OPEN I-O ACCOUNTS
           END-IF

           PERFORM LOAD-ACCOUNTS

           PERFORM UNTIL EOF-FLAG = "Y"
              READ INPUTFILE
                 AT END
                    MOVE "Y" TO EOF-FLAG
                 NOT AT END
                    MOVE FUNCTION TRIM(INPUT-REC) TO MSG
                    PERFORM PROCESS-COMMAND
              END-READ
           END-PERFORM

           CLOSE INPUTFILE
           CLOSE OUTPUTFILE
           CLOSE ACCOUNTS
           STOP RUN.

       LOAD-ACCOUNTS.
           MOVE 0 TO ACCT-COUNT
           OPEN INPUT ACCOUNTS
           PERFORM UNTIL 1 = 0
              READ ACCOUNTS NEXT RECORD
                 AT END EXIT PERFORM
                 NOT AT END
                    ADD 1 TO ACCT-COUNT
              END-READ
           END-PERFORM
           CLOSE ACCOUNTS
           OPEN I-O ACCOUNTS.

       PROCESS-COMMAND.
           IF MSG = "LOGIN"
              PERFORM DO-LOGIN
           ELSE
              IF MSG = "CREATE"
                 PERFORM DO-CREATE
              ELSE
                 MOVE "Invalid choice, must be LOGIN or CREATE" TO MSG
                 PERFORM WRITE-OUTPUT
              END-IF
           END-IF.

       DO-LOGIN.
           MOVE "Enter username:" TO MSG
           PERFORM WRITE-OUTPUT
           READ INPUTFILE AT END EXIT PARAGRAPH
              NOT AT END MOVE FUNCTION TRIM(INPUT-REC) TO USERNAME
           END-READ

           MOVE "Enter password:" TO MSG
           PERFORM WRITE-OUTPUT
           READ INPUTFILE AT END EXIT PARAGRAPH
              NOT AT END MOVE FUNCTION TRIM(INPUT-REC) TO PASSWORD
           END-READ

           PERFORM CHECK-CREDENTIALS
           IF VALID-LOGIN = "Y"
              MOVE "You have successfully logged in" TO MSG
              PERFORM WRITE-OUTPUT
              PERFORM USER-MENU
           ELSE
              MOVE "Incorrect username/password, please try again" TO MSG
              PERFORM WRITE-OUTPUT
           END-IF.

       CHECK-CREDENTIALS.
           MOVE "N" TO VALID-LOGIN
           OPEN INPUT ACCOUNTS
           PERFORM UNTIL 1=0
              READ ACCOUNTS NEXT RECORD
                 AT END EXIT PERFORM
                 NOT AT END
                    UNSTRING ACCT-REC
                       DELIMITED BY ALL " "
                       INTO ACCT-USER ACCT-PASS
                    END-UNSTRING
                    IF USERNAME = FUNCTION TRIM(ACCT-USER)
                       AND PASSWORD = FUNCTION TRIM(ACCT-PASS)
                       MOVE "Y" TO VALID-LOGIN
                       EXIT PERFORM
                    END-IF
              END-READ
           END-PERFORM
           CLOSE ACCOUNTS
           OPEN I-O ACCOUNTS.

       USER-MENU.
           MOVE "Choose: 1=Search job, 2=Learn skill, 3=Return" TO MSG
           PERFORM WRITE-OUTPUT
           READ INPUTFILE AT END EXIT PARAGRAPH
              NOT AT END MOVE FUNCTION NUMVAL(INPUT-REC) TO SKILL-CHOICE
           END-READ

           EVALUATE SKILL-CHOICE
              WHEN 1
                 MOVE "Under Construction" TO MSG
                 PERFORM WRITE-OUTPUT
              WHEN 2
                 MOVE "Pick a skill (1-5)" TO MSG
                 PERFORM WRITE-OUTPUT
                 READ INPUTFILE AT END EXIT PARAGRAPH
                    NOT AT END MOVE FUNCTION NUMVAL(INPUT-REC) TO SKILL-CHOICE
                 END-READ
                 MOVE "Under Construction" TO MSG
                 PERFORM WRITE-OUTPUT
              WHEN 3
                 MOVE "Returning to main menu" TO MSG
                 PERFORM WRITE-OUTPUT
              WHEN OTHER
                 MOVE "Invalid option" TO MSG
                 PERFORM WRITE-OUTPUT
           END-EVALUATE.

       DO-CREATE.
           IF ACCT-COUNT >= 5
              MOVE "All permitted accounts created, come back later" TO MSG
              PERFORM WRITE-OUTPUT
              EXIT PARAGRAPH
           END-IF

           MOVE "Enter new username:" TO MSG
           PERFORM WRITE-OUTPUT
           READ INPUTFILE AT END EXIT PARAGRAPH
              NOT AT END MOVE FUNCTION TRIM(INPUT-REC) TO USERNAME
           END-READ

           *> Check uniqueness
           OPEN INPUT ACCOUNTS
           PERFORM UNTIL 1=0
              READ ACCOUNTS NEXT RECORD
                 AT END EXIT PERFORM
                 NOT AT END
                    UNSTRING ACCT-REC
                       DELIMITED BY ALL " "
                       INTO ACCT-USER ACCT-PASS
                    END-UNSTRING
                    IF USERNAME = FUNCTION TRIM(ACCT-USER)
                       MOVE "Username taken" TO MSG
                       PERFORM WRITE-OUTPUT
                       CLOSE ACCOUNTS
                       OPEN I-O ACCOUNTS
                       EXIT PARAGRAPH
                    END-IF
              END-READ
           END-PERFORM
           CLOSE ACCOUNTS

           MOVE "Enter password:" TO MSG
           PERFORM WRITE-OUTPUT
           READ INPUTFILE AT END EXIT PARAGRAPH
              NOT AT END MOVE FUNCTION TRIM(INPUT-REC) TO PASSWORD
           END-READ

           PERFORM CHECK-PASSWORD
           IF PASSWORD-VALID = "Y"
              MOVE SPACES TO ACCT-REC
              STRING FUNCTION TRIM(USERNAME) DELIMITED BY SIZE
                     " "                     DELIMITED BY SIZE
                     FUNCTION TRIM(PASSWORD) DELIMITED BY SIZE
                     INTO ACCT-REC
              END-STRING

              *> Append new record
              OPEN EXTEND ACCOUNTS
              WRITE ACCT-REC
              CLOSE ACCOUNTS

              ADD 1 TO ACCT-COUNT
              MOVE "Account created, restart to login" TO MSG
              PERFORM WRITE-OUTPUT
           ELSE
              MOVE "Password does not meet requirements" TO MSG
              PERFORM WRITE-OUTPUT
           END-IF.

       CHECK-PASSWORD.
           MOVE FUNCTION LENGTH(FUNCTION TRIM(PASSWORD)) TO PASSWORD-LEN

           MOVE "N" TO HAS-UPPER
           MOVE "N" TO HAS-DIGIT
           MOVE "N" TO HAS-SPECIAL
           MOVE "N" TO PASSWORD-VALID

           IF PASSWORD-LEN < 8 OR PASSWORD-LEN > 12
              EXIT PARAGRAPH
           END-IF

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > PASSWORD-LEN
              EVALUATE TRUE
                 WHEN PASSWORD(I:1) >= "A" AND PASSWORD(I:1) <= "Z"
                    MOVE "Y" TO HAS-UPPER
                 WHEN PASSWORD(I:1) >= "0" AND PASSWORD(I:1) <= "9"
                    MOVE "Y" TO HAS-DIGIT
                 WHEN (PASSWORD(I:1) >= "a" AND PASSWORD(I:1) <= "z")
                    CONTINUE
                 WHEN OTHER
                    MOVE "Y" TO HAS-SPECIAL
              END-EVALUATE
           END-PERFORM

           IF HAS-UPPER = "Y" AND HAS-DIGIT = "Y" AND HAS-SPECIAL = "Y"
              MOVE "Y" TO PASSWORD-VALID
           END-IF.

       WRITE-OUTPUT.
           MOVE MSG TO OUT-REC
           WRITE OUT-REC
           DISPLAY MSG.
