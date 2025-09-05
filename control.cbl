       IDENTIFICATION DIVISION.
       PROGRAM-ID. CONTROLFLOW.
      * Every COBOL program must have an identification division.
      * PROGRAM-ID gives the program a name.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      * File declarations: we will read user input, write logs, and store accounts.

           SELECT INPUTFILE ASSIGN TO "user_input.txt"
              ORGANIZATION IS LINE SEQUENTIAL.
      * INPUTFILE = where “user inputs” come from (line by line).

           SELECT OUTPUTFILE ASSIGN TO "output_log.txt"
              ORGANIZATION IS LINE SEQUENTIAL.
      * OUTPUTFILE = log of program’s messages.

           SELECT ACCOUNTS ASSIGN TO "accounts.txt"
              ORGANIZATION IS LINE SEQUENTIAL.
      * ACCOUNTS = persistent storage of up to 5 username/password pairs.

       DATA DIVISION.
       FILE SECTION.
      * File record layouts. Each FD (File Description) describes one file.

       FD  INPUTFILE.
       01  INPUT-REC            PIC X(100).
      * Each line of input is up to 100 characters.

       FD  OUTPUTFILE.
       01  OUT-REC              PIC X(120).
      * Each output record is up to 120 characters.

       FD  ACCOUNTS.
       01  ACCT-REC.
           05 ACCT-USER         PIC X(20).
           05 FILLER            PIC X.
           05 ACCT-PASS         PIC X(20).
      * Accounts file: username (20 chars), a space, password (20 chars).

       WORKING-STORAGE SECTION.
      * Variables held in memory during program execution.

       77  EOF-FLAG             PIC X VALUE "N".
      * End-of-file flag (Y/N).

       77  USERNAME             PIC X(20).
       77  PASSWORD             PIC X(20).
       77  USER-CHOICE          PIC 9.
       77  VALID-LOGIN          PIC X VALUE "N".
       77  ACCT-COUNT           PIC 9 VALUE 0.
       77  SKILL-CHOICE         PIC 9.
       77  MSG                  PIC X(100).
      * General variables for input processing, flags, and counts.

       * For password validation
       77  PASSWORD-LEN         PIC 99.
       77  I                    PIC 99.
       77  HAS-UPPER            PIC X VALUE "N".
       77  HAS-DIGIT            PIC X VALUE "N".
       77  HAS-SPECIAL          PIC X VALUE "N".
       77  PASSWORD-VALID       PIC X VALUE "N".
      * Flags used in password checking.

       PROCEDURE DIVISION.
      * This is where execution starts.

       MAIN-PARA.
           OPEN INPUT INPUTFILE
           OPEN OUTPUT OUTPUTFILE
           OPEN I-O ACCOUNTS
      * Open files: input for reading, output for writing, accounts for both.

           IF NOT STATUS-ACCOUNTS = "00"
              OPEN OUTPUT ACCOUNTS
              CLOSE ACCOUNTS
              OPEN I-O ACCOUNTS
           END-IF
      * If accounts file didn’t exist, create it first.

           PERFORM LOAD-ACCOUNTS
      * Count how many accounts already exist.

           PERFORM UNTIL EOF-FLAG = "Y"
              READ INPUTFILE
                 AT END MOVE "Y" TO EOF-FLAG
                 NOT AT END
                    MOVE FUNCTION TRIM(INPUT-REC) TO MSG
                    PERFORM PROCESS-COMMAND
              END-READ
           END-PERFORM
      * Main loop: read each line from user_input.txt, process as command.

           CLOSE INPUTFILE
           CLOSE OUTPUTFILE
           CLOSE ACCOUNTS
           STOP RUN.
      * End of program.

       LOAD-ACCOUNTS.
           MOVE 0 TO ACCT-COUNT
           OPEN INPUT ACCOUNTS
           PERFORM UNTIL 1 = 0
              READ ACCOUNTS NEXT RECORD
                 AT END EXIT PERFORM
                 NOT AT END ADD 1 TO ACCT-COUNT
              END-READ
           END-PERFORM
           CLOSE ACCOUNTS
           OPEN I-O ACCOUNTS.
      * Count how many accounts are already saved in accounts.txt.

       PROCESS-COMMAND.
           EVALUATE TRUE
              WHEN MSG = "LOGIN"
                   PERFORM DO-LOGIN
              WHEN MSG = "CREATE"
                   PERFORM DO-CREATE
              WHEN OTHER
                   PERFORM WRITE-OUTPUT
                      USING "Invalid choice, must be LOGIN or CREATE"
           END-EVALUATE.
      * Decide what to do depending on the command string.

       DO-LOGIN.
           PERFORM WRITE-OUTPUT USING "Enter username:"
           READ INPUTFILE AT END EXIT PARAGRAPH
              NOT AT END MOVE FUNCTION TRIM(INPUT-REC) TO USERNAME
           END-READ

           PERFORM WRITE-OUTPUT USING "Enter password:"
           READ INPUTFILE AT END EXIT PARAGRAPH
              NOT AT END MOVE FUNCTION TRIM(INPUT-REC) TO PASSWORD
           END-READ

           PERFORM CHECK-CREDENTIALS
           IF VALID-LOGIN = "Y"
              PERFORM WRITE-OUTPUT USING "You have successfully logged in"
              PERFORM USER-MENU
           ELSE
              PERFORM WRITE-OUTPUT
                USING "Incorrect username/password, please try again"
           END-IF.
      * LOGIN: read username and password, check against accounts file.

       CHECK-CREDENTIALS.
           MOVE "N" TO VALID-LOGIN
           OPEN INPUT ACCOUNTS
           PERFORM UNTIL 1=0
              READ ACCOUNTS NEXT RECORD
                 AT END EXIT PERFORM
                 NOT AT END
                    IF USERNAME = ACCT-USER
                       AND PASSWORD = ACCT-PASS
                       MOVE "Y" TO VALID-LOGIN
                       EXIT PERFORM
                    END-IF
              END-READ
           END-PERFORM
           CLOSE ACCOUNTS
           OPEN I-O ACCOUNTS.
      * Validate credentials by scanning through accounts.txt.

       USER-MENU.
           PERFORM WRITE-OUTPUT
             USING "Choose: 1=Search job, 2=Learn skill, 3=Return"
           READ INPUTFILE AT END EXIT PARAGRAPH
              NOT AT END MOVE FUNCTION NUMVAL(INPUT-REC) TO SKILL-CHOICE
           END-READ

           EVALUATE SKILL-CHOICE
              WHEN 1
                 PERFORM WRITE-OUTPUT USING "Under Construction"
              WHEN 2
                 PERFORM WRITE-OUTPUT USING "Pick a skill (1-5)"
                 READ INPUTFILE AT END EXIT PARAGRAPH
                    NOT AT END MOVE FUNCTION NUMVAL(INPUT-REC) TO SKILL-CHOICE
                 END-READ
                 PERFORM WRITE-OUTPUT USING "Under Construction"
              WHEN 3
                 PERFORM WRITE-OUTPUT USING "Returning to main menu"
              WHEN OTHER
                 PERFORM WRITE-OUTPUT USING "Invalid option"
           END-EVALUATE.
      * Once logged in, user can search jobs, learn skills, or return.

       DO-CREATE.
           IF ACCT-COUNT >= 5
              PERFORM WRITE-OUTPUT
                USING "All permitted accounts created, come back later"
              EXIT PARAGRAPH
           END-IF

           PERFORM WRITE-OUTPUT USING "Enter new username:"
           READ INPUTFILE AT END EXIT PARAGRAPH
              NOT AT END MOVE FUNCTION TRIM(INPUT-REC) TO USERNAME
           END-READ

           * Check uniqueness
           OPEN INPUT ACCOUNTS
           PERFORM UNTIL 1=0
              READ ACCOUNTS NEXT RECORD
                 AT END EXIT PERFORM
                 NOT AT END
                    IF USERNAME = ACCT-USER
                       PERFORM WRITE-OUTPUT USING "Username taken"
                       CLOSE ACCOUNTS
                       OPEN I-O ACCOUNTS
                       EXIT PARAGRAPH
                    END-IF
              END-READ
           END-PERFORM
           CLOSE ACCOUNTS
           OPEN I-O ACCOUNTS

           PERFORM WRITE-OUTPUT USING "Enter password:"
           READ INPUTFILE AT END EXIT PARAGRAPH
              NOT AT END MOVE FUNCTION TRIM(INPUT-REC) TO PASSWORD
           END-READ

           PERFORM CHECK-PASSWORD
           IF PASSWORD-VALID = "Y"
              STRING USERNAME DELIMITED BY SIZE
                     " " DELIMITED BY SIZE
                     PASSWORD DELIMITED BY SIZE
                     INTO ACCT-REC
              WRITE ACCT-REC
              ADD 1 TO ACCT-COUNT
              PERFORM WRITE-OUTPUT USING "Account created, restart to login"
           ELSE
              PERFORM WRITE-OUTPUT USING "Password does not meet requirements"
           END-IF.
      * CREATE: ensures <5 accounts, username unique, password valid.

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

           IF HAS-UPPER = "Y"
              AND HAS-DIGIT = "Y"
              AND HAS-SPECIAL = "Y"
              MOVE "Y" TO PASSWORD-VALID
           END-IF.
      * Password must be 8–12 chars with at least 1 uppercase, 1 digit, 1 special.

       WRITE-OUTPUT SECTION.
       USING MSG.
           DISPLAY MSG
           MOVE MSG TO OUT-REC
           WRITE OUT-REC.
      * Utility to print to screen and also write to log file.
