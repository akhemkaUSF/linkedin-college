       IDENTIFICATION DIVISION.
       PROGRAM-ID. CONTROLFLOW.   *> PROGRAM-ID gives the program its name (CONTROLFLOW)

       ENVIRONMENT DIVISION. *> big section where we describe our machine environment (files, devices, terminals, etc)
       INPUT-OUTPUT SECTION. *> declare external files -- we tell COBOL what files exist and how we control them
       FILE-CONTROL. *> starts the list of file declarations. we write SELECT statements to connect logical names in our program to actual files on disk
           SELECT INPUTFILE ASSIGN TO "user_input" *> reads simulated user input (user_input.txt)
              ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUTPUTFILE ASSIGN TO "output_log.txt" *>writes logs (output_log.txt)
              ORGANIZATION IS LINE SEQUENTIAL. *> each record is a line of text
           SELECT ACCOUNTS ASSIGN TO "accounts.txt" *> stores accounts persistently (accounts.txt)
              ORGANIZATION IS LINE SEQUENTIAL
              FILE STATUS IS ACC-FS. *>gives us a way to check if opening the file succeeded
           SELECT PROFILE-FILE ASSIGN TO DYNAMIC WS-FILENAME
              ORGANIZATION IS LINE SEQUENTIAL
              FILE STATUS IS PROFILE-STATUS.
           SELECT CONNECTIONS ASSIGN TO "connections.txt"
              ORGANIZATION IS LINE SEQUENTIAL
              FILE STATUS IS CONN-FS.

           SELECT NETWORK ASSIGN TO "network.txt"
              ORGANIZATION IS LINE SEQUENTIAL
              FILE STATUS IS NET-FS.
           SELECT CONN-TMP ASSIGN TO "connections.tmp"
              ORGANIZATION IS LINE SEQUENTIAL
              FILE STATUS IS TMP-FS.

           SELECT PROFILES-INDEX ASSIGN TO "profiles.idx"
              ORGANIZATION IS LINE SEQUENTIAL
              FILE STATUS IS PRO-FS.

       DATA DIVISION *> we describe all the data the program can use -- the files, variables, and structure and size of each piece of data
       FILE SECTION. *> we're defining the files in this section
       FD  INPUTFILE. *> FD is a file description. Marks the start of a record layout for a file we declared earlier in the FILE-CONTROL section
       01  INPUT-REC            PIC X(100). *> defines type and size: alphanumeric, 100 characters long. we start the line 01 because it's one complete record

       FD  OUTPUTFILE. *> start of record layout for output file
       01  OUT-REC              PIC X(120). *> each line is alphanumeric, 100 characters

       FD  ACCOUNTS.
       01  ACCT-REC             PIC X(100). *>each line is alphanumeric, 100 characters

       FD  PROFILE-FILE.
       01  PF-REC              PIC X(512).

       FD  CONNECTIONS.
       01  CONN-REC            PIC X(100).

       FD  NETWORK.
       01  NET-REC            PIC X(100).

       FD  CONN-TMP.
       01  TMP-REC            PIC X(100).

       FD  PROFILES-INDEX.
       01  PRF-REC            PIC X(120).

       WORKING-STORAGE SECTION.
       77 VALID-YEAR PIC X VALUE "N". *> defines program variables in memory
       77  ACC-FS               PIC XX VALUE SPACES.  *> file status for ACCOUNTS. we use 77 because it's a standalone variable
       77  PROFILE-STATUS               PIC XX VALUE SPACES. 
       77  CONN-FS             PIC XX VALUE SPACES.
       77  NET-FS             PIC XX VALUE SPACES.
       77  TMP-FS             PIC XX VALUE SPACES.
       77  PRO-FS             PIC XX VALUE SPACES.

       77  FIRST-NAME           PIC X(50).
       77  LAST-NAME            PIC X(50).

       77  EOF-FLAG             PIC X  VALUE "N". *> END of File flag variable. "N" is the initial value (since we're not at the end of the file) 
       77  PROFILE-EOF          PIC X  VALUE "N".
       77  USERNAME             PIC X(20).
       77  PASSWORD             PIC X(20).
       77  VALID-LOGIN          PIC X  VALUE "N". *> password validation. doesn't actually becmoe "Y" until we validate the password
       77  ACCT-COUNT           PIC 9  VALUE 0. *> number of accounts accounts.txt
       77      OPTION-CHOICE         PIC 9  VALUE 0. *> option selection from the user
       77  MSG                  PIC X(150). 
       77  WS-TEMP    PIC X(10).
       77  FIELD-LEN PIC 9(4) VALUE ZERO.


       77  WS-FILENAME          PIC X(128).
       77  WS-FILENAME-SAVED   PIC X(128).
       77  WS-FIELD             PIC X(5000).
       77  IDX                  PIC 9  VALUE 1.
       *> Fields used when splitting an account line
       77  ACCT-USER            PIC X(20).
       77  ACCT-PASS            PIC X(20).

       *> Password validation helpers
       77  PASSWORD-LEN         PIC 99.
       77  I                    PIC 99. *> I is just an integer counter
       77  HAS-UPPER            PIC X  VALUE "N".
       77  HAS-DIGIT            PIC X  VALUE "N".
       77  HAS-SPECIAL          PIC X  VALUE "N".
       77  PASSWORD-VALID       PIC X  VALUE "N".

       *> Connections feature working storage
       77  TARGET-USER         PIC X(20).
       77  USER-FOUND          PIC X  VALUE "N".
       77  REQ-FOUND           PIC X  VALUE "N".
       77  PENDING-SENDER      PIC X(20).
       77  PENDING-RECIP       PIC X(20).

       77  ACCEPT-USER        PIC X(20).
       77  CANON-A            PIC X(20).
       77  CANON-B            PIC X(20).
       77  WS-DISPLAY-NAME     PIC X(50).
       77  RESP-CHAR           PIC X.

       77  OTHER-USER          PIC X(20).
       77  CONN-ANY            PIC X  VALUE "N".
       77  PARAM-USER          PIC X(20).

       77  LK-FIRST-NAME      PIC X(50).
       77  LK-LAST-NAME       PIC X(50).
       77  IDX-USER           PIC X(20).
       77  IDX-FN             PIC X(50).
       77  IDX-LN             PIC X(50).
       77  NAME-FOUND         PIC X  VALUE "N".

       PROCEDURE DIVISION. *> equivalent of the main function in other languages 
       MAIN-PARA. *> main entry point
           OPEN INPUT INPUTFILE *> opens input file
           OPEN OUTPUT OUTPUTFILE *> opens output file 

           *> Try opening ACCOUNTS for I-O; if it doesn't exist, create it
           OPEN I-O ACCOUNTS
           IF ACC-FS NOT = "00"
              CLOSE ACCOUNTS
              OPEN OUTPUT ACCOUNTS
              CLOSE ACCOUNTS
              OPEN I-O ACCOUNTS
           END-IF

           *> Try opening CONNECTIONS for I-O; if it doesn't exist, create it
           OPEN I-O CONNECTIONS
           IF CONN-FS NOT = "00"
              CLOSE CONNECTIONS
              OPEN OUTPUT CONNECTIONS
              CLOSE CONNECTIONS
              OPEN I-O CONNECTIONS
           END-IF

           *> Ensure NETWORK file exists
           OPEN I-O NETWORK
           IF NET-FS NOT = "00"
              CLOSE NETWORK
              OPEN OUTPUT NETWORK
              CLOSE NETWORK
              OPEN I-O NETWORK
           END-IF

           *> Ensure CONN-TMP is closed/clean (will be created on demand)
           CLOSE CONN-TMP

           *> Ensure PROFILES-INDEX file exists
           OPEN I-O PROFILES-INDEX
           IF PRO-FS NOT = "00"
              CLOSE PROFILES-INDEX
              OPEN OUTPUT PROFILES-INDEX
              CLOSE PROFILES-INDEX
              OPEN I-O PROFILES-INDEX
           END-IF
           CLOSE PROFILES-INDEX

           PERFORM LOAD-ACCOUNTS *> count number of accounts in the file

           PERFORM UNTIL EOF-FLAG = "Y"
               PERFORM PROCESS-COMMAND
           END-PERFORM

           *> close all the files once we're done
           CLOSE INPUTFILE 
           CLOSE OUTPUTFILE
           CLOSE ACCOUNTS
           STOP RUN.

       
       *> initial function where we acctually load the accounts file 
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

       *> entry point into login/create account control flow
       PROCESS-COMMAND.
    *> Print menu options
           MOVE "What would you like to do? Type LOGIN to sign in. Type CREATE to make a new account." TO MSG
           PERFORM WRITE-OUTPUT
       
           *> Now read the actual choice from input
           READ INPUTFILE
               AT END
                   MOVE "Y" TO EOF-FLAG
                   EXIT PARAGRAPH
               NOT AT END
                   MOVE FUNCTION TRIM(INPUT-REC) TO MSG
           END-READ

           IF MSG = "LOGIN"
               PERFORM DO-LOGIN
           ELSE
               IF MSG = "CREATE"
                   PERFORM DO-CREATE
               ELSE
                IF MSG = "STARTOVER"
                       PERFORM CLEAR-ALL-FILES
                       EXIT PARAGRAPH
                ELSE 
                    MOVE "Invalid Input" to MSG
                    PERFORM WRITE-OUTPUT
           END-IF
           END-IF
           END-IF.



       *> Clear core persistent text files when user types STARTOVER (no registry)
       CLEAR-ALL-FILES.
           *> ACCOUNTS
           CLOSE ACCOUNTS
           OPEN OUTPUT ACCOUNTS
           IF ACC-FS = "00"
              CLOSE ACCOUNTS
              OPEN I-O ACCOUNTS
           END-IF

           *> CONNECTIONS
           CLOSE CONNECTIONS
           OPEN OUTPUT CONNECTIONS
           IF CONN-FS = "00"
              CLOSE CONNECTIONS
              OPEN I-O CONNECTIONS
           END-IF

           *> NETWORK
           CLOSE NETWORK
           OPEN OUTPUT NETWORK
           IF NET-FS = "00"
              CLOSE NETWORK
              OPEN I-O NETWORK
           END-IF

           *> Reset derived in-memory counters/flags
           MOVE 0 TO ACCT-COUNT

           *> PROFILES-INDEX
           CLOSE PROFILES-INDEX
           OPEN OUTPUT PROFILES-INDEX
           IF PRO-FS = "00"
              CLOSE PROFILES-INDEX
           END-IF

           MOVE "All data cleared (accounts, connections, network)." TO MSG
           PERFORM WRITE-OUTPUT
           EXIT PARAGRAPH.

       DO-LOGIN.
           MOVE "Enter username:" TO MSG
           PERFORM WRITE-OUTPUT *> write output to file
           READ INPUTFILE AT END EXIT PARAGRAPH *> read the next line, exit if we're at the end
              NOT AT END MOVE FUNCTION TRIM(INPUT-REC) TO USERNAME *> store the next line in username
           END-READ

           MOVE "Enter password:" TO MSG 
           PERFORM WRITE-OUTPUT *> write password to file
           READ INPUTFILE AT END EXIT PARAGRAPH
              NOT AT END MOVE FUNCTION TRIM(INPUT-REC) TO PASSWORD
           END-READ

           PERFORM CHECK-CREDENTIALS *> validate the credentials and act accordingly
           IF VALID-LOGIN = "Y" 
              MOVE "You have successfully logged in" TO MSG
              PERFORM WRITE-OUTPUT
              PERFORM USER-MENU *> move onto the user menu with three main options
           ELSE
              MOVE "Incorrect username/password, please try again" TO MSG
              PERFORM WRITE-OUTPUT
           END-IF.

       CHECK-CREDENTIALS. *> make sure that the username and password are in the accounts file
           MOVE "N" TO VALID-LOGIN 
           OPEN INPUT ACCOUNTS *> open the accounts file
           PERFORM UNTIL 1=0 *> infinite loop --> 1 will never equal 0
              READ ACCOUNTS NEXT RECORD
                 AT END EXIT PERFORM *> exit once we're at the end of the file 
                 NOT AT END
                    UNSTRING ACCT-REC
                       DELIMITED BY ALL " "
                       INTO ACCT-USER ACCT-PASS
                    END-UNSTRING
                    *> check if username and password match any of the accounts in account.txt
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
        *> three options 
           MOVE "Choose: 1=Search job, 2=Learn skill, 3=Create/Edit My Profile, 4=Output Profile, 5=Search Profile, 6=Return, 7=View Pending Requests" TO MSG
           *> print out the contents of MSG
           PERFORM WRITE-OUTPUT
           *> whatever number we select is the option we want 
           READ INPUTFILE AT END EXIT PARAGRAPH
              NOT AT END MOVE FUNCTION NUMVAL(INPUT-REC) TO OPTION-CHOICE
           END-READ
           MOVE OPTION-CHOICE TO MSG 
           PERFORM WRITE-OUTPUT
           *> function to evaluate the option they choose
           EVALUATE OPTION-CHOICE *> we use the same value for both the overaching options and the skills. no reason to store both at the same time
              WHEN 1
                 MOVE "Under Construction" TO MSG
                 PERFORM WRITE-OUTPUT

              *> add an option prompting them to find someone they know,
              *> and then perform the search function

              WHEN 2
              *> lists the skills we actually want to select
                 MOVE "Pick a skill (1-5)" TO MSG
                 MOVE "1. COBOL Basics" TO MSG
                 PERFORM WRITE-OUTPUT
                 MOVE "2. File Handling" TO MSG
                 PERFORM WRITE-OUTPUT
                 MOVE "3. Data Validation" TO MSG
                 PERFORM WRITE-OUTPUT
                 MOVE "4. Debugging Techniques" TO MSG
                 PERFORM WRITE-OUTPUT
                 MOVE "5. System Integration" TO MSG
                 PERFORM WRITE-OUTPUT

                 READ INPUTFILE AT END EXIT PARAGRAPH
                    NOT AT END MOVE FUNCTION NUMVAL(INPUT-REC) TO OPTION-CHOICE
                 END-READ
                 MOVE "Under Construction" TO MSG
                 PERFORM WRITE-OUTPUT
              WHEN 3
               PERFORM DO-PROFILE
            WHEN 4
               MOVE 'N' TO PROFILE-EOF
               PERFORM SET-MY-PROFILE-FILENAME
               OPEN INPUT PROFILE-FILE
               IF PROFILE-STATUS NOT = "00"
                  MOVE "PROFILE FILE DOES NOT EXIST." TO MSG
                  PERFORM WRITE-OUTPUT
                  PERFORM USER-MENU
               ELSE
                   MOVE "OUTPUTTING PROFILE..." TO MSG 
                   PERFORM WRITE-OUTPUT
                   PERFORM UNTIL PROFILE-EOF = "Y"
                       PERFORM PRINT-PROFILE
                   END-PERFORM
                   MOVE "Connections:" TO MSG
                   PERFORM WRITE-OUTPUT
                   PERFORM LIST-MY-CONNECTIONS
                   CLOSE PROFILE-FILE
                   PERFORM USER-MENU
               END-IF
            WHEN 5
                 PERFORM SEARCH-PROFILE
                 PERFORM USER-MENU
              WHEN 7
                 PERFORM LIST-PENDING-REQUESTS
                 PERFORM USER-MENU
              WHEN 6
                 EXIT PARAGRAPH
              WHEN OTHER
                 MOVE "Invalid option, you must select a number 1-6" TO MSG
                 PERFORM WRITE-OUTPUT
           END-EVALUATE.

       *> create account function
       DO-CREATE.
           *>too many accounts created 
           IF ACCT-COUNT >= 5
              MOVE "All permitted accounts created, come back later" TO MSG
              PERFORM WRITE-OUTPUT
              EXIT PARAGRAPH *> go back to where this function was called from in process command
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

           PERFORM CHECK-PASSWORD *>perform password validation
           IF PASSWORD-VALID = "Y" 
              MOVE SPACES TO ACCT-REC
              STRING FUNCTION TRIM(USERNAME) DELIMITED BY SIZE
                     " "                     DELIMITED BY SIZE
                     FUNCTION TRIM(PASSWORD) DELIMITED BY SIZE
                     INTO ACCT-REC
              END-STRING

              *> Append new record
              OPEN EXTEND ACCOUNTS *>add the new username and password to the accounts.txt file
              WRITE ACCT-REC
              CLOSE ACCOUNTS

              ADD 1 TO ACCT-COUNT *> increment the account count
              MOVE "Account created successfully. Please select LOGIN from the menu to sign in." TO MSG
              PERFORM WRITE-OUTPUT
              EXIT PARAGRAPH *> return to PROCESS-COMMAND, then MAIN-PARA will show the menu again
           ELSE
              MOVE "Password does not meet requirements" TO MSG
              PERFORM WRITE-OUTPUT
           END-IF.

       CHECK-PASSWORD. *> perform password validation
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
       DO-PROFILE.
           *> First Name (required) - collect before creating file
           MOVE "Enter First Name:" TO MSG
           PERFORM WRITE-OUTPUT
           MOVE SPACES TO WS-FIELD
           PERFORM UNTIL WS-FIELD NOT = SPACES
              READ INPUTFILE AT END MOVE SPACES TO WS-FIELD
                 NOT AT END MOVE INPUT-REC TO WS-FIELD
              END-READ
              IF WS-FIELD = SPACES
                 MOVE "First Name is required. Please re-enter:" TO MSG
                 PERFORM WRITE-OUTPUT
              END-IF
           END-PERFORM
           MOVE FUNCTION TRIM(WS-FIELD) TO FIRST-NAME

           *> Last Name (required) - collect before creating file
           MOVE "Enter Last Name:" TO MSG
           PERFORM WRITE-OUTPUT
           MOVE SPACES TO WS-FIELD
           PERFORM UNTIL WS-FIELD NOT = SPACES
              READ INPUTFILE AT END MOVE SPACES TO WS-FIELD
                 NOT AT END MOVE INPUT-REC TO WS-FIELD
              END-READ
              IF WS-FIELD = SPACES
                 MOVE "Last Name is required. Please re-enter:" TO MSG
                 PERFORM WRITE-OUTPUT
              END-IF
           END-PERFORM
           MOVE FUNCTION TRIM(WS-FIELD) TO LAST-NAME

           *> Build filename using First_Last format
           MOVE SPACES TO WS-FILENAME
           STRING FUNCTION TRIM(FIRST-NAME) DELIMITED BY SIZE
                  "_"                       DELIMITED BY SIZE
                  FUNCTION TRIM(LAST-NAME)  DELIMITED BY SIZE
                  INTO WS-FILENAME
           END-STRING

           *> Clear the old profile file
           OPEN OUTPUT PROFILE-FILE
           CLOSE PROFILE-FILE

           *> Update profiles index (username -> first/last)
           MOVE FUNCTION TRIM(FIRST-NAME) TO LK-FIRST-NAME
           MOVE FUNCTION TRIM(LAST-NAME)  TO LK-LAST-NAME
           INSPECT LK-FIRST-NAME REPLACING ALL " " BY "_"
           INSPECT LK-LAST-NAME  REPLACING ALL " " BY "_"
           OPEN EXTEND PROFILES-INDEX
           MOVE SPACES TO PRF-REC
           STRING FUNCTION TRIM(USERNAME)     DELIMITED BY SIZE
                  " "                         DELIMITED BY SIZE
                  FUNCTION TRIM(LK-FIRST-NAME) DELIMITED BY SIZE
                  " "                         DELIMITED BY SIZE
                  FUNCTION TRIM(LK-LAST-NAME)  DELIMITED BY SIZE
                  INTO PRF-REC
           END-STRING
           WRITE PRF-REC
           CLOSE PROFILES-INDEX

           *> Now reopen for writing fresh data
           OPEN OUTPUT PROFILE-FILE

           *> Write First Name to file (already collected)
           MOVE SPACES TO PF-REC
           STRING "First Name: " DELIMITED BY SIZE
                  FIRST-NAME     DELIMITED BY SIZE
                  INTO PF-REC
           WRITE PF-REC

           *> Write Last Name to file (already collected)
           MOVE SPACES TO PF-REC
           STRING "Last Name: " DELIMITED BY SIZE
                  LAST-NAME     DELIMITED BY SIZE
                  INTO PF-REC
           WRITE PF-REC

           *> Write Username (link profile to account)
           MOVE SPACES TO PF-REC
           STRING "Username: " DELIMITED BY SIZE
                  USERNAME      DELIMITED BY SIZE
                  INTO PF-REC
           WRITE PF-REC

           *> Continue with the rest of the profile fields...
           *> University/College (required)
           MOVE "Enter University/College:" TO MSG
           PERFORM WRITE-OUTPUT
           MOVE SPACES TO WS-FIELD
           PERFORM UNTIL WS-FIELD NOT = SPACES
              READ INPUTFILE AT END MOVE SPACES TO WS-FIELD
                 NOT AT END MOVE INPUT-REC TO WS-FIELD
              END-READ
              IF WS-FIELD = SPACES
                 MOVE "University/College is required. Please re-enter:" TO MSG
                 PERFORM WRITE-OUTPUT
              END-IF
           END-PERFORM
           MOVE SPACES TO PF-REC
           STRING "University/College: " DELIMITED BY SIZE
                  WS-FIELD             DELIMITED BY SIZE
                  INTO PF-REC
           WRITE PF-REC

           *> Major (required)
           MOVE "Enter Major:" TO MSG
           PERFORM WRITE-OUTPUT
           MOVE SPACES TO WS-FIELD
           PERFORM UNTIL WS-FIELD NOT = SPACES
              READ INPUTFILE AT END MOVE SPACES TO WS-FIELD
                 NOT AT END MOVE INPUT-REC TO WS-FIELD
              END-READ
              IF WS-FIELD = SPACES
                 MOVE "Major is required. Please re-enter:" TO MSG
                 PERFORM WRITE-OUTPUT
              END-IF
           END-PERFORM
           MOVE SPACES TO PF-REC
           STRING "Major: " DELIMITED BY SIZE
                  WS-FIELD  DELIMITED BY SIZE
                  INTO PF-REC
           WRITE PF-REC

           *> Graduation Year (required)
           MOVE "Enter Graduation Year (YYYY):" TO MSG
           PERFORM WRITE-OUTPUT
           MOVE SPACES TO WS-FIELD
           MOVE "N" TO VALID-YEAR
           PERFORM UNTIL VALID-YEAR = "Y"
              READ INPUTFILE AT END MOVE SPACES TO WS-FIELD
                 NOT AT END MOVE INPUT-REC TO WS-FIELD
              END-READ
              MOVE FUNCTION LENGTH(FUNCTION TRIM(WS-FIELD TRAILING)) TO FIELD-LEN
              MOVE FUNCTION TRIM(WS-FIELD TRAILING) TO WS-TEMP
              IF FUNCTION NUMVAL(WS-TEMP) > 0
                 AND FIELD-LEN = 4
                 AND (WS-FIELD(1:2) = "19" OR WS-FIELD(1:2) = "20")
                 MOVE "Y" TO VALID-YEAR
              ELSE
                 MOVE "Graduation year must be a 4-digit year starting with 19 or 20. Please re-enter:" TO MSG
                 PERFORM WRITE-OUTPUT
                 MOVE "N" TO VALID-YEAR
              END-IF
           END-PERFORM
           MOVE SPACES TO PF-REC
           STRING "Graduation Year: " DELIMITED BY SIZE
                  WS-FIELD          DELIMITED BY SIZE
                  INTO PF-REC
           WRITE PF-REC

           *> About Me (optional, write even if blank)
           MOVE "Enter About Me (optional, blank to skip):" TO MSG
           PERFORM WRITE-OUTPUT
           READ INPUTFILE AT END MOVE SPACES TO WS-FIELD
              NOT AT END MOVE INPUT-REC TO WS-FIELD
           END-READ
           MOVE SPACES TO PF-REC
           STRING "About Me: " DELIMITED BY SIZE
                  WS-FIELD   DELIMITED BY SIZE
                  INTO PF-REC
           WRITE PF-REC

           *> Experience (3 entries, always write 4 labeled lines each)
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 3
              MOVE "Enter Experience Title (blank to skip):" TO MSG
              PERFORM WRITE-OUTPUT
              READ INPUTFILE AT END MOVE SPACES TO WS-FIELD
                 NOT AT END MOVE INPUT-REC TO WS-FIELD
              END-READ
              MOVE SPACES TO PF-REC
              STRING "Experience Title: " DELIMITED BY SIZE
                     WS-FIELD          DELIMITED BY SIZE
                     INTO PF-REC
              WRITE PF-REC

              MOVE "Enter Company:" TO MSG
              PERFORM WRITE-OUTPUT
              READ INPUTFILE AT END MOVE SPACES TO WS-FIELD
                 NOT AT END MOVE INPUT-REC TO WS-FIELD
              END-READ
              MOVE SPACES TO PF-REC
              STRING "Company: " DELIMITED BY SIZE
                     WS-FIELD  DELIMITED BY SIZE
                     INTO PF-REC
              WRITE PF-REC

              MOVE "Enter Dates:" TO MSG
              PERFORM WRITE-OUTPUT
              READ INPUTFILE AT END MOVE SPACES TO WS-FIELD
                 NOT AT END MOVE INPUT-REC TO WS-FIELD
              END-READ
              MOVE SPACES TO PF-REC
              STRING "Dates: " DELIMITED BY SIZE
                     WS-FIELD DELIMITED BY SIZE
                     INTO PF-REC
              WRITE PF-REC

              MOVE "Enter Description (optional):" TO MSG
              PERFORM WRITE-OUTPUT
              READ INPUTFILE AT END MOVE SPACES TO WS-FIELD
                 NOT AT END MOVE INPUT-REC TO WS-FIELD
              END-READ
              MOVE SPACES TO PF-REC
              STRING "Description: " DELIMITED BY SIZE
                     WS-FIELD     DELIMITED BY SIZE
                     INTO PF-REC
              WRITE PF-REC
           END-PERFORM

           *> Education (3 entries, always write 3 labeled lines each)
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 3
              MOVE "Enter Education Degree (blank to skip):" TO MSG
              PERFORM WRITE-OUTPUT
              READ INPUTFILE AT END MOVE SPACES TO WS-FIELD
                 NOT AT END MOVE INPUT-REC TO WS-FIELD
              END-READ
              MOVE SPACES TO PF-REC
              STRING "Education Degree: " DELIMITED BY SIZE
                     WS-FIELD          DELIMITED BY SIZE
                     INTO PF-REC
              WRITE PF-REC

              MOVE "Enter University:" TO MSG
              PERFORM WRITE-OUTPUT
              READ INPUTFILE AT END MOVE SPACES TO WS-FIELD
                 NOT AT END MOVE INPUT-REC TO WS-FIELD
              END-READ
              MOVE SPACES TO PF-REC
              STRING "Education University: " DELIMITED BY SIZE
                     WS-FIELD              DELIMITED BY SIZE
                     INTO PF-REC
              WRITE PF-REC

              MOVE "Enter Years Attended:" TO MSG
              PERFORM WRITE-OUTPUT
              READ INPUTFILE AT END MOVE SPACES TO WS-FIELD
                 NOT AT END MOVE INPUT-REC TO WS-FIELD
              END-READ
              MOVE SPACES TO PF-REC
              STRING "Years Attended: " DELIMITED BY SIZE
                     WS-FIELD          DELIMITED BY SIZE
                     INTO PF-REC
              WRITE PF-REC
           END-PERFORM

           CLOSE PROFILE-FILE
           MOVE "Profile saved successfully." TO MSG
           PERFORM WRITE-OUTPUT
           PERFORM USER-MENU
           EXIT PARAGRAPH.

       PRINT-PROFILE.
           READ PROFILE-FILE
              AT END
                  MOVE "Y" TO PROFILE-EOF
              NOT AT END
                  IF PF-REC(1:9) = "Username:"
                     CONTINUE
                  ELSE
                     MOVE FUNCTION TRIM(PF-REC) TO MSG
                     PERFORM WRITE-OUTPUT
                  END-IF
           END-READ.

       *> add a function to search for the profile 
       *> check if the profile file exists that we're searching for 
       *> if the profiel exists, you just move that value to the variable
       *> profile-file and you perform print-profile

       WRITE-OUTPUT.
           MOVE MSG TO OUT-REC
           WRITE OUT-REC
           DISPLAY MSG.

             *> Add a new menu option for searching profiles

       *> Module to search for a profile by name
       SEARCH-PROFILE.
           MOVE "Enter the full name of the person you are looking for:" TO MSG
           PERFORM WRITE-OUTPUT
           READ INPUTFILE AT END EXIT PARAGRAPH
              NOT AT END MOVE FUNCTION TRIM(INPUT-REC) TO WS-FIELD
           END-READ

           MOVE "Searching for profile..." TO MSG
           PERFORM WRITE-OUTPUT
           MOVE WS-FILENAME TO WS-FILENAME-SAVED

           *> Convert the input name to filename format (First_Last)
           *> Replace spaces with underscores for filename
           MOVE SPACES TO WS-FILENAME
           MOVE WS-FIELD TO MSG
           PERFORM WRITE-OUTPUT

           MOVE FUNCTION TRIM(WS-FIELD TRAILING) TO WS-FILENAME
           INSPECT WS-FILENAME
               REPLACING FIRST " " BY "_"
           MOVE WS-FILENAME TO MSG

           *> Try to open the profile file directly using the converted filename
           MOVE 'N' TO PROFILE-EOF
           OPEN INPUT PROFILE-FILE
           IF PROFILE-STATUS = "00"
              MOVE "Profile found. Displaying profile:" TO MSG
              PERFORM WRITE-OUTPUT
              PERFORM UNTIL PROFILE-EOF = "Y"
                  PERFORM PRINT-PROFILE
              END-PERFORM
              CLOSE PROFILE-FILE
              *> Attempt to extract target username from the profile file content
              MOVE SPACES TO TARGET-USER
              OPEN INPUT PROFILE-FILE
              PERFORM UNTIL 1 = 0
                 READ PROFILE-FILE NEXT RECORD
                    AT END EXIT PERFORM
                    NOT AT END
                       IF PF-REC(1:9) = "Username:"
                          *> Value starts after "Username: " (position 11)
                          MOVE FUNCTION TRIM(PF-REC(11:100)) TO TARGET-USER
                          EXIT PERFORM
                       END-IF
                 END-READ
              END-PERFORM
              CLOSE PROFILE-FILE
              *> Show this profile's connections (always display the header)
              MOVE "Connections:" TO MSG
              PERFORM WRITE-OUTPUT
              MOVE TARGET-USER TO PARAM-USER
              PERFORM LIST-CONNECTIONS-FOR-USER
              MOVE "Send a connection request to this user? (Y/N):" TO MSG
              PERFORM WRITE-OUTPUT
              READ INPUTFILE AT END MOVE SPACE TO RESP-CHAR
                 NOT AT END MOVE FUNCTION TRIM(INPUT-REC)(1:1) TO RESP-CHAR
              END-READ
              IF RESP-CHAR = "Y" OR RESP-CHAR = "y"
                 IF TARGET-USER = SPACES
                    *> Fallback: derive from full name if profile has no Username line
                    PERFORM MAKE-USERNAME-FROM-FULLNAME
                 END-IF
                 PERFORM SEND-CONNECTION-REQUEST-DIRECT
              END-IF
              MOVE WS-FILENAME-SAVED TO WS-FILENAME
           ELSE
              MOVE "Profile not found." TO MSG
              PERFORM WRITE-OUTPUT
              MOVE WS-FILENAME-SAVED TO WS-FILENAME
           END-IF.
       

*> ================= Connections Feature =================
*> Public entry: prompt for a username and attempt to send a request
       SEND-CONNECTION-REQUEST.
           MOVE "Enter the username you want to connect with:" TO MSG
           PERFORM WRITE-OUTPUT
           READ INPUTFILE AT END EXIT PARAGRAPH
              NOT AT END MOVE FUNCTION TRIM(INPUT-REC) TO TARGET-USER
           END-READ
           PERFORM SEND-CONNECTION-REQUEST-DIRECT.

*> Entry that uses TARGET-USER directly (expects it to be set)
      SEND-CONNECTION-REQUEST-DIRECT.
          IF TARGET-USER = SPACES
             MOVE "No username provided." TO MSG
             PERFORM WRITE-OUTPUT
             EXIT PARAGRAPH
          END-IF

          *> prevent self-requests
          IF FUNCTION TRIM(TARGET-USER) = FUNCTION TRIM(USERNAME)
             MOVE "You cannot send a connection request to yourself." TO MSG
             PERFORM WRITE-OUTPUT
             EXIT PARAGRAPH
          END-IF

           *> validate that target user exists in ACCOUNTS
           MOVE "N" TO USER-FOUND
           OPEN INPUT ACCOUNTS
           PERFORM UNTIL 1 = 0
              READ ACCOUNTS NEXT RECORD
                 AT END EXIT PERFORM
                 NOT AT END
                    UNSTRING ACCT-REC
                       DELIMITED BY ALL " "
                       INTO ACCT-USER ACCT-PASS
                    END-UNSTRING
                    IF FUNCTION TRIM(ACCT-USER) = FUNCTION TRIM(TARGET-USER)
                       MOVE "Y" TO USER-FOUND
                       EXIT PERFORM
                    END-IF
              END-READ
           END-PERFORM
           CLOSE ACCOUNTS
           OPEN I-O ACCOUNTS

           IF USER-FOUND NOT = "Y"
              MOVE "Recipient account not found. Request not sent." TO MSG
              PERFORM WRITE-OUTPUT
              EXIT PARAGRAPH
           END-IF

           *> prevent sending if already connected
           MOVE FUNCTION TRIM(USERNAME)    TO CANON-A
           MOVE FUNCTION TRIM(TARGET-USER) TO CANON-B
           PERFORM IS-CONNECTED
           IF REQ-FOUND = "Y"
              MOVE "You are already connected with this user." TO MSG
              PERFORM WRITE-OUTPUT
              EXIT PARAGRAPH
           END-IF

           *> check for duplicate pending request (sender -> recipient)
           MOVE "N" TO REQ-FOUND
           MOVE FUNCTION TRIM(USERNAME)     TO PENDING-SENDER
           MOVE FUNCTION TRIM(TARGET-USER)  TO PENDING-RECIP
           PERFORM FIND-PENDING
           IF REQ-FOUND = "Y"
              MOVE "You have already sent a pending request to this user." TO MSG
              PERFORM WRITE-OUTPUT
              EXIT PARAGRAPH
           END-IF

           *> check for reverse pending (recipient already requested you)
           MOVE "N" TO REQ-FOUND
           MOVE FUNCTION TRIM(TARGET-USER)  TO PENDING-SENDER
           MOVE FUNCTION TRIM(USERNAME)     TO PENDING-RECIP
           PERFORM FIND-PENDING
           IF REQ-FOUND = "Y"
              MOVE "This user has already sent you a connection request (pending)." TO MSG
              PERFORM WRITE-OUTPUT
              EXIT PARAGRAPH
           END-IF

           *> append the new request
           MOVE SPACES TO CONN-REC
           STRING FUNCTION TRIM(USERNAME) DELIMITED BY SIZE
                  " "                  DELIMITED BY SIZE
                  FUNCTION TRIM(TARGET-USER) DELIMITED BY SIZE
                  INTO CONN-REC
           END-STRING
           OPEN EXTEND CONNECTIONS
           WRITE CONN-REC
           CLOSE CONNECTIONS

           MOVE "Connection request sent." TO MSG
           PERFORM WRITE-OUTPUT
           EXIT PARAGRAPH.

*> List all pending requests where current user is the recipient
       LIST-PENDING-REQUESTS.
           MOVE "Your pending connection requests:" TO MSG
           PERFORM WRITE-OUTPUT
           OPEN INPUT CONNECTIONS
           MOVE "N" TO REQ-FOUND
           PERFORM UNTIL 1 = 0
              READ CONNECTIONS NEXT RECORD
                 AT END EXIT PERFORM
                 NOT AT END
                    UNSTRING CONN-REC
                       DELIMITED BY ALL " "
                       INTO PENDING-SENDER PENDING-RECIP
                    END-UNSTRING
                    IF FUNCTION TRIM(PENDING-RECIP) = FUNCTION TRIM(USERNAME)
                       MOVE "Y" TO REQ-FOUND
                       *> Build a friendlier display name from username (e.g., JohnSmith -> John Smith)
                       MOVE SPACES TO WS-DISPLAY-NAME
                       PERFORM MAKE-DISPLAY-NAME
                       MOVE SPACES TO MSG
                       STRING "Request from: " DELIMITED BY SIZE
                              FUNCTION TRIM(WS-DISPLAY-NAME) DELIMITED BY SIZE
                              INTO MSG
                       END-STRING
                       PERFORM WRITE-OUTPUT

                       MOVE "Accept this request?  Y = Yes,  N = No,  Enter = Finish" TO MSG
                       PERFORM WRITE-OUTPUT
                       READ INPUTFILE AT END MOVE SPACE TO RESP-CHAR
                          NOT AT END MOVE FUNCTION TRIM(INPUT-REC)(1:1) TO RESP-CHAR
                       END-READ
                       IF RESP-CHAR = "Y" OR RESP-CHAR = "y"
                          *> Accept this single request (safe sequence)
                          MOVE FUNCTION TRIM(PENDING-SENDER) TO PENDING-SENDER
                          MOVE FUNCTION TRIM(USERNAME)       TO PENDING-RECIP
                          CLOSE CONNECTIONS
                          PERFORM ACCEPT-REQUEST-DIRECT
                          OPEN INPUT CONNECTIONS
                       ELSE
                          IF RESP-CHAR = SPACE
                             *> Enter ends the review immediately
                             EXIT PERFORM
                          ELSE
                             *> 'N' or 'n' skips to the next request
                             CONTINUE
                          END-IF
                       END-IF
                    END-IF
              END-READ
           END-PERFORM
           CLOSE CONNECTIONS
           IF REQ-FOUND NOT = "Y"
              MOVE "(none)" TO MSG
              PERFORM WRITE-OUTPUT
           END-IF
           EXIT PARAGRAPH.
       *> Convert a username like JohnSmith or John_Smith to a display name "John Smith"
       MAKE-DISPLAY-NAME.
           MOVE 1 TO I
           MOVE 1 TO FIELD-LEN
           MOVE SPACES TO WS-DISPLAY-NAME
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > FUNCTION LENGTH(FUNCTION TRIM(PENDING-SENDER))
              EVALUATE TRUE
                 WHEN PENDING-SENDER(I:1) = "_"
                    MOVE " " TO WS-DISPLAY-NAME(FIELD-LEN:1)
                    ADD 1 TO FIELD-LEN
                 WHEN I > 1 AND PENDING-SENDER(I:1) >= "A" AND PENDING-SENDER(I:1) <= "Z"
                    IF WS-DISPLAY-NAME(FIELD-LEN - 1:1) NOT = " "
                       MOVE " " TO WS-DISPLAY-NAME(FIELD-LEN:1)
                       ADD 1 TO FIELD-LEN
                    END-IF
                    MOVE PENDING-SENDER(I:1) TO WS-DISPLAY-NAME(FIELD-LEN:1)
                    ADD 1 TO FIELD-LEN
                 WHEN OTHER
                    MOVE PENDING-SENDER(I:1) TO WS-DISPLAY-NAME(FIELD-LEN:1)
                    ADD 1 TO FIELD-LEN
              END-EVALUATE
           END-PERFORM
           EXIT PARAGRAPH.

      GET-DISPLAY-NAME-FOR-OTHER.
          *> Default to heuristic formatting from username
          MOVE "N" TO NAME-FOUND
          MOVE SPACES TO WS-DISPLAY-NAME
          MOVE OTHER-USER TO PENDING-SENDER
          PERFORM MAKE-DISPLAY-NAME

          *> Try profiles index for an authoritative full name
          OPEN INPUT PROFILES-INDEX
          IF PRO-FS = "00"
             PERFORM UNTIL 1 = 0
                READ PROFILES-INDEX NEXT RECORD
                   AT END EXIT PERFORM
                   NOT AT END
                      UNSTRING PRF-REC
                         DELIMITED BY ALL " "
                         INTO IDX-USER IDX-FN IDX-LN
                      END-UNSTRING
                      IF FUNCTION TRIM(IDX-USER) = FUNCTION TRIM(OTHER-USER)
                         MOVE "Y" TO NAME-FOUND
                         MOVE IDX-FN TO LK-FIRST-NAME
                         MOVE IDX-LN TO LK-LAST-NAME
                         EXIT PERFORM
                      END-IF
                END-READ
             END-PERFORM
          END-IF
          CLOSE PROFILES-INDEX

          IF NAME-FOUND = "Y"
             INSPECT LK-FIRST-NAME REPLACING ALL "_" BY " "
             INSPECT LK-LAST-NAME  REPLACING ALL "_" BY " "
             MOVE SPACES TO WS-DISPLAY-NAME
             STRING FUNCTION TRIM(LK-FIRST-NAME) DELIMITED BY SIZE
                    " "                          DELIMITED BY SIZE
                    FUNCTION TRIM(LK-LAST-NAME)  DELIMITED BY SIZE
                    INTO WS-DISPLAY-NAME
             END-STRING
          END-IF
          EXIT PARAGRAPH.

      *> Build TARGET-USER from WS-FIELD by removing spaces and underscores
      MAKE-USERNAME-FROM-FULLNAME.
          MOVE SPACES TO TARGET-USER
          MOVE 1 TO I
          MOVE 1 TO FIELD-LEN
          PERFORM VARYING I FROM 1 BY 1 UNTIL I > FUNCTION LENGTH(FUNCTION TRIM(WS-FIELD))
             EVALUATE TRUE
                WHEN WS-FIELD(I:1) = " " OR WS-FIELD(I:1) = "_"
                   CONTINUE
                WHEN OTHER
                   MOVE WS-FIELD(I:1) TO TARGET-USER(FIELD-LEN:1)
                   ADD 1 TO FIELD-LEN
             END-EVALUATE
          END-PERFORM
          EXIT PARAGRAPH.

      SET-MY-PROFILE-FILENAME.
          MOVE SPACES TO WS-FILENAME
          STRING FUNCTION TRIM(FIRST-NAME) DELIMITED BY SIZE
                 "_"                       DELIMITED BY SIZE
                 FUNCTION TRIM(LAST-NAME)  DELIMITED BY SIZE
                 INTO WS-FILENAME
          END-STRING
          EXIT PARAGRAPH.

*> Helper: find a pending request (SENDER -> RECIPIENT). Uses PENDING-SENDER/PENDING-RECIP and sets REQ-FOUND to "Y" if found.
       FIND-PENDING.
           OPEN INPUT CONNECTIONS
           MOVE "N" TO REQ-FOUND
           PERFORM UNTIL 1 = 0
              READ CONNECTIONS NEXT RECORD
                 AT END EXIT PERFORM
                 NOT AT END
                    UNSTRING CONN-REC
                       DELIMITED BY ALL " "
                       INTO ACCT-USER ACCT-PASS
                    END-UNSTRING
                    *> Reuse ACCT-USER/ACCT-PASS fields as temp sender/recipient holders
                    IF FUNCTION TRIM(ACCT-USER) = FUNCTION TRIM(PENDING-SENDER)
                       AND FUNCTION TRIM(ACCT-PASS) = FUNCTION TRIM(PENDING-RECIP)
                       MOVE "Y" TO REQ-FOUND
                       EXIT PERFORM
                    END-IF
              END-READ
           END-PERFORM
           CLOSE CONNECTIONS
           EXIT PARAGRAPH.

*> Check whether two users are already connected (canonical pair search).
*> Inputs: CANON-A, CANON-B; Output: REQ-FOUND = "Y" if connected
       IS-CONNECTED.
           MOVE "N" TO REQ-FOUND
           MOVE FUNCTION TRIM(CANON-A) TO CANON-A
           MOVE FUNCTION TRIM(CANON-B) TO CANON-B
           *> order lexicographically into CANON-A <= CANON-B
           IF CANON-A > CANON-B
              MOVE CANON-A TO WS-TEMP
              MOVE CANON-B TO CANON-A
              MOVE WS-TEMP TO CANON-B
           END-IF
           OPEN INPUT NETWORK
           PERFORM UNTIL 1 = 0
              READ NETWORK NEXT RECORD
                 AT END EXIT PERFORM
                 NOT AT END
                    UNSTRING NET-REC DELIMITED BY ALL " "
                       INTO ACCT-USER ACCT-PASS
                    END-UNSTRING
                    IF FUNCTION TRIM(ACCT-USER) = CANON-A
                       AND FUNCTION TRIM(ACCT-PASS) = CANON-B
                       MOVE "Y" TO REQ-FOUND
                       EXIT PERFORM
                    END-IF
              END-READ
           END-PERFORM
           CLOSE NETWORK
           EXIT PARAGRAPH.

*> Append a new connection as canonical pair to NETWORK if not present.
       ADD-CONNECTION.
           MOVE FUNCTION TRIM(CANON-A) TO CANON-A
           MOVE FUNCTION TRIM(CANON-B) TO CANON-B
           IF CANON-A > CANON-B
              MOVE CANON-A TO WS-TEMP
              MOVE CANON-B TO CANON-A
              MOVE WS-TEMP TO CANON-B
           END-IF
           OPEN EXTEND NETWORK
           MOVE SPACES TO NET-REC
           STRING CANON-A DELIMITED BY SIZE
                  " "    DELIMITED BY SIZE
                  CANON-B DELIMITED BY SIZE
                  INTO NET-REC
           END-STRING
           WRITE NET-REC
           CLOSE NETWORK
           EXIT PARAGRAPH.

*> Remove a single pending request (PENDING-SENDER -> PENDING-RECIP) from CONNECTIONS
       REMOVE-PENDING.
           OPEN INPUT CONNECTIONS
           OPEN OUTPUT CONN-TMP
           PERFORM UNTIL 1 = 0
              READ CONNECTIONS NEXT RECORD
                 AT END EXIT PERFORM
                 NOT AT END
                    UNSTRING CONN-REC DELIMITED BY ALL " "
                       INTO ACCT-USER ACCT-PASS
                    END-UNSTRING
                    IF NOT (FUNCTION TRIM(ACCT-USER) = FUNCTION TRIM(PENDING-SENDER)
                           AND FUNCTION TRIM(ACCT-PASS) = FUNCTION TRIM(PENDING-RECIP))
                       MOVE CONN-REC TO TMP-REC
                       WRITE TMP-REC
                    END-IF
              END-READ
           END-PERFORM
           CLOSE CONNECTIONS
           CLOSE CONN-TMP
           *> Replace connections.txt with tmp file contents
           OPEN OUTPUT CONNECTIONS
           CLOSE CONNECTIONS
           OPEN INPUT CONN-TMP
           OPEN EXTEND CONNECTIONS
           PERFORM UNTIL 1 = 0
              READ CONN-TMP NEXT RECORD
                 AT END EXIT PERFORM
                 NOT AT END
                    MOVE TMP-REC TO CONN-REC
                    WRITE CONN-REC
              END-READ
           END-PERFORM
           CLOSE CONN-TMP
           CLOSE CONNECTIONS
           EXIT PARAGRAPH.

*> Accept a pending request from PENDING-SENDER to PENDING-RECIP (current user).
       ACCEPT-REQUEST.
           MOVE "N" TO REQ-FOUND
           PERFORM FIND-PENDING
           IF REQ-FOUND NOT = "Y"
              MOVE "No such pending request found." TO MSG
              PERFORM WRITE-OUTPUT
              EXIT PARAGRAPH
           END-IF
           *> check already connected
           MOVE PENDING-SENDER TO CANON-A
           MOVE PENDING-RECIP  TO CANON-B
           PERFORM IS-CONNECTED
           IF REQ-FOUND = "Y"
              MOVE "You are already connected with this user." TO MSG
              PERFORM WRITE-OUTPUT
              EXIT PARAGRAPH
           END-IF
           *> add connection and remove pending
           PERFORM ADD-CONNECTION
           PERFORM REMOVE-PENDING
           MOVE "Connection accepted." TO MSG
           PERFORM WRITE-OUTPUT
           EXIT PARAGRAPH.

      *> Accept when caller already knows the pending exists (no scan). Uses PENDING-SENDER/RECIP
      ACCEPT-REQUEST-DIRECT.
          *> check already connected
          MOVE PENDING-SENDER TO CANON-A
          MOVE PENDING-RECIP  TO CANON-B
          PERFORM IS-CONNECTED
          IF REQ-FOUND = "Y"
             MOVE "You are already connected with this user." TO MSG
             PERFORM WRITE-OUTPUT
             EXIT PARAGRAPH
          END-IF
          *> add connection and remove pending
          PERFORM ADD-CONNECTION
          PERFORM REMOVE-PENDING
          MOVE "Connection accepted." TO MSG
          PERFORM WRITE-OUTPUT
          EXIT PARAGRAPH.

       *> List full-name style connections for the logged-in user
      LIST-MY-CONNECTIONS.
           OPEN INPUT NETWORK
           PERFORM UNTIL 1 = 0
              READ NETWORK NEXT RECORD
                 AT END EXIT PERFORM
                 NOT AT END
                    UNSTRING NET-REC DELIMITED BY ALL " "
                       INTO ACCT-USER ACCT-PASS
                    END-UNSTRING
                    IF FUNCTION TRIM(ACCT-USER) = FUNCTION TRIM(USERNAME)
                       MOVE FUNCTION TRIM(ACCT-PASS) TO OTHER-USER
                    ELSE
                       IF FUNCTION TRIM(ACCT-PASS) = FUNCTION TRIM(USERNAME)
                          MOVE FUNCTION TRIM(ACCT-USER) TO OTHER-USER
                       ELSE
                          MOVE SPACES TO OTHER-USER
                       END-IF
                    END-IF
                    IF OTHER-USER NOT = SPACES
                       PERFORM GET-DISPLAY-NAME-FOR-OTHER
                       MOVE FUNCTION TRIM(WS-DISPLAY-NAME) TO MSG
                       PERFORM WRITE-OUTPUT
                    END-IF
              END-READ
           END-PERFORM
           CLOSE NETWORK
           EXIT PARAGRAPH.

       *> List full-name style connections for the user specified in PARAM-USER
       LIST-CONNECTIONS-FOR-USER.
           OPEN INPUT NETWORK
           PERFORM UNTIL 1 = 0
              READ NETWORK NEXT RECORD
                 AT END EXIT PERFORM
                 NOT AT END
                    UNSTRING NET-REC DELIMITED BY ALL " "
                       INTO ACCT-USER ACCT-PASS
                    END-UNSTRING
                    IF FUNCTION TRIM(ACCT-USER) = FUNCTION TRIM(PARAM-USER)
                       MOVE FUNCTION TRIM(ACCT-PASS) TO OTHER-USER
                    ELSE
                       IF FUNCTION TRIM(ACCT-PASS) = FUNCTION TRIM(PARAM-USER)
                          MOVE FUNCTION TRIM(ACCT-USER) TO OTHER-USER
                       ELSE
                          MOVE SPACES TO OTHER-USER
                       END-IF
                    END-IF
                    IF OTHER-USER NOT = SPACES
                       PERFORM GET-DISPLAY-NAME-FOR-OTHER
                       MOVE FUNCTION TRIM(WS-DISPLAY-NAME) TO MSG
                       PERFORM WRITE-OUTPUT
                    END-IF
              END-READ
           END-PERFORM
           CLOSE NETWORK
           EXIT PARAGRAPH.