IDENTIFICATION DIVISION.
       PROGRAM-ID. CONTROLFLOW.   *> PROGRAM-ID gives the program its name (CONTROLFLOW)

       ENVIRONMENT DIVISION. *> big section where we describe our machine environment (files, devices, terminals, etc)
       INPUT-OUTPUT SECTION. *> declare external files -- we tell COBOL what files exist and how we control them
       FILE-CONTROL. *> starts the list of file declarations. we write SELECT statements to connect logical names in our program to actual files on disk
           SELECT INPUTFILE ASSIGN TO "data/user_input.txt" *> reads simulated user input
              ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUTPUTFILE ASSIGN TO "data/output_log.txt" *>writes logs (output_log.txt)
              ORGANIZATION IS LINE SEQUENTIAL. *> each record is a line of text
           SELECT ACCOUNTS ASSIGN TO "data/accounts.txt" *> stores accounts persistently (accounts.txt)
              ORGANIZATION IS LINE SEQUENTIAL
              FILE STATUS IS ACC-FS. *>gives us a way to check if opening the file succeeded
           
           SELECT PROFILE-FILE ASSIGN TO DYNAMIC WS-FILENAME
              ORGANIZATION IS LINE SEQUENTIAL
              FILE STATUS IS PROFILE-STATUS.
           
           SELECT CONNECTIONS ASSIGN TO "data/connections.txt"
              ORGANIZATION IS LINE SEQUENTIAL
              FILE STATUS IS CONN-FS.

           SELECT NETWORK ASSIGN TO "data/network.txt"
              ORGANIZATION IS LINE SEQUENTIAL
              FILE STATUS IS NET-FS.
           
           SELECT CONN-TMP ASSIGN TO "data/connections.tmp"
              ORGANIZATION IS LINE SEQUENTIAL
              FILE STATUS IS TMP-FS.

           SELECT PROFILES-INDEX ASSIGN TO "data/profiles.idx"
              ORGANIZATION IS LINE SEQUENTIAL
              FILE STATUS IS PRO-FS.

           SELECT JOB-FILE ASSIGN TO DYNAMIC WS-JOB-FILENAME
              ORGANIZATION IS LINE SEQUENTIAL
              FILE STATUS IS JOB-FS.
           
           SELECT JOB-INDEX ASSIGN TO "data/jobs.idx"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS BROWSE-FS.

           SELECT APPLICATIONS ASSIGN TO "data/applications.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS APPLICATIONS-FS.

           SELECT MESSAGES ASSIGN TO "data/messages.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS MSG-FS.

       DATA DIVISION. *> we describe all the data the program can use -- the files, variables, and structure and size of each piece of data
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

       FD  JOB-FILE.           
       01  JOB-REC            PIC X(120).

       FD  JOB-INDEX.
       01  BROWSE-REC         PIC X(1000).

       FD APPLICATIONS.
       01 APPLICATIONS-REC    PIC X(120).

       FD  MESSAGES.
       01  MESSAGE-REC        PIC X(300).

       WORKING-STORAGE SECTION.
       77 VALID-YEAR PIC X VALUE "N". *> defines program variables in memory
       77  ACC-FS              PIC XX VALUE SPACES.  *> file status for ACCOUNTS. we use 77 because it's a standalone variable
       77  PROFILE-STATUS      PIC XX VALUE SPACES. 
       77  CONN-FS             PIC XX VALUE SPACES.
       77  NET-FS              PIC XX VALUE SPACES.
       77  TMP-FS              PIC XX VALUE SPACES.
       77  PRO-FS              PIC XX VALUE SPACES.
       77  JOB-FS              PIC XX VALUE SPACES.
       77  BROWSE-FS           PIC XX VALUE SPACES.
       77  APPLICATIONS-FS     PIC XX VALUE SPACES.

       77  MSG-FS              PIC XX VALUE SPACES.

       77  FIRST-NAME           PIC X(50).
       77  LAST-NAME            PIC X(50).

       77  EOF-FLAG             PIC X  VALUE "N". *> END of File flag variable. "N" is the initial value (since we're not at the end of the file) 
       77  PROFILE-EOF          PIC X  VALUE "N".
       77  JOB-EOF              PIC X  VALUE "N".
       77  APPLICATIONS-EOF              PIC X  VALUE "N".
       77  USERNAME             PIC X(20).
       77  PASSWORD             PIC X(20).
       77  VALID-LOGIN          PIC X  VALUE "N". *> password validation. doesn't actually becmoe "Y" until we validate the password
       77  ACCT-COUNT           PIC 9  VALUE 0. *> number of accounts accounts.txt
       77  OPTION-CHOICE        PIC 9  VALUE 0. *> option selection from the user
       77  MSG                  PIC X(150). 
       77  WS-TEMP              PIC X(10).
       77  FIELD-LEN            PIC 9(4) VALUE ZERO.


       77  WS-FILENAME          PIC X(128).
       77  WS-FILENAME-SAVED   PIC X(128).
       77  PROFILE-NAME        PIC X(128).
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
       77  NET-A              PIC X(50).
       77  NET-B              PIC X(50).


       77  WS-JOB-FILENAME    PIC X(128).
       77  JOB-NAME           PIC X(500).

       *> Core job fields
       77  JOB-TITLE          PIC X(300).
       77  DESCRIPTION        PIC X(5000).
       77  EMPLOYER           PIC X(300).
       77  LOCATION           PIC X(300).
       77  SALARY             PIC X(300).

       *> Minimal helpers to make a safe filename
       77  SAFE-TITLE         PIC X(128).
       77  SAFE-EMPLOYER      PIC X(128).

       *> Messaging feature working storage
       77  MSG-SENDER         PIC X(20).
       77  MSG-RECIPIENT      PIC X(20).
       77  MSG-CONTENT        PIC X(200).
       77  VALID-MSG          PIC X(20).
       77  MSG-EOF            PIC X  VALUE "N".
       77  MSG-FOUND          PIC X  VALUE "N".

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

           OPEN I-O JOB-INDEX
           IF JOB-FS NOT = "00"
              CLOSE JOB-INDEX
              OPEN OUTPUT JOB-INDEX
              CLOSE JOB-INDEX
              OPEN I-O JOB-INDEX
           END-IF

           OPEN I-O APPLICATIONS
           IF APPLICATIONS-FS NOT = "00"
              CLOSE APPLICATIONS
              OPEN OUTPUT APPLICATIONS
              CLOSE APPLICATIONS
              OPEN I-O APPLICATIONS
           END-IF

           *> Ensure MESSAGES file exists
           OPEN I-O MESSAGES
           IF MSG-FS NOT = "00"
              CLOSE MESSAGES
              OPEN OUTPUT MESSAGES
              CLOSE MESSAGES
              OPEN I-O MESSAGES
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
           CLOSE APPLICATIONS
           CLOSE JOB-INDEX
           MOVE 0 TO RETURN-CODE
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

           CLOSE MESSAGES
           OPEN OUTPUT MESSAGES
           IF MSG-FS = "00"
              CLOSE MESSAGES
              OPEN I-O MESSAGES
           END-IF

           *> Reset derived in-memory counters/flags
           MOVE 0 TO ACCT-COUNT

           *> PROFILES-INDEX
           CLOSE PROFILES-INDEX
           OPEN OUTPUT PROFILES-INDEX
           IF PRO-FS = "00"
              CLOSE PROFILES-INDEX
           END-IF

           CLOSE JOB-INDEX
           OPEN OUTPUT JOB-INDEX
           IF JOB-FS = "00"
              CLOSE JOB-INDEX
           END-IF

           CLOSE APPLICATIONS
           OPEN OUTPUT APPLICATIONS
           IF APPLICATIONS-FS = "00"
              CLOSE APPLICATIONS
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
        *> user menu options presented after login
           MOVE "====================USER MENU====================" TO MSG
           PERFORM WRITE-OUTPUT
           MOVE "0 = Return" TO MSG
           PERFORM WRITE-OUTPUT
           MOVE "1 = Job/Internship Search" TO MSG
           PERFORM WRITE-OUTPUT
           MOVE "2 = Learn skill" TO MSG
           PERFORM WRITE-OUTPUT
           MOVE "3 = Create/Edit Profile" TO MSG
           PERFORM WRITE-OUTPUT
           MOVE "4 = Output Profile" TO MSG
           PERFORM WRITE-OUTPUT
           MOVE "5 = Search Profile" TO MSG
           PERFORM WRITE-OUTPUT
           MOVE "6 = View Pending Requests" TO MSG
           PERFORM WRITE-OUTPUT
           MOVE "7 = View My Network" TO MSG
           PERFORM WRITE-OUTPUT
           MOVE "8 = Messages" TO MSG
           PERFORM WRITE-OUTPUT
           MOVE "=================================================" TO MSG
           PERFORM WRITE-OUTPUT
           MOVE "Enter your choice (0-8):" TO MSG
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
                 PERFORM JOB-INTERNSHIP-SEARCH
                 PERFORM USER-MENU
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
              WHEN 6
                 PERFORM LIST-PENDING-REQUESTS
                 PERFORM USER-MENU
              WHEN 7
                 PERFORM VIEW-MY-NETWORK
                 PERFORM USER-MENU
              WHEN 8 
                 PERFORM MESSAGING-MENU
                 PERFORM USER-MENU
              WHEN 0
                 EXIT PARAGRAPH
              WHEN OTHER
                 MOVE "Invalid option, you must select a number 0-8" TO MSG
                 PERFORM WRITE-OUTPUT
           END-EVALUATE.

       JOB-INTERNSHIP-SEARCH.
          MOVE "==============JOB/INTERNSHIP SEARCH==============" TO MSG
          PERFORM WRITE-OUTPUT 
          MOVE "0 = Return to Main Menu" TO MSG
          PERFORM WRITE-OUTPUT
          MOVE "1 = Post a Job/Internship" TO MSG
           PERFORM WRITE-OUTPUT
           MOVE "2 = Browse Jobs/Internships" TO MSG
           PERFORM WRITE-OUTPUT
           MOVE "3 = View Applications" TO MSG
          MOVE "=================================================" TO MSG
          PERFORM WRITE-OUTPUT
          MOVE "Enter your choice (0-3):" TO MSG
           PERFORM WRITE-OUTPUT
          READ INPUTFILE AT END EXIT PARAGRAPH
              NOT AT END MOVE FUNCTION NUMVAL(INPUT-REC) TO OPTION-CHOICE
           END-READ
           MOVE OPTION-CHOICE TO MSG 
           PERFORM WRITE-OUTPUT
           *> function to evaluate the option they choose
           EVALUATE OPTION-CHOICE 
             WHEN 0
                EXIT PARAGRAPH 
             WHEN 1 
                PERFORM POST-JOB-INTERNSHIP
                PERFORM JOB-INTERNSHIP-SEARCH
             WHEN 2
                PERFORM BROWSE-JOB-INTERNSHIP
                PERFORM JOB-INTERNSHIP-SEARCH
             WHEN 3
                PERFORM VIEW-APPLICATIONS
                PERFORM JOB-INTERNSHIP-SEARCH
           END-EVALUATE.
       
       VIEW-APPLICATIONS. 
           MOVE "Your Job Applications" to MSG
           PERFORM WRITE-OUTPUT 
           OPEN INPUT APPLICATIONS
           IF APPLICATIONS-FS NOT = "00"
              MOVE "ERROR: could not open applications" TO MSG
              STRING MSG DELIMITED BY SIZE
                     BROWSE-FS DELIMITED BY SIZE
                     ")"      DELIMITED BY SIZE
                     INTO MSG
              END-STRING
              PERFORM WRITE-OUTPUT
              EXIT PARAGRAPH
           END-IF

           PERFORM UNTIL 1 = 2
              READ APPLICATIONS
                  AT END
                      EXIT PERFORM
                  NOT AT END
                      MOVE SPACES TO TARGET-USER JOB-NAME

                      UNSTRING APPLICATIONS-REC DELIMITED BY "|"
                         INTO TARGET-USER
                              JOB-NAME
                      END-UNSTRING

                      IF TARGET-USER = USERNAME
                          MOVE SPACES TO WS-JOB-FILENAME
                          STRING "data/jobs/"                DELIMITED BY SIZE
                                 FUNCTION TRIM(JOB-NAME)     DELIMITED BY SIZE
                                 INTO WS-JOB-FILENAME
                          END-STRING

                          MOVE "N" TO JOB-EOF
                          OPEN INPUT JOB-FILE


                          IF JOB-FS = "00"
                              PERFORM UNTIL JOB-EOF = "Y"
                                  PERFORM PRINT-JOB
                              END-PERFORM
                          END-IF

                          CLOSE JOB-FILE        
                      END-IF
              END-READ
          END-PERFORM

           CLOSE APPLICATIONS
           EXIT PARAGRAPH.

       BROWSE-JOB-INTERNSHIP.
           PERFORM PRINT-JOBS-INTERNSHIPS
           MOVE "Would you like more details on a specific job? (Y/N)" to MSG
           PERFORM WRITE-OUTPUT
           READ INPUTFILE AT END MOVE SPACE TO RESP-CHAR
                 NOT AT END MOVE FUNCTION TRIM(INPUT-REC)(1:1) TO RESP-CHAR
           END-READ
           IF RESP-CHAR = "Y" OR RESP-CHAR = "y"
              PERFORM VIEW-JOB-DETAILS
           END-IF
           EXIT PARAGRAPH.

       VIEW-JOB-DETAILS.
           MOVE "Enter the job title of the job you are looking for:" TO MSG
           PERFORM WRITE-OUTPUT
           
           READ INPUTFILE AT END EXIT PARAGRAPH
              NOT AT END MOVE FUNCTION TRIM(INPUT-REC) TO JOB-TITLE
           END-READ

           MOVE "Enter the employer of the job you are looking for" TO MSG
           PERFORM WRITE-OUTPUT
           READ INPUTFILE AT END EXIT PARAGRAPH
              NOT AT END MOVE FUNCTION TRIM(INPUT-REC) TO EMPLOYER
           END-READ

           MOVE FUNCTION TRIM(JOB-TITLE TRAILING) TO SAFE-TITLE
           MOVE FUNCTION LENGTH(FUNCTION TRIM(SAFE-TITLE TRAILING)) TO FIELD-LEN
           IF FIELD-LEN > 0
              INSPECT SAFE-TITLE(1:FIELD-LEN) REPLACING ALL " " BY "_"
           END-IF

           MOVE FUNCTION TRIM(EMPLOYER TRAILING) TO SAFE-EMPLOYER
           MOVE FUNCTION LENGTH(FUNCTION TRIM(SAFE-EMPLOYER TRAILING)) TO FIELD-LEN
           IF FIELD-LEN > 0
              INSPECT SAFE-EMPLOYER(1:FIELD-LEN) REPLACING ALL " " BY "_"
           END-IF

           MOVE SPACES TO JOB-NAME
           STRING FUNCTION TRIM(SAFE-TITLE)    DELIMITED BY SIZE
                 "_"                          DELIMITED BY SIZE
                 FUNCTION TRIM(SAFE-EMPLOYER) DELIMITED BY SIZE
                 INTO JOB-NAME
           END-STRING

           MOVE JOB-NAME TO MSG
           PERFORM WRITE-OUTPUT

           MOVE SPACES TO WS-JOB-FILENAME

           MOVE "Searching for job post..." TO MSG
           PERFORM WRITE-OUTPUT

           STRING "data/jobs/" DELIMITED BY SIZE
                  FUNCTION TRIM(JOB-NAME) DELIMITED BY SIZE
                  INTO WS-JOB-FILENAME
           END-STRING

           *> Try to open the job file directly using the converted filename
           MOVE 'N' TO JOB-EOF
           OPEN INPUT JOB-FILE
           IF JOB-FS = "00"
              MOVE "Job Posting found. Displaying job:" TO MSG
              PERFORM WRITE-OUTPUT
              PERFORM UNTIL JOB-EOF = "Y"
                  PERFORM PRINT-JOB
              END-PERFORM
              
               MOVE "Would you like to apply to this job? (Y/N)" TO MSG 
               PERFORM WRITE-OUTPUT
               READ INPUTFILE AT END MOVE SPACE TO RESP-CHAR
                 NOT AT END MOVE FUNCTION TRIM(INPUT-REC)(1:1) TO RESP-CHAR
               END-READ
               IF RESP-CHAR = "Y" OR RESP-CHAR = "y"
                   PERFORM SUBMIT-APPLICATION
               END-IF
              CLOSE JOB-FILE
           EXIT PARAGRAPH.
             
       SUBMIT-APPLICATION.
          CLOSE APPLICATIONS
          OPEN EXTEND APPLICATIONS

           *> If file not found, create it, then reopen for EXTEND
           IF APPLICATIONS-FS = "35"
              OPEN OUTPUT APPLICATIONS
              IF APPLICATIONS-FS NOT = "00"
                 STRING "ERROR: cannot create data/applications.txt (FS="
                        APPLICATIONS-FS
                        ")"
                        DELIMITED BY SIZE INTO MSG
                 END-STRING
                 PERFORM WRITE-OUTPUT
                 EXIT PARAGRAPH
              END-IF
              CLOSE APPLICATIONS
              OPEN EXTEND APPLICATIONS
           END-IF

           *> If still not OK, bail out with the visible FS code
           IF APPLICATIONS-FS NOT = "00"
              STRING "ERROR: cannot open data/applications.txt (FS="
                     APPLICATIONS-FS
                     ")"
                     DELIMITED BY SIZE INTO MSG
              END-STRING
              PERFORM WRITE-OUTPUT
              EXIT PARAGRAPH
           END-IF

           *> Build a single line: username | job-name
           MOVE SPACES TO APPLICATIONS-REC
           STRING
              FUNCTION TRIM(USERNAME)  DELIMITED BY SIZE
              " | "                    DELIMITED BY SIZE
              FUNCTION TRIM(JOB-NAME)  DELIMITED BY SIZE
              INTO APPLICATIONS-REC
           END-STRING

           *> Append it
           WRITE APPLICATIONS-REC

           *> Report result
           IF APPLICATIONS-FS NOT = "00"
              STRING "ERROR: write failed (FS="
                     APPLICATIONS-FS
                     ")"
                     DELIMITED BY SIZE INTO MSG
              END-STRING
              PERFORM WRITE-OUTPUT
           ELSE
              MOVE "You have submitted an application" TO MSG
              PERFORM WRITE-OUTPUT
           END-IF

           CLOSE APPLICATIONS
           EXIT PARAGRAPH.

       PRINT-JOB. 
           READ JOB-FILE
              AT END
                  MOVE "Y" TO JOB-EOF
              NOT AT END
                  MOVE FUNCTION TRIM(JOB-REC) TO MSG
                  PERFORM WRITE-OUTPUT
           END-READ.
       
       PRINT-JOBS-INTERNSHIPS.
          OPEN INPUT JOB-INDEX
           IF BROWSE-FS NOT = "00"
              MOVE "ERROR: could not open jobs.idx (status=" TO MSG
              STRING MSG DELIMITED BY SIZE
                     BROWSE-FS DELIMITED BY SIZE
                     ")"      DELIMITED BY SIZE
                     INTO MSG
              END-STRING
              PERFORM WRITE-OUTPUT
              EXIT PARAGRAPH
           END-IF

           PERFORM UNTIL 1 = 2
              READ JOB-INDEX
                 AT END
                    EXIT PERFORM
                 NOT AT END
                    *> Clear all fields before processing
                    MOVE SPACES TO JOB-NAME
                    MOVE SPACES TO JOB-TITLE
                    MOVE SPACES TO EMPLOYER
                    MOVE SPACES TO LOCATION
                    MOVE SPACES TO SALARY

                    UNSTRING BROWSE-REC DELIMITED BY "|"
                       INTO
                            JOB-NAME
                            JOB-TITLE
                            EMPLOYER
                            LOCATION
                            SALARY
                    END-UNSTRING

                    *> Trim each field by copying to temp and back
                    MOVE FUNCTION TRIM(JOB-NAME) TO WS-FIELD
                    MOVE SPACES TO JOB-NAME
                    MOVE WS-FIELD TO JOB-NAME

                    MOVE FUNCTION TRIM(JOB-TITLE) TO WS-FIELD
                    MOVE SPACES TO JOB-TITLE
                    MOVE WS-FIELD TO JOB-TITLE

                    MOVE FUNCTION TRIM(EMPLOYER) TO WS-FIELD
                    MOVE SPACES TO EMPLOYER
                    MOVE WS-FIELD TO EMPLOYER

                    MOVE FUNCTION TRIM(LOCATION) TO WS-FIELD
                    MOVE SPACES TO LOCATION
                    MOVE WS-FIELD TO LOCATION

                    MOVE FUNCTION TRIM(SALARY) TO WS-FIELD
                    MOVE SPACES TO SALARY
                    MOVE WS-FIELD TO SALARY

                    MOVE SPACES TO MSG
                    STRING "Job Title: "              DELIMITED BY SIZE
                           FUNCTION TRIM(JOB-TITLE)   DELIMITED BY SIZE
                           INTO MSG
                    END-STRING
                    PERFORM WRITE-OUTPUT

                    MOVE SPACES TO MSG
                    STRING "Employer: "               DELIMITED BY SIZE
                           FUNCTION TRIM(EMPLOYER)    DELIMITED BY SIZE
                           INTO MSG
                    END-STRING
                    PERFORM WRITE-OUTPUT

                    MOVE SPACES TO MSG
                    STRING "Location: "               DELIMITED BY SIZE
                           FUNCTION TRIM(LOCATION)    DELIMITED BY SIZE
                           INTO MSG
                    END-STRING
                    PERFORM WRITE-OUTPUT

                    MOVE SPACES TO MSG
                    PERFORM WRITE-OUTPUT
              END-READ
             

           END-PERFORM



           CLOSE JOB-INDEX
           EXIT PARAGRAPH.

       POST-JOB-INTERNSHIP.
          MOVE "Enter Job Title:" TO MSG
          PERFORM WRITE-OUTPUT
          MOVE SPACES TO WS-FIELD
          PERFORM UNTIL WS-FIELD NOT = SPACES
              READ INPUTFILE AT END MOVE SPACES TO WS-FIELD
                  NOT AT END MOVE INPUT-REC TO WS-FIELD
              END-READ
              IF WS-FIELD = SPACES
                  MOVE "Job Title is required. Please re-enter:" TO MSG
                  PERFORM WRITE-OUTPUT
              END-IF
          END-PERFORM
          MOVE FUNCTION TRIM(WS-FIELD TRAILING) TO JOB-TITLE

          *> --------------------------
          *> Collect Description (REQ)
          *> --------------------------
          MOVE "Enter Description (brief text of the role):" TO MSG
          PERFORM WRITE-OUTPUT
          MOVE SPACES TO WS-FIELD
          PERFORM UNTIL WS-FIELD NOT = SPACES
              READ INPUTFILE AT END MOVE SPACES TO WS-FIELD
                  NOT AT END MOVE INPUT-REC TO WS-FIELD
              END-READ
              IF WS-FIELD = SPACES
                  MOVE "Description is required. Please re-enter:" TO MSG
                  PERFORM WRITE-OUTPUT
              END-IF
          END-PERFORM
          MOVE FUNCTION TRIM(WS-FIELD TRAILING) TO DESCRIPTION

          *> -----------------------
          *> Collect Employer (REQ)
          *> -----------------------
          MOVE "Enter Employer (company/organization):" TO MSG
          PERFORM WRITE-OUTPUT
          MOVE SPACES TO WS-FIELD
          PERFORM UNTIL WS-FIELD NOT = SPACES
              READ INPUTFILE AT END MOVE SPACES TO WS-FIELD
                  NOT AT END MOVE INPUT-REC TO WS-FIELD
              END-READ
              IF WS-FIELD = SPACES
                  MOVE "Employer is required. Please re-enter:" TO MSG
                  PERFORM WRITE-OUTPUT
              END-IF
          END-PERFORM
          MOVE FUNCTION TRIM(WS-FIELD TRAILING) TO EMPLOYER

          *> ----------------------
          *> Collect Location (REQ)
          *> ----------------------
          MOVE "Enter Location (e.g., Remote, New York, NY):" TO MSG
          PERFORM WRITE-OUTPUT
          MOVE SPACES TO WS-FIELD
          PERFORM UNTIL WS-FIELD NOT = SPACES
              READ INPUTFILE AT END MOVE SPACES TO WS-FIELD
                  NOT AT END MOVE INPUT-REC TO WS-FIELD
              END-READ
              IF WS-FIELD = SPACES
                  MOVE "Location is required. Please re-enter:" TO MSG
                  PERFORM WRITE-OUTPUT
              END-IF
          END-PERFORM
          MOVE FUNCTION TRIM(WS-FIELD TRAILING) TO LOCATION

          *> -----------------------
          *> Collect Salary (OPTION)
          *> -----------------------
          MOVE "Enter Salary (optional, e.g., $50,000/year, $25/hour):" TO MSG
          PERFORM WRITE-OUTPUT
          READ INPUTFILE AT END MOVE SPACES TO WS-FIELD
              NOT AT END MOVE INPUT-REC TO WS-FIELD
          END-READ
          MOVE FUNCTION TRIM(WS-FIELD TRAILING) TO SALARY

          *> ==================================
          *> Build simple filename: data/jobs/<title>_<employer>
          *> (Spaces -> underscores; keep it minimal)
          *> ==================================
          MOVE FUNCTION TRIM(JOB-TITLE TRAILING) TO SAFE-TITLE
          MOVE FUNCTION LENGTH(FUNCTION TRIM(SAFE-TITLE TRAILING)) TO FIELD-LEN
          IF FIELD-LEN > 0
              INSPECT SAFE-TITLE(1:FIELD-LEN) REPLACING ALL " " BY "_"
          END-IF

          MOVE FUNCTION TRIM(EMPLOYER TRAILING) TO SAFE-EMPLOYER
          MOVE FUNCTION LENGTH(FUNCTION TRIM(SAFE-EMPLOYER TRAILING)) TO FIELD-LEN
          IF FIELD-LEN > 0
              INSPECT SAFE-EMPLOYER(1:FIELD-LEN) REPLACING ALL " " BY "_"
          END-IF

          MOVE SPACES TO JOB-NAME
          STRING FUNCTION TRIM(SAFE-TITLE)    DELIMITED BY SIZE
                 "_"                          DELIMITED BY SIZE
                 FUNCTION TRIM(SAFE-EMPLOYER) DELIMITED BY SIZE
                 INTO JOB-NAME
          END-STRING

          MOVE SPACES TO WS-JOB-FILENAME
          STRING "data/jobs/"                 DELIMITED BY SIZE
                 FUNCTION TRIM(JOB-NAME)      DELIMITED BY SIZE
                 INTO WS-JOB-FILENAME
          END-STRING

          *> ==================================
          *> Create/clear the dedicated posting file
          *> ==================================
          OPEN OUTPUT JOB-FILE
          CLOSE JOB-FILE

          *> ==================================
          *> Append one-line summary to JOB-INDEX
          *> (kept simple for browsing)
          *> ==================================
          OPEN EXTEND JOB-INDEX
          INITIALIZE BROWSE-REC
          MOVE SPACES TO BROWSE-REC
          STRING FUNCTION TRIM(JOB-NAME)   DELIMITED BY SIZE
                 " | "                     DELIMITED BY SIZE
                 FUNCTION TRIM(JOB-TITLE)  DELIMITED BY SIZE
                 " | "                     DELIMITED BY SIZE
                 FUNCTION TRIM(EMPLOYER)   DELIMITED BY SIZE
                 " | "                     DELIMITED BY SIZE
                 FUNCTION TRIM(LOCATION)   DELIMITED BY SIZE
                 " | "                     DELIMITED BY SIZE
                 FUNCTION TRIM(SALARY)     DELIMITED BY SIZE
                 INTO BROWSE-REC
          END-STRING
          WRITE BROWSE-REC
          CLOSE JOB-INDEX

          *> ==================================
          *> Write the full posting content
          *> ==================================
          OPEN OUTPUT JOB-FILE

          MOVE SPACES TO JOB-REC
          STRING "Job Title: "   DELIMITED BY SIZE
                 JOB-TITLE       DELIMITED BY SIZE
                 INTO JOB-REC
          WRITE JOB-REC

          MOVE SPACES TO JOB-REC
          STRING "Description: " DELIMITED BY SIZE
                 DESCRIPTION     DELIMITED BY SIZE
                 INTO JOB-REC
          WRITE JOB-REC

          MOVE SPACES TO JOB-REC
          STRING "Employer: "    DELIMITED BY SIZE
                 EMPLOYER        DELIMITED BY SIZE
                 INTO JOB-REC
          WRITE JOB-REC

          MOVE SPACES TO JOB-REC
          STRING "Location: "    DELIMITED BY SIZE
                 LOCATION        DELIMITED BY SIZE
                 INTO JOB-REC
          WRITE JOB-REC

          MOVE SPACES TO JOB-REC
          STRING "Salary: "      DELIMITED BY SIZE
                 SALARY          DELIMITED BY SIZE
                 INTO JOB-REC
          WRITE JOB-REC

          CLOSE JOB-FILE

          MOVE "Job/Internship posting saved successfully." TO MSG
          PERFORM WRITE-OUTPUT    
          EXIT PARAGRAPH.

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

           *> Prepare sanitized components for filenames/index entries
           MOVE FUNCTION TRIM(FIRST-NAME) TO LK-FIRST-NAME
           MOVE FUNCTION LENGTH(FUNCTION TRIM(LK-FIRST-NAME TRAILING)) TO FIELD-LEN
           IF FIELD-LEN > 0
              INSPECT LK-FIRST-NAME(1:FIELD-LEN) REPLACING ALL " " BY "_"
           END-IF
           MOVE FUNCTION TRIM(LAST-NAME)  TO LK-LAST-NAME
           MOVE FUNCTION LENGTH(FUNCTION TRIM(LK-LAST-NAME TRAILING)) TO FIELD-LEN
           IF FIELD-LEN > 0
              INSPECT LK-LAST-NAME(1:FIELD-LEN) REPLACING ALL " " BY "_"
           END-IF

           *> Build filename using sanitized values inside data/profiles directory
           MOVE SPACES TO PROFILE-NAME
           STRING FUNCTION TRIM(LK-FIRST-NAME) DELIMITED BY SIZE
                  "_"                       DELIMITED BY SIZE
                  FUNCTION TRIM(LK-LAST-NAME)  DELIMITED BY SIZE
                  INTO PROFILE-NAME
           END-STRING
           MOVE FUNCTION TRIM(PROFILE-NAME TRAILING) TO PROFILE-NAME
           MOVE SPACES TO WS-FILENAME
           STRING "data/profiles/" DELIMITED BY SIZE
                  FUNCTION TRIM(PROFILE-NAME) DELIMITED BY SIZE
                  INTO WS-FILENAME
           END-STRING

           *> Clear the old profile file
           OPEN OUTPUT PROFILE-FILE
           CLOSE PROFILE-FILE

           *> Update profiles index (username -> first/last)
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
           MOVE WS-FIELD TO MSG
           PERFORM WRITE-OUTPUT

           MOVE SPACES TO PROFILE-NAME
           MOVE FUNCTION TRIM(WS-FIELD TRAILING) TO PROFILE-NAME
           MOVE FUNCTION LENGTH(FUNCTION TRIM(PROFILE-NAME TRAILING)) TO FIELD-LEN
           IF FIELD-LEN > 0
              INSPECT PROFILE-NAME(1:FIELD-LEN) REPLACING ALL " " BY "_"
           END-IF
           MOVE FUNCTION TRIM(PROFILE-NAME TRAILING) TO PROFILE-NAME
           MOVE PROFILE-NAME TO MSG
           PERFORM WRITE-OUTPUT

           MOVE SPACES TO WS-FILENAME
           STRING "data/profiles/" DELIMITED BY SIZE
                  FUNCTION TRIM(PROFILE-NAME) DELIMITED BY SIZE
                  INTO WS-FILENAME
           END-STRING

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
       
       *> ====================================
       *> Messaging Feature
       *> ====================================
       MESSAGING-MENU.
           MOVE "Messages - Choose: 1=Send a New Message, 2=View My Messages, 0=Return" TO MSG
           PERFORM WRITE-OUTPUT
           MOVE "==================MESSAGES MENU==================" TO MSG
          PERFORM WRITE-OUTPUT 
          MOVE "0 = Return to Main Menu" TO MSG
          PERFORM WRITE-OUTPUT
          MOVE "1 = Send a New Message" TO MSG
           PERFORM WRITE-OUTPUT
           MOVE "2 = View My Messages" TO MSG
           PERFORM WRITE-OUTPUT
          MOVE "==================================================" TO MSG
          PERFORM WRITE-OUTPUT
          MOVE "Enter your choice (0-2):" TO MSG
           PERFORM WRITE-OUTPUT
           READ INPUTFILE AT END EXIT PARAGRAPH
              NOT AT END MOVE FUNCTION NUMVAL(INPUT-REC) TO OPTION-CHOICE
           END-READ
           
           MOVE OPTION-CHOICE TO MSG
           PERFORM WRITE-OUTPUT
           
           EVALUATE OPTION-CHOICE
              WHEN 1
                 PERFORM SEND-NEW-MESSAGE
              WHEN 2
                 PERFORM VIEW-MY-MESSAGES
              WHEN 0
                 EXIT PARAGRAPH
              WHEN OTHER
                 MOVE "Invalid option, choose 0, 1, or 2" TO MSG
                 PERFORM WRITE-OUTPUT
           END-EVALUATE
           EXIT PARAGRAPH.

       SEND-NEW-MESSAGE.
           *> Prompt for recipient username
           MOVE "Enter recipient username:" TO MSG
           PERFORM WRITE-OUTPUT
           READ INPUTFILE AT END EXIT PARAGRAPH
              NOT AT END MOVE FUNCTION TRIM(INPUT-REC) TO MSG-RECIPIENT
           END-READ

           *> Validate recipient exists in accounts
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
                    IF MSG-RECIPIENT = FUNCTION TRIM(ACCT-USER)
                       MOVE "Y" TO USER-FOUND
                       EXIT PERFORM
                    END-IF
              END-READ
           END-PERFORM
           CLOSE ACCOUNTS
           OPEN I-O ACCOUNTS

           IF USER-FOUND = "N"
              MOVE "User not found. Message not sent." TO MSG
              PERFORM WRITE-OUTPUT
              EXIT PARAGRAPH
           END-IF

           *> Validate recipient is in sender's network (connected)
           MOVE FUNCTION TRIM(USERNAME)      TO CANON-A
           MOVE FUNCTION TRIM(MSG-RECIPIENT) TO CANON-B
           MOVE "N" TO REQ-FOUND
           PERFORM IS-CONNECTED
           IF REQ-FOUND = "N"
              MOVE "You can only message users you are connected with." TO MSG
              PERFORM WRITE-OUTPUT
              EXIT PARAGRAPH
           END-IF

           *> Prompt for message content
           MOVE "N" TO VALID-MSG
           PERFORM UNTIL VALID-MSG = "Y"
               MOVE "Enter your message (max 200 characters):" TO MSG
               PERFORM WRITE-OUTPUT
               READ INPUTFILE AT END EXIT PARAGRAPH
                  NOT AT END MOVE FUNCTION TRIM(INPUT-REC) TO MSG-CONTENT
               END-READ
    
               IF MSG-CONTENT = SPACES
                  MOVE "Message cannot be empty. Please try again." TO MSG
                  PERFORM WRITE-OUTPUT
               ELSE
                   IF FUNCTION LENGTH(FUNCTION TRIM(MSG-CONTENT)) > 200
                       MOVE "Message too long! Maximum 200 characters. Please try again." TO MSG
                       PERFORM WRITE-OUTPUT
                   ELSE
                       MOVE "Y" TO VALID-MSG
                   END-IF
               END-IF
           END-PERFORM

           *> Store the message in messages.txt
           *> Format: SENDER|RECIPIENT|MESSAGE_CONTENT
           CLOSE MESSAGES
           OPEN EXTEND MESSAGES
           MOVE SPACES TO MESSAGE-REC
           STRING FUNCTION TRIM(USERNAME)      DELIMITED BY SIZE
                  "|"                          DELIMITED BY SIZE
                  FUNCTION TRIM(MSG-RECIPIENT) DELIMITED BY SIZE
                  "|"                          DELIMITED BY SIZE
                  FUNCTION TRIM(MSG-CONTENT)   DELIMITED BY SIZE
                  INTO MESSAGE-REC
           END-STRING
           WRITE MESSAGE-REC
           CLOSE MESSAGES
           OPEN I-O MESSAGES

           MOVE "Message sent successfully." TO MSG
           PERFORM WRITE-OUTPUT
           EXIT PARAGRAPH.

       VIEW-MY-MESSAGES.
           MOVE "My Messages:" TO MSG
           PERFORM WRITE-OUTPUT
           MOVE "N" TO MSG-FOUND
           MOVE "N" TO MSG-EOF

           CLOSE MESSAGES
           OPEN INPUT MESSAGES

           PERFORM UNTIL MSG-EOF = "Y"
              READ MESSAGES NEXT RECORD
                 AT END
                    MOVE "Y" TO MSG-EOF
                 NOT AT END
                    IF MESSAGE-REC = SPACES
                       CONTINUE
                    ELSE
                       MOVE SPACES TO MSG-SENDER
                       MOVE SPACES TO MSG-RECIPIENT
                       MOVE SPACES TO MSG-CONTENT
                       UNSTRING MESSAGE-REC
                          DELIMITED BY "|"
                          INTO MSG-SENDER
                               MSG-RECIPIENT
                               MSG-CONTENT
                       END-UNSTRING
                       IF FUNCTION TRIM(MSG-RECIPIENT) = FUNCTION TRIM(USERNAME)
                          MOVE "Y" TO MSG-FOUND
                          PERFORM DISPLAY-SINGLE-MESSAGE
                       END-IF
                    END-IF
              END-READ
           END-PERFORM

           CLOSE MESSAGES
           OPEN I-O MESSAGES

           IF MSG-FOUND NOT = "Y"
              MOVE "You have no messages at this time." TO MSG
              PERFORM WRITE-OUTPUT
           END-IF
           EXIT PARAGRAPH.

       DISPLAY-SINGLE-MESSAGE.
           MOVE SPACES TO MSG
           STRING "From: " DELIMITED BY SIZE
                  FUNCTION TRIM(MSG-SENDER) DELIMITED BY SIZE
                  INTO MSG
           END-STRING
           PERFORM WRITE-OUTPUT

           MOVE SPACES TO MSG
           STRING "Message: " DELIMITED BY SIZE
                  FUNCTION TRIM(MSG-CONTENT) DELIMITED BY SIZE
                  INTO MSG
           END-STRING
           PERFORM WRITE-OUTPUT

           MOVE SPACES TO MSG
           PERFORM WRITE-OUTPUT
           EXIT PARAGRAPH.

*> ================= Connections Feature =================
      COPY "networking.cpy".

      SET-MY-PROFILE-FILENAME.
          MOVE SPACES TO WS-FILENAME
          STRING FUNCTION TRIM(FIRST-NAME) DELIMITED BY SIZE
                 "_"                       DELIMITED BY SIZE
                 FUNCTION TRIM(LAST-NAME)  DELIMITED BY SIZE
                 INTO WS-FILENAME
          END-STRING
          EXIT PARAGRAPH.
