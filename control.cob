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
              ORGANIZATION IS LINE SEQUENTIAL.

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

       WORKING-STORAGE SECTION.
       77 VALID-YEAR PIC X VALUE "N". *> defines program variables in memory
       77  ACC-FS               PIC XX VALUE SPACES.  *> file status for ACCOUNTS. we use 77 because it's a standalone variable

       77  EOF-FLAG             PIC X  VALUE "N". *> END of File flag variable. "N" is the initial value (since we're not at the end of the file) 
       77  PROFILE-EOF          PIC X  VALUE "N".
       77  USERNAME             PIC X(20).
       77  PASSWORD             PIC X(20).
       77  VALID-LOGIN          PIC X  VALUE "N". *> password validation. doesn't actually becmoe "Y" until we validate the password
       77  ACCT-COUNT           PIC 9  VALUE 0. *> number of accounts accounts.txt
       77      OPTION-CHOICE         PIC 9  VALUE 0. *> option selection from the user
       77  MSG                  PIC X(100). 
       77  WS-TEMP    PIC X(10).
       77  FIELD-LEN PIC 9(4) VALUE ZERO.


       77  WS-FILENAME          PIC X(128).
       77  WS-FIELD             PIC X(400).
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
                       *> Ensure file is not open before truncating
                       CLOSE ACCOUNTS
                       *> Truncate accounts.txt (or create empty file)
                       OPEN OUTPUT ACCOUNTS
                       MOVE "All accounts cleared. Returning to main menu." TO MSG
                       PERFORM WRITE-OUTPUT
                       IF ACC-FS = "00"
                          CLOSE ACCOUNTS
                          OPEN I-O ACCOUNTS
         
                          *> Reset in-memory count so logic matches disk
                          MOVE 0 TO ACCT-COUNT
                       ELSE
                          *> Surface the file-status for debugging
                          STRING "STARTOVER failed. FILE STATUS=" ACC-FS
                                 DELIMITED BY SIZE INTO MSG
                          END-STRING
                          PERFORM WRITE-OUTPUT
                       END-IF
                       EXIT PARAGRAPH
                   ELSE 
                       MOVE "Invalid Input" to MSG
                       PERFORM WRITE-OUTPUT
                   END-IF
               END-IF
           END-IF.



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
           MOVE "Choose: 1=Search job, 2=Learn skill, 3=Create/Edit My Profile, 4=Output Profile, 5=Return" TO MSG
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
               OPEN INPUT PROFILE-FILE
               PERFORM WRITE-OUTPUT
               PERFORM UNTIL PROFILE-EOF = "Y"
                   PERFORM PRINT-PROFILE
               END-PERFORM
               CLOSE PROFILE-FILE
               PERFORM USER-MENU
            WHEN 5
               EXIT PARAGRAPH
            WHEN OTHER
                 MOVE "Invalid option, you must select a number 1-5" TO MSG
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
           *> Build <username>.txt filename in WS-FILENAME
           MOVE USERNAME TO WS-FILENAME
           *> Clear the old profile file
           OPEN OUTPUT PROFILE-FILE
           CLOSE PROFILE-FILE

             *> Now reopen for writing fresh data
           OPEN OUTPUT PROFILE-FILE

           *> First Name (required)
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
           MOVE SPACES TO PF-REC
           STRING "First Name: " DELIMITED BY SIZE
                  WS-FIELD       DELIMITED BY SIZE
                  INTO PF-REC
           WRITE PF-REC

           *> Last Name (required)
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
           MOVE SPACES TO PF-REC
           STRING "Last Name: " DELIMITED BY SIZE
                  WS-FIELD      DELIMITED BY SIZE
                  INTO PF-REC
           WRITE PF-REC

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
                     MOVE FUNCTION TRIM(PF-REC) TO MSG
                     PERFORM WRITE-OUTPUT
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
       USER-MENU.
           MOVE "Choose: 1=Search job, 2=Learn skill, 3=Create/Edit My Profile, " -
           "4=Output Profile, 5=Search Profile, 6=Return" TO MSG
           PERFORM WRITE-OUTPUT.
           READ INPUTFILE AT END EXIT PARAGRAPH
              NOT AT END MOVE FUNCTION NUMVAL(INPUT-REC) TO OPTION-CHOICE
           END-READ
           MOVE OPTION-CHOICE TO MSG 
           PERFORM WRITE-OUTPUT
           EVALUATE OPTION-CHOICE
              WHEN 1
                 MOVE "Under Construction" TO MSG
                 PERFORM WRITE-OUTPUT
              WHEN 2
                 MOVE "Under Construction" TO MSG
                 PERFORM WRITE-OUTPUT
              WHEN 3
                 PERFORM DO-PROFILE
              WHEN 4
                 PERFORM DISPLAY-PROFILE
              WHEN 5
                 PERFORM SEARCH-PROFILE
              WHEN 6
                 EXIT PARAGRAPH
              WHEN OTHER
                 MOVE "Invalid option, you must select a number 1-6" TO MSG
                 PERFORM WRITE-OUTPUT
           END-EVALUATE.

       *> Module to display the complete profile of the logged-in user
       DISPLAY-PROFILE.
           OPEN INPUT PROFILE-FILE
           MOVE "Displaying your profile:" TO MSG
           PERFORM WRITE-OUTPUT
           PERFORM UNTIL PROFILE-EOF = "Y"
               PERFORM PRINT-PROFILE
           END-PERFORM
           CLOSE PROFILE-FILE.

       *> Module to search for a profile by name
       SEARCH-PROFILE.
           MOVE "Enter the full name of the person you are looking for:" TO MSG
           PERFORM WRITE-OUTPUT
           READ INPUTFILE AT END EXIT PARAGRAPH
              NOT AT END MOVE FUNCTION TRIM(INPUT-REC) TO WS-FIELD
           END-READ

           MOVE "Searching for profile..." TO MSG
           PERFORM WRITE-OUTPUT

           *> Open the accounts file to search for the username
           OPEN INPUT ACCOUNTS
           MOVE "N" TO VALID-LOGIN
           PERFORM UNTIL 1=0
              READ ACCOUNTS NEXT RECORD
                 AT END EXIT PERFORM
                 NOT AT END
                    UNSTRING ACCT-REC
                       DELIMITED BY ALL " "
                       INTO ACCT-USER ACCT-PASS
                    END-UNSTRING
                    IF WS-FIELD = FUNCTION TRIM(ACCT-USER)
                       MOVE "Y" TO VALID-LOGIN
                       EXIT PERFORM
                    END-IF
              END-READ
           END-PERFORM
           CLOSE ACCOUNTS

           IF VALID-LOGIN = "Y"
              MOVE WS-FIELD TO WS-FILENAME
              STRING ".txt" DELIMITED BY SIZE INTO WS-FILENAME
              OPEN INPUT PROFILE-FILE
              MOVE "Profile found. Displaying profile:" TO MSG
              PERFORM WRITE-OUTPUT
              PERFORM UNTIL PROFILE-EOF = "Y"
                  PERFORM PRINT-PROFILE
              END-PERFORM
              CLOSE PROFILE-FILE
           ELSE
              MOVE "Profile not found." TO MSG
              PERFORM WRITE-OUTPUT
           END-IF.

       

