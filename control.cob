       IDENTIFICATION DIVISION.
       PROGRAM-ID. CONTROLFLOW.   *> PROGRAM-ID gives the program its name (CONTROLFLOW)

       ENVIRONMENT DIVISION. *> big section where we describe our machine environment (files, devices, terminals, etc)
       INPUT-OUTPUT SECTION. *> declare external files -- we tell COBOL what files exist and how we control them
       FILE-CONTROL. *> starts the list of file declarations. we write SELECT statements to connect logical names in our program to actual files on disk
           SELECT INPUTFILE ASSIGN TO "user_input.txt" *> reads simulated user input (user_input.txt)
              ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUTPUTFILE ASSIGN TO "output_log.txt" *>writes logs (output_log.txt)
              ORGANIZATION IS LINE SEQUENTIAL. *> each record is a line of text
           SELECT ACCOUNTS ASSIGN TO "accounts.txt" *> stores accounts persistently (accounts.txt)
              ORGANIZATION IS LINE SEQUENTIAL
              FILE STATUS IS ACC-FS. *>gives us a way to check if opening the file succeeded

       DATA DIVISION *> we describe all the data the program can use -- the files, variables, and structure and size of each piece of data
       FILE SECTION. *> we're defining the files in this section
       FD  INPUTFILE. *> FD is a file description. Marks the start of a record layout for a file we declared earlier in the FILE-CONTROL section
       01  INPUT-REC            PIC X(100). *> defines type and size: alphanumeric, 100 characters long. we start the line 01 because it's one complete record

       FD  OUTPUTFILE. *> start of record layout for output file
       01  OUT-REC              PIC X(120). *> each line is alphanumeric, 100 characters

       FD  ACCOUNTS.
       01  ACCT-REC             PIC X(100). *>each line is alphanumeric, 100 characters

       WORKING-STORAGE SECTION. *> defines program variables in memory
       77  ACC-FS               PIC XX VALUE SPACES.  *> file status for ACCOUNTS. we use 77 because it's a standalone variable

       77  EOF-FLAG             PIC X  VALUE "N". *> END of File flag variable. "N" is the initial value (since we're not at the end of the file) 
       77  USERNAME             PIC X(20).
       77  PASSWORD             PIC X(20).
       77  VALID-LOGIN          PIC X  VALUE "N". *> password validation. doesn't actually becmoe "Y" until we validate the password
       77  ACCT-COUNT           PIC 9  VALUE 0. *> number of accounts accounts.txt
       77      OPTION-CHOICE         PIC 9  VALUE 0. *> option selection from the user
       77  MSG                  PIC X(100). 

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
       
           *> Decide what to do based on the user’s choice
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
           MOVE "Choose: 1=Search job, 2=Learn skill, 3=Return" TO MSG
           *> print out the contents of MSG
           PERFORM WRITE-OUTPUT
           *> whatever number we select is the option we want 
           READ INPUTFILE AT END EXIT PARAGRAPH
              NOT AT END MOVE FUNCTION NUMVAL(INPUT-REC) TO OPTION-CHOICE
           END-READ

           *> function to evaluate the option they choose
           EVALUATE OPTION-CHOICE *> we use the same value for both the overaching options and the skills. no reason to store both at the same time
              WHEN 1
                 MOVE "Under Construction" TO MSG
                 PERFORM WRITE-OUTPUT
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
                 *> start over
                 MOVE "Returning to main menu" TO MSG
                 PERFORM WRITE-OUTPUT
              WHEN OTHER
                 MOVE "Invalid option, you must select a number 1-3" TO MSG
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

       WRITE-OUTPUT.
           MOVE MSG TO OUT-REC
           WRITE OUT-REC
           DISPLAY MSG.
