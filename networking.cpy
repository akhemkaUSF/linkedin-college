      *> Networking routines extracted from control.cob for reuse and maintenance.
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

                       MOVE "Accept this request?  Y = Yes,  N = No,  Enter = Skip" TO MSG
                       PERFORM WRITE-OUTPUT
                       READ INPUTFILE AT END MOVE SPACE TO RESP-CHAR
                          NOT AT END MOVE FUNCTION TRIM(INPUT-REC)(1:1) TO RESP-CHAR
                       END-READ
                       EVALUATE TRUE
                           WHEN RESP-CHAR = "Y" OR RESP-CHAR = "y"
                              MOVE FUNCTION TRIM(PENDING-SENDER) TO PENDING-SENDER
                              MOVE FUNCTION TRIM(USERNAME)       TO PENDING-RECIP
                              CLOSE CONNECTIONS
                              PERFORM ACCEPT-REQUEST-DIRECT
                              OPEN INPUT CONNECTIONS
                           WHEN RESP-CHAR = "N" OR RESP-CHAR = "n"
                              MOVE FUNCTION TRIM(PENDING-SENDER) TO PENDING-SENDER
                              MOVE FUNCTION TRIM(USERNAME)       TO PENDING-RECIP
                              CLOSE CONNECTIONS
                              PERFORM REJECT-REQUEST-DIRECT
                              OPEN INPUT CONNECTIONS
                           WHEN RESP-CHAR = SPACES
                              CONTINUE
                        END-EVALUATE                
                     END-IF
               END-READ
            END-PERFORM.
           CLOSE CONNECTIONS
           IF REQ-FOUND NOT = "Y"
              MOVE "(none)" TO MSG
              PERFORM WRITE-OUTPUT
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

      *> Caller rejects a request and it is removed from the pending requests
      REJECT-REQUEST-DIRECT.

         PERFORM REMOVE-PENDING
         MOVE "Connection rejected--request removed from queue." TO MSG
         PERFORM WRITE-OUTPUT
         EXIT PARAGRAPH.

      *> Network viewing routines extracted for reuse across menus.
       VIEW-MY-NETWORK.
           MOVE "My Network:" TO MSG
           PERFORM WRITE-OUTPUT
           PERFORM LIST-MY-CONNECTIONS
           EXIT PARAGRAPH.

       LIST-MY-CONNECTIONS.
           MOVE "N" TO CONN-ANY
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
                       MOVE "Y" TO CONN-ANY
                       PERFORM GET-DISPLAY-NAME-FOR-OTHER
                       MOVE FUNCTION TRIM(WS-DISPLAY-NAME) TO MSG
                       PERFORM WRITE-OUTPUT
                    END-IF
              END-READ
           END-PERFORM
           CLOSE NETWORK
           IF CONN-ANY NOT = "Y"
              MOVE "(none)" TO MSG
              PERFORM WRITE-OUTPUT
           END-IF
           EXIT PARAGRAPH.

       LIST-CONNECTIONS-FOR-USER.
           MOVE "N" TO CONN-ANY
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
                       MOVE "Y" TO CONN-ANY
                       PERFORM GET-DISPLAY-NAME-FOR-OTHER
                       MOVE FUNCTION TRIM(WS-DISPLAY-NAME) TO MSG
                       PERFORM WRITE-OUTPUT
                    END-IF
              END-READ
           END-PERFORM
           CLOSE NETWORK
           IF CONN-ANY NOT = "Y"
              MOVE "(none)" TO MSG
              PERFORM WRITE-OUTPUT
           END-IF
           EXIT PARAGRAPH.

       GET-DISPLAY-NAME-FOR-OTHER.
           MOVE "N" TO NAME-FOUND
           MOVE SPACES TO WS-DISPLAY-NAME
           MOVE OTHER-USER TO PENDING-SENDER
           PERFORM MAKE-DISPLAY-NAME

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
