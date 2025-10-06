# COBOL Program Control Flow

This document explains the control flow of the COBOL account system program.

---

## 🏠 Main Menu

**Output:**
```
What would you like to do?
Type LOGIN to sign in to an existing account
Type CREATE to make a new account
```

- **If input = LOGIN** → go to `DO-LOGIN`  
- **If input = CREATE** → go to `DO-CREATE`  
- **Else** →  
  Output: `Invalid choice, must be LOGIN or CREATE` → back to Main Menu  

---

## 🔐 LOGIN (DO-LOGIN)

- Output: `Enter username:`  
- Read username  
- Output: `Enter password:`  
- Read password  

→ **If match in accounts.txt**  
   - Output: `You have successfully logged in`  
   - Go to **USER MENU**  

→ **Else**  
   - Output: `Incorrect username/password, please try again`  
   - Back to Main Menu  

---

## 🆕 CREATE (DO-CREATE)

- Output: `Enter new username:`  
- Read username  

→ **If 5 accounts already exist**  
   - Output: `All permitted accounts created, come back later`  
   - Back to Main Menu  

→ **If username already exists**  
   - Output: `Username taken`  
   - Back to Main Menu  

- Output: `Enter password:`  
- Read password  

→ **If password invalid**  
   - Output: `Password does not meet requirements`  
   - Back to Main Menu  

→ **If password valid**  
   - Append to accounts.txt  
   - Output: `Account created successfully. Please LOGIN to sign in.`  
   - Back to Main Menu  

---

## 📋 USER MENU

**Output:**
```
Choose: 1=Search job, 2=Learn skill, 3=Create/Edit My Profile,
4=Output Profile, 5=Search Profile, 6=Return,
7=View Pending Requests, 8=View My Network
```

- **1 – Search job** → Output `Under Construction`  
- **2 – Learn skill** → Show 1-5 skill list, read a choice, then output `Under Construction`  
- **3 – Create/Edit My Profile** → Jump to profile workflow (`DO-PROFILE`)  
- **4 – Output Profile** → Display profile file, list connections via `LIST-MY-CONNECTIONS`, then return  
- **5 – Search Profile** → Run `SEARCH-PROFILE`, optionally send or queue connection requests  
- **6 – Return** → Exit the user menu  
- **7 – View Pending Requests** → Invoke `LIST-PENDING-REQUESTS` to accept/reject inbound requests  
- **8 – View My Network** → Call `VIEW-MY-NETWORK` (copybook) to list established connections or `(none)`  

- **Anything else** → Output `Invalid option, you must select a number 1-8`  

---

## ✅ Summary

- The program always returns to the **Main Menu** after completing any branch.  
- Input and output are strictly line-driven, following the prompts.  
- Accounts are persistent across runs via `data/accounts.txt`.  
- Runtime files (input scripts, connections, profiles, logs) live under the `data/` directory.  
- The program sets `RETURN-CODE` to `0` before exiting.  
