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
Choose: 1=Search job, 2=Learn skill, 3=Return
```

- **If input = 1**  
   Output: `Under Construction` → Back to Main Menu  

- **If input = 2**  
   Output:  
   ```
   Pick a skill (1-5):
   1. COBOL Basics
   2. File Handling
   3. Data Validation
   4. Debugging Techniques
   5. System Integration
   ```  
   → Output: `"You selected <skill> (under construction)"` → Back to Main Menu  

- **If input = 3**  
   Output: `Returning to main menu` → Back to Main Menu  

- **Else**  
   Output: `Invalid option` → Back to Main Menu  

---

## ✅ Summary

- The program always returns to the **Main Menu** after completing any branch.  
- Input and output are strictly line-driven, following the prompts.  
- Accounts are persistent across runs via `accounts.txt`.  
