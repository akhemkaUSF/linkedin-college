# COBOL Control Flow Demo

This repository contains a COBOL program (`control.cob`) that simulates a simple account system with login and account creation logic.  
- **Accounts are persisted** in `accounts.txt`  
- **User input** is read from `user_input.txt`  
- **Logs** are written to `output_log.txt`  

---

## 📦 Getting Started

Clone this repository to your local machine:

```bash
git clone <REPO_LINK_HERE>
cd controlflow-demo
```

---

## 🌱 Creating a New Branch

Always work on a separate branch rather than pushing changes directly to `main`.

```bash
git checkout -b feature/my-changes
```

Replace `feature/my-changes` with a descriptive branch name.

---

## ⚙️ Building the Program

This project uses [GnuCOBOL](https://gnucobol.sourceforge.io/).  

### Compile
```bash
cobc -x -free control.cob -o controlflow
```

### Run
```bash
./controlflow
```

The program will:
- Read commands from `user_input.txt`
- Append new accounts into `accounts.txt`
- Log system messages into `output_log.txt`

---

## ⬆️ Uploading Your Work

Once you’ve made changes and tested them:

```bash
git add .
git commit -m "Describe the changes you made"
git push origin feature/my-changes
```

Then open a Pull Request on GitHub to merge your branch into `main`.

---

## 📂 File Overview

- `control.cob` — main COBOL program
- `user_input.txt` — simulated user commands (input)
- `output_log.txt` — program logs (output)
- `accounts.txt` — persistent account storage

---

## 📝 Sample Input

Here is a simple `user_input.txt` you can use to test the program:

```
CREATE
alice
Password1!
LOGIN
alice
Password1!
3
```

This will:
1. Create an account for `alice` with password `Password1!`.
2. Log into the account.
3. Return to the main menu.
