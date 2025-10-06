# COBOL Control Flow Demo

This repository contains a COBOL program (`control.cob`) that simulates a simple account system with login and account creation logic.  
- **Accounts are persisted** in `data/accounts.txt`  
- **User input** is read from `data/user_input.txt`  
- **Logs** are written to `data/output_log.txt`  

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
- Read commands from `data/user_input.txt`
- Append new accounts into `data/accounts.txt`
- Log system messages into `data/output_log.txt`
- Store pending requests, network links, and profiles under the `data/` directory
- Present the post-login menu across multiple lines so each line fits within the 120-character log record
- Exit with `RETURN-CODE` 0 on successful completion

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
- `networking.cpy` — shared networking (requests + connections) routines
- `data/user_input.txt` — simulated user commands (input)
- `data/output_log.txt` — program logs (output)
- `data/accounts.txt` — persistent account storage
- `data/connections.txt`, `data/network.txt`, `data/connections.tmp` — connection workflow data
- `data/profiles.idx` & `data/profiles/` — profile index plus individual profile files

---

## 📝 Sample Input

Here is a simple `data/user_input.txt` you can use to test the program:

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
