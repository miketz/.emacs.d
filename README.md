# .emacs.d

Storing my .emacs.d folder online to sync across computers.

# External dependencies

Maybe optional but nice to have.

- git
- ripgrep, rg
- fd
- enchant spell checker (*used by jinx*)

# Git incantations
Create a new github repo from an existing local repo:

```bash
git remote add origin https://github.com/miketz/.emacs.d.git
git push -u origin master
```

Download from github to a new computer:

```bash
git clone https://github.com/miketz/.emacs.d.git
```

Get latest changes from github:

```bash
git pull origin master
```

Push local changes up to github:

```bash
git push origin master
```

Revert changes to modified files.

```bash
git reset --hard
```

Remove all untracked files and directories.

```bash
git clean -fd
```