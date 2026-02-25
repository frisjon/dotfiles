ssh config
```
.ssh 700
key.pub 644
key 600
```

emacs server in windows
[https://emacs.stackexchange.com/questions/35545/setting-up-emacsclient-on-ms-windows]
```
D:\emacs\emacs-29.1\bin\emacsclientw.exe -n -c -a ""
```
change `D:\emacs\emacs-29.1\` for wherever you installed your emacs in windows


wemacs script requires variable found in file `~/.my_secret`
```
cat "MY_SECRET_DNI=..." >>  ~/.my_secret
cat "export MY_SECRET_DNI" >> ~/.my_secret
```
