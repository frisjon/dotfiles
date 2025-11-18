#https://stackoverflow.com/questions/1251999/how-can-i-replace-each-newline-n-with-a-space-using-sed
sed ':a;N;$!ba;s/\n/ /g' filename
