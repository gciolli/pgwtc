# example input:
# Time=4800  Note on, chan=1 pitch=79 vol=90
/^Time=[0-9]\+  Note /s/^Time=\([0-9]\+\)  Note on, chan=\([0-9]\+\) pitch=\([0-9]\+\) vol=\([0-9]\+\)$/@BWV@\t\1\t\2\t\3\t\4/gp
