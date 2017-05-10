#!/bin/sh

# Regression testing script for Theatr
# Step through a list of files
#  Compile, run, and check the output of each expected-to-work test
#  Compile and check the error of each expected-to-fail test

PRE="./preprocessor.py"
THEATR="./theatr.native"
LLI="lli"
LLC="llc"
CC="cc"

Run() {
    eval $* || {
	SignalError "$1 failed on $*"
	return 1
    }
}

basename=`echo $1 | sed 's/.*\\///
                             s/.th//'`

Run "$PRE" $1 "${basename}.temp" &&
Run "$THEATR" "<" "${basename}.temp" ">" "${basename}.ll" &&    
Run "$LLC" "${basename}.ll" ">" "${basename}.s" &&
Run "$CC" "-pthread" "-o" "${basename}.exe" "${basename}.s" "queue.o" "filedwld.o" "-lcurl" &&
Run "./${basename}.exe"
