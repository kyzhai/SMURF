#!/bin/bash

SMURF="./semantic-test.byte"
TESTDIR="./tests/semantic-tests"

# Set time limit for all operations
ulimit -t 30

globallog=testall.log
rm -f $globallog
error=0
globalerror=0
numtests=0
numpass=0
keep=0
delete=0

Usage() {
    echo "Usage: test-semantic.sh [options] [.sm files]"
    exit 1
}

SignalError() {
    if [ $error -eq 0 ] ; then
    echo "FAILED"
    error=1
    fi
    echo "  $1"
}

# Compare <outfile> <reffile> <difffile>
# Compares the outfile with reffile.  Differences, if any, written to difffile
Compare() {
    generatedfiles="$generatedfiles $3"
    echo diff -b $1 $2 ">" $3 1>&2
    diff -b "$1" "$2" > "$3" 2>&1 || {
    SignalError "$1 differs"
    echo "FAILED $1 differs from $2" 1>&2
    }
}

# Run <args>
# Report the command, run it, and report any errors
Run() {
    echo $* 2>&1
    eval $* || {
    echo -e "[FAILED] $1 failed on $*\n" 
    return 1
    }
}

Check() {

    error=0

    numtests=$((numtests+1))
#    echo $1 >&1
    basename=`echo $1 | sed 's/.*\\///
                             s/.sm//'`
    reffile=`echo $1 | sed 's/.sm$//'`
    basedir="`echo $1 | sed 's/\/[^\/]*$//'`/." # remove the last '/' in the string

    echo 1>&2
    echo "Testing $basename:" 1>&2

    generatedfiles=""

    generatedfiles="$generatedfiles ${basedir}/${basename}.out" &&
    if [ $SMURF == "./toplevel.byte" ]; then
        Run "$SMURF" $1 ">& ${basedir}/${basename}.out" 2>&1
    else 
        Run "$SMURF" "<" $1 ">& ${basedir}/${basename}.out" 2>&1
    fi 
    Compare "${basedir}/${basename}.out" "${basedir}/exp/${basename}.out" "${basedir}/${basename}.diff"

    # Report the status and clean up the generated files

    if [ $error -eq 0 ] ; then
    if [ $keep -eq 0 ] ; then
        rm -f $generatedfiles
    fi
#    echo "OK"
    numpass=$((numpass+1))
    echo "\033[34m${basename}: \033[32;1mPASS\033[0m" 1>&2
    else
    if [ $delete -eq 1 ] ; then
        rm -f $generatedfiles
    fi
    echo "\033[34m${basename}: \033[31;1mFAIL\033[0m" 1>&2
    globalerror=$error
    fi
}

while getopts kdpsh c; do
    case $c in
    k) # Keep intermediate files
        keep=1
        ;;
    h) # Help
        Usage
        ;;
    d) # delete intermediate files on fail
        delete=1 
        ;;
    esac
done

shift `expr $OPTIND - 1`

if [ $# -ge 1 ]
then
    files=$@
else
    files=`find $TESTDIR -name "*.sm" | sort`
fi

for file in $files
do
    case $file in
    *)
        Check $file 
        ;;
    esac
done
    echo "$*\n"
    echo "\033[1mNumber of semantic tests ran: \033[0m $numtests" 1>&2
    echo "\033[1mNumber of semantic tests pass: \033[0m $numpass" 1>&2
exit $globalerror
