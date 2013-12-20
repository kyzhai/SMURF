#!/bin/bash

SMURF="./toplevel.native"
TESTDIR="./tests/interp-tests/rtownsend"

# Set time limit for all operations
ulimit -t 30

globallog=testall.log
rm -f $globallog
error=0
globalerror=0

keep=0

Usage() {
    echo "Usage: testall.sh [options] [.sm files]"
    exit 1
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

#    echo $1 >&1
    basename=`echo $1 | sed 's/.*\\///
                             s/.sm//'`
    reffile=`echo $1 | sed 's/.sm$//'`
    basedir="`echo $1 | sed 's/\/[^\/]*$//'`/." # remove the last '/' in the string

#    echo 1>&2
#    echo "###### Testing $basename" 1>&2

    generatedfiles=""

    generatedfiles="$generatedfiles ${basename}.out" &&
    if [ $SMURF == "./toplevel.native" ]; then
#        Run "$SMURF" $1 "-o ${basedir}/exp/${basename}.midi >& ${basedir}/${basename}.out" 2>&1
        Run "$SMURF" $1 "-o ${basedir}/exp/${basename}.midi" 2>&1
    else 
        Run "$SMURF" "<" $1 ">& ${basedir}/exp/${basename}.out" 2>&1
    fi 

    # Report the status and clean up the generated files

    if [ $error -eq 0 ] ; then
    if [ $keep -eq 0 ] ; then
        rm -f $generatedfiles
    fi
#echo "OK"
#    echo "###### SUCCESS" 1>&2
    else
#    echo "###### FAILED" 1>&2
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

exit $globalerror
