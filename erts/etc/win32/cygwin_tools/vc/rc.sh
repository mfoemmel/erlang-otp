#! /bin/sh
# set -x
# Save the command line for debug outputs
SAVE="$@"
CMD=""
OUTPUT_FILENAME=""

# Find the correct rc.exe. This could be done by the configure script,
# But as we seldom use the resource compiler, it might as well be done here...
RCC=""
save_ifs=$IFS
IFS=:
for p in $PATH; do 
    if [ -f $p/rc.exe ]; then 
	if [ -n "`$p/rc.exe -? 2>&1 | grep -i "resource compiler"`" ]; then 
	    RCC=$p/rc.exe
	fi
    fi
done
IFS=$save_ifs

if [ -z "$RCC" ]; then
    echo 'rc.exe not found!' >&2
    exit 1
fi

while test -n "$1" ; do
    x="$1"
    case "$x" in
	-o)
	    shift
	    MPATH=`cygpath -m $1`;
	    OUTPUT_FILENAME="$MPATH";;
	-o/*)
	    y=`echo $x | sed 's,^-[Io]\(/.*\),\1,g'`;
	    MPATH=`cygpath -m $y`;
	    OUTPUT_FILENAME="$MPATH";;
	-I)
	    shift
	    MPATH=`cygpath -m $1`;
	    CMD="$CMD -I\"$MPATH\"";;
	-I/*)
	    y=`echo $x | sed 's,^-[Io]\(/.*\),\1,g'`;
	    MPATH=`cygpath -m $y`;
	    CMD="$CMD -I\"$MPATH\"";;
	/*)
	    MPATH=`cygpath -m $x`;
	    CMD="$CMD \"$MPATH\"";; 
	*)
	    y=`echo $x | sed 's,",\\\",g'`;
	    CMD="$CMD \"$y\"";;
    esac
    shift
done
p=$$
if [ -n "$OUTPUT_FILENAME" ]; then
    CMD="-Fo$OUTPUT_FILENAME $CMD"
fi
if [ "X$RC_SH_DEBUG_LOG" != "X" ]; then
    echo rc.sh "$SAVE" >>$RC_SH_DEBUG_LOG
    echo rc.exe $CMD >>$RC_SH_DEBUG_LOG
fi
eval $RCC "$CMD"  >/tmp/rc.exe.${p}.1 2>/tmp/rc.exe.${p}.2
RES=$?
tail +2 /tmp/rc.exe.${p}.2 >&2
cat /tmp/rc.exe.${p}.1
rm -f /tmp/rc.exe.${p}.2 /tmp/rc.exe.${p}.1
exit $RES
