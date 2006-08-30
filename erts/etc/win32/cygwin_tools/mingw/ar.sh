#! /bin/sh
CMD=""
if [ -z "$MINGW_EXE_PATH" ]; then
    echo "You have to set MINGW_EXE_PATH to run ar.sh" >&2
    exit 1
fi
while test -n "$1" ; do
    x="$1"
    case "$x" in
	-o|-out:)
	    shift
	    case "$1" in
		/*)
		    MPATH=`cygpath -m $1`;;
		 *)
		    MPATH=$1;;
	    esac
	    CMD="rcv \"$MPATH\" $CMD";; 
	-out:*)
	    y=`echo $x | sed 's,^-out:\(.*\),\1,g'`;
	    MPATH=`cygpath -m $y`;
	    CMD="rcv \"$MPATH\" $CMD";;
	-o*)
	    y=`echo $x | sed 's,^-o\(.*\),\1,g'`;
	    MPATH=`cygpath -m $y`;
	    CMD="rcv \"$MPATH\" $CMD";; 
	/*)
	    MPATH=`cygpath -m $x`;
	    CMD="$CMD \"$MPATH\"";; 
	*)
	    y=`echo $x | sed 's,",\\\",g'`;
	    CMD="$CMD \"$y\"";;
    esac
    shift
done

eval $MINGW_EXE_PATH/ar.exe $CMD
