#! /bin/sh
# Note! This shellscript expects to be run in a cygwin environment,
# it converts erlc command lines to native windows erlc commands, which
# basically means running the command cygpath on whatever is a path...

CMD=""
CLASSPATH=`cygpath -m -p $CLASSPATH`
export CLASSPATH
#echo "CLASSPATH=$CLASSPATH"
SAVE="$@"
while test -n "$1" ; do
    x="$1"
    case "$x" in
	-I/*|-o/*|-d/*)
	    y=`echo $x | sed 's,^-[Iod]\(/.*\),\1,g'`;
	    z=`echo $x | sed 's,^-\([Iod]\)\(/.*\),\1,g'`;
	    #echo "Foooo:$z"
	    MPATH=`cygpath -m $y`;
	    CMD="$CMD -$z\"$MPATH\"";; 
	-d|-I|-o)
	    shift;
	    MPATH=`cygpath -m $1`;
	    CMD="$CMD $x $MPATH";; 
	/*)
	    #echo "absolute:"$x;
	    MPATH=`cygpath -m $x`;
	    CMD="$CMD \"$MPATH\"";; 
	*)
	    y=`echo $x | sed 's,",\\\",g'`;
	    CMD="$CMD \"$y\"";;
    esac
    shift
done
#echo javac.exe $CMD
eval javac.exe $CMD
