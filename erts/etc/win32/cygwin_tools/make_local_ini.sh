#! /bin/bash
# Create a local init-file for erlang in the build environment.
if [ -z "$1" ]; then
    if [ -z $ERL_TOP ]; then
	echo "error: $0: No rootdir available"
 	exit 1
    else
	RDIR=$ERL_TOP
    fi
else
    RDIR=$1
fi

DDIR=`(cygpath -d $RDIR 2>/dev/null || cygpath -w $RDIR) | sed 's,\\\,\\\\\\\\,g'`


cat > $RDIR/bin/erl.ini <<EOF
[erlang]
Bindir=$DDIR\\\\bin\\\\win32
Progname=erl
Rootdir=$DDIR
EOF

