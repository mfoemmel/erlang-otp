#! /bin/bash
# Create a local init-file for erlang in the build environment.
if [ -z "$1" ]; then
	echo "error: $0: No rootdir given"
 	exit 1
else
    RDIR=$1
fi
if [ -z "$2" ]; then
	echo "error: $0: No bindir given"
 	exit 1
else
    BDIR=$2
fi

DRDIR=`(cygpath -d $RDIR 2>/dev/null || cygpath -w $RDIR) | sed 's,\\\,\\\\\\\\,g'`
DBDIR=`(cygpath -d $BDIR 2>/dev/null || cygpath -w $BDIR) | sed 's,\\\,\\\\\\\\,g'`


cat > $RDIR/bin/erl.ini <<EOF
[erlang]
Bindir=$DBDIR
Progname=erl
Rootdir=$DRDIR
EOF

