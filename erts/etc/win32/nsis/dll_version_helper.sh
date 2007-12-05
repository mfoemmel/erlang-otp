#! /bin/sh
# This little helper digs out the current version of microsoft CRT
# by compiling hello world and "parsing" the manifest file...

# To debug using a fake version:

# echo "8.0.50727.763"
# exit 0

cat > hello.c <<EOF
#include <stdio.h>

int main(void)
{
    printf("Hello world\n");
    return 0;
}

EOF
cl /MD hello.c > /dev/null 2>&1
if [ '!' -f hello.exe.manifest ]; then
    echo "This compiler does not generate manifest files - OK if using mingw" >&2
    exit 0
fi
VERSION=`grep '<assemblyIdentity' hello.exe.manifest | sed 's,.*version=.\([0-9\.]*\).*,\1,g' | grep -v '<'`
rm -f hello.c hello.obj hello.exe hello.exe.manifest
if [ -z "$VERSION" ]; then
    exit 1
fi
echo $VERSION
exit 0
