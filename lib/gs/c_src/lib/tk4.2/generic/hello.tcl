#!/bin/sh
# The next line restarts using wish4.2 \
exec wish4.2 $0 ${1+"$@"}

# List here all packages used in this file
package require Tcl 7.6
package require Tk 4.2

# Here the Tk-script starts:

button .b -text "Hello, World" -command exit
pack .b
