#!/bin/sh
# The next line restarts using tclsh7.6 \
exec tclsh7.6 $0 ${1+"$@"}

# List here all packages used in this file
package require Tcl 7.6

# Here the Tcl-script starts:
puts stdout "Hello, World"
