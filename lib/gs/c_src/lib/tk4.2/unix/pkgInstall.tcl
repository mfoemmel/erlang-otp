#!/bin/sh
# The next line restarts using tclsh7.6 \
exec tclsh7.6 $0 ${1+"$@"}

#
# pkgInstall.tcl
#
# script to install a pkgIndex.tcl file
# into <exec-prefix>/lib
#
# Written by:	Jan Nijtmans
#		NICI (Nijmegen Institute of Cognition and Information)
#		email: nijtmans@nici.kun.nl
#		url:   http://www.cogsci.kun.nl/~nijtmans/

if {[llength $argv] != 1} {
    puts stderr "usage: pkgInstall.tcl <prefix>"
    exit 1
}

set dir [lindex $argv 0]
set header ""

set f [open pkgIndex.tcl r]
set line [gets $f]
while {![regexp "package ifneeded " $line] && ![eof $f]} {
    append header "${line}\n"
    set line [gets $f]
}
set pkgIndex "${line}\n"
while {![info complete $pkgIndex] && ![eof $f]} {
    set line [gets $f]
    append pkgIndex "${line}\n"
}
close $f
regexp {package ifneeded ([A-Z][a-z_]+) ([0-9\.]+) } $pkgIndex \
		trash Pkgname version
set separator "if \{\[info tclversion\] < $tcl_version\} return\n"

if {![catch {set f [open [file join $dir pkgIndex.tcl] r]}]} {
    set line {}
    while {![string match "*package ifneeded*" $line] &&
	    ![string match "if*" $line] && ![eof $f]} {
	set line [gets $f]
    }
    while {![eof $f]} {
	set chunk "${line}\n"
	while {![info complete $chunk] && ![eof $f]} {
	    set line [gets $f]
	    append chunk "${line}\n"
	}
	if [string match "*package ifneeded*" $chunk] {
	    if {![string match "package ifneeded $Pkgname $version *" $chunk]} {
		append header $chunk
	    }
	} elseif [string match "if*" $chunk] {
	    regsub -all "\\\$tcl_version" $chunk "\[info tclversion\]" chunk
	    if {[string match "if \{\\\[info tclversion\\\] < $tcl_version\} return*" $chunk]} {
		set separator ""
	    }
	    append header $chunk
	}
	set line [gets $f]
    }
    close $f
}

set f [open [file join $dir pkgIndex.tcl] w]
puts -nonewline $f $header
puts -nonewline $f $separator
puts -nonewline $f $pkgIndex
flush $f
close $f
