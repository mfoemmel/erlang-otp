# namesp.tcl --
#
# Small namespace facility for Tcl7.6+/Tk4.2+. It contains
# a replacements of the "proc" commands which enables them
# to be used with namespaces. Also it contains the functions
# "static" and "namespace"
#
# Written by:
#
#	Jan Nijtmans
#	NICI (Nijmegen Institute of Cognition and Information)
#	email: nijtmans@nici.kun.nl
#	url:   http://www.cogsci.kun.nl
#
# original idea from Scott Hess <http://www.winternet.com/~shess/>

# Only use this file if the "@scope" command doesn't exist
# (Itcl 2.1 has the namespace command built in already)

if [string compare [uplevel #0 info commands @scope] {}] {
    return
}

set ___namespace {}

# proc --
# A replacement for the built-in "proc" command, which
# registers the proc not only under the given name, but also
# under all possible alternative names. For example, if the
# proc "x" is defined in namespace "a b", it is accessible
# in 3 different names: "x", "b x" and "a b x". Also it
# sets "info namespace" in the body such that inside the
# proc the current namespace can be determined.
#
# Arguments:
# name -		Name
# parms -		Arguments
# body -		Body

if {![string compare [info commands tcl__proc] {}]} {
    rename proc tcl__proc
}

tcl__proc proc {args} {
    if {[llength $args]!=3} {
	error "wrong # args: should be \"proc name args body\""
    }
    set namespace [uplevel set ___namespace]
    set name [lindex $args 0]
    lappend namespace $name
    set parms [lindex $args 1]
    set body "set ___namespace \{$namespace\} ;[lindex $args 2]"
    tcl__proc $name $parms $body
    for {set i [expr [llength $namespace]-2]} {$i>=0} {incr i -1} {
	set name "[lindex $namespace $i] $name"
	tcl__proc $name $parms $body
    }
}

# namespace --
# Execute code in another namespace. If the
# namespace doesn't exist yet, it will be
# created.
#
# Arguments:
# name -		Name of the namespace
# body -		Code to be executed in the

proc namespace {name args} {
    global auto_namespace
    set namespace [uplevel set ___namespace]
    set ___namespace $namespace
    lappend namespace $name
    if [info exists auto_namespace($name)] {
	foreach {origin commands} $auto_namespace($name) {
	    foreach option $commands "
		if \[tcl string compare \[info commands \"$name \$option\"\] \"$name \$option\"\] \{
		tcl__proc \"$name \$option\" args \"return \\\[eval $origin $name \$option \\\$args\\\]\"
		\}
	    "
	}
    }
    proc $name {option args} "if \[tcl string compare \[info command \"$namespace \$option\"\] \{\}\] \{return \[uplevel \\\{$namespace \$option\\\} \$args\]\} elseif \[tcl string compare \[info command \"$namespace \"\] \{\}\] \{return \[uplevel \\\{$namespace \\\} \$option \$args\]\} else \{error \"bad option \\\"\$option\\\": should be \[foreach option \[info commands \"$namespace *\"\] {lappend list \[lindex \$option 1\],}; set index \[tcl string last \{ \} \[set list \[lsort \$list\]\]\]; tcl string trimright \[tcl string range \$list 0 \$index\]or\[string range \$list \$index end\] \{,\}\]\"\}"
    set ___namespace $namespace
    foreach arg $args {
	eval $arg
    }
}

# static --
# Define static variables.
#
# Arguments:
# varname -		Variable name

proc static {args} {
    set namespace [uplevel set ___namespace]
    foreach var $args {
	uplevel upvar #0 \{$namespace $var\} \{$var\}
    }
}
