# ------------------------------------------------------------
# A wish init script to make it possible for
# Tcl/Tk and erlang to communicate.
# Written by Ola Samuelsson in August 1995
# © Ericsson Software Technology AB / Erlang Systems
# ------------------------------------------------------------

# Protocol:
# \1 =   it's an event
# \2 =   it's a reply for a call
# \3 =   it's a error reply for call
# \4 =   it's an error
# \5 =   stopbyte (end of message)


proc erlsend {w1 {w2 ""} {w3 ""} {w4 ""} {w5 ""} {w6 ""} {w7 ""} {w8 ""}} {
	set msg [concat $w1 $w2 $w3 $w4 $w5 $w6 $w7 $w8]
	puts -nonewline stdout "\1$msg\5"
	flush stdout
}

proc erlcall {w} {
    set errcode [catch $w result]
    if {$errcode == 0} {
	puts -nonewline stdout "\2$result\5"
	flush stdout
    } else {
	puts -nonewline stdout "\3$result\5"
	flush stdout
    }
}

proc erlexec {w} {
    set errcode [catch $w result]
    if {$errcode != 0} {
	erlerror $result
    }
}

proc erlerror {w} {
    puts -nonewline stdout "\4$w\5"
    flush stdout
}

wm withdraw .
set command ""

fileevent stdin active {
    set line [gets stdin]
    append command "$line\n"
    if [info complete "${command}\n"] {
	if [catch {eval $command} msg] {
	    puts stderr $msg
	} else {
	    if [string compare $msg {}] {
		puts stdout $msg
	    }
	}
	set command ""
    }
}



