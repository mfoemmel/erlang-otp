# ------------------------------------------------------------
# Some tcl scripts used from erlang
# By Ola Samuelsson in May 1995
# © Ericsson Software Technology / Erlang System

# ------------------------------------------------------------
# A Simple erlang Prompter Widget. 
# By Ola Samuelsson in May 1995
# © Ericsson Software Technology / Erlang System
#
# To be used with the gstk package, a graphical
# interface in Erlang.
# This simple shell is really a text widget with 
# an odd set of bindings.
# Create a Shell: 
#    shell .w
# Prompt a command line with:
#    shellPrompt .w Prompt
# Define your own instance bindings
# usually <Return> to get the text.
#
# get the text with:
#    shellGet .w
# and set the text with
#    shellSet .w "Hello There"
# put text before prompter (used in Erlang put_chars)
#    shellPut .w "before prompter"
#

proc shell {w} {
    text $w
    shellInit $w
    shellPrompt $w \n
    return $w
}

proc shellInit {w} {
    shellBind 
    bindtags $w [list Shell $w . all]
}    

proc shellBind {} {
    global tkpriv tk_strictMotif

    bind Shell <Enter> {}
    bind Shell <FocusIn> {}
    bind Shell <1> {
	tkShellButton1 %W %x %y
	%W tag remove sel 0.0 end
    }
    bind Shell <B1-Motion> {
	set tkPriv(x) %x
	set tkPriv(y) %y
	tkTextSelectTo %W %x %y
    }
    bind Shell <Double-1> {
	set tkPriv(selectMode) word
	tkTextSelectTo %W %x %y
	if [%W compare sel.first > endzone] {
	    %W mark set insert sel.first
	    %W see insert
	}
    }
    bind Shell <Triple-1> {
	set tkPriv(selectMode) line
	tkTextSelectTo %W %x %y
	if [%W compare sel.first > endzone] {
	    %W mark set insert sel.first
	    %W see insert
	}
    }
    bind Shell <ButtonRelease-1> {
	tkCancelRepeat
    }
    bind Shell <Left> {
	shellSetCursor %W [%W index {insert - 1c}]
    }
    bind Shell <Right> {
	shellSetCursor %W [%W index {insert + 1c}]
    }
    bind Shell <Shift-Left> {
	tkTextKeySelect %W [%W index {insert - 1c}]
    }
    bind Shell <Shift-Right> {
	tkTextKeySelect %W [%W index {insert + 1c}]
    }
    bind Shell <Control-Left> {
	shellSetCursor %W [%W index {insert - 1c wordstart}]
    }
    bind Shell <Control-Right> {
	shellSetCursor %W [%W index {insert wordend}]
    }
    bind Shell <Shift-Control-Left> {
	tkTextKeySelect %W [%W index {insert - 1c wordstart}]
    }
    bind Shell <Shift-Control-Right> {
	tkTextKeySelect %W [%W index {insert wordend}]
    }
    bind Shell <Tab> {
	tkTextInsert %W \t
	focus %W
	break
    }
    bind Shell <Shift-Tab> {
	# Needed only to keep <Tab> binding from triggering;  doesn't
	# have to actually do anything.
    }
    bind Shell <Control-Tab> {
	focus [tk_focusNext %W]
    }
    bind Shell <Control-Shift-Tab> {
	focus [tk_focusPrev %W]
    }
    bind Shell <Control-i> {
	tkTextInsert %W \t
    }
    bind Shell <Return> {
	tkTextInsert %W \n
    }
    bind Shell <Delete> {
	if {[%W tag nextrange sel 1.0 end] != ""} {
	    %W delete sel.first sel.last
	} else {
	    %W delete insert
	    %W see insert
	}
    }
    bind Shell <BackSpace> {
	if {[%W tag nextrange sel 1.0 end] != ""} {
	    %W delete sel.first sel.last
	} elseif [%W compare insert > {endzone + 1 char}] {
	    %W delete insert-1c
	    %W see insert
	}
    }
    bind Shell <Control-space> {
	%W mark set anchor insert
    }
    bind Shell <Select> {
	%W mark set anchor insert
    }
    bind Shell <Control-Shift-space> {
	set tkPriv(selectMode) char
	tkTextKeyExtend %W insert
    }
    bind Shell <Shift-Select> {
	set tkPriv(selectMode) char
	tkTextKeyExtend %W insert
    }
    bind Shell <Insert> {
	catch {tkTextInsert %W [selection get -displayof %W]}
    }
    bind Shell <KeyPress> {
	tkTextInsert %W %A
    }
    bind Shell <Alt-KeyPress> {# nothing }
    bind Shell <Meta-KeyPress> {# nothing}
    bind Shell <Control-KeyPress> {# nothing}
    bind Shell <Escape> {# nothing}
    bind Shell <Control-a> {
	shellSetCursor %W {endzone + 1 char}
    }
    bind Shell <Control-b> {
	shellSetCursor %W insert-1c
    }
    bind Shell <Control-d> {
	%W delete insert
    }
    bind Shell <Control-e> {
	shellSetCursor %W {end - 1 char}
    }
    bind Shell <Control-f> {
	shellSetCursor %W insert+1c
    }
    bind Shell <Control-k> {
	clipboard clear -displayof %W
	clipboard append -displayof %W [%W get insert {end - 1 char}]
	%W delete insert {end - 1 char}
    }
    bind Shell <Control-o> {
	%W insert insert \n
	%W mark set insert insert-1c
    }
    bind Shell <Meta-b> {
	shellSetCursor %W {insert - 1c wordstart}
    }
    bind Shell <Meta-d> {
	%W delete insert {insert wordend}
    }
    bind Shell <Meta-f> {
	shellSetCursor %W {insert wordend}
    }
    bind Shell <Meta-BackSpace> {
	%W delete {insert -1c wordstart} insert
    }
    bind Shell <Meta-w> {
	if {[selection own -displayof %W] == "%W"} {
	    clipboard clear -displayof %W
	    clipboard append -displayof %W [selection get -displayof %W]
	}
    } 
    bind Shell <Control-w> {
	if {[selection own -displayof %W] == "%W"} {
	    clipboard clear -displayof %W
	    clipboard append -displayof %W [selection get -displayof %W]
	    %W delete sel.first sel.last
	}
    }
    bind Shell <Control-y> {
	catch {
	    %W insert insert [selection get -displayof %W \
		    -selection CLIPBOARD]
	}
    }
    bind Shell <Control-h> {
	if [%W compare insert != 1.0] {
	    %W delete insert-1c
	    %W see insert
	}
    }
    bind Shell <2> {
	%W scan mark %x %y
	set tkPriv(x) %x
	set tkPriv(y) %y
	set tkPriv(mouseMoved) 0
    }
    bind Shell <B2-Motion> {
	if {(%x != $tkPriv(x)) || (%y != $tkPriv(y))} {
	    set tkPriv(mouseMoved) 1
	}
	if $tkPriv(mouseMoved) {
	    %W scan dragto %x %y
	}
    }
    bind Shell <ButtonRelease-2> {
	if !$tkPriv(mouseMoved) {
	    catch {
		%W insert insert [selection get -displayof %W]
		tkTextSeeInsert %W
	    }
	}
    }
    #rename shellBind {}
    set tkPriv(prevPos) {}
}


# ------------------------------------------------------------
# When button 1 is pressed we want to set the insert point
# which must be within the endzone. Otherwise we ignore it.
#
proc tkShellButton1 {w x y} {
    global tkPriv

    set tkPriv(selectMode) char
    set tkPriv(mouseMoved) 0
    set tkPriv(pressX) $x
    set pos @$x,$y
    $w mark set anchor $pos
    if [$w compare $pos > endzone] {
	$w mark set insert $pos
	$w see insert
    }
    if {[$w cget -state] == "normal"} {focus $w}
}

# ------------------------------------------------------------
# Prompt a command line.
# Define the new endzone.
# The endzone is the area where you are allowed to type text
# The Shell defines the endzone range from insert to end .
#
proc shellPrompt {w prompt} {
    global tkPriv

    $w mark set insert {end - 1 chars}
    $w mark set zone_start {end - 2 chars}
    $w insert insert $prompt
    $w mark set endzone {insert - 1 chars}
    $w tag remove sel 1.0 end
    $w see insert
    $w mark set zone_start {zone_start + 1 chars}
}

# ------------------------------------------------------------
# Insert text just before prompter
#
proc shellPut {w text} {
    $w insert zone_start $text
    $w see insert
}

# ------------------------------------------------------------
# Return the Endzone, e.g the prompted line of text
#
proc shellGet {w} {
    $w get {endzone + 1 chars} {end -1 chars}
}

# ------------------------------------------------------------
# Set text at endzone
#
proc shellSet {w text} {
    $w delete {endzone +1 chars} end
    tkTextInsert $w $text
    $w see insert
}
    
# ------------------------------------------------------------
# Set cursor is only allowed within the endzone.
# If we are before the endzone we set cursor to the
# beginning of the endzone.
#
proc shellSetCursor {w pos} {
    global tkPriv

    if [$w compare $pos == end] {
	set pos {end - 1 chars}
    }
    if [$w compare $pos <= endzone] {
	set pos {endzone + 1 chars}
    }
    $w mark set insert $pos
    $w tag remove sel 1.0 end
    $w see insert
}

# ------------------------------------------------------------
# end of shell_widget.tcl

# ------------------------------------------------------------
# Scrolled object.
# This is a script that creates a
# scrolled object of specified type
# and connects it with scrollbars.
#
# Give a name for the frame and you'll find
#   name.z, 
#   name.sy,
#   name.pad.sx
#   name.pad.it
#

proc so_create {type w} {
    set parent [frame $w] 
    eval {$type $parent.z -highlightt 0}
    $parent.z config -yscrollcommand [list $parent.sy set] \
	    -xscrollcommand [list $parent.pad.sx set]
    scrollbar $parent.sy -orient vertical  -takefocus 0 \
	    -command [list $parent.z yview]
    # create extra frame to hold pad
    frame $parent.pad
    scrollbar $parent.pad.sx -orient horizontal -takefocus 0 \
	    -command [list $parent.z xview]
    #create padding based on the scrollbars width
    set pad [expr [$parent.sy cget -width] + 2 * \
	    ([$parent.sy cget -bd] + \
	    [$parent.sy cget -highlightthickness])]
    frame $parent.pad.it -width $pad -height $pad
    # Arrange everything
    so_plain $parent
    return $parent
}

proc so_top_right {w} {
    so_unpack $w
    pack $w.pad -side top -fill x
    pack $w.pad.it -side right
    pack $w.pad.sx -side top -fill x
    pack $w.sy -side right -fill y
    pack $w.z -side left -fill both -expand true    
}

proc so_top_left {w} {
    so_unpack $w
    pack $w.pad -side top -fill x
    pack $w.pad.it -side left
    pack $w.pad.sx -side top -fill x
    pack $w.sy -side left -fill y
    pack $w.z -side right -fill both -expand true    
}

proc so_bottom_right {w} {
    so_unpack $w
    pack $w.pad -side bottom -fill x
    pack $w.pad.it -side right
    pack $w.pad.sx -side bottom -fill x
    pack $w.sy -side right -fill y
    pack $w.z -side left -fill both -expand true    
}

proc so_bottom_left {w} {
    so_unpack $w
    pack $w.pad -side bottom -fill x
    pack $w.pad.it -side left
    pack $w.pad.sx -side bottom -fill x
    pack $w.sy -side left -fill y
    pack $w.z -side right -fill both -expand true    
}

proc so_bottom {w} {
    so_unpack $w
    pack $w.pad -side bottom -fill x
    pack $w.pad.sx -side bottom -fill x
    pack $w.z -side left -fill both -expand true    
}

proc so_top {w} {
    so_unpack $w
    pack $w.pad -side top -fill x
    pack $w.pad.sx -side top -fill x
    pack $w.z -side left -fill both -expand true    
}

proc so_right {w} {
    so_unpack $w
    pack $w.sy -side right -fill y
    pack $w.z -side left -fill both -expand true    
}

proc so_left {w} {
    so_unpack $w
    pack $w.sy -side left -fill y
    pack $w.z -side right -fill both -expand true    
}

proc so_plain {w} {
    so_unpack $w
    pack $w.z -side left -fill both -expand true
}

proc so_unpack {w} {
    pack forget $w.pad.it
    pack forget $w.pad.sx
    pack forget $w.pad
    pack forget $w.sy
    pack forget $w.z
}

# ------------------------------------------------------------
# editor stuff

proc ed_load {w file} {
    if [catch {open $file r} fileId] {
	puts stderr "$fileId"
	return -code error
    } else {
	$w delete 1.0 end
	while {![eof $fileId]} {
	    $w insert end [read $fileId 1000]
	}
	close $fileId
    }
}

proc ed_save {w file} {
    if [catch {open $file w} fileId] {
	puts stderr "$fileId"
	return -code error
    } else {
	puts -nonewline $fileId [$w get 1.0 end]
	close $fileId
    }
}

# returns rect1 text1 rect2 text2 ...
proc mkgrid {canvas colws startrow endrow height font fg bg} {
    set res [list]
    set ncols [llength $colws]
    set y [expr ($startrow-1)*$height+$startrow]
    for {set row $startrow} {$row <= $endrow} {incr row} {
	set x 1
	for {set col 0} {$col < $ncols} {incr col} {
	    set colw [lindex $colws $col]
# each object is tagged q<columnNo>
# is this (ugly) way we can find to what column an object belongs
# even later...
	    set r [$canvas create re $x $y [expr $x+$colw] [expr $y+$height]\
		    -f $bg -outline $fg -tag "q$col"]
	    set t [$canvas create te [expr $x+2] [expr $y+2]\
		    -anch nw -fo $font -fi $fg -tag "q$col"]
	    $canvas raise $t
	    lappend res $r $t
	    set x [expr $x+$colw+1]
	}
	set y [expr $y+$height+1]
    }
    return $res
}

# new x values
proc calc_new_xs {colws} {
    set ncols [llength $colws]
    set res [list]
    set x 1
    for {set col 0} {$col < $ncols} {incr col} {
	lappend res $x
	set x [expr $x+1+[lindex $colws $col]]
    }
    lappend res $x
    return $res
}

proc resize_grid_cols {canvas colws} {
    set item 1
    set xs [calc_new_xs $colws]
    while {[set nbr_of_coords\
	    [llength [set coords [$canvas coords $item]]]] > 0} {
	set tags [$canvas itemcget $item -tag]
	set first [string first "q" $tags]
# find the column of the current object by
# searching for the q tag.
	set col [string range $tags [expr 1 + $first]\
		[expr [string wordend $tags $first] -1]]
	switch $nbr_of_coords {
	    2 {  # a text object
		set y [lindex $coords 1]
		set newx [expr [lindex $xs $col] + 2]
		$canvas coords $item $newx $y
	    }
	    4 { # a rectangle object
		set y1 [lindex $coords 1]
		set y2 [lindex $coords 3]
		set newx1 [lindex $xs $col]
		set newx2 [expr [lindex $xs [expr $col + 1]]-1]
		$canvas coords $item $newx1 $y1 $newx2 $y2
	    }
	}
        set item [expr $item+1]
    }
}

    

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

global command
set command ""

proc dbg {str} {
   .console.e insert 0.0 "$str\n"
   update idletasks
}

proc erlsend {w1 {w2 ""} {w3 ""} {w4 ""} {w5 ""} {w6 ""} {w7 ""} {w8 ""}} {
    global sock
	set msg [concat $w1 $w2 $w3 $w4 $w5 $w6 $w7 $w8]
	puts -nonewline $sock "\1$msg\5"
	flush $sock
}

proc erlcall {w} {
    global sock
    set errcode [catch $w result]
    if {$errcode == 0} {
	puts -nonewline $sock "\2$result\5"
	flush $sock
    } else {
	puts -nonewline $sock "\3$result\5"
	flush $sock
    }
}

proc erlexec {w} {
    set errcode [catch $w result]
    if {$errcode != 0} {
	erlerror $result
    }
}

proc erlerror {w} {
    global sock
    puts -nonewline $sock "\4$w\5"
    flush $sock
}

proc get_sock {} {
  global sock
  dbg "getting sock"
  do_read
  dbg "got sock"
}

proc timer_sock {} {
  dbg "\ntimer sock"
  do_read
}

proc make_dbg_console {} {
    toplevel .console
    button .console.fb -text "get sock" -command get_sock
    text .console.e
    pack .console.e 
    pack .console.fb
    dbg "tcl socket port is:$portno"
    update idletasks
}

set portno $argv
# make_dbg_console
wm withdraw .

global sock
set sock [socket 127.0.0.1 $portno]
fconfigure $sock -blocking false -translation auto -buffering none

fileevent $sock readable do_read

proc do_read {} {
    global sock command
    set errcode [catch {gets $sock} line]
    if {$errcode != 0} {
	exit
    }
    set a [eof $sock]
    if [eof $sock] { exit }
    append command "$line\n"
    if [info complete "${command}\n"] {
	catch {eval $command} msg
	set command ""
    }
}

