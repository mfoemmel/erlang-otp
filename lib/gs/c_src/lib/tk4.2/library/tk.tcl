# tk.tcl --
#
# Initialization script normally executed in the interpreter for each
# Tk-based application.  Arranges class bindings for widgets.
#
# SCCS: @(#) tk.tcl 1.87 96/09/30 09:28:02
#
# Copyright (c) 1992-1994 The Regents of the University of California.
# Copyright (c) 1994-1996 Sun Microsystems, Inc.
#
# ========================================================================
# >>>>>>>>>>>>>>>> INCLUDES MODIFICATIONS FOR [incr Tcl] <<<<<<<<<<<<<<<<<
#
#  AUTHOR:  Michael J. McLennan       Phone: (610)712-2842
#           AT&T Bell Laboratories   E-mail: michael.mclennan@att.com
#     RCS:  $Id: init.tcl,v 1.5 1995/09/14 16:22:25 mmc Exp $
# ========================================================================
#             Copyright (c) 1993-1995  AT&T Bell Laboratories
# ------------------------------------------------------------------------
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.

# Insist on running with compatible versions of Tcl and Tk.

package require -exact Tk 4.2
package require -exact Tcl 7.6

catch {source -rsrc tk:tclIndex}

# Add Tk's directory to the end of the auto-load search path, if it
# isn't already on the path:

if {[info exists auto_path] && $tk_library != ""} {
    if {[lsearch -exact $auto_path $tk_library] < 0} {
	lappend auto_path $tk_library
    }
}

# Turn off strict Motif look and feel as a default.

set tk_strictMotif 0

# tkScreenChanged --
# This procedure is invoked by the binding mechanism whenever the
# "current" screen is changing.  The procedure does two things.
# First, it uses "upvar" to make global variable "tkPriv" point at an
# array variable that holds state for the current display.  Second,
# it initializes the array if it didn't already exist.
#
# Arguments:
# screen -		The name of the new screen.

proc tkScreenChanged screen {
    set x [string last . $screen]
    if {$x > 0} {
	set disp [string range $screen 0 [expr $x - 1]]
    } else {
	set disp $screen
    }

    uplevel #0 upvar #0 tkPriv.$disp tkPriv
    global tkPriv
    if [info exists tkPriv] {
	set tkPriv(screen) $screen
	return
    }
    set tkPriv(afterId) {}
    set tkPriv(buttons) 0
    set tkPriv(buttonWindow) {}
    set tkPriv(dragging) 0
    set tkPriv(focus) {}
    set tkPriv(grab) {}
    set tkPriv(initPos) {}
    set tkPriv(inMenubutton) {}
    set tkPriv(listboxPrev) {}
    set tkPriv(mouseMoved) 0
    set tkPriv(oldGrab) {}
    set tkPriv(popup) {}
    set tkPriv(postedMb) {}
    set tkPriv(pressX) 0
    set tkPriv(pressY) 0
    set tkPriv(screen) $screen
    set tkPriv(selectMode) char
    set tkPriv(window) {}
}

# ------------------------------------------------------------------------
#  USAGE: tk_unknown ?command arg arg...?	(Itcl only)
#
#  Unknown command handler for Tk.  Checks to see if "command" is
#  the name of a Tk window with a scoped command.  If it is, and if
#  the "itcl::purist" flag is not set, then the scoped command is
#  invoked automatically to access the widget.  This provides backward
#  compatibility for Tcl/Tk applications that assume all widgets are
#  accessible from the global scope.
#
#  When the "itcl::purist" mode is set, widgets are protected by the
#  namespace that contains them, and must be invoked using a scoped
#  command.
#
#  If the "command" is not a window name, then this procedure returns
#  "-code continue", telling the unknown facility to try another
#  handler.
# ------------------------------------------------------------------------

if [string compare [uplevel #0 info commands @scope] {}] {

    proc winfo_command window {
	return [winfo command $window]
    }

    namespace itcl {
	public variable purist 0

	public proc tk_unknown {args} {
	    global purist

	    set win [lindex $args 0]

	    if {[catch {winfo command $win} cmd] == 0 &&
		$cmd != "" && !$purist} {
		return [uplevel $cmd [lrange $args 1 end]]
	    }
	return -code continue
	}
	unknown_handler "tk" itcl::tk_unknown
    }

} else {
    proc winfo_command window {
	return $window
    }
}

if [info exists auto_namespace] {

# Create the auto_namespace array for all subcommands of "clipboard",
# "grab", "grid", "image", "tkwait", "option", "pack", "place",
# "selection", "winfo" and "wm" (I hope I didn't forget any). This
# has the effect that as soon as an application defines its own "image"
# namespace (for example), wrapper functions will be created for the
# image commands of tk. All original subcommands will still work within
# the new namespace.

set auto_namespace(clipboard)	{tk {clear append}}
set auto_namespace(grab)	{tk {current release set status}}
set auto_namespace(grid)	{tk {bbox columnconfigure configure forget info
				 location propagate rowconfigure size slaves}}
set auto_namespace(image)	{tk {create delete height names type types
				 width}}
set auto_namespace(tkwait)	{tk {variable visibility window}}
set auto_namespace(option)	{tk {add clear get readfile}}
set auto_namespace(pack)	{tk {configure forget info propagate slaves}}
set auto_namespace(place)	{tk {configure forget info slaves}}
set auto_namespace(selection)	{tk {clear get handle own}}
set auto_namespace(winfo)	{tk {atom atomname cells children class
				 colormapfull containing depth exists fpixels
				 geometry height id interps ismapped manager
				 name parent pathname pixels pointerx pointerxy
				 pointery reqheight reqwidth rgb rootx rooty
				 screen screencells screendepth screenheight
				 screenmmheight screenmmwidth screenvisual
				 screenwidth server toplevel viewable visual
				 visualid visualsavailable vrootheight
				 vrootwidth vrootx vrooty width x y}}
set auto_namespace(wm)		{tk {aspect client command deiconify focusmodel
				 frame geometry grid group iconbitmap iconify
				 iconmask iconname iconposition iconwindow
				 maxsize minsize overrideredirect positionfrom
				 protocol resizable sizefrom state title
				 transient withdraw}}

}

# Do initial setup for tkPriv, so that it is always bound to something
# (otherwise, if someone references it, it may get set to a non-upvar-ed
# value, which will cause trouble later).

tkScreenChanged [winfo screen .]

# tkEventMotifBindings --
# This procedure is invoked as a trace whenever tk_strictMotif is
# changed.  It is used to turn on or turn off the motif virtual
# bindings.
#
# Arguments:
# n1 - the name of the variable being changed ("tk_strictMotif").

proc tkEventMotifBindings {n1 dummy dummy} {
    upvar $n1 name
    
    if $name {
	set op delete
    } else {
	set op add
    }

    event $op <<Cut>> <Control-Key-w>
    event $op <<Copy>> <Meta-Key-w> 
    event $op <<Paste>> <Control-Key-y>
}

#----------------------------------------------------------------------
# Define the set of common virtual events.
#----------------------------------------------------------------------

switch $tcl_platform(platform) {
    "unix" {
	event add <<Cut>> <Control-Key-x> <Key-F20> 
	event add <<Copy>> <Control-Key-c> <Key-F16>
	event add <<Paste>> <Control-Key-v> <Key-F18>
	trace variable tk_strictMotif w tkEventMotifBindings
	set tk_strictMotif $tk_strictMotif
    }
    "windows" {
	event add <<Cut>> <Control-Key-x> <Shift-Key-Delete>
	event add <<Copy>> <Control-Key-c> <Control-Key-Insert>
	event add <<Paste>> <Control-Key-v> <Shift-Key-Insert>
    }
    "macintosh" {
	event add <<Cut>> <Control-Key-x> <Key-F2> 
	event add <<Copy>> <Control-Key-c> <Key-F3>
	event add <<Paste>> <Control-Key-v> <Key-F4>
	event add <<Clear>> <Clear>
    }
}

# ----------------------------------------------------------------------
# Read in files that define all of the class bindings.
# ----------------------------------------------------------------------

if {[info exists tk_library] && [string compare $tk_library {}]} {
    source [file join $tk_library button.tcl]
    source [file join $tk_library entry.tcl]
    source [file join $tk_library listbox.tcl]
    source [file join $tk_library menu.tcl]
    source [file join $tk_library scale.tcl]
    source [file join $tk_library scrlbar.tcl]
    source [file join $tk_library text.tcl]
}

# ----------------------------------------------------------------------
# Default bindings for keyboard traversal.
# ----------------------------------------------------------------------

bind all <Tab> {focus [tk_focusNext %W]}
bind all <Shift-Tab> {focus [tk_focusPrev %W]}

# tkCancelRepeat --
# This procedure is invoked to cancel an auto-repeat action described
# by tkPriv(afterId).  It's used by several widgets to auto-scroll
# the widget when the mouse is dragged out of the widget with a
# button pressed.
#
# Arguments:
# None.

proc tkCancelRepeat {} {
    global tkPriv
    after cancel $tkPriv(afterId)
    set tkPriv(afterId) {}
}
