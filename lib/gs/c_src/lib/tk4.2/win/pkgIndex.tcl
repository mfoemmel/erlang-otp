# Tcl package index file, version 1.0

package ifneeded Tk 4.2 [list load [file join $dir tk42.dll] Tk]
package ifneeded Tktest 4.2 [list tclPkgSetup $dir Tktest 4.2 {{tktest.dll load {testdeleteapps testevent testmakeexist testsend}}}]
package ifneeded Tksquare 4.2 [list tclPkgSetup $dir Tksquare 4.2 {{tksquare.dll load square}}]
