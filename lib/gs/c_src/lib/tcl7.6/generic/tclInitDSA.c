#include "tcl.h"

int
Tcl_InitStandAlone(interp)
    Tcl_Interp *interp;
{
    Tcl_AppendResult(interp,
	"The function \"Tcl_InitStandAlone\" is not available in\n",
	"the shared Tcl library. Link your standalone application\n",
	"with the static Tcl library (libtcl",TCL_VERSION,
	".a) explicitely",(char *) NULL);
    return TCL_ERROR;
}
