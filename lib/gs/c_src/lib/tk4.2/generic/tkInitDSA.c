#include "tk.h"

int
Tk_InitStandAlone(interp)
    Tcl_Interp *interp;
{
    Tcl_AppendResult(interp,
	"The function \"Tk_InitStandAlone\" is not available in\n",
	"the shared Tk library. Link your standalone application\n",
	"with the static Tk library (libtk",TK_VERSION,
	".a) explicitely",(char *) NULL);
    return TCL_ERROR;
}
