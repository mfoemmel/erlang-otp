#include "tclInt.h"

#include "../library/tclIndex.h"
#include "../library/init.c"
#include "../library/parray.c"
#include "../library/ldAout.c"
#include "../library/tkcon.c"

static Tcl_StaticFile table[] = {
    {"tcl:tclIndex", tclIndex_h},
    {"init", init_c},
    {"parray", parray_c},
    {"ldAout", ldAout_c},
    {"tkcon", tkcon_c},
    {(char *) NULL, (char **) NULL}
};

int
Tcl_InitStandAlone(interp)
    Tcl_Interp *interp;
{
    Tcl_DefineStaticFile(table);
    return Tcl_Init(interp);
}
