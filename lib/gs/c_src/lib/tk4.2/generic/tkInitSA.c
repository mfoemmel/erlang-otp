#include "tkInt.h"

#ifdef TCL_ACTIVE
#include "../library/tclIndex.h"
#include "../library/tk.c"
#include "../library/bgerror.c"
#include "../library/button.c"
#include "../library/clrpick.c"
#include "../library/comdlg.c"
#include "../library/console.c"
#include "../library/dialog.c"
#include "../library/entry.c"
#include "../library/focus.c"
#include "../library/listbox.c"
#include "../library/menu.c"
#include "../library/msgbox.c"
#include "../library/obsolete.c"
#include "../library/optMenu.c"
#include "../library/palette.c"
#include "../library/scale.c"
#include "../library/scrlbar.c"
#include "../library/tearoff.c"
#include "../library/text.c"
#include "../library/tkfbox.c"
#include "../library/xmfbox.c"

static char *prolog_c[] = {
#include "../library/prolog.c"
(char *) NULL
};

static Tcl_StaticFile table[] = {
    {"tk:tclIndex", tclIndex_h},
    {"tk", tk_c},
    {"bgerror", bgerror_c},
    {"button", button_c},
    {"clrpick", clrpick_c},
    {"comdlg", comdlg_c},
    {"console", console_c},
    {"dialog", dialog_c},
    {"entry", entry_c},
    {"focus", focus_c},
    {"listbox", listbox_c},
    {"menu", menu_c},
    {"msgbox", msgbox_c},
    {"obsolete", obsolete_c},
    {"optMenu", optMenu_c},
    {"palette", palette_c},
    {"scale", scale_c},
    {"scrlbar", scrlbar_c},
    {"tearoff", tearoff_c},
    {"text", text_c},
    {"tkfbox", tkfbox_c},
    {"xmfbox", xmfbox_c},
    {"prolog", prolog_c},
    {(char *) NULL, (char **) NULL}
};
#endif

int
Tk_InitStandAlone(interp)
    Tcl_Interp *interp;
{
#ifdef TCL_ACTIVE
    Tcl_DefineStaticFile(table);
#endif
    return Tk_Init(interp);
}
