/* etkMisc glue and common utils file */

#include "etk.h"

#include "tclInt.h"
#ifndef TCL_GENERIC_ONLY
#   include "tclPort.h"
#endif

#include <tk.h>

#ifdef getservbyname
#undef getservbyname
#endif

#ifdef getsockopt
#undef getsockopt
#endif

#ifdef ntohs
#undef ntohs
#endif

#ifdef setsockopt
#undef setsockopt
#endif


extern void Etk_Exit(int, char*);

extern int Tcl_UpdateCmd(ClientData clientData,
			 Tcl_Interp *interp,
			 int argc, char **argv);

extern int ErlangOperation(ClientData clientData,
			   Tcl_Interp *interp,	
			   int argc, char **argv);

/* Here is the stuff needed to build a stripped down version of
**   etk and etk_drv
*/

/* ETK: MOVED FROM tclCmdsIL.c */
char *tclExecutableName = NULL;

int Tcl_IsSafe(interp)
    Tcl_Interp *interp;		/* Is this interpreter "safe" ? */
{
    return 0;
}

/* ETK: MOVED FROM (And changed) tclInterp.c */

Tcl_Interp *
Tcl_GetSlave(interp, slavePath)
    Tcl_Interp *interp;		/* Interpreter to start search from. */
    CONST char *slavePath;	/* Path of slave to find. */
{
    return NULL;
}

Tcl_Interp *
Tcl_GetMaster(interp)
    Tcl_Interp *interp;		/* Get the master of this interpreter. */
{
    return NULL;
}


/* ETK: MOVED FROM (And changed) tclCmdMZ.c */
char **
Tcl_GetStaticFile(rsrcName)
    CONST char *rsrcName;
{
    return (char **) NULL;
}

/* ETK: MOVED FROM (And changed) TkPlatfomInit.c */

int
TkPlatformInit(interp)
    Tcl_Interp *interp;
{
    return TCL_OK;
}

int Etk_Execute(cmd, data, interp, argc, argv)
int cmd; ClientData data; Tcl_Interp* interp; int argc; char** argv;
{
    switch(cmd) {
    case ERL_TK_BIND:
	return Tk_BindCmd(data, interp, argc, argv);

    case ERL_TK_DESTROY:
	return Tk_DestroyCmd(data, interp, argc, argv);

    case ERL_TK_LOWER:
	return Tk_LowerCmd(data, interp, argc, argv);
	
    case ERL_TK_RAISE:
	return Tk_RaiseCmd(data, interp, argc, argv);
	
    case ERL_TK_BELL:
	return Tk_BellCmd(data, interp, argc, argv);
	
    case ERL_TK_BUTTON:
	return Tk_ButtonCmd(data, interp, argc, argv);
	
    case ERL_TK_CHECKBUTTON:
	return Tk_CheckbuttonCmd(data, interp, argc, argv);
	
    case ERL_TK_RADIOBUTTON:
	return Tk_RadiobuttonCmd(data, interp, argc, argv);
	
    case ERL_TK_LABEL:
	return Tk_LabelCmd(data, interp, argc, argv);
	
    case ERL_TK_UPDATE:
	return Tk_UpdateCmd(data, interp, argc, argv);
	
    case ERL_TK_WINFO:
	return Tk_WinfoCmd(data, interp, argc, argv);
	
    case ERL_TK_WM:
	return Tk_WmCmd(data, interp, argc, argv);
	
    case ERL_TK_BINDTAGS:
	return Tk_BindtagsCmd(data, interp, argc, argv);
	
    case ERL_TK_CANVAS:
	return Tk_CanvasCmd(data, interp, argc, argv);
	
    case ERL_TK_CLIPBOARD:
	return Tk_ClipboardCmd(data, interp, argc, argv);
	
    case ERL_TK_ENTRY:
	return Tk_EntryCmd(data, interp, argc, argv);
	
    case ERL_TK_FRAME:
	return Tk_FrameCmd(data, interp, argc, argv);

    case ERL_TK_TOPLEVEL:
	return Tk_ToplevelCmd(data, interp, argc, argv);
	
    case ERL_TK_FOCUS:
	return Tk_FocusCmd(data, interp, argc, argv);
	
    case ERL_TK_GRAB:
	return Tk_GrabCmd(data, interp, argc, argv);
	
    case ERL_TK_IMAGE:
	return Tk_ImageCmd(data, interp, argc, argv);
	
    case ERL_TK_LISTBOX:
	return Tk_ListboxCmd(data, interp, argc, argv);
	
    case ERL_TK_MENU:
	return Tk_MenuCmd(data, interp, argc, argv);
	
    case ERL_TK_MENUBUTTON:
	return Tk_MenubuttonCmd(data, interp, argc, argv);
	
    case ERL_TK_MESSAGE:
	return Tk_MessageCmd(data, interp, argc, argv);
	
    case ERL_TK_OPTION:
	return Tk_OptionCmd(data, interp, argc, argv);
	
    case ERL_TK_PACK:
	return Tk_PackCmd(data, interp, argc, argv);
	
    case ERL_TK_PLACE:
	return Tk_PlaceCmd(data, interp, argc, argv);
	
    case ERL_TK_SCALE:
	return Tk_ScaleCmd(data, interp, argc, argv);
	
    case ERL_TK_SCROLLBAR:
	return Tk_ScrollbarCmd(data, interp, argc, argv);
	
    case ERL_TK_SELECTION:
	return Tk_SelectionCmd(data, interp, argc, argv);
	
    case ERL_TK_TEXT:
	return Tk_TextCmd(data, interp, argc, argv);

    case ERL_TK_GRID:
	return Tk_GridCmd(data, interp, argc, argv);
	
    case ERL_TK_TK:
	return Tk_TkCmd(data, interp, argc, argv);
	
    case ERL_TK_CMD: {
	Tcl_CmdInfo info;

	if (Tcl_GetCommandInfo(interp, argv[0], &info))
	    return (*info.proc)(info.clientData, interp, argc, argv);
	else {
	    Tcl_SetResult(interp, "widget not found", TCL_STATIC);
	    return TCL_ERROR;
	}
    }

    case ERL_TK_GETVAR: {
	char *value = Tcl_GetVar(interp, argv[1], TCL_GLOBAL_ONLY);
	if (value == NULL)
	    value = "";
	Tcl_AppendResult(interp, value, (char*) NULL);
	return TCL_OK;
    }

    case ERL_TK_SETVAR:
	if (Tcl_SetVar(interp, argv[1], argv[2],
		       TCL_GLOBAL_ONLY|TCL_LEAVE_ERR_MSG) == NULL) {
	    return TCL_ERROR;
	}
	return TCL_OK;

    case ERL_TK_EVENT:
	return Tk_EventCmd(data, interp, argc, argv);

    case ERL_TK_WLINK:
	if (argc < 3)
	    Tcl_AppendResult(interp, "wrong # args: should be \"",
			     argv[0], " window operation-id\"", (char *) NULL);
	else {
	    char* wargv[4];

	    wargv[0] = "winfo";
	    wargv[1] = "exists";
	    wargv[2] = argv[1];
	    wargv[3] = NULL;
	    if (Tk_WinfoCmd(data, interp, 3, wargv) == TCL_OK) {
		if (strcmp(interp->result, "1") == 0) {
		    wargv[0] = "operation";
		    wargv[1] = argv[2];
		    wargv[2] = NULL;
		    if (ErlangOperation(data, interp, 2, wargv) == TCL_OK)
			return TCL_OK;
		}
		else
		    Tcl_SetResult(interp, "widget not found", TCL_STATIC);
	    }
	}
	return TCL_ERROR;
	

    case ERL_TK_OPERATION:
	if (argc < 2) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"",
			     argv[0], " operation-id ?args?\"", (char *) NULL);
	    return TCL_ERROR;
	}
	return ErlangOperation(data, interp, argc, argv);

    default:
	Etk_Exit(1, "unknown etk command");
	return 0;
    }
}

#ifdef __WIN32__

int Etk_WinSockFunctions(Ws2Funcs* wf)
{
    static char dll_name[] = "ws2_32";
    HMODULE module;

    if (wf->WSAStartup != NULL)
	return TRUE;

    if ((module = LoadLibrary(dll_name)) == NULL)
	return FALSE;

    wf->WSAStartup = (LPFN_WSASTARTUP) GetProcAddress(module, "WSAStartup");
    wf->WSACleanup = (LPFN_WSACLEANUP) GetProcAddress(module, "WSACleanup");
    wf->WSAGetLastError = (LPFN_WSAGETLASTERROR) GetProcAddress(module, "WSAGetLastError");
    wf->WSAWaitForMultipleEvents = (LPFN_WSAWAITFORMULTIPLEEVENTS) GetProcAddress(module, "WSAWaitForMultipleEvents");
    wf->WSACreateEvent = (LPFN_WSACREATEEVENT) GetProcAddress(module, "WSACreateEvent");

    wf->WSACloseEvent = (LPFN_WSACLOSEEVENT) GetProcAddress(module, "WSACloseEvent");
    wf->WSAResetEvent = (LPFN_WSARESETEVENT) GetProcAddress(module, "WSAResetEvent");
    wf->WSAEventSelect = (LPFN_WSAEVENTSELECT) GetProcAddress(module, "WSAEventSelect");
    wf->WSAEnumNetworkEvents = (LPFN_WSAENUMNETWORKEVENTS) GetProcAddress(module, "WSAEnumNetworkEvents");
    wf->WSASend = (LPFN_WSASEND) GetProcAddress(module, "WSASend");
    
    wf->accept = (LPFN_ACCEPT) GetProcAddress(module, "accept");
    wf->bind = (LPFN_BIND) GetProcAddress(module, "bind");
    wf->closesocket = (LPFN_CLOSESOCKET) GetProcAddress(module, "closesocket");
    wf->connect = (LPFN_CONNECT) GetProcAddress(module, "connect");
    wf->ioctlsocket = (LPFN_IOCTLSOCKET) GetProcAddress(module, "ioctlsocket");

    wf->getsockopt = (LPFN_GETSOCKOPT) GetProcAddress(module, "getsockopt");
    wf->htonl = (LPFN_HTONL) GetProcAddress(module, "htonl");
    wf->htons = (LPFN_HTONS) GetProcAddress(module, "htons");
    wf->inet_addr = (LPFN_INET_ADDR) GetProcAddress(module, "inet_addr");
    wf->inet_ntoa = (LPFN_INET_NTOA) GetProcAddress(module, "inet_ntoa");
    wf->listen = (LPFN_LISTEN) GetProcAddress(module, "listen");
    wf->ntohs = (LPFN_NTOHS) GetProcAddress(module, "ntohs");
    wf->ntohl = (LPFN_NTOHL) GetProcAddress(module, "ntohl");
    wf->recv = (LPFN_RECV) GetProcAddress(module, "recv");
    wf->send = (LPFN_SEND) GetProcAddress(module, "send");
    wf->recvfrom = (LPFN_RECVFROM) GetProcAddress(module, "recvfrom");
    wf->sendto = (LPFN_SENDTO) GetProcAddress(module, "sendto");
    wf->setsockopt = (LPFN_SETSOCKOPT) GetProcAddress(module, "setsockopt");
    wf->shutdown = (LPFN_SHUTDOWN) GetProcAddress(module, "shutdown");
    wf->socket = (LPFN_SOCKET) GetProcAddress(module, "socket");
    wf->gethostbyaddr = (LPFN_GETHOSTBYADDR) GetProcAddress(module, "gethostbyaddr");
    wf->gethostbyname = (LPFN_GETHOSTBYNAME) GetProcAddress(module, "gethostbyname");
    wf->gethostname = (LPFN_GETHOSTNAME) GetProcAddress(module, "gethostname");
    wf->getservbyname = (LPFN_GETSERVBYNAME) GetProcAddress(module, "getservbyname");
    wf->getsockname = (LPFN_GETSOCKNAME) GetProcAddress(module, "getsockname");
    wf->getpeername = (LPFN_GETPEERNAME) GetProcAddress(module, "getpeername");
    return TRUE;
}

#endif

