/* 
 * tclLoadWin.c --
 *
 *    This procedure provides a version of dlopen() that
 *    works with the Windows "LoadLibrary" and "GetProcAddress"
 *    API for dynamic loading.
 *
 */

#include <windows.h>
#include "tcl.h"
#include "tclPort.h"

typedef struct LibraryList {
    HINSTANCE handle;
    struct LibraryList *nextPtr;
} LibraryList;

static LibraryList *libraryList = NULL;       /* List of currently loaded DLL's.  */

/*
 * Declarations for functions that are only used in this file.
 */

static void           UnloadLibraries _ANSI_ARGS_((ClientData clientData));


/*
 *----------------------------------------------------------------------
 *
 * TclSynchSpawn --
 *
 *    32-bit entry point to the 16-bit SynchSpawn code.
 *
 * Results:
 *    1 on success, 0 on failure.
 *
 * Side effects:
 *    Spawns a command and waits for it to complete.
 *
 *----------------------------------------------------------------------
 */
int 
TclSynchSpawn(void *args, int type, void **trans, int *pidPtr)
{

    /*
     * Currently unsupported by the standalone library.
     */

    return 0;
}

/*
 *----------------------------------------------------------------------
 *
 * TclWinGetTclInstance --
 *
 *	Retrieves the global library instance handle.
 *
 * Results:
 *	Returns the global library instance handle.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */

HINSTANCE
TclWinGetTclInstance()
{
    return (HINSTANCE) NULL;
}
/*
 *----------------------------------------------------------------------
 *
 * dlopen --
 *
 *    This function is an alternative for the functions
 *    TclWinLoadLibrary and TclWinGetTclInstance.  It is
 *    responsible for adding library handles to the library list so
 *    the libraries can be freed when tcl.dll is unloaded.
 *
 * Results:
 *    Returns the handle of the newly loaded library, or NULL on
 *    failure. If path is NULL, the global library instance handle
 *    is returned.
 *
 * Side effects:
 *    Loads the specified library into the process.
 *
 *----------------------------------------------------------------------
 */

VOID *dlopen(path, mode)
    CONST char *path;
    int mode;
{
    VOID *handle;
    LibraryList *ptr;
    static int initialized = 0;

    if (!initialized) {
      initialized = 1;
      Tcl_CreateExitHandler((Tcl_ExitProc *) UnloadLibraries,
          (ClientData) &libraryList);
    }
    if (path == (char *) NULL) {
      return (VOID *)GetWindowLong(GetActiveWindow(), GWL_HINSTANCE);
    }
    handle = (VOID *) LoadLibrary(path);
    if (handle != NULL) {
          ptr = (LibraryList*) ckalloc(sizeof(LibraryList));
          ptr->handle = (HINSTANCE) handle;
          ptr->nextPtr = libraryList;
          libraryList = ptr;
    }
    return handle;
}


/*
 *----------------------------------------------------------------------
 *
 * dlclose --
 *
 *    This function is an alternative for the function
 *    FreeLibrary.  It is responsible for removing library
 *    handles from the library list and remove the dll
 *    from memory.
 *
 * Results:
 *    -1 on error, 0 on success.
 *
 * Side effects:
 *    Removes the specified library from the process.
 *
 *----------------------------------------------------------------------
 */

int
dlclose(handle)
    VOID *handle;
{
    LibraryList *ptr, *prevPtr;

    ptr = libraryList; prevPtr = NULL;
    while (ptr != NULL) {
      if (ptr->handle == (HINSTANCE) handle) {
          FreeLibrary((HINSTANCE) handle);
          if (prevPtr) {
              prevPtr->nextPtr = ptr->nextPtr;
          } else {
              libraryList = ptr->nextPtr;
          }
          ckfree(ptr);
          return 0;
      }
      prevPtr = ptr;
      ptr = ptr->nextPtr;
    }
    return -1;
}

/*
 *----------------------------------------------------------------------
 *
 * dlsym --
 *
 *    This function is an alternative for the system function
 *    GetProcAddress. It returns the address of a
 *    symbol, give the handle returned by dlopen().
 *
 * Results:
 *    Returns the address of the symbol in the dll.
 *
 * Side effects:
 *    None.
 *
 *----------------------------------------------------------------------
 */

VOID *dlsym(handle, symbol)
    VOID *handle;
    CONST char *symbol;
{
    return (VOID *) GetProcAddress((HINSTANCE) handle, symbol);
}


/*
 *----------------------------------------------------------------------
 *
 * dlerror --
 *
 *    This function returns a string describing the error which
 *    occurred in dlopen().
 *
 * Results:
 *    Returns an error message.
 *
 * Side effects:
 *    errno is set.
 *
 *----------------------------------------------------------------------
 */
char *
dlerror()
{
    TclWinConvertError(GetLastError());
    return Tcl_ErrnoMsg(errno);
}

/*
 *----------------------------------------------------------------------
 *
 * UnloadLibraries --
 *
 *    Frees any dynamically allocated libraries loaded by Tcl.
 *
 * Results:
 *    None.
 *
 * Side effects:
 *    Frees the libraries on the library list as well as the list.
 *
 *----------------------------------------------------------------------
 */

static void
UnloadLibraries(clientData)
    ClientData clientData;
{
    LibraryList *ptr;
    LibraryList *list = *((LibraryList **) clientData);

    while (list != NULL) {
      FreeLibrary(list->handle);
      ptr = list->nextPtr;
      ckfree(list);
      list = ptr;
    }
}
