/* 
 * tkPhotoUtil.c --
 *
 *	This file contains photo related utility functions.
 *
 * Written by:	Jan Nijtmans
 *		NICI (Nijmegen Institute of Cognition and Information)
 *		email: nijtmans@nici.kun.nl
 *		url:   http://www.cogsci.kun.nl/~nijtmans/
 *
 * SCCS: @(#) tkPhotoUtil.c 1.0 97/03/10 18:53:12
 */

#include "tkInt.h"
#include "tkPort.h"

#ifdef MAC_TCL
#  include "::compat:dlfcn.h"
#else
#  ifdef HAVE_DLFCN_H
#    include <dlfcn.h>
#  else
#    include "../compat/dlfcn.h"
#  endif
#endif

/*
 * In some systems, like SunOS 4.1.3, the RTLD_NOW flag isn't defined
 * and this argument to dlopen must always be 1.
 */

#ifndef RTLD_NOW
#   define RTLD_NOW 1
#endif


/*
 *----------------------------------------------------------------------
 *
 * TkPhotoPutBlock --
 *
 *	This procedure is called to put image data into a photo image.
 *	The difference with Tk_PhotoPutBlock is that it handles the
 *	transparency information as well.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	The image data is stored.  The image may be expanded.
 *	The Tk image code is informed that the image has changed.
 *
 *----------------------------------------------------------------------
 */

int
TkPhotoPutBlock(handle, blockPtr, x, y, width, height)
    Tk_PhotoHandle handle;	/* Opaque handle for the photo image
				 * to be updated. */
    Tk_PhotoImageBlock *blockPtr;
				/* Pointer to a structure describing the
				 * pixel data to be copied into the image. */
    int x, y;			/* Coordinates of the top-left pixel to
				 * be updated in the image. */
    int width, height;		/* Dimensions of the area of the image
				 * to be updated. */
{
    int alphaOffset;

    alphaOffset = blockPtr->offset[3];
    if ((alphaOffset< 0) || (alphaOffset>= blockPtr->pixelSize)) {
	alphaOffset = blockPtr->offset[0];
	if (alphaOffset < blockPtr->offset[1]) {
	    alphaOffset = blockPtr->offset[1];
	}
	if (alphaOffset < blockPtr->offset[2]) {
	    alphaOffset = blockPtr->offset[2];
	}
	if (++alphaOffset >= blockPtr->pixelSize) {
	    alphaOffset = blockPtr->offset[0];
	}
    }
    if (alphaOffset != blockPtr->offset[0]) {
	int X, Y, end;
	unsigned char *pixelPtr, *imagePtr, *rowPtr;
	rowPtr = imagePtr = blockPtr->pixelPtr;
	for (Y = 0; Y < height; Y++) {
	    X = 0; pixelPtr = rowPtr + alphaOffset;
	    while(X < width) {
		/* search for first non-transparent pixel */
		while ((X < width) && !*pixelPtr) {
		    X++; pixelPtr += blockPtr->pixelSize;
		}
		end = X;
		/* search for first transparent pixel */
		while ((end < width) && *pixelPtr) {
		    end++; pixelPtr += blockPtr->pixelSize;
		}
		if (end > X) {
 		    blockPtr->pixelPtr =  rowPtr + blockPtr->pixelSize * X;
		    Tk_PhotoPutBlock(handle, blockPtr, x+X, y+Y, end-X, 1);
		}
		X = end;
	    }
	    rowPtr += blockPtr->pitch;
	}
	blockPtr->pixelPtr = imagePtr;
    } else {
	Tk_PhotoPutBlock(handle,blockPtr,x,y,width,height);
    }
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * TkLoadLib --
 *
 *	This procedure is called to load a shared library into memory.
 *
 * Results:
 *	TCL_OK if function succeeds. Otherwise TCL_ERROR while the
 *	interpreter will contain an error-message.
 *
 * Side effects:
 *	Library functions become available.
 *
 *----------------------------------------------------------------------
 */

typedef struct Functions {
    VOID *handle;
    int (* first) _ANSI_ARGS_((void));
    int (* next) _ANSI_ARGS_((void));
} Functions;

#define FAILED ((VOID *) -114)

int
TkLoadLib(interp, libName, libVersion, handlePtr, symbols, num)
    Tcl_Interp *interp;
    CONST char *libName;
    CONST char *libVersion;
    VOID **handlePtr;
    char **symbols;
    int num;
{
    VOID *handle = (VOID *) NULL;
    Functions *lib = (Functions *) handlePtr;
    char **p = (char **) &(lib->first);
    char **q = symbols;
    char buf[256];
    int length;

    if (lib->handle != NULL) {
	return (lib->handle != FAILED) ? TCL_OK : TCL_ERROR;
    }

    length = strlen(libName);
    if ((libVersion != NULL) && (*libVersion) && (length>2)
	    && !strncmp(libName+length-3,".s",2)) {
	strcpy(buf,libName);
	if (libName[length-1] == 'l') {
	    length -= 3;
	} else {
	    buf[length] = '.';
	}
	strcpy(buf+length+1,libVersion);
	handle = dlopen(buf, RTLD_NOW);
    }

    if (handle == NULL) {
	dlerror();
	handle = dlopen(libName, RTLD_NOW);
    }
    if (handle == NULL) {
	Tcl_AppendResult(interp,"cannot open ",libName,
		": ", dlerror(), (char *) NULL);
	lib->handle = FAILED;
	return TCL_ERROR;
    }

    buf[1] = '_';
    while (*q) {
	*p = (char *) dlsym(handle,*q);
	if (*p == (char *)NULL) {
	    strcpy(buf+1,*q);
	    *p = (char *) dlsym(handle,buf);
	    if ((num > 0) && (*p == (char *)NULL)) {
		Tcl_AppendResult(interp,"cannot open ",libName,
			": symbol \"",*q,"\" not found", (char *) NULL);
		dlclose(handle);
		lib->handle = FAILED;
		return TCL_ERROR;
	    }
	}
	q++; num--;
	p += (Tk_Offset(Functions, next) - Tk_Offset(Functions, first)) /
		sizeof(char *);
    }
    lib->handle = handle;

    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * TkLoadFailed --
 *
 *    Mark the loaded library as invalid. Remove it from memory
 *    if possible. It will no longer be used in the future.
 *
 * Results:
 *    None.
 *
 * Side effects:
 *    Next time the same handle is used by TkLoadLib, it will
 *    fail immediately, without trying to load it.
 *
 *----------------------------------------------------------------------
 */

void
TkLoadFailed(handlePtr)
    VOID **handlePtr;
{
    if ((*handlePtr != NULL) && (*handlePtr != FAILED)) {
      /* Oops, still loaded. First remove it from memory */
      dlclose(*handlePtr);
    }
    *handlePtr = FAILED;
}
