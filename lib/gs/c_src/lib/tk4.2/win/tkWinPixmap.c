/* 
 * tkWinPixmap.c --
 *
 *	This file contains the Xlib emulation functions pertaining to
 *	creating and destroying pixmaps.
 *
 * Copyright (c) 1995 Sun Microsystems, Inc.
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 * SCCS: @(#) tkWinPixmap.c 1.15 96/10/11 14:58:31
 */

#include "tkWinInt.h"


/*
 *----------------------------------------------------------------------
 *
 * Tk_GetPixmap --
 *
 *	Creates an in memory drawing surface.
 *
 * Results:
 *	Returns a handle to a new pixmap.
 *
 * Side effects:
 *	Allocates a new Win32 bitmap.
 *
 *----------------------------------------------------------------------
 */

Pixmap
Tk_GetPixmap(display, d, width, height, depth)
    Display* display;
    Drawable d;
    int width;
    int height;
    int depth;
{
    TkWinDrawable *newTwdPtr, *twdPtr;
    
    display->request++;

    newTwdPtr = (TkWinDrawable*) ckalloc(sizeof(TkWinDrawable));
    newTwdPtr->type = TWD_BITMAP;
    newTwdPtr->bitmap.depth = depth;
    twdPtr = (TkWinDrawable *)d;
    if (twdPtr->type != TWD_BITMAP) {
	if (twdPtr->window.winPtr == NULL) {
	    newTwdPtr->bitmap.colormap = DefaultColormap(display,
		    DefaultScreen(display));
	} else {
	    newTwdPtr->bitmap.colormap = twdPtr->window.winPtr->atts.colormap;
	}
    } else {
	newTwdPtr->bitmap.colormap = twdPtr->bitmap.colormap;
    }
    newTwdPtr->bitmap.handle = CreateBitmap(width, height, 1, depth, NULL);

    if (newTwdPtr->bitmap.handle == NULL) {
	ckfree((char *) newTwdPtr);
	return None;
    }
    
    return (Pixmap)newTwdPtr;
}

/*
 *----------------------------------------------------------------------
 *
 * Tk_FreePixmap --
 *
 *	Release the resources associated with a pixmap.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Deletes the bitmap created by Tk_GetPixmap.
 *
 *----------------------------------------------------------------------
 */

void
Tk_FreePixmap(display, pixmap)
    Display* display;
    Pixmap pixmap;
{
    TkWinDrawable *twdPtr = (TkWinDrawable *) pixmap;

    display->request++;
    if (twdPtr != NULL) {
	DeleteObject(twdPtr->bitmap.handle);
	ckfree((char *)twdPtr);
    }
}

/*
 *----------------------------------------------------------------------
 *
 * TkSetPixmapColormap --
 *
 *	The following function is a hack used by the photo widget to
 *	explicitly set the colormap slot of a Pixmap.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */

void
TkSetPixmapColormap(pixmap, colormap)
    Pixmap pixmap;
    Colormap colormap;
{
    TkWinDrawable *twdPtr = (TkWinDrawable *)pixmap;
    twdPtr->bitmap.colormap = colormap;
}


