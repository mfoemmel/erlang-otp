/*
 * imgGIF.c --
 *
 * A photo image file handler for GIF files. Reads 87a and 89a GIF files.
 * At present THERE ARE WRITE functions for 87a and 89a GIF.
 *
 * GIF images may be read using the -data option of the photo image by
 * representing the data as BASE64 encoded ascii (SAU 6/96)
 *
 * Derived from the giftoppm code found in the pbmplus package 
 * and tkImgFmtPPM.c in the tk4.0b2 distribution by -
 *
 * Reed Wade (wade@cs.utk.edu), University of Tennessee
 *
 * Copyright (c) 1995-1996 Sun Microsystems, Inc.
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 * This file also contains code from the giftoppm and the ppmtogif programs, 
 * which are copyrighted as follows:
 *
 * +-------------------------------------------------------------------+
 * | Copyright 1990, David Koblas.                                     |
 * |   Permission to use, copy, modify, and distribute this software   |
 * |   and its documentation for any purpose and without fee is hereby |
 * |   granted, provided that the above copyright notice appear in all |
 * |   copies and that both that copyright notice and this permission  |
 * |   notice appear in supporting documentation.  This software is    |
 * |   provided "as is" without express or implied warranty.           |
 * +-------------------------------------------------------------------+
 *
 * It also contains parts of the the LUG package developed by Raul Rivero.
 *
 * The GIF write function uses the Xiaolin Wu quantize function:
 *
 * +-------------------------------------------------------------------+
 * |           C Implementation of Wu's Color Quantizer (v. 2)         |
 * |           (see Graphics Gems vol. II, pp. 126-133)                |
 * |                                                                   |
 * | Author: Xiaolin Wu                                                |
 * |         Dept. of Computer Science                                 |
 * |         Univ. of Western Ontario                                  |
 * |         London, Ontario N6A 5B7                                   |
 * |         wu@csd.uwo.ca                                             |
 * |                                                                   |
 * | Algorithm: Greedy orthogonal bipartition of RGB space for         |
 * |            variance minimization aided by inclusion-exclusion     |
 * |            tricks. For speed no nearest neighbor search is done.  |
 * |            Slightly better performance can be expected by more    |
 * |            sophisticated but more expensive versions.             |
 * |                                                                   |
 * | The author thanks Tom Lane at Tom_Lane@G.GP.CS.CMU.EDU for much   |
 * | of additional documentation and a cure to a previous bug.         |
 * |                                                                   |
 * | Free to distribute, comments and suggestions are appreciated.     |
 * +-------------------------------------------------------------------+
 *
 * SCCS: @(#) imgGIF.c 1.13 97/01/21 19:54:13
 */

/*
 * GIF's are represented as data in base64 format.
 * base64 strings consist of 4 6-bit characters -> 3 8 bit bytes.
 * A-Z, a-z, 0-9, + and / represent the 64 values (in order).
 * '=' is a trailing padding char when the un-encoded data is not a
 * multiple of 3 bytes.  We'll ignore white space when encountered.
 * Any other invalid character is treated as an EOF
 */

#define GIF_SPECIAL	 (256)
#define GIF_PAD		(GIF_SPECIAL+1)
#define GIF_SPACE	(GIF_SPECIAL+2)
#define GIF_BAD		(GIF_SPECIAL+3)
#define GIF_DONE	(GIF_SPECIAL+4)

static char base64_table[64] = {
    'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H',
    'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P',
    'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X',
    'Y', 'Z', 'a', 'b', 'c', 'd', 'e', 'f',
    'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n',
    'o', 'p', 'q', 'r', 's', 't', 'u', 'v',
    'w', 'x', 'y', 'z', '0', '1', '2', '3',
    '4', '5', '6', '7', '8', '9', '+', '/'
};

/*
 * structure to "mimic" FILE for Mread, so we can look like fread.
 * The decoder state keeps track of which byte we are about to read,
 * or EOF.
 */

#include "tcl.h"
#include "tkInt.h"
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

typedef struct mFile {
    Tcl_DString *buffer;	/* pointer to dynamical string for writing */
    unsigned char *data;	/* mmencoded source string */
    int c;			/* bits left over from previous character */
    int state;			/* decoder state (0-4 or GIF_DONE) */
    int linelength;		/* length of phisical line already written */
} MFile;

/*
 * The format record for the GIF file format:
 */

static int      FileMatchGIF _ANSI_ARGS_((FILE *f, char *fileName,
		    char *formatString, int *widthPtr, int *heightPtr));
static int      FileReadGIF  _ANSI_ARGS_((Tcl_Interp *interp,
		    FILE *f, char *fileName, char *formatString,
		    Tk_PhotoHandle imageHandle, int destX, int destY,
		    int width, int height, int srcX, int srcY));
static int	StringMatchGIF _ANSI_ARGS_(( char *string,
		    char *formatString, int *widthPtr, int *heightPtr));
static int	StringReadGIF _ANSI_ARGS_((Tcl_Interp *interp, char *string,
		    char *formatString, Tk_PhotoHandle imageHandle,
		    int destX, int destY, int width, int height,
		    int srcX, int srcY));
static int 	FileWriteGIF _ANSI_ARGS_(( Tcl_Interp *interp,  
		    char *filename, char *formatString, 
		    Tk_PhotoImageBlock *blockPtr));
static int	StringWriteGIF _ANSI_ARGS_((Tcl_Interp *interp,
		    Tcl_DString *dataPtr, char *formatString,
		    Tk_PhotoImageBlock *blockPtr));
static int	WriteGIF _ANSI_ARGS_((Tcl_Interp *interp,
		    FILE *fp, char *formatString,
		    Tk_PhotoImageBlock *blockPtr));

Tk_PhotoImageFormat tkImgFmtGIF = {
	"GIF",			/* name */
	FileMatchGIF,   /* fileMatchProc */
	StringMatchGIF, /* stringMatchProc */
	FileReadGIF,    /* fileReadProc */
	StringReadGIF,  /* stringReadProc */
	FileWriteGIF,   /* fileWriteProc */
	StringWriteGIF, /* stringWriteProc */
};

#define INTERLACE		0x40
#define LOCALCOLORMAP		0x80
#define BitSet(byte, bit)	(((byte) & (bit)) == (bit))
#define MAXCOLORMAPSIZE		256
#define CM_RED			0
#define CM_GREEN		1
#define CM_BLUE			2
#define CM_ALPHA		3
#define MAX_LWZ_BITS		12
#define LM_to_uint(a,b)         (((b)<<8)|(a))
#define ReadOK(file,buffer,len)	(Fread(buffer, len, 1, file) != 0)

/*
 * 			 HACK ALERT!!  HACK ALERT!!  HACK ALERT!!
 * This code is hard-wired for reading from files.  In order to read
 * from a data stream, we'll trick fread so we can reuse the same code
 */
 
static int fromData=0;

/*
 * Prototypes for local procedures defined in this file:
 */

static int		DoExtension _ANSI_ARGS_((FILE *fd, int label,
			    int *transparent));
static int		GetCode _ANSI_ARGS_((FILE *fd, int code_size,
			    int flag));
static int		GetDataBlock _ANSI_ARGS_((FILE *fd,
			    unsigned char *buf));
static int		LWZReadByte _ANSI_ARGS_((FILE *fd, int flag,
			    int input_code_size));
static int		ReadColorMap _ANSI_ARGS_((FILE *fd, int number,
			    unsigned char buffer[MAXCOLORMAPSIZE][4]));
static int		ReadGIFHeader _ANSI_ARGS_((FILE *f, int *widthPtr,
			    int *heightPtr));
static int		ReadImage _ANSI_ARGS_((Tcl_Interp *interp,
			    char *imagePtr, FILE *fd, int len, int rows, 
			    unsigned char cmap[MAXCOLORMAPSIZE][4],
			    int width, int height, int srcX, int srcY,
			    int interlace, int transparent));
/*
 * these are for the BASE64 image reader code only
 */

static int		Fread _ANSI_ARGS_((unsigned char *dst, size_t size,
			    size_t count, FILE *file));
static int		Mread _ANSI_ARGS_((unsigned char *dst, size_t size,
			    size_t count, MFile *handle));
static int		Mgetc _ANSI_ARGS_((MFile *handle));
static int		char64 _ANSI_ARGS_((int c));
static void		mInit _ANSI_ARGS_((unsigned char *string,
			    MFile *handle));
static int		Fwrite _ANSI_ARGS_((unsigned char *src, size_t size,
			    size_t count, FILE *file));
static int		Mwrite _ANSI_ARGS_((unsigned char *src, size_t size,
			    size_t count, MFile *handle));
static int		Fputc _ANSI_ARGS_((int c, FILE *file));
static int		Mputc _ANSI_ARGS_((int c, MFile *handle));
static void		mWriteInit _ANSI_ARGS_((Tcl_DString *buffer,
			    MFile *handle));

/*
 *----------------------------------------------------------------------
 *
 * FileMatchGIF --
 *
 *  This procedure is invoked by the photo image type to see if
 *  a file contains image data in GIF format.
 *
 * Results:
 *  The return value is 1 if the first characters in file f look
 *  like GIF data, and 0 otherwise.
 *
 * Side effects:
 *  The access position in f may change.
 *
 *----------------------------------------------------------------------
 */

static int
FileMatchGIF(f, fileName, formatString, widthPtr, heightPtr)
    FILE *f;			/* The image file, open for reading. */
    char *fileName;		/* The name of the image file. */
    char *formatString;		/* User-specified format string, or NULL. */
    int *widthPtr, *heightPtr;	/* The dimensions of the image are
				 * returned here if the file is a valid
				 * raw GIF file. */
{
	return ReadGIFHeader(f, widthPtr, heightPtr);
}

/*
 *----------------------------------------------------------------------
 *
 * FileReadGIF --
 *
 *	This procedure is called by the photo image type to read
 *	GIF format data from a file and write it into a given
 *	photo image.
 *
 * Results:
 *	A standard TCL completion code.  If TCL_ERROR is returned
 *	then an error message is left in interp->result.
 *
 * Side effects:
 *	The access position in file f is changed, and new data is
 *	added to the image given by imageHandle.
 *
 *----------------------------------------------------------------------
 */

typedef struct myblock {
    Tk_PhotoImageBlock ck;
    int dummy; /* extra space for offset[3], if not included already
		  in Tk_PhotoImageBlock */
} myblock;

#define block bl.ck

static int
FileReadGIF(interp, f, fileName, formatString, imageHandle, destX, destY,
	width, height, srcX, srcY)
    Tcl_Interp *interp;		/* Interpreter to use for reporting errors. */
    FILE *f;			/* The image file, open for reading. */
    char *fileName;		/* The name of the image file. */
    char *formatString;		/* User-specified format string, or NULL. */
    Tk_PhotoHandle imageHandle;	/* The photo image to write into. */
    int destX, destY;		/* Coordinates of top-left pixel in
				 * photo image to be written to. */
    int width, height;		/* Dimensions of block of photo image to
				 * be written to. */
    int srcX, srcY;		/* Coordinates of top-left pixel to be used
				 * in image being read. */
{
    int fileWidth, fileHeight;
    int nBytes;
    myblock bl;
    unsigned char buf[100];
    int bitPixel;
    unsigned int colorResolution;
    unsigned int background;
    unsigned int aspectRatio;
    unsigned char colorMap[MAXCOLORMAPSIZE][4];
    int transparent = -1;

    if (!ReadGIFHeader(f, &fileWidth, &fileHeight)) {
	Tcl_AppendResult(interp, "couldn't read GIF header from file \"",
		fileName, "\"", NULL);
	return TCL_ERROR;
    }
    if ((fileWidth <= 0) || (fileHeight <= 0)) {
	Tcl_AppendResult(interp, "GIF image file \"", fileName,
		"\" has dimension(s) <= 0", (char *) NULL);
	return TCL_ERROR;
    }

    if (Fread(buf, 1, 3, f) != 3) {
	return TCL_OK;
    }
    bitPixel = 2<<(buf[0]&0x07);
    colorResolution = ((((unsigned int) buf[0]&0x70)>>3)+1);
    background = buf[1];
    aspectRatio = buf[2];

    if (BitSet(buf[0], LOCALCOLORMAP)) {    /* Global Colormap */
	if (!ReadColorMap(f, bitPixel, colorMap)) {
	    Tcl_AppendResult(interp, "error reading color map",
		    (char *) NULL);
	    return TCL_ERROR;
	}
    }

    if ((srcX + width) > fileWidth) {
	width = fileWidth - srcX;
    }
    if ((srcY + height) > fileHeight) {
	height = fileHeight - srcY;
    }
    if ((width <= 0) || (height <= 0)
	    || (srcX >= fileWidth) || (srcY >= fileHeight)) {
	return TCL_OK;
    }

    Tk_PhotoExpand(imageHandle, destX + width, destY + height);

    block.width = width;
    block.height = height;
    block.pixelSize = 4;
    block.pitch = block.pixelSize * block.width;
    block.offset[0] = 0;
    block.offset[1] = 1;
    block.offset[2] = 2;
    block.offset[3] = 3;
    nBytes = height * block.pitch;
    block.pixelPtr = (unsigned char *) ckalloc((unsigned) nBytes);

    while (1) {
	if (Fread(buf, 1, 1, f) != 1) {
	    /*
	     * Premature end of image.  We should really notify
	     * the user, but for now just show garbage.
	     */

	    break;
	}

	if (buf[0] == ';') {
	    /*
	     * GIF terminator.
	     */

	    break;
	}

	if (buf[0] == '!') {
	    /*
	     * This is a GIF extension.
	     */

	    if (Fread(buf, 1, 1, f) != 1) {
		interp->result =
			"error reading extension function code in GIF image";
		goto error;
	    }
	    if (DoExtension(f, buf[0], &transparent) < 0) {
		interp->result = "error reading extension in GIF image";
		goto error;
	    }
	    continue;
	}

	if (buf[0] != ',') {
	    /*
	     * Not a valid start character; ignore it.
	     */
	    continue;
	}

	if (Fread(buf, 1, 9, f) != 9) {
	    interp->result = "couldn't read left/top/width/height in GIF image";
	    goto error;
	}

	bitPixel = 1<<((buf[8]&0x07)+1);

	if (BitSet(buf[8], LOCALCOLORMAP)) {
	    if (!ReadColorMap(f, bitPixel, colorMap)) {
		    Tcl_AppendResult(interp, "error reading color map", 
			    (char *) NULL);
		    goto error;
	    }
	}
	if (ReadImage(interp, (char *) block.pixelPtr, f, width, height,
		colorMap, fileWidth, fileHeight, srcX, srcY,
		BitSet(buf[8], INTERLACE), transparent) != TCL_OK) {
	    goto error;
	}
	break;
    }

    if (transparent == -1) {
	Tk_PhotoPutBlock(imageHandle, &block, destX, destY, width, height);
    } else {
	TkPhotoPutBlock(imageHandle, &block, destX, destY, width, height);
    }
    ckfree((char *) block.pixelPtr);
    return TCL_OK;

    error:
    ckfree((char *) block.pixelPtr);
    return TCL_ERROR;

}

/*
 *----------------------------------------------------------------------
 *
 * StringMatchGIF --
 *
 *  This procedure is invoked by the photo image type to see if
 *  a string contains image data in GIF format.
 *
 * Results:
 *  The return value is 1 if the first characters in the string
 *  like GIF data, and 0 otherwise.
 *
 * Side effects:
 *  the size of the image is placed in widthPre and heightPtr.
 *
 *----------------------------------------------------------------------
 */

static int
StringMatchGIF(string, formatString, widthPtr, heightPtr)
    char *string;		/* the string containing the image data */
    char *formatString;		/* the image format string */
    int *widthPtr;		/* where to put the string width */
    int *heightPtr;		/* where to put the string height */
{
    unsigned char header[10];
    int got;
    MFile handle;
    mInit((unsigned char *) string, &handle);
    got = Mread(header, 10, 1, &handle);
    if (got != 10
	    || ((strncmp("GIF87a", (char *) header, 6) != 0)
	    && (strncmp("GIF89a", (char *) header, 6) != 0))) {
	return 0;
    }
    *widthPtr = LM_to_uint(header[6],header[7]);
    *heightPtr = LM_to_uint(header[8],header[9]);
    return 1;
}

/*
 *----------------------------------------------------------------------
 *
 * StringReadGif -- --
 *
 *	This procedure is called by the photo image type to read
 *	GIF format data from a base64 encoded string, and give it to
 *	the photo image.
 *
 * Results:
 *	A standard TCL completion code.  If TCL_ERROR is returned
 *	then an error message is left in interp->result.
 *
 * Side effects:
 *	new data is added to the image given by imageHandle.  This
 *	procedure calls FileReadGif by redefining the operation of
 *	fprintf temporarily.
 *
 *----------------------------------------------------------------------
 */

static int
StringReadGIF(interp,string,formatString,imageHandle,
	destX, destY, width, height, srcX, srcY)
    Tcl_Interp *interp;		/* interpreter for reporting errors in */
    char *string;		/* string containing the image */
    char *formatString;		/* format string if any */
    Tk_PhotoHandle imageHandle;	/* the image to write this data into */
    int destX, destY;		/* The rectangular region of the  */
    int  width, height;		/*   image to copy */
    int srcX, srcY;
{
	int result;
	MFile handle;
	mInit((unsigned char *)string,&handle);
	fromData = 1;
	result = FileReadGIF(interp, (FILE *) &handle, "inline data",
		formatString, imageHandle, destX, destY, width, height,
		srcX, srcY);
	fromData = 0;
	return(result);
}

/*
 *----------------------------------------------------------------------
 *
 * ReadGIFHeader --
 *
 *	This procedure reads the GIF header from the beginning of a
 *	GIF file and returns the dimensions of the image.
 *
 * Results:
 *	The return value is 1 if file "f" appears to start with
 *	a valid GIF header, 0 otherwise.  If the header is valid,
 *	then *widthPtr and *heightPtr are modified to hold the
 *	dimensions of the image.
 *
 * Side effects:
 *	The access position in f advances.
 *
 *----------------------------------------------------------------------
 */

static int
ReadGIFHeader(f, widthPtr, heightPtr)
    FILE *f;			/* Image file to read the header from */
    int *widthPtr, *heightPtr;	/* The dimensions of the image are
				 * returned here. */
{
    unsigned char buf[7];

    if ((Fread(buf, 1, 6, f) != 6)
	    || ((strncmp("GIF87a", (char *) buf, 6) != 0)
	    && (strncmp("GIF89a", (char *) buf, 6) != 0))) {
	return 0;
    }

    if (Fread(buf, 1, 4, f) != 4) {
	return 0;
    }

    *widthPtr = LM_to_uint(buf[0],buf[1]);
    *heightPtr = LM_to_uint(buf[2],buf[3]);
    return 1;
}

/*
 *-----------------------------------------------------------------
 * The code below is copied from the giftoppm program and modified
 * just slightly.
 *-----------------------------------------------------------------
 */

static int
ReadColorMap(fd,number,buffer)
FILE        *fd;
int     number;
unsigned char   buffer[MAXCOLORMAPSIZE][4];
{
	int     i;
	unsigned char   rgb[3];

	for (i = 0; i < number; ++i) {
		if (! ReadOK(fd, rgb, sizeof(rgb)))
			return 0;

		buffer[i][CM_RED] = rgb[0] ;
		buffer[i][CM_GREEN] = rgb[1] ;
		buffer[i][CM_BLUE] = rgb[2] ;
		buffer[i][CM_ALPHA] = 255 ;
	}
	return 1;
}



static int
DoExtension(fd, label, transparent)
FILE    *fd;
int label;
int	*transparent;
{
	static unsigned char buf[256];
	int count = 0;

	switch (label) {
		case 0x01:      /* Plain Text Extension */
			break;

		case 0xff:      /* Application Extension */
			break;

		case 0xfe:      /* Comment Extension */
			do {
				count = GetDataBlock(fd, (unsigned char*) buf);
			} while (count > 0);
			return count;

		case 0xf9:      /* Graphic Control Extension */
			count = GetDataBlock(fd, (unsigned char*) buf);
			if (count < 0) {
				return 1;
			}
			if ((buf[0] & 0x1) != 0) {
				*transparent = buf[3];
			}

			do {
			    count = GetDataBlock(fd, (unsigned char*) buf);
			} while (count > 0);
			return count;
	}

	do {
	    count = GetDataBlock(fd, (unsigned char*) buf);
	} while (count > 0);
	return count;
}

static int ZeroDataBlock = 0;

static int
GetDataBlock(fd, buf)
FILE        *fd;
unsigned char   *buf;
{
	unsigned char   count;

	if (! ReadOK(fd,&count,1)) {
		return -1;
	}

	ZeroDataBlock = count == 0;

	if ((count != 0) && (! ReadOK(fd, buf, count))) {
		return -1;
	}

	return count;
}


static int
ReadImage(interp, imagePtr, fd, len, rows, cmap,
	width, height, srcX, srcY, interlace, transparent)
Tcl_Interp *interp;
char 	*imagePtr;
FILE    *fd;
int len, rows;
unsigned char   cmap[MAXCOLORMAPSIZE][4];
int width, height;
int srcX, srcY;
int interlace;
int transparent;
{
	unsigned char   c;
	int     v;
	int     xpos = 0, ypos = 0, pass = 0;
	char	*pixelPtr;


	/*
	 *  Initialize the Compression routines
	 */
	if (! ReadOK(fd,&c,1))  {
	    Tcl_AppendResult(interp, "error reading GIF image: ",
		    Tcl_PosixError(interp), (char *) NULL);
	    return TCL_ERROR;
	}

	if (LWZReadByte(fd, 1, c) < 0) {
	    interp->result = "format error in GIF image";
	    return TCL_ERROR;
	}

	if (transparent!=-1) {
	    cmap[transparent][CM_RED] = 0;
	    cmap[transparent][CM_GREEN] = 0;
	    cmap[transparent][CM_BLUE] = 0;
	    cmap[transparent][CM_ALPHA] = 0;
	}

	pixelPtr = imagePtr;
	while ((v = LWZReadByte(fd,0,c)) >= 0 ) {

		if ((xpos>=srcX) && (xpos<srcX+len) &&
			(ypos>=srcY) && (ypos<srcY+rows)) {
		    *pixelPtr++ = cmap[v][CM_RED];
		    *pixelPtr++ = cmap[v][CM_GREEN];
		    *pixelPtr++ = cmap[v][CM_BLUE];
		    *pixelPtr++ = cmap[v][CM_ALPHA];
		}
		++xpos;
		if (xpos == width) {
			xpos = 0;
			if (interlace) {
				switch (pass) {
					case 0:
					case 1:
						ypos += 8; break;
					case 2:
						ypos += 4; break;
					case 3:
						ypos += 2; break;
				}

				while (ypos >= height) {
					++pass;
					switch (pass) {
						case 1:
							ypos = 4; break;
						case 2:
							ypos = 2; break;
						case 3:
							ypos = 1; break;
						default:
							return TCL_OK;
					}
				}
			} else {
				++ypos;
			}
			pixelPtr = imagePtr + (ypos-srcY) * len * 4;
		}
		if (ypos >= height)
			break;
	}
	return TCL_OK;
}

static int
LWZReadByte(fd, flag, input_code_size)
FILE    *fd;
int flag;
int input_code_size;
{
	static int  fresh = 0;
	int     code, incode;
	static int  code_size, set_code_size;
	static int  max_code, max_code_size;
	static int  firstcode, oldcode;
	static int  clear_code, end_code;
	static int  table[2][(1<< MAX_LWZ_BITS)];
	static int  stack[(1<<(MAX_LWZ_BITS))*2], *sp;
	register int    i;


	if (flag) {

		set_code_size = input_code_size;
		code_size = set_code_size+1;
		clear_code = 1 << set_code_size ;
		end_code = clear_code + 1;
		max_code_size = 2*clear_code;
		max_code = clear_code+2;

		GetCode(fd, 0, 1);

		fresh = 1;

		for (i = 0; i < clear_code; ++i) {
			table[0][i] = 0;
			table[1][i] = i;
		}
		for (; i < (1<<MAX_LWZ_BITS); ++i) {
			table[0][i] = table[1][0] = 0;
		}

		sp = stack;

		return 0;

	} else if (fresh) {

		fresh = 0;
		do {
			firstcode = oldcode = GetCode(fd, code_size, 0);
		} while (firstcode == clear_code);
		return firstcode;
	}

	if (sp > stack)
		return *--sp;

	while ((code = GetCode(fd, code_size, 0)) >= 0) {
		if (code == clear_code) {
			for (i = 0; i < clear_code; ++i) {
				table[0][i] = 0;
				table[1][i] = i;
			}

			for (; i < (1<<MAX_LWZ_BITS); ++i) {
				table[0][i] = table[1][i] = 0;
			}

			code_size = set_code_size+1;
			max_code_size = 2*clear_code;
			max_code = clear_code+2;
			sp = stack;
			firstcode = oldcode = GetCode(fd, code_size, 0);
			return firstcode;

	} else if (code == end_code) {
		int     count;
		unsigned char   buf[260];

		if (ZeroDataBlock)
			return -2;

		while ((count = GetDataBlock(fd, buf)) > 0)
			;

		if (count != 0)
			return -2;
	}

	incode = code;

	if (code >= max_code) {
		*sp++ = firstcode;
		code = oldcode;
	}

	while (code >= clear_code) {
		*sp++ = table[1][code];
		if (code == table[0][code]) {
			return -2;

			/*
			 * Used to be this instead, Steve Ball suggested
			 * the change to just return.

			printf("circular table entry BIG ERROR\n");
			*/
		}
		code = table[0][code];
	}

	*sp++ = firstcode = table[1][code];

	if ((code = max_code) <(1<<MAX_LWZ_BITS)) {

		table[0][code] = oldcode;
		table[1][code] = firstcode;
		++max_code;
		if ((max_code>=max_code_size) && (max_code_size < (1<<MAX_LWZ_BITS))) {
			max_code_size *= 2;
			++code_size;
		}
	}

	oldcode = incode;

	if (sp > stack)
		return *--sp;
	}
	return code;
}


static int
GetCode(fd, code_size, flag)
FILE    *fd;
int code_size;
int flag;
{
	static unsigned char    buf[280];
	static int      curbit, lastbit, done, last_byte;
	int         i, j, ret;
	unsigned char       count;

	if (flag) {
		curbit = 0;
		lastbit = 0;
		done = 0;
		return 0;
	}


	if ( (curbit+code_size) >= lastbit) {
		if (done) {
			/* ran off the end of my bits */
			return -1;
		}
		buf[0] = buf[last_byte-2];
		buf[1] = buf[last_byte-1];

		if ((count = GetDataBlock(fd, &buf[2])) == 0)
			done = 1;

		last_byte = 2 + count;
		curbit = (curbit - lastbit) + 16;
		lastbit = (2+count)*8 ;
	}

	ret = 0;
	for (i = curbit, j = 0; j < code_size; ++i, ++j)
		ret |= ((buf[ i / 8 ] & (1 << (i % 8))) != 0) << j;


	curbit += code_size;

	return ret;
}

/*
 * This software is copyrighted as noted below.  It may be freely copied,
 * modified, and redistributed, provided that the copyright notice is
 * preserved on all copies.
 *
 * There is no warranty or other guarantee of fitness for this software,
 * it is provided solely "as is".  Bug reports or fixes may be sent
 * to the author, who may or may not act on them as he desires.
 *
 * You may not include this software in a program or other software product
 * without supplying the source, or without informing the end-user that the
 * source is available for no extra charge.
 *
 * If you modify this software, you should include a notice giving the
 * name of the person performing the modification, the date of modification,
 * and the reason for such modification.
 */






/*
 * FileWriteGIF - writes a image in GIF format.
 *-------------------------------------------------------------------------
 * Author:          		Lolo
 *                              Engeneering Projects Area 
 *	            		Department of Mining 
 *                  		University of Oviedo
 * e-mail			zz11425958@zeus.etsimo.uniovi.es
 *                  		lolo@pcsig22.etsimo.uniovi.es
 * Date:            		Fri September 20 1996
 *----------------------------------------------------------------------
 * FileWriteGIF-
 *
 *    This procedure is called by the photo image type to write
 *    GIF format data from a photo image into a given file 
 *
 * Results:
 *	A standard TCL completion code.  If TCL_ERROR is returned
 *	then an error message is left in interp->result.
 *
 *----------------------------------------------------------------------
 */

 /*
  *  Types, defines and variables needed to write and compress a GIF.
  */

typedef int (* ifunptr) _ANSI_ARGS_((void));	

#define LSB(a)                  ((unsigned char) (((short)(a)) & 0x00FF))
#define MSB(a)                  ((unsigned char) (((short)(a)) >> 8))

#define GIFBITS 12
#define HSIZE  5003            /* 80% occupancy */
#define LUGUSED                 12345

static int ssize;
static int csize;
static int rsize;
static unsigned char *pixelo;
static int pixelSize;
static int pixelPitch;
static int greenOffset;
static int blueOffset;
static int alphaOffset;
static int num;
static unsigned char mapa[MAXCOLORMAPSIZE][3];

/*
 * I use a intermediate format ( a simple bitmap ) with
 * this format ...
 */

typedef struct {
    int xsize, ysize;		/* sizes */
    int depth;			/* # of colors */
    int colors;			/* # of colors */
    unsigned char *r, *g, *b;	/* components or bitmap (planes < 8) */
    unsigned char *cmap;	/* cmap if planes < 8 */
    int magic;			/* used ? */
} bitmap_hdr;

/*
 *	Definition of new functions to write GIFs
 */

static int quantize _ANSI_ARGS_((bitmap_hdr *inbitmap,
		bitmap_hdr *outbitmap, int no_colors));
static int color _ANSI_ARGS_((int red,int green, int blue));
static void compress _ANSI_ARGS_((int init_bits, FILE *outfile,
		ifunptr readValue));
static int nuevo _ANSI_ARGS_((int red, int green ,int blue,
		unsigned char mapa[MAXCOLORMAPSIZE][3]));
static int savemap _ANSI_ARGS_((Tk_PhotoImageBlock *blockPtr,
		unsigned char mapa[MAXCOLORMAPSIZE][3]));
static int ReadValue _ANSI_ARGS_((void));
static int no_bits _ANSI_ARGS_((int colors));

static int
FileWriteGIF (interp, filename, formatString, blockPtr)
    Tcl_Interp *interp;		/* Interpreter to use for reporting errors. */
    char	*filename;
    char	*formatString;
    Tk_PhotoImageBlock *blockPtr;
{
    FILE *fp;
    Tcl_DString nameBuffer; 
    char *fullName;
    int result;

    if ((fullName=Tcl_TranslateFileName(interp,filename,&nameBuffer))==NULL) {
	return TCL_ERROR;
    }
    if (!(fp=fopen(fullName,"wb"))) {
	Tcl_AppendResult(interp, filename, ": ", Tcl_PosixError(interp),
		(char *)NULL);
	Tcl_DStringFree(&nameBuffer);
	return TCL_ERROR;
    }
    Tcl_DStringFree(&nameBuffer);
    result = WriteGIF(interp, fp, formatString, blockPtr);
    fclose(fp);
    return result;
}

static int
StringWriteGIF(interp, dataPtr, formatString, blockPtr)
    Tcl_Interp *interp;
    Tcl_DString *dataPtr;
    char *formatString;
    Tk_PhotoImageBlock *blockPtr;
{
    int result;
    MFile handle;
    mWriteInit(dataPtr,&handle);

    fromData = 1;
    result = WriteGIF(interp, (FILE *) &handle, formatString, blockPtr);
    switch(handle.state) {
	case 1:
	    *handle.data++ = base64_table[(handle.c<<4)&63];
	    *handle.data++ = '='; *handle.data++ = '='; break;
	case 2:
	    *handle.data++ = base64_table[(handle.c<<2)&63];
	    *handle.data++ = '='; break;
    }
    Tcl_DStringSetLength(dataPtr,
	    ((char *) handle.data) - Tcl_DStringValue(dataPtr));
    fromData = 0;

    return(result);
}

static int
WriteGIF(interp, fp, formatString, blockPtr)
    Tcl_Interp *interp;
    FILE *fp;
    char *formatString;
    Tk_PhotoImageBlock *blockPtr;
{
    int  resolution;
    long  numcolormap;

    long  width,height,x;
    unsigned char c;
    unsigned int top,left;
    int num;
    bitmap_hdr in,new;

    top = 0;
    left = 0;

    pixelSize=blockPtr->pixelSize;
    greenOffset=blockPtr->offset[1]-blockPtr->offset[0];
    blueOffset=blockPtr->offset[2]-blockPtr->offset[0];
    alphaOffset = blockPtr->offset[0];
    if (alphaOffset < blockPtr->offset[2]) {
	alphaOffset = blockPtr->offset[2];
    }
    if (++alphaOffset < pixelSize) {
	alphaOffset -= blockPtr->offset[0];
    } else {
	alphaOffset = 0;
    }

    Fwrite(alphaOffset ? "GIF89a":"GIF87a",1,6,fp);
    
    for (x=0;x<MAXCOLORMAPSIZE;x++) {
	mapa[x][CM_RED] = 255;
	mapa[x][CM_GREEN] = 255;
	mapa[x][CM_BLUE] = 255;
    }

	
    width=blockPtr->width;
    height=blockPtr->height;
    pixelo=blockPtr->pixelPtr + blockPtr->offset[0];
    pixelPitch=blockPtr->pitch;
    if ((num=savemap(blockPtr,mapa))<0) {
	Tcl_AppendResult(interp, "too many colors", (char *) NULL);
      if (num < -1) { /* temporary removed because it doesn't work*/
	in.xsize=width;
	in.ysize=height;
	in.magic= LUGUSED;
	quantize(&in,&new,MAXCOLORMAPSIZE);
	num=savemap(blockPtr,mapa);
      }
	return TCL_ERROR;
    }
    if (num<3) num=3;
    c=LSB(width);
    Fputc(c,fp);
    c=MSB(width);
    Fputc(c,fp);
    c=LSB(height);
    Fputc(c,fp);
    c=MSB(height);
    Fputc(c,fp);

    c= (1 << 7) | (no_bits(num) << 4) | (no_bits(num));
    Fputc(c,fp);
    resolution = no_bits(num)+1;

    numcolormap=1 << resolution;
    c=0;

    /*  background color */
	
    Fputc(c,fp);

    /*  zero for future expansion  */

    Fputc(c,fp);

    for (x=0; x<numcolormap ;x++) {
	Fputc(mapa[x][CM_RED],fp);
	Fputc(mapa[x][CM_GREEN],fp);
	Fputc(mapa[x][CM_BLUE],fp);
    }

    /*
     * Write out extension for transparent colour index, if necessary.
     */
    if (alphaOffset) {
	Fputc( '!', fp);
	Fputc(0xf9, fp);
	Fputc(   4, fp);
	Fputc(   1, fp);
	Fputc(   0, fp);
	Fputc(   0, fp);
	Fputc(   0, fp);
	Fputc(   0, fp);
    }

    c=',';
    Fputc(c,fp);
    c=LSB(top);
    Fputc(c,fp);
    c=MSB(top);
    Fputc(c,fp);
    c=LSB(left);
    Fputc(c,fp);
    c=MSB(left);
    Fputc(c,fp);

    c=LSB(width);
    Fputc(c,fp);
    c=MSB(width);
    Fputc(c,fp);

    c=LSB(height);
    Fputc(c,fp);
    c=MSB(height);
    Fputc(c,fp);

    c=0;
    Fputc(c,fp);
    c=resolution;
    Fputc(c,fp);

    ssize = rsize = blockPtr->width;
    csize = blockPtr->height;
    compress(resolution+1,fp,ReadValue);
 
    Fputc(0,fp);
    Fputc(';',fp);

    return TCL_OK;	
}

static int
color(red, green, blue)
    int red;
    int green;
    int blue;
{
    int x;
    for (x=(alphaOffset != 0);x<=MAXCOLORMAPSIZE;x++) {
	if ((mapa[x][CM_RED]==red) && (mapa[x][CM_GREEN]==green) &&
		(mapa[x][CM_BLUE]==blue)) {
	    return x;
	}
    }
    return -1;
}


static int
nuevo(red, green, blue, mapa)
    int red,green,blue;
    unsigned char mapa[MAXCOLORMAPSIZE][3];
{
    int x;
    for (x=(alphaOffset != 0);x<num;x++) {
	if ((mapa[x][CM_RED]==red) && (mapa[x][CM_GREEN]==green) &&
		(mapa[x][CM_BLUE]==blue)) {
	    return 0;
	}
    }
    return 1;
}

static int
savemap(blockPtr,mapa)
    Tk_PhotoImageBlock *blockPtr;
    unsigned char mapa[MAXCOLORMAPSIZE][3];
{
    unsigned char  *colores;
    int x,y;
    unsigned char  red,green,blue;

    if (alphaOffset) {
	num = 1;
	mapa[0][CM_RED] = 0xc9;
	mapa[0][CM_GREEN] = 0xc9;
	mapa[0][CM_BLUE] = 0xc9;
    } else {
	num = 0;
    }
    /* put black, white and gray in table; */
    mapa[num][CM_RED] = 0;
    mapa[num][CM_GREEN] = 0;
    mapa[num][CM_BLUE] = 0;
    mapa[num+1][CM_RED] = 255;
    mapa[num+1][CM_GREEN] = 255;
    mapa[num+1][CM_BLUE] = 255;
    mapa[num+2][CM_RED] = 0xc9;
    mapa[num+2][CM_GREEN] = 0xc9;
    mapa[num+2][CM_BLUE] = 0xc9;
    for(y=0;y<blockPtr->height;y++) {
	colores=blockPtr->pixelPtr + blockPtr->offset[0]
		+ y * blockPtr->pitch;
	for(x=0;x<blockPtr->width;x++) {
	    if (!alphaOffset || (colores[alphaOffset] != 0)) {
		red = colores[0];
		green = colores[greenOffset];
		blue = colores[blueOffset];
		if (nuevo(red,green,blue,mapa)) {
		    if (num>255) 
			return -1;

		    mapa[num][CM_RED]=red;
		    mapa[num][CM_GREEN]=green;
		    mapa[num][CM_BLUE]=blue;
		    num++;
		}
	    }
	    colores += pixelSize;
	}
    }
    return num-1;
}

static int
ReadValue()
{
    unsigned int col;

    if ((ssize == 0) && (csize == 0)) {
	return EOF;
    }
    if (alphaOffset && (pixelo[alphaOffset]==0)) {
	col = 0;
    } else {
	col = color(pixelo[0],pixelo[greenOffset],pixelo[blueOffset]);
    }
    pixelo += pixelSize;
    if (--ssize <= 0) {
	ssize = rsize;
	csize--;
	pixelo += pixelPitch - (rsize * pixelSize);
    }

    return col;
}

/*
 * Return the number of bits ( -1 ) to represent a given
 * number of colors ( ex: 256 colors => 7 ).
 */
static int
no_bits( colors )
int colors;
{
    register int bits = 0;

    colors--;
    while ( colors >> bits ) {
	bits++;
    }

    return (bits-1);
}


/*
 *
 * GIF Image compression - modified 'compress'
 *
 * Based on: compress.c - File compression ala IEEE Computer, June 1984.
 *
 * By Authors:  Spencer W. Thomas       (decvax!harpo!utah-cs!utah-gr!thomas)
 *              Jim McKie               (decvax!mcvax!jim)
 *              Steve Davies            (decvax!vax135!petsd!peora!srd)
 *              Ken Turkowski           (decvax!decwrl!turtlevax!ken)
 *              James A. Woods          (decvax!ihnp4!ames!jaw)
 *              Joe Orost               (decvax!vax135!petsd!joe)
 *
 */
#include <ctype.h>

static void output _ANSI_ARGS_((long code));
static void cl_block _ANSI_ARGS_((void));
static void cl_hash _ANSI_ARGS_((int hsize));
static void char_init _ANSI_ARGS_((void));
static void char_out _ANSI_ARGS_((int c));
static void flush_char _ANSI_ARGS_((void));

static int n_bits;		/* number of bits/code */
static int maxbits = GIFBITS;	/* user settable max # bits/code */
static long maxcode;		/* maximum code, given n_bits */
static long maxmaxcode = (long)1 << GIFBITS;
				/* should NEVER generate this code */
#define MAXCODE(n_bits)		(((long) 1 << (n_bits)) - 1)

static int		htab[HSIZE];
static unsigned int	codetab[HSIZE];
#define HashTabOf(i)	htab[i]
#define CodeTabOf(i)	codetab[i]

static long hsize = HSIZE;	/* for dynamic table sizing */

/*
 * To save much memory, we overlay the table used by compress() with those
 * used by decompress().  The tab_prefix table is the same size and type
 * as the codetab.  The tab_suffix table needs 2**GIFBITS characters.  We
 * get this from the beginning of htab.  The output stack uses the rest
 * of htab, and contains characters.  There is plenty of room for any
 * possible stack (stack used to be 8000 characters).
 */

static int free_ent = 0;  /* first unused entry */

/*
 * block compression parameters -- after all codes are used up,
 * and compression rate changes, start over.
 */
static int clear_flg = 0;

static int offset;
static unsigned int in_count = 1;            /* length of input */
static unsigned int out_count = 0;           /* # of codes output (for debugging) */

/*
 * compress stdin to stdout
 *
 * Algorithm:  use open addressing double hashing (no chaining) on the
 * prefix code / next character combination.  We do a variant of Knuth's
 * algorithm D (vol. 3, sec. 6.4) along with G. Knott's relatively-prime
 * secondary probe.  Here, the modular division first probe is gives way
 * to a faster exclusive-or manipulation.  Also do block compression with
 * an adaptive reset, whereby the code table is cleared when the compression
 * ratio decreases, but after the table fills.  The variable-length output
 * codes are re-sized at this point, and a special CLEAR code is generated
 * for the decompressor.  Late addition:  construct the table according to
 * file size for noticeable speed improvement on small files.  Please direct
 * questions about this implementation to ames!jaw.
 */

static int g_init_bits;
static FILE *g_outfile;

static int ClearCode;
static int EOFCode;

static void compress( init_bits, outfile, readValue )
    int init_bits;
    FILE *outfile;
    ifunptr readValue;
{
    register long fcode;
    register long i = 0;
    register int c;
    register long ent;
    register long disp;
    register long hsize_reg;
    register int hshift;

    /*
     * Set up the globals:  g_init_bits - initial number of bits
     *                      g_outfile   - pointer to output file
     */
    g_init_bits = init_bits;
    g_outfile = outfile;

    /*
     * Set up the necessary values
     */
    offset = 0;
    out_count = 0;
    clear_flg = 0;
    in_count = 1;
    maxcode = MAXCODE(n_bits = g_init_bits);

    ClearCode = (1 << (init_bits - 1));
    EOFCode = ClearCode + 1;
    free_ent = ClearCode + 2;

    char_init();

    ent = readValue();

    hshift = 0;
    for ( fcode = (long) hsize;  fcode < 65536L; fcode *= 2L )
        hshift++;
    hshift = 8 - hshift;                /* set hash code range bound */

    hsize_reg = hsize;
    cl_hash( (int) hsize_reg);            /* clear hash table */

    output( (long)ClearCode );

#ifdef SIGNED_COMPARE_SLOW
    while ( (c = readValue() ) != (unsigned) EOF ) {
#else
    while ( (c = readValue()) != EOF ) {
#endif

        in_count++;

        fcode = (long) (((long) c << maxbits) + ent);
        i = (((long)c << hshift) ^ ent);    /* xor hashing */

        if ( HashTabOf (i) == fcode ) {
            ent = CodeTabOf (i);
            continue;
        } else if ( (long) HashTabOf (i) < 0 )      /* empty slot */
            goto nomatch;
        disp = hsize_reg - i;           /* secondary hash (after G. Knott) */
        if ( i == 0 )
            disp = 1;
probe:
        if ( (i -= disp) < 0 )
            i += hsize_reg;

        if ( HashTabOf(i) == fcode ) {
            ent = CodeTabOf (i);
            continue;
        }
        if ( (long) HashTabOf(i) > 0 )
            goto probe;
nomatch:
        output ( (long) ent );
        out_count++;
        ent = c;
#ifdef SIGNED_COMPARE_SLOW
        if ( (unsigned) free_ent < (unsigned) maxmaxcode) {
#else
        if ( free_ent < maxmaxcode ) {
#endif
            CodeTabOf (i) = free_ent++; /* code -> hashtable */
            HashTabOf (i) = fcode;
        } else
                cl_block();
    }
    /*
     * Put out the final code.
     */
    output( (long)ent );
    out_count++;
    output( (long) EOFCode );

    return;
}

/*****************************************************************
 * TAG( output )
 *
 * Output the given code.
 * Inputs:
 *      code:   A n_bits-bit integer.  If == -1, then EOF.  This assumes
 *              that n_bits =< (long) wordsize - 1.
 * Outputs:
 *      Outputs code to the file.
 * Assumptions:
 *      Chars are 8 bits long.
 * Algorithm:
 *      Maintain a GIFBITS character long buffer (so that 8 codes will
 * fit in it exactly).  Use the VAX insv instruction to insert each
 * code in turn.  When the buffer fills up empty it and start over.
 */

static unsigned long cur_accum = 0;
static int  cur_bits = 0;

static
unsigned long masks[] = { 0x0000, 0x0001, 0x0003, 0x0007, 0x000F,
                                  0x001F, 0x003F, 0x007F, 0x00FF,
                                  0x01FF, 0x03FF, 0x07FF, 0x0FFF,
                                  0x1FFF, 0x3FFF, 0x7FFF, 0xFFFF };

static void
output(code)
    long  code;
{
    cur_accum &= masks[cur_bits];

    if (cur_bits > 0) {
	cur_accum |= ((long) code << cur_bits);
    } else {
	cur_accum = code;
    }

    cur_bits += n_bits;

    while (cur_bits >= 8 ) {
	char_out((unsigned int)(cur_accum & 0xff));
	cur_accum >>= 8;
	cur_bits -= 8;
    }

    /*
     * If the next entry is going to be too big for the code size,
     * then increase it, if possible.
     */

    if ((free_ent > maxcode)|| clear_flg ) {
	if (clear_flg) {
	    maxcode = MAXCODE(n_bits = g_init_bits);
	    clear_flg = 0;
	} else {
	    n_bits++;
	    if (n_bits == maxbits) {
		maxcode = maxmaxcode;
	    } else {
		maxcode = MAXCODE(n_bits);
	    }
	}
    }

    if (code == EOFCode) {
	/*
	 * At EOF, write the rest of the buffer.
	 */
        while (cur_bits > 0) {
	    char_out((unsigned int)(cur_accum & 0xff));
	    cur_accum >>= 8;
	    cur_bits -= 8;
	}
	flush_char();
    }
}

/*
 * Clear out the hash table
 */
static void
cl_block()             /* table clear for block compress */
{

        cl_hash ( (int) hsize );
        free_ent = ClearCode + 2;
        clear_flg = 1;

        output((long) ClearCode);
}

static void
cl_hash(hsize)          /* reset code table */
    int hsize;
{
    register int *htab_p = htab+hsize;
    register long i;
    register long m1 = -1;

    i = hsize - 16;
    do {                            /* might use Sys V memset(3) here */
	*(htab_p-16) = m1;
	*(htab_p-15) = m1;
	*(htab_p-14) = m1;
	*(htab_p-13) = m1;
	*(htab_p-12) = m1;
	*(htab_p-11) = m1;
	*(htab_p-10) = m1;
	*(htab_p-9) = m1;
	*(htab_p-8) = m1;
	*(htab_p-7) = m1;
	*(htab_p-6) = m1;
	*(htab_p-5) = m1;
	*(htab_p-4) = m1;
	*(htab_p-3) = m1;
	*(htab_p-2) = m1;
	*(htab_p-1) = m1;
	htab_p -= 16;
    } while ((i -= 16) >= 0);

    for (i += 16; i > 0; i--) {
	*--htab_p = m1;
    }
}

/******************************************************************************
 *
 * GIF Specific routines
 *
 ******************************************************************************/

/*
 * Number of characters so far in this 'packet'
 */
static int a_count;

/*
 * Set up the 'byte output' routine
 */
static void
char_init()
{
    a_count = 0;
    cur_accum = 0;
    cur_bits = 0;
}

/*
 * Define the storage for the packet accumulator
 */
static unsigned char accum[256];

/*
 * Add a character to the end of the current packet, and if it is 254
 * characters, flush the packet to disk.
 */
static void
char_out( c )
    int c;
{
    accum[a_count++] = c;
    if (a_count >= 254) {
	flush_char();
    }
}

/*
 * Flush the packet to disk, and reset the accumulator
 */
static void
flush_char()
{
    unsigned char c;
    if (a_count > 0) {
	c = a_count;
	Fwrite(&c, 1, 1, g_outfile);
	Fwrite(accum, 1, a_count, g_outfile);
	a_count = 0;
    }
}

/* The End */

/*
 * This software is copyrighted as noted below.  It may be freely copied,
 * modified, and redistributed, provided that the copyright notice is
 * preserved on all copies.
 *
 * There is no warranty or other guarantee of fitness for this software,
 * it is provided solely "as is".  Bug reports or fixes may be sent
 * to the author, who may or may not act on them as he desires.
 *
 * You may not include this software in a program or other software product
 * without supplying the source, or without informing the end-user that the
 * source is available for no extra charge.
 *
 * If you modify this software, you should include a notice giving the
 * name of the person performing the modification, the date of modification,
 * and the reason for such modification.
 */


/*
 * quantize.c - quantize a bitmap.
 *
 * FileWriteGIF's author:    	Lolo
 *                  		University of Oviedo
 * Date:            		Fri Oct 20 1995 
*/

/*
 * History:
 *
 * Raul Rivero - Fri Jun 26 1992
 *      One problem was detected when multiple calls to the 
 *      quantizer. The resulting color map was incoherent. The 
 *      problem was fixed reseting all the static buffers.
 *
 */

struct box {
    int r0;                      /* min value, exclusive */
    int r1;                      /* max value, inclusive */
    int g0;
    int g1;
    int b0;
    int b1;
    int vol;
};

/*
 * Histogram is in elements 1..HISTSIZE along each axis,
 * element 0 is for base or marginal value
 * NB: these must start out 0!
 */

static float	m2[33][33][33];
static long	wt[33][33][33], mr[33][33][33], mg[33][33][33], mb[33][33][33];
static unsigned char	*Ir, *Ig, *Ib;
static int	size; /*image size*/
static int	K;    /*color look-up table size*/
static unsigned short   *Qadd;

static double Var _ANSI_ARGS_((struct box *cube));
static double Maximize _ANSI_ARGS_((struct box *cube, int dir,
		int first, int last, int *cut, long whole_r, long whole_g,
		long whole_b, long whole_w));
static long Vol _ANSI_ARGS_((struct box *cube,long mmt[33][33][33]));
static long Bottom _ANSI_ARGS_((struct box *cube, int dir,
		long mmt[33][33][33]));
static long Top _ANSI_ARGS_((struct box *cube, int dir, int pos,
		long mmt[33][33][33]));
static void Mark _ANSI_ARGS_((struct box *cube, int label, unsigned char *tag));
static void Hist3d _ANSI_ARGS_((long *vwt, long *vmr, long *vmg,
		long *vmb, float *m2));
static void M3d _ANSI_ARGS_((long *vwt, long *vmr, long *vmg, long *vmb,
		float *m2));
static int Cut _ANSI_ARGS_((struct box *set1, struct box *set2));

static int quantize(inbitmap, outbitmap, no_colors)
    bitmap_hdr *inbitmap;
    bitmap_hdr *outbitmap;
    int no_colors;
{
    struct box cube[MAXCOLORMAPSIZE];
    unsigned char *tag;
    unsigned char lut_r[MAXCOLORMAPSIZE], lut_g[MAXCOLORMAPSIZE],
	    lut_b[MAXCOLORMAPSIZE];
    int next;
    register long i, weight;
    register int k;
    double vv[MAXCOLORMAPSIZE], temp;
    unsigned char *ptr;
    unsigned char *line;
    int y;

    if ( inbitmap->magic != LUGUSED ) {
	return TCL_ERROR;
    }

    /* set the image */
    K  = no_colors;
    Ir = inbitmap->r;
    Ig = inbitmap->g;
    Ib = inbitmap->b;
    size = inbitmap->xsize * inbitmap->ysize;

    /* Reset the buffers */
    memset( m2, 0, 33*33*33*sizeof(float) );
    memset( wt, 0, 33*33*33*sizeof(long) );
    memset( mr, 0, 33*33*33*sizeof(long) );
    memset( mg, 0, 33*33*33*sizeof(long) );
    memset( mb, 0, 33*33*33*sizeof(long) );

    Hist3d((long *) wt, (long *) mr, (long *) mg, (long *) mb, (float *) m2);

    M3d((long *) wt, (long *) mr, (long *) mg, (long *) mb, (float *) m2);

    cube[0].r0 = cube[0].g0 = cube[0].b0 = 0;
    cube[0].r1 = cube[0].g1 = cube[0].b1 = 32;
    next = 0;
    for ( i = 1; i < K; ++i ) {
	if ( Cut(&cube[next], &cube[i]) ) {
	    /* volume test ensures we won't try to cut one-cell box */
	    vv[next] = (cube[next].vol>1) ? Var(&cube[next]) : 0.0;
	    vv[i] = (cube[i].vol>1) ? Var(&cube[i]) : 0.0;
	} else {
	    vv[next] = 0.0;   /* don't try to split this box again */
	    i--;              /* didn't create box i */
	}
	next = 0; temp = vv[0];
	for (k = 1;k <= i; ++k) {
	    if ( vv[k] > temp ) {
		temp = vv[k]; next = k;
	    }
	}
	if (temp <= 0.0) {
	    K = i+1;
	    break;
	}
    }
    /*  Partition done  */

    /* the space for array m2 can be freed now */

    tag = (unsigned char *) ckalloc(33*33*33);
    memset(tag, 0, 33*33*33);
    for (k = 0; k < K; ++k ){
	Mark( &cube[k], k, tag );
	weight = Vol( &cube[k], wt );
	if (weight) {
	    lut_r[k] = (unsigned char) (Vol(&cube[k], mr) / weight);
	    lut_g[k] = (unsigned char) (Vol(&cube[k], mg) / weight);
	    lut_b[k] = (unsigned char) (Vol(&cube[k], mb) / weight);
	} else {
	    lut_r[k] = lut_g[k] = lut_b[k] = 0;
	}
    }

    outbitmap->magic = LUGUSED;
    outbitmap->xsize = inbitmap->xsize;
    outbitmap->ysize = inbitmap->ysize;
    outbitmap->depth = 1 + no_bits( no_colors );
    outbitmap->colors = no_colors;
    ptr = outbitmap->cmap = (unsigned char *) ckalloc(3 * no_colors);
    memset(ptr, 0, 3 * no_colors);
    for ( k = 0; k < K; ++k ) {
	*ptr++ = lut_r[k];
	*ptr++ = lut_g[k];
	*ptr++ = lut_b[k];
    }

    line=pixelo;
    for( y = 0; y < size; ++y ) {
	line[0] = lut_r[tag[Qadd[i]]];
	line[greenOffset] = lut_g[tag[Qadd[i]]];
	line[blueOffset] = lut_b[tag[Qadd[i]]];
	line += pixelSize;
    }

    ckfree((char *) Qadd);
    ckfree(tag);
    return TCL_OK;
}

/* build 3-D color histogram of counts, r/g/b, c^2 */
static void Hist3d(vwt, vmr, vmg, vmb, m2)
    long *vwt, *vmr, *vmg, *vmb;
    float *m2;
{
    register int ind, r, g, b;
    int inr, ing, inb, table[MAXCOLORMAPSIZE];
    register long i;

    for (i = 0; i < MAXCOLORMAPSIZE; ++i) {
	table[i] = i * i;
    }
    Qadd = (unsigned short *) ckalloc( sizeof(short) * size ); 
    memset(Qadd, 0, sizeof(short) * size);
    for( i = 0; i < size; ++i ) {
	r=(unsigned char )pixelo[i*pixelSize];
	g=(unsigned char )pixelo[i*pixelSize+1];
	b=(unsigned char )pixelo[i*pixelSize+2];
	inr = (r>>3) + 1;
	ing = (g>>3) + 1;
	inb = (b>>3) + 1;
	Qadd[i] = ind = (inr<<10) + (inr<<6) + inr + (ing<<5) + ing + inb;
             /*[inr][ing][inb]*/
	++vwt[ind];
	vmr[ind] += r;
	vmg[ind] += g;
	vmb[ind] += b;
	m2[ind] += (float) (table[r]+table[g]+table[b]);
    }
}

/* At conclusion of the histogram step, we can interpret
 *   wt[r][g][b] = sum over voxel of P(c)
 *   mr[r][g][b] = sum over voxel of r*P(c)  ,  similarly for mg, mb
 *   m2[r][g][b] = sum over voxel of c^2*P(c)
 * Actually each of these should be divided by 'size' to give the usual
 * interpretation of P() as ranging from 0 to 1, but we needn't do that here.
 */

/* We now convert histogram into moments so that we can rapidly calculate
 * the sums of the above quantities over any desired box.
 */

static void M3d(vwt, vmr, vmg, vmb, m2) /* compute cumulative moments. */
    long *vwt, *vmr, *vmg, *vmb;
    float *m2;
{
    register unsigned short int ind1, ind2;
    register unsigned char i, r, g, b;
    long line, line_r, line_g, line_b,
	    area[33], area_r[33], area_g[33], area_b[33];
    double line2, area2[33];

    for (r = 1; r <= 32; ++r) {
	for (i = 0; i <= 32; ++i) {
	    area2[i] = 0.0;
	    area[i] = area_r[i] = area_g[i] = area_b[i] = 0;
	}
	for (g = 1; g <= 32; ++g) {
	    line = line_r = line_g = line_b = (long) 0;
	    line2 = 0.0;
	    for (b = 1; b <= 32; ++b) {
		ind1 = (r<<10) + (r<<6) + r + (g<<5) + g + b; /* [r][g][b] */
		line += vwt[ind1];
		line_r += vmr[ind1];
		line_g += vmg[ind1];
		line_b += vmb[ind1];
		line2 += m2[ind1];
		area[b] += line;
		area_r[b] += line_r;
		area_g[b] += line_g;
		area_b[b] += line_b;
		area2[b] += line2;
		ind2 = ind1 - 1089; /* [r-1][g][b] */
		vwt[ind1] = vwt[ind2] + area[b];
		vmr[ind1] = vmr[ind2] + area_r[b];
		vmg[ind1] = vmg[ind2] + area_g[b];
		vmb[ind1] = vmb[ind2] + area_b[b];
		m2[ind1] = (float) (m2[ind2] + area2[b]);
	    }
	}
    }
}

/* Compute sum over a box of any given statistic */
static long
Vol(cube, mmt)
    struct box *cube;
    long mmt[33][33][33];
{
    return( mmt[cube->r1][cube->g1][cube->b1]
	    -mmt[cube->r1][cube->g1][cube->b0]
	    -mmt[cube->r1][cube->g0][cube->b1]
	    +mmt[cube->r1][cube->g0][cube->b0]
	    -mmt[cube->r0][cube->g1][cube->b1]
	    +mmt[cube->r0][cube->g1][cube->b0]
	    +mmt[cube->r0][cube->g0][cube->b1]
	    -mmt[cube->r0][cube->g0][cube->b0] );
}

/* The next two routines allow a slightly more efficient calculation
 * of Vol() for a proposed subbox of a given box.  The sum of Top()
 * and Bottom() is the Vol() of a subbox split in the given direction
 * and with the specified new upper bound.
 */

/* Compute part of Vol(cube, mmt) that doesn't depend on r1, g1, or b1 */
/* (depending on dir) */
static long
Bottom(cube, dir, mmt)
    struct box *cube;
    int dir;
    long mmt[33][33][33];
{
    switch (dir) {
	case CM_RED:
	    return( -mmt[cube->r0][cube->g1][cube->b1]
		    +mmt[cube->r0][cube->g1][cube->b0]
		    +mmt[cube->r0][cube->g0][cube->b1]
		    -mmt[cube->r0][cube->g0][cube->b0] );
	    break;
	case CM_GREEN:
	    return( -mmt[cube->r1][cube->g0][cube->b1]
	            +mmt[cube->r1][cube->g0][cube->b0]
	            +mmt[cube->r0][cube->g0][cube->b1]
	            -mmt[cube->r0][cube->g0][cube->b0] );
	    break;
	case CM_BLUE:
	    return( -mmt[cube->r1][cube->g1][cube->b0]
	            +mmt[cube->r1][cube->g0][cube->b0]
	            +mmt[cube->r0][cube->g1][cube->b0]
	            -mmt[cube->r0][cube->g0][cube->b0] );
	    break;
    }
    return 0;
}

/* Compute remainder of Vol(cube, mmt), substituting pos for */
/* r1, g1, or b1 (depending on dir) */
static long
Top(cube, dir, pos, mmt)
    struct box *cube;
    int dir;
    int pos;
    long mmt[33][33][33];
{
  switch(dir){
    case CM_RED:
            return( mmt[pos][cube->g1][cube->b1]
                   -mmt[pos][cube->g1][cube->b0]
                   -mmt[pos][cube->g0][cube->b1]
                   +mmt[pos][cube->g0][cube->b0] );
            break;
    case CM_GREEN:
            return( mmt[cube->r1][pos][cube->b1]
                   -mmt[cube->r1][pos][cube->b0]
                   -mmt[cube->r0][pos][cube->b1]
                   +mmt[cube->r0][pos][cube->b0] );
            break;
    case CM_BLUE:
            return( mmt[cube->r1][cube->g1][pos]
                   -mmt[cube->r1][cube->g0][pos]
                   -mmt[cube->r0][cube->g1][pos]
                   +mmt[cube->r0][cube->g0][pos] );
            break;
    default:
	    return 0;
  }
}

/* Compute the weighted variance of a box */
/* NB: as with the raw statistics, this is really the variance * size */
static double
Var(cube)
    struct box *cube;
{
  double dr, dg, db, xx;

  dr = Vol(cube, mr);
  dg = Vol(cube, mg);
  db = Vol(cube, mb);
  xx =  m2[cube->r1][cube->g1][cube->b1]
        -m2[cube->r1][cube->g1][cube->b0]
        -m2[cube->r1][cube->g0][cube->b1]
        +m2[cube->r1][cube->g0][cube->b0]
        -m2[cube->r0][cube->g1][cube->b1]
        +m2[cube->r0][cube->g1][cube->b0]
        +m2[cube->r0][cube->g0][cube->b1]
        -m2[cube->r0][cube->g0][cube->b0];

  return( xx - (dr*dr+dg*dg+db*db) / Vol(cube,wt) );
}

/* We want to minimize the sum of the variances of two subboxes.
 * The sum(c^2) terms can be ignored since their sum over both subboxes
 * is the same (the sum for the whole box) no matter where we split.
 * The remaining terms have a minus sign in the variance formula,
 * so we drop the minus sign and MAXIMIZE the sum of the two terms.
 */

static double
Maximize(cube, dir, first, last, cut,
         whole_r, whole_g, whole_b, whole_w)
    struct box *cube;
    int dir;
    int first, last, *cut;
    long whole_r, whole_g, whole_b, whole_w;
{
  register long half_r, half_g, half_b, half_w;
  long base_r, base_g, base_b, base_w;
  register int i;
  register double temp, max;

  base_r = Bottom(cube, dir, mr);
  base_g = Bottom(cube, dir, mg);
  base_b = Bottom(cube, dir, mb);
  base_w = Bottom(cube, dir, wt);
  max = 0.0;
  *cut = -1;
  for ( i = first; i < last; ++i ) {
    half_r = base_r + Top(cube, dir, i, mr);
    half_g = base_g + Top(cube, dir, i, mg);
    half_b = base_b + Top(cube, dir, i, mb);
    half_w = base_w + Top(cube, dir, i, wt);
    /* now half_x is sum over lower half of box, if split at i */
    if ( half_w == 0 ) {      /* subbox could be empty of pixels! */
      continue;             /* never split into an empty box */
    } else
        temp = (half_r*half_r + half_g*half_g +
                half_b*half_b)/half_w;

    half_r = whole_r - half_r;
    half_g = whole_g - half_g;
    half_b = whole_b - half_b;
    half_w = whole_w - half_w;
    if ( half_w == 0 ) {      /* subbox could be empty of pixels! */
      continue;             /* never split into an empty box */
    } else
       temp += (half_r*half_r + half_g*half_g +
                half_b*half_b)/half_w;

    if ( temp > max ) {
      max = temp;
      *cut=i;
    }
  }
  return(max);
}

static int
Cut(set1, set2)
    struct box *set1, *set2;
{
    unsigned char dir;
    int cutr, cutg, cutb;
    double maxr, maxg, maxb;
    long whole_r, whole_g, whole_b, whole_w;

    whole_r = Vol(set1, mr);
    whole_g = Vol(set1, mg);
    whole_b = Vol(set1, mb);
    whole_w = Vol(set1, wt);

    maxr = Maximize(set1, CM_RED, set1->r0+1, set1->r1, &cutr,
	    whole_r, whole_g, whole_b, whole_w );
    maxg = Maximize( set1, CM_GREEN, set1->g0+1, set1->g1, &cutg,
	    whole_r, whole_g, whole_b, whole_w );
    maxb = Maximize( set1, CM_BLUE, set1->b0+1, set1->b1, &cutb,
	    whole_r, whole_g, whole_b, whole_w );

    if ( (maxr >= maxg) && (maxr >= maxb) ) {
	dir = CM_RED;
	if (cutr < 0) {
	    return 0; /* can't split the box */
	}
    } else {
	if ((maxg >= maxr) && (maxg >= maxb)) {
	    dir = CM_GREEN;
	} else {
	    dir = CM_BLUE;
	}
    }

    set2->r1 = set1->r1;
    set2->g1 = set1->g1;
    set2->b1 = set1->b1;

    switch (dir){
	case CM_RED:
	    set2->r0 = set1->r1 = cutr;
	    set2->g0 = set1->g0;
	    set2->b0 = set1->b0;
	    break;
	case CM_GREEN:
	    set2->g0 = set1->g1 = cutg;
	    set2->r0 = set1->r0;
	    set2->b0 = set1->b0;
	    break;
	case CM_BLUE:
	    set2->b0 = set1->b1 = cutb;
	    set2->r0 = set1->r0;
	    set2->g0 = set1->g0;
	    break;
    }

    set1->vol = (set1->r1-set1->r0) * (set1->g1-set1->g0) * (set1->b1-set1->b0);
    set2->vol = (set2->r1-set2->r0) * (set2->g1-set2->g0) * (set2->b1-set2->b0);

    return 1;
}

static void Mark(cube, label, tag)
struct box *cube;
int label;
unsigned char *tag;
{
    register int r, g, b;

    for (r = cube->r0+1; r <= cube->r1; ++r) {
	for (g = cube->g0+1; g <= cube->g1; ++g) {
	    for (b = cube->b0+1; b <= cube->b1; ++b) {
		tag[ (r<<10) + (r<<6) + r + (g<<5) + g + b ] = label;
	    }
	}
    }
}


/*
 *----------------------------------------------------------------------
 *
 * Minit -- --
 *
 *  This procedure initializes a base64 decoder handle
 *
 * Results:
 *  none
 *
 * Side effects:
 *  the base64 handle is initialized
 *
 *----------------------------------------------------------------------
 */

static void
mInit(string,handle)
   unsigned char *string;	/* string containing initial mmencoded data */
   MFile *handle;		/* mmdecode "file" handle */
{
   handle->data = string;
   handle->state = 0;
}

/*
 *----------------------------------------------------------------------
 *
 * mWriteInit -- --
 *
 *  This procedure initializes a base64 decoder handle for writing
 *
 * Results:
 *  none
 *
 * Side effects:
 *  the base64 handle is initialized
 *
 *----------------------------------------------------------------------
 */

static void
mWriteInit(buffer, handle)
    Tcl_DString *buffer;
    MFile *handle;		/* mmencode "file" handle */
{
    Tcl_DStringSetLength(buffer, 1024);
    handle->buffer = buffer;
    handle->data = (unsigned char *) Tcl_DStringValue(buffer);
    handle->state = 0;
    handle->linelength = 0;
}

/*
 *----------------------------------------------------------------------
 *
 * Mread --
 *
 *  This procedure is invoked by the GIF file reader as a 
 *  temporary replacement for "fread", to get GIF data out
 *  of a string (using Mgetc).
 *
 * Results:
 *  The return value is the number of characters "read"
 *
 * Side effects:
 *  the base64 handle will change state.
 *
 *----------------------------------------------------------------------
 */

static int
Mread(dst, chunkSize, numChunks, handle)  
   unsigned char *dst;	/* where to put the result */
   size_t chunkSize;	/* size of each transfer */
   size_t numChunks;	/* number of chunks */
   MFile *handle;	/* mmdecode "file" handle */
{
   register int i, c;
   int count = chunkSize * numChunks;

   for(i=0; i<count && (c=Mgetc(handle)) != GIF_DONE; i++) {
	*dst++ = c;
   }
   return i;
}

/*
 *----------------------------------------------------------------------
 *
 * Mwrite --
 *
 *  This procedure is invoked by the GIF file reader as a 
 *  temporary replacement for "fwrite", to put GIF data
 *  into string (using Mputc).
 *
 * Results:
 *  The return value is the number of characters "written"
 *
 * Side effects:
 *  the base64 handle will change state.
 *
 *----------------------------------------------------------------------
 */

static int
Mwrite(src, chunkSize, numChunks, handle)  
    unsigned char *src;	/* where to get the data */
    size_t chunkSize;	/* size of each transfer */
    size_t numChunks;	/* number of chunks */
    MFile *handle;	/* mmencode "file" handle */
{
    register int i;
    int count = chunkSize * numChunks;
    int curcount = ((char *) handle->data) - Tcl_DStringValue(handle->buffer);
    int bufcount = curcount + count + count/3 +count/52 + 1024;

    /* make sure that the DString contains enough space */
    if (bufcount >= (handle->buffer->spaceAvl)) {
	/* We allocate 4k extra, so we don't have to */
	/* reallocate the buffer too often */
	Tcl_DStringSetLength(handle->buffer, bufcount + 4096);
	handle->data = (unsigned char *) Tcl_DStringValue(handle->buffer) + curcount;
    }

    /* write the data */
    for(i=0; (i<count) && (Mputc(*src++,handle) != GIF_DONE); i++) {
	/* empty loop body */
    }
    return i;
}

/*
 *----------------------------------------------------------------------
 *
 * Mgetc --
 *
 *  This procedure decodes and returns the next byte from a base64
 *  encoded string.
 *
 * Results:
 *  The next byte (or GIF_DONE) is returned.
 *
 * Side effects:
 *  The base64 handle will change state.
 *
 *----------------------------------------------------------------------
 */

static int
Mgetc(handle)
   MFile *handle;		/* Handle containing decoder data and state. */
{
    int c;
    int result = 0;		/* Initialization needed only to prevent
				 * gcc compiler warning. */
     
    if (handle->state == GIF_DONE) {
	return(GIF_DONE);
    }

    do {
	c = char64(*handle->data);
	handle->data++;
    } while (c==GIF_SPACE);

    if (c>GIF_SPECIAL) {
	handle->state = GIF_DONE;
	return(handle->state ? handle->c : GIF_DONE);
    }

    switch (handle->state++) {
	case 0:
	   handle->c = c<<2;
	   result = Mgetc(handle);
	   break;
	case 1:
	   result = handle->c | (c>>4);
	   handle->c = (c&0xF)<<4;
	   break;
	case 2:
	   result = handle->c | (c>>2);
	   handle->c = (c&0x3) << 6;
	   break;
	case 3:
	   result = handle->c | c;
	   handle->state = 0;
	   break;
    }
    return(result);
}

/*
 *----------------------------------------------------------------------
 *
 * Mputc --
 *
 *  This procedure encodes and writes the next byte to a base64
 *  encoded string.
 *
 * Results:
 *  the written byte is returned.
 *
 * Side effects:
 *  the base64 handle will change state.
 *
 *----------------------------------------------------------------------
 */

static int
Mputc(c,handle)
    register int c;		/* character to be written */
    register MFile *handle;	/* handle containing decoder data and state */
{

    /*
     *	In fact, here should be checked first if the dynamic
     *	string contains enough space for the next character.
     *  This would be very expensive to do for each character.
     *  Therefore we just allocate 1024 bytes immediately in
     *  the beginning and also take a 1024 bytes margin inside
     *  every Fwrite. At least this check is done then only
     *  every 256 bytes, which is much faster. Because the GIF
     *  header is less than 1024 bytes and pixel data is
     *  written in 256 byte portions, this should be safe.
     */
    c &= 0xff;
    switch (handle->state++) {
	case 0:
	    *handle->data++ = base64_table[(c>>2)&63]; break;
	case 1:
	    c |= handle->c<<8;
	    *handle->data++ = base64_table[(c>>4)&63]; break;
	case 2:
	    handle->state = 0;
	    c |= handle->c<<8;
	    *handle->data++ = base64_table[(c>>6)&63];
	    *handle->data++ = base64_table[c&63]; break;
    }
    handle->c = c;
    if (handle->linelength++ > 52) {
	handle->linelength = 0;
	*handle->data++ = '\n';
    }
    return c & 0xff;
}

/*
 *----------------------------------------------------------------------
 *
 * char64 --
 *
 *	This procedure converts a base64 ascii character into its binary
 *	equivalent.  This code is a slightly modified version of the
 *	char64 proc in N. Borenstein's metamail decoder.
 *
 * Results:
 *	The binary value, or an error code.
 *
 * Side effects:
 *	None.
 *----------------------------------------------------------------------
 */

static int
char64(c)
int c;
{
    switch(c) {
        case 'A': return(0);  case 'B': return(1);  case 'C': return(2);
        case 'D': return(3);  case 'E': return(4);  case 'F': return(5);
        case 'G': return(6);  case 'H': return(7);  case 'I': return(8);
        case 'J': return(9);  case 'K': return(10); case 'L': return(11);
        case 'M': return(12); case 'N': return(13); case 'O': return(14);
        case 'P': return(15); case 'Q': return(16); case 'R': return(17);
        case 'S': return(18); case 'T': return(19); case 'U': return(20);
        case 'V': return(21); case 'W': return(22); case 'X': return(23);
        case 'Y': return(24); case 'Z': return(25); case 'a': return(26);
        case 'b': return(27); case 'c': return(28); case 'd': return(29);
        case 'e': return(30); case 'f': return(31); case 'g': return(32);
        case 'h': return(33); case 'i': return(34); case 'j': return(35);
        case 'k': return(36); case 'l': return(37); case 'm': return(38);
        case 'n': return(39); case 'o': return(40); case 'p': return(41);
        case 'q': return(42); case 'r': return(43); case 's': return(44);
        case 't': return(45); case 'u': return(46); case 'v': return(47);
        case 'w': return(48); case 'x': return(49); case 'y': return(50);
        case 'z': return(51); case '0': return(52); case '1': return(53);
        case '2': return(54); case '3': return(55); case '4': return(56);
        case '5': return(57); case '6': return(58); case '7': return(59);
        case '8': return(60); case '9': return(61); case '+': return(62);
        case '/': return(63);

	case ' ': case '\t': case '\n': case '\r': case '\f': return(GIF_SPACE);
	case '=':  return(GIF_PAD);
	case '\0': return(GIF_DONE);
	default: return(GIF_BAD);
    }
}

/*
 *----------------------------------------------------------------------
 *
 * Fread --
 *
 *  This procedure calls either fread or Mread to read data
 *  from a file or a base64 encoded string.
 *
 * Results: - same as fread
 *
 *----------------------------------------------------------------------
 */

static int
Fread(dst, hunk, count, file)
    unsigned char *dst;		/* where to put the result */
    size_t hunk,count;		/* how many */
    FILE *file;
{
    if (fromData) {
	return(Mread(dst, hunk, count, (MFile *) file));
    } else {
	return(fread((void *)dst, hunk, count, file)); 
    }
}

/*
 *----------------------------------------------------------------------
 *
 * Fwrite --
 *
 *  This procedure calls either fwrite or Mwrite to write data
 *  to a file or to a base64 encoded string.
 *
 * Results: - same as fwrite
 *
 *----------------------------------------------------------------------
 */

static int
Fwrite(src, hunk, count, file)
    unsigned char *src;		/* where to get the data */
    size_t hunk,count;		/* how many */
    FILE *file;
{
    if (fromData) {
	return(Mwrite(src, hunk, count, (MFile *) file));
    } else {
	return(fwrite(src, hunk, count, file)); 
    }
}

/*
 *----------------------------------------------------------------------
 *
 * Fputc --
 *
 *  This procedure calls either fputc or Mputc to write data
 *  to a file or a base64 encoded string.
 *
 * Results: - same as fread
 *
 *----------------------------------------------------------------------
 */

static int
Fputc(c,file)
   register int c;		/* character to be written */
   register FILE *file;		/* handle containing decoder data and state */
{
    if (fromData) {
	return(Mputc(c, (MFile *) file));
    } else {
	return(fputc(c, file)); 
    }
}
