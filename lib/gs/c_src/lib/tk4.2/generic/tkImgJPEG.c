/*
 * tkImgJPEG.c --
 *
 *	A photo image file handler for JPEG files.
 *
 * This Tk image format handler reads and writes JPEG files in the standard
 * JFIF file format.  ("JPEG" should be the format name.)  It can also read
 * and write strings containing base64-encoded JPEG data.
 *
 * Several options can be provided in the format string, for example:
 *
 *	imageObject read input.jpg -shrink -format "jpeg -grayscale"
 *	imageObject write output.jpg -format "jpeg -quality 50 -progressive"
 *
 * The supported options for reading are:
 *	-fast:        Fast, low-quality processing
 *	-grayscale:   Force incoming image to grayscale
 * The supported options for writing are:
 *	-quality N:   Compression quality (0..100; 5-95 is useful range)
 *	              Default value: 75
 *	-smooth N:    Perform smoothing (10-30 is enough for most GIF's)
 *		      Default value: 0
 *	-grayscale:   Create monochrome JPEG file
 *	-optimize:    Optimize Huffman table
 *	-progressive: Create progressive JPEG file
 *
 *
 * Copyright (c) 1996-1997 Thomas G. Lane.
 * This file is based on tkImgPPM.c from the Tk 4.2 distribution.
 * That file is
 *	Copyright (c) 1994 The Australian National University.
 *	Copyright (c) 1994-1996 Sun Microsystems, Inc.
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 * You will need a copy of the IJG JPEG library, version 5 or later,
 * to use this file.  If you didn't receive it with this package, see
 *	ftp://ftp.uu.net/graphics/jpeg/
 *
 * Author: Tom Lane (tgl@sss.pgh.pa.us)
 * Modified for dynamical loading by Jan Nijtmans (nijtmans@nici.kun.nl)
 *
 * SCCS: @(#) tkImgJPEG.c
 */

/* system includes */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <setjmp.h>

/* Tk */
#include "tkInt.h"

/* undef Tcl macros that conflict with libjpeg stuff (sigh) */
#undef EXTERN

/* libjpeg */
#ifdef MAC_TCL
#  include "::compat:jpeglib.h"
#  include "::conpat:jerror.h"
#else
#  include <sys/types.h>
#ifdef HAVE_JPEGLIB_H
#  include <jpeglib.h>
#  include <jerror.h>
#else
#  include "../compat/jpeglib.h"
#  include "../compat/jerror.h"
#endif
#endif

#ifdef __WIN32__
#define JPEG_LIB_NAME "jpeg.dll"
#endif

#ifndef JPEG_LIB_NAME
#define JPEG_LIB_NAME "libjpeg.so"
#endif

#ifndef JPEG_LIB_SUFFIX
#define JPEG_LIB_SUFFIX "6"
#endif

/*
 * The format record for the JPEG file format:
 */

static int	FileMatchJPEG _ANSI_ARGS_((FILE *f, char *fileName,
		    char *formatString, int *widthPtr, int *heightPtr));
static int	StringMatchJPEG _ANSI_ARGS_((char *string,
		    char *formatString, int *widthPtr, int *heightPtr));
static int	FileReadJPEG _ANSI_ARGS_((Tcl_Interp *interp,
		    FILE *f, char *fileName, char *formatString,
		    Tk_PhotoHandle imageHandle, int destX, int destY,
		    int width, int height, int srcX, int srcY));
static int	StringReadJPEG _ANSI_ARGS_((Tcl_Interp *interp,
		    char *string, char *formatString,
		    Tk_PhotoHandle imageHandle, int destX, int destY,
		    int width, int height, int srcX, int srcY));
static int	FileWriteJPEG _ANSI_ARGS_((Tcl_Interp *interp,
		    char *fileName, char *formatString,
		    Tk_PhotoImageBlock *blockPtr));
static int	StringWriteJPEG _ANSI_ARGS_((Tcl_Interp *interp,
		    Tcl_DString *dataPtr, char *formatString,
		    Tk_PhotoImageBlock *blockPtr));

Tk_PhotoImageFormat tkImgFmtJPEG = {
    "JPEG",			/* name */
    FileMatchJPEG,		/* fileMatchProc */
    StringMatchJPEG,		/* stringMatchProc */
    FileReadJPEG,		/* fileReadProc */
    StringReadJPEG,		/* stringReadProc */
    FileWriteJPEG,		/* fileWriteProc */
    StringWriteJPEG,		/* stringWriteProc */
};

/*
 * We use Tk_ParseArgv to parse any options supplied in the format string.
 */

static int fast;		/* static variables hold parse results */
static int grayscale;		/* ... icky, and not reentrant ... */
static int quality;
static int smooth;
static int optimize;
static int progressive;

static Tk_ArgvInfo readOptTable[] = {
    {"-fast", TK_ARGV_CONSTANT, (char *) 1, (char *) &fast,
	"Fast, low-quality processing"},
    {"-grayscale", TK_ARGV_CONSTANT, (char *) 1, (char *) &grayscale,
	"Force incoming image to grayscale"},
    {NULL, TK_ARGV_END, (char *) NULL, (char *) NULL,
	(char *) NULL}
};

static Tk_ArgvInfo writeOptTable[] = {
    {"-quality", TK_ARGV_INT, (char *) NULL, (char *) &quality,
	"Compression quality (0..100; 5-95 is useful range)"},
    {"-smooth", TK_ARGV_INT, (char *) NULL, (char *) &smooth,
	"Smoothing factor (default = 0, 10-30 is enough for typical GIFs.)"},
    {"-grayscale", TK_ARGV_CONSTANT, (char *) 1, (char *) &grayscale,
	"Create monochrome JPEG file"},
    {"-optimize", TK_ARGV_CONSTANT, (char *) 1, (char *) &optimize,
	"Optimize Huffman table"},
    {"-progressive", TK_ARGV_CONSTANT, (char *) 1, (char *) &progressive,
	"Create progressive JPEG file"},
    {NULL, TK_ARGV_END, (char *) NULL, (char *) NULL,
	(char *) NULL}
};


/*
 * JPEG strings are represented as data in base64 format.
 * base64 strings consist of 4 6-bit characters -> 3 8 bit bytes.
 * A-Z, a-z, 0-9, + and / represent the 64 values (in order).
 * '=' is a trailing padding char when the un-encoded data is not a
 * multiple of 3 bytes.  We'll ignore white space when encountered.
 * Any other invalid character is treated as an EOF
 */

#define JPG_SPECIAL	(256)
#define JPG_PAD		(JPG_SPECIAL+1)
#define JPG_SPACE	(JPG_SPECIAL+2)
#define JPG_BAD		(JPG_SPECIAL+3)
#define JPG_DONE	(JPG_SPECIAL+4)

static CONST char base64_table[64] = {
    'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H',
    'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P',
    'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X',
    'Y', 'Z', 'a', 'b', 'c', 'd', 'e', 'f',
    'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n',
    'o', 'p', 'q', 'r', 's', 't', 'u', 'v',
    'w', 'x', 'y', 'z', '0', '1', '2', '3',
    '4', '5', '6', '7', '8', '9', '+', '/'
};

typedef struct {		/* Mimics FILE struct for I/O to string */
  char *data;			/* next source or destination byte */
  int state;			/* decoder state (0-4 or JPG_DONE) */
  int c;			/* bits left over from previous character */
} MFile;

/*
 * Declarations for libjpeg source and destination managers to handle
 * reading and writing base64-encoded strings.
 */

#define STRING_BUF_SIZE  4096	/* choose any convenient size */

typedef struct str_source_mgr {	/* Source manager for reading from string */
  struct jpeg_source_mgr pub;	/* public fields */

  MFile handle;			/* base64 stream */
  JOCTET buffer[STRING_BUF_SIZE]; /* buffer for a chunk of decoded data */
} *str_src_ptr;

typedef struct str_destination_mgr { /* Manager for string output */
  struct jpeg_destination_mgr pub; /* public fields */

  Tcl_DString *dstring;		/* Dynamic string for accumulating result */
  int linelength;		/* length of physical line already written */
  MFile handle;			/* base64 stream */
  JOCTET buffer[STRING_BUF_SIZE]; /* buffer for a chunk of uncoded data */
} *str_dest_ptr;

/*
 * Other declarations
 */

struct my_error_mgr {		/* Extended libjpeg error manager */
  struct jpeg_error_mgr pub;	/* public fields */
  jmp_buf setjmp_buffer;	/* for return to caller from error exit */
};

/*
 * Prototypes for local procedures defined in this file:
 */

static int	CommonReadJPEG _ANSI_ARGS_((Tcl_Interp *interp,
		    j_decompress_ptr cinfo, char *formatString,
		    Tk_PhotoHandle imageHandle, int destX, int destY,
		    int width, int height, int srcX, int srcY));
static int	CommonWriteJPEG _ANSI_ARGS_((Tcl_Interp *interp,
		    j_compress_ptr cinfo, char *formatString,
		    Tk_PhotoImageBlock *blockPtr));
static void	jpeg_string_src _ANSI_ARGS_((j_decompress_ptr, char*));
static void	str_init_source _ANSI_ARGS_((j_decompress_ptr));
static boolean	str_fill_input_buffer _ANSI_ARGS_((j_decompress_ptr));
static void	str_skip_input_data _ANSI_ARGS_((j_decompress_ptr, long));
static void	str_term_source _ANSI_ARGS_((j_decompress_ptr));
static void	jpeg_string_dest _ANSI_ARGS_((j_compress_ptr, Tcl_DString*));
static void	str_init_destination _ANSI_ARGS_((j_compress_ptr));
static boolean	str_empty_output_buffer _ANSI_ARGS_((j_compress_ptr));
static void	str_term_destination _ANSI_ARGS_((j_compress_ptr));
static size_t	Mwrite _ANSI_ARGS_((char *src, size_t count,
		    str_dest_ptr dest));
static int	char64 _ANSI_ARGS_((int c));
static void	my_error_exit _ANSI_ARGS_((j_common_ptr cinfo));
static void	my_output_message _ANSI_ARGS_((j_common_ptr cinfo));
static void	append_jpeg_message _ANSI_ARGS_((Tcl_Interp *interp,
		    j_common_ptr cinfo));
static int	load_jpeg_library _ANSI_ARGS_((Tcl_Interp *interp));

/*
 * these are for the BASE64 image reader code only
 */

static int		Mread _ANSI_ARGS_((char *dst, size_t size,
			    size_t count, MFile *handle));
static int		Mgetc _ANSI_ARGS_((MFile *handle));

/*
 * Stuff to support dynamic loading of libjpeg
 */

static struct JpegFunctions {
    VOID *handle;
    int (* abort_decompress) _ANSI_ARGS_((j_decompress_ptr));
    int (* destroy_compress) _ANSI_ARGS_((j_compress_ptr));
    int (* destroy_decompress) _ANSI_ARGS_((j_decompress_ptr));
    int (* finish_compress) _ANSI_ARGS_((j_compress_ptr));
    int (* finish_decompress) _ANSI_ARGS_((j_decompress_ptr));
    int (* read_header) _ANSI_ARGS_((j_decompress_ptr, int));
    JDIMENSION (* read_scanlines) _ANSI_ARGS_((j_decompress_ptr,
			JSAMPARRAY, JDIMENSION));
    int (* resync_to_restart) _ANSI_ARGS_((j_decompress_ptr, int));
    int (* set_defaults) _ANSI_ARGS_((j_compress_ptr));
    int (* start_compress) _ANSI_ARGS_((j_compress_ptr, int));
    int (* start_decompress) _ANSI_ARGS_((j_decompress_ptr));
    int (* stdio_dest) _ANSI_ARGS_((j_compress_ptr, FILE *));
    int (* stdio_src) _ANSI_ARGS_((j_decompress_ptr, FILE *));
    struct jpeg_error_mgr *(* std_error) _ANSI_ARGS_((struct jpeg_error_mgr *));
    JDIMENSION (* write_scanlines) _ANSI_ARGS_((j_compress_ptr,
			JSAMPARRAY, JDIMENSION));
    int (* set_colorspace) _ANSI_ARGS_((j_compress_ptr, J_COLOR_SPACE));
    int (* set_quality) _ANSI_ARGS_((j_compress_ptr, int, int));
    int (* simple_progression) _ANSI_ARGS_((j_compress_ptr));
    int (* CreateCompress) _ANSI_ARGS_((j_compress_ptr, int, size_t));
    int (* CreateDecompress) _ANSI_ARGS_((j_decompress_ptr, int, size_t));
    int (* create_compress) _ANSI_ARGS_((j_compress_ptr, int, size_t));
    int (* create_decompress) _ANSI_ARGS_((j_decompress_ptr, int, size_t));
} jpeg = {0};

static char *symbols[] = {
#ifdef NEED_SHORT_EXTERNAL_NAMES
    "jAbrtDecompress",
    "jDestCompress",
    "jDestDecompress",
    "jFinCompress",
    "jFinDecompress",
    "jReadHeader",
    "jReadScanlines",
    "jResyncRestart",
    "jSetDefaults",
    "jStrtCompress",
    "jStrtDecompress",
    "jStdDest",
    "jStdSrc",
    "jStdError",
    "jWrtScanlines",
    /*	the following 3 symbols are not crucial. They implement
	resp. the "-grayscale", "-quality" and "-progressive"
	options. If any of the symbols are missing from the
	library, the corresponding option just has no effect. */
    "jSetColorspace",
    "jSetQuality",
    "jSimProgress",
    "jCreaCompress",
    "jCreaDecompress",
    "jCreaCompress",
    "jCreaDecompress",
#else
    "jpeg_abort_decompress",
    "jpeg_destroy_compress",
    "jpeg_destroy_decompress",
    "jpeg_finish_compress",
    "jpeg_finish_decompress",
    "jpeg_read_header",
    "jpeg_read_scanlines",
    "jpeg_resync_to_restart",
    "jpeg_set_defaults",
    "jpeg_start_compress",
    "jpeg_start_decompress",
    "jpeg_stdio_dest",
    "jpeg_stdio_src",
    "jpeg_std_error",
    "jpeg_write_scanlines",
    /*	the following 3 symbols are not crucial. They implement
	resp. the "-grayscale", "-quality" and "-progressive"
	options. If any of the symbols are missing from the
	library, the corresponding option just has no effect. */
    "jpeg_set_colorspace",
    "jpeg_set_quality",
    "jpeg_simple_progression",
    /* In later versions, jpeg_create_compress and jpeg_create_decompress
       are macros pointing to jpeg_CreateCompress and jpeg_CreateDecompres.
       Which one is found depends on the version. */
    "jpeg_CreateCompress",
    "jpeg_CreateDecompress",
    "jpeg_create_compress",
    "jpeg_create_decompress",
#endif
    (char *) NULL
};


static int
load_jpeg_library(interp)
    Tcl_Interp *interp;
{
    struct jpeg_compress_struct *cinfo; /* libjpeg's parameter structure */
    struct my_error_mgr jerror;	/* for controlling libjpeg error handling */
    int i;

    if (TkLoadLib(interp, JPEG_LIB_NAME, JPEG_LIB_SUFFIX,
	    &jpeg.handle, symbols, 15) != TCL_OK) {
	return TCL_ERROR;
    }
    if (jpeg.CreateCompress == NULL) {
	if (jpeg.create_compress == NULL) {
	    goto load_failed;
	}
	jpeg.CreateCompress = jpeg.create_compress;
    }
    if (jpeg.CreateDecompress == NULL) {
	if (jpeg.create_decompress == NULL) {
	    goto load_failed;
	}
	jpeg.CreateDecompress = jpeg.create_decompress;
    }

    /* overallocat size, so we don't get a core-dump if the library
       thinks that the structure is much larger */
    cinfo = (struct jpeg_compress_struct *)
	    ckalloc(8*sizeof(struct jpeg_compress_struct));
    cinfo->err = jpeg.std_error(&jerror.pub);
    jerror.pub.error_exit = my_error_exit;
    jerror.pub.output_message = my_output_message;
    /* Establish the setjmp return context for my_error_exit to use. */
    if (setjmp(jerror.setjmp_buffer)) {
      /* If we get here, the JPEG library is invalid. */
      jpeg.destroy_compress(cinfo);
      ckfree(cinfo);
load_failed:
      Tcl_AppendResult(interp, "couldn't load \"", JPEG_LIB_NAME,
		       "\": please upgrade to at least version 6a", (char *) NULL);
      TkLoadFailed(&jpeg.handle);
      return TCL_ERROR;
    }

    /* Now we can initialize libjpeg. */
    ((char *) cinfo)[sizeof(struct jpeg_compress_struct)] = 53;
    jpeg.CreateCompress(cinfo, JPEG_LIB_VERSION,
			(size_t) sizeof(struct jpeg_compress_struct));
    if (((char *) cinfo)[sizeof(struct jpeg_compress_struct)] != 53) {
	/* Oops. The library changed this value, which is outside the
	 * structure. Definitely, the library is invalid!!!! */
	ERREXIT(cinfo, JMSG_NOMESSAGE);
    }

    /* Set up JPEG compression parameters. */
    cinfo->image_width = 16;
    cinfo->image_height = 16;
    cinfo->input_components = 3;
    cinfo->in_color_space = JCS_RGB;
    cinfo->data_precision = -1;
    cinfo->optimize_coding = TRUE;
    cinfo->dct_method = -1;
    cinfo->X_density = 0;
    cinfo->Y_density = 0;
    jpeg.set_defaults(cinfo);

    if ((cinfo->data_precision != BITS_IN_JSAMPLE) ||
	    (cinfo->optimize_coding != FALSE) ||
	    (cinfo->dct_method != JDCT_DEFAULT) ||
	    (cinfo->X_density != 1) ||
	    (cinfo->Y_density != 1)) {
	ERREXIT(cinfo, JMSG_NOMESSAGE);
    }
    for (i = 0; i < NUM_ARITH_TBLS; i++) {
	if ((cinfo->arith_dc_L[i] != 0) ||
		(cinfo->arith_dc_U[i] != 1) ||
		(cinfo->arith_ac_K[i] != 5)) {
	    ERREXIT(cinfo, JMSG_NOMESSAGE);
	}
    }
    jpeg.destroy_compress(cinfo);
    ckfree(cinfo);
    return TCL_OK;
}
/*
 *----------------------------------------------------------------------
 *
 * FileMatchJPEG --
 *
 *	This procedure is invoked by the photo image type to see if
 *	a file contains image data in JPEG format.
 *
 * Results:
 *	The return value is >0 if the first characters in file "f" look
 *	like JPEG data, and 0 otherwise.  For a valid file, the image
 *	dimensions are determined.
 *
 * Side effects:
 *	The access position in f may change.
 *
 *----------------------------------------------------------------------
 */

static int
FileMatchJPEG(f, fileName, formatString, widthPtr, heightPtr)
    FILE *f;			/* The image file, open for reading. */
    char *fileName;		/* The name of the image file. */
    char *formatString;		/* User-specified format string, or NULL. */
    int *widthPtr, *heightPtr;	/* The dimensions of the image are
				 * returned here if the file is a valid
				 * JPEG file. */
{
    unsigned char buf[256];
    int i;

    if ((fread(buf, 1, 3, f) != 3)||strncmp(buf,"\377\330\377", 3)) {
	return 0;
    }
    buf[0] = buf[2];
    /* at top of loop: have just read first FF of a marker into buf[0] */
    for (;;) {
	/* get marker type byte, skipping any padding FFs */
	while (buf[0] == 0x0ff) {
	    if (fread(buf,1,1,f) != 1) {
		return 0;
	    }
	}
	/* look for SOF0, SOF1, or SOF2, which are the only JPEG variants
	 * currently accepted by libjpeg.
	 */
	if (buf[0] == 0x0c0 || buf[0] == 0x0c1 || buf[0] == 0x0c2)
	    break;
	/* nope, skip the marker parameters */
	if (fread(buf, 1, 2, f) != 2) {
	    return 0;
	}
	i = (buf[0]<<8) + buf[1] - 1;
	while (i>256) {
	    fread(buf,1,256,f);
	    i -= 256;
	}
	if ((i<1) || (fread(buf, 1, i, f)) != (size_t) i) {
	    return 0;
	}
	buf[0] = buf[i-1];
	/* skip any inter-marker junk (there shouldn't be any, really) */
	while (buf[0] != 0x0ff) {
	    if (fread(buf,1,1,f) != 1) {
		return 0;
	    }
	}
    }
    /* Found the SOFn marker, get image dimensions */
    if (fread(buf,1,7,f) != 7) {
	return 0;
    }
    *heightPtr = (buf[3]<<8) + buf[4];
    *widthPtr = (buf[5]<<8) + buf[6];

    return 1;
}

/*
 *----------------------------------------------------------------------
 *
 * StringMatchJPEG --
 *
 *	This procedure is invoked by the photo image type to see if
 *	a string contains image data in JPEG format.
 *
 * Results:
 *	The return value is >0 if the first characters in the string look
 *	like JPEG data, and 0 otherwise.  For a valid image, the image
 *	dimensions are determined.
 *
 * Side effects:
 *  the size of the image is placed in widthPtr and heightPtr.
 *
 *----------------------------------------------------------------------
 */

static int
StringMatchJPEG(string, formatString, widthPtr, heightPtr)
    char *string;		/* the string containing the image data */
    char *formatString;		/* User-specified format string, or NULL. */
    int *widthPtr, *heightPtr;	/* The dimensions of the image are
				 * returned here if the string is a valid
				 * JPEG image. */
{
    unsigned char buf[256];
    int i;
    MFile handle;

    handle.data = string;
    handle.state = 0;

    if ((Mread(buf, 1, 3, &handle) != 3)||strncmp(buf,"\377\330\377", 3)) {
	return 0;
    }
    buf[0] = buf[2];
    /* at top of loop: have just read first FF of a marker into buf[0] */
    for (;;) {
	/* get marker type byte, skipping any padding FFs */
	while (buf[0] == 0x0ff) {
	    if (Mread(buf,1,1,&handle) != 1) {
		return 0;
	    }
	}
	/* look for SOF0, SOF1, or SOF2, which are the only JPEG variants
	 * currently accepted by libjpeg.
	 */
	if (buf[0] == 0x0c0 || buf[0] == 0x0c1 || buf[0] == 0x0c2)
	    break;
	/* nope, skip the marker parameters */
	if (Mread(buf, 1, 2, &handle) != 2) {
	    return 0;
	}
	i = (buf[0]<<8) + buf[1] - 1;
	while (i>256) {
	    Mread(buf,1,256,&handle);
	    i -= 256;
	}
	if ((i<1) || (Mread(buf, 1, i, &handle)) != i) {
	    return 0;
	}
	buf[0] = buf[i-1];
	/* skip any inter-marker junk (there shouldn't be any, really) */
	while (buf[0] != 0x0ff) {
	    if (Mread(buf,1,1,&handle) != 1) {
		return 0;
	    }
	}
    }
    /* Found the SOFn marker, get image dimensions */
    if (Mread(buf,1,7,&handle) != 7) {
	return 0;
    }
    *heightPtr = (buf[3]<<8) + buf[4];
    *widthPtr = (buf[5]<<8) + buf[6];

    return 1;
}

/*
 *----------------------------------------------------------------------
 *
 * Mread --
 *
 *  This procedure is invoked by the JPEG file reader as a 
 *  temporary replacement for "fread", to get JPEG data out
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
   char *dst;		/* where to put the result */
   size_t chunkSize;	/* size of each transfer */
   size_t numChunks;	/* number of chunks */
   MFile *handle;	/* mmdecode "file" handle */
{
   register int i, c;
   int count = chunkSize * numChunks;

   for(i=0; i<count && (c=Mgetc(handle)) != JPG_DONE; i++) {
	*dst++ = c;
   }
   return i;
}

/*
 *----------------------------------------------------------------------
 *
 * FileReadJPEG --
 *
 *	This procedure is called by the photo image type to read
 *	JPEG format data from a file and write it into a given
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

static int
FileReadJPEG(interp, f, fileName, formatString, imageHandle, destX, destY,
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
    struct jpeg_decompress_struct cinfo; /* libjpeg's parameter structure */
    struct my_error_mgr jerror;	/* for controlling libjpeg error handling */
    int result;

    if (load_jpeg_library(interp) != TCL_OK) {
	return TCL_ERROR;
    }

    /* Initialize JPEG error handler */
    /* We set up the normal JPEG error routines, then override error_exit. */
    cinfo.err = jpeg.std_error(&jerror.pub);
    jerror.pub.error_exit = my_error_exit;
    jerror.pub.output_message = my_output_message;

    /* Establish the setjmp return context for my_error_exit to use. */
    if (setjmp(jerror.setjmp_buffer)) {
      /* If we get here, the JPEG code has signaled an error. */
      Tcl_AppendResult(interp, "couldn't read JPEG file \"", fileName,
		       "\": ", (char *) NULL);
      append_jpeg_message(interp, (j_common_ptr) &cinfo);
      jpeg.destroy_decompress(&cinfo);
      return TCL_ERROR;
    }

    /* Now we can initialize libjpeg. */
    jpeg.CreateDecompress(&cinfo, JPEG_LIB_VERSION,
			(size_t) sizeof(struct jpeg_decompress_struct));
    jpeg.stdio_src(&cinfo, f);

    /* Share code with StringReadJPEG. */
    result = CommonReadJPEG(interp, &cinfo, formatString, imageHandle,
			    destX, destY, width, height, srcX, srcY);

    /* Reclaim libjpeg's internal resources. */
    jpeg.destroy_decompress(&cinfo);

    return result;
}

/*
 *----------------------------------------------------------------------
 *
 * StringReadJPEG --
 *
 *	This procedure is called by the photo image type to read
 *	JPEG format data from a base64 encoded string, and give it to
 *	the photo image.
 *
 * Results:
 *	A standard TCL completion code.  If TCL_ERROR is returned
 *	then an error message is left in interp->result.
 *
 * Side effects:
 *	New data is added to the image given by imageHandle.
 *
 *----------------------------------------------------------------------
 */

static int
StringReadJPEG(interp, string, formatString, imageHandle, destX, destY,
	width, height, srcX, srcY)
    Tcl_Interp *interp;		/* Interpreter to use for reporting errors. */
    char *string;		/* String containing the image. */
    char *formatString;		/* User-specified format string, or NULL. */
    Tk_PhotoHandle imageHandle;	/* The photo image to write into. */
    int destX, destY;		/* Coordinates of top-left pixel in
				 * photo image to be written to. */
    int width, height;		/* Dimensions of block of photo image to
				 * be written to. */
    int srcX, srcY;		/* Coordinates of top-left pixel to be used
				 * in image being read. */
{
    struct jpeg_decompress_struct cinfo; /* libjpeg's parameter structure */
    struct my_error_mgr jerror;	/* for controlling libjpeg error handling */
    int result;

    if (load_jpeg_library(interp) != TCL_OK) {
	return TCL_ERROR;
    }

    /* Initialize JPEG error handler */
    /* We set up the normal JPEG error routines, then override error_exit. */
    cinfo.err = jpeg.std_error(&jerror.pub);
    jerror.pub.error_exit = my_error_exit;
    jerror.pub.output_message = my_output_message;

    /* Establish the setjmp return context for my_error_exit to use. */
    if (setjmp(jerror.setjmp_buffer)) {
      /* If we get here, the JPEG code has signaled an error. */
      Tcl_AppendResult(interp, "couldn't read JPEG string: ", (char *) NULL);
      append_jpeg_message(interp, (j_common_ptr) &cinfo);
      jpeg.destroy_decompress(&cinfo);
      return TCL_ERROR;
    }

    /* Now we can initialize libjpeg. */
    jpeg.CreateDecompress(&cinfo, JPEG_LIB_VERSION,
			(size_t) sizeof(struct jpeg_decompress_struct));
    jpeg_string_src(&cinfo, string);

    /* Share code with FileReadJPEG. */
    result = CommonReadJPEG(interp, &cinfo, formatString, imageHandle,
			    destX, destY, width, height, srcX, srcY);

    /* Reclaim libjpeg's internal resources. */
    jpeg.destroy_decompress(&cinfo);

    return result;
}

/*
 *----------------------------------------------------------------------
 *
 * CommonReadJPEG --
 *
 *	The common guts of FileReadJPEG and StringReadJPEG.
 *	The decompress struct has already been set up and the
 *	appropriate data source manager initialized.
 *	The caller should do jpeg_destroy_decompress upon return.
 *
 *----------------------------------------------------------------------
 */

typedef struct myblock {
    Tk_PhotoImageBlock ck;
    int dummy; /* extra space for offset[3], if not included already
		  in Tk_PhotoImageBlock */
} myblock;

static int
CommonReadJPEG(interp, cinfo, formatString, imageHandle, destX, destY,
	width, height, srcX, srcY)
    Tcl_Interp *interp;		/* Interpreter to use for reporting errors. */
    j_decompress_ptr cinfo;	/* Already-constructed decompress struct. */
    char *formatString;		/* User-specified format string, or NULL. */
    Tk_PhotoHandle imageHandle;	/* The photo image to write into. */
    int destX, destY;		/* Coordinates of top-left pixel in
				 * photo image to be written to. */
    int width, height;		/* Dimensions of block of photo image to
				 * be written to. */
    int srcX, srcY;		/* Coordinates of top-left pixel to be used
				 * in image being read. */
{
    int fileWidth, fileHeight, stopY, curY, outY, outWidth, outHeight;
    myblock bl;
#define block bl.ck
    JSAMPARRAY buffer;		/* Output row buffer */

    /* Ready to read header data. */
    jpeg.read_header(cinfo, TRUE);

    /* This code only supports 8-bit-precision JPEG files. */
    if ((cinfo->data_precision != 8) ||
	(sizeof(JSAMPLE) != sizeof(unsigned char))) {
      Tcl_AppendResult(interp, "Unsupported JPEG precision", (char *) NULL);
      return TCL_ERROR;
    }

    /* Process format parameters to adjust decompression options. */
    if (formatString != NULL) {
      int argc;
      char **argv;
      if (Tcl_SplitList(interp, formatString, &argc, &argv) != TCL_OK)
	return TCL_ERROR;
      fast = 0;
      grayscale = 0;
      if (Tk_ParseArgv(interp, (Tk_Window) NULL, &argc, argv,
		       readOptTable, TK_ARGV_NO_LEFTOVERS|TK_ARGV_NO_DEFAULTS)
	  != TCL_OK) {
	ckfree((char *) argv);
	return TCL_ERROR;
      }
      if (fast) {
	/* Select recommended processing options for quick-and-dirty output. */
	cinfo->two_pass_quantize = FALSE;
	cinfo->dither_mode = JDITHER_ORDERED;
	cinfo->dct_method = JDCT_FASTEST;
	cinfo->do_fancy_upsampling = FALSE;
      }
      if (grayscale) {
	/* Force monochrome output. */
	cinfo->out_color_space = JCS_GRAYSCALE;
      }
      ckfree((char *) argv);
    }

    jpeg.start_decompress(cinfo);

    /* Check dimensions. */
    fileWidth = (int) cinfo->output_width;
    fileHeight = (int) cinfo->output_height;
    if ((srcX + width) > fileWidth) {
	outWidth = fileWidth - srcX;
    } else {
	outWidth = width;
    }
    if ((srcY + height) > fileHeight) {
	outHeight = fileHeight - srcY;
    } else {
	outHeight = height;
    }
    if ((outWidth <= 0) || (outHeight <= 0)
	|| (srcX >= fileWidth) || (srcY >= fileHeight)) {
	return TCL_OK;
    }

    /* Check colorspace. */
    switch (cinfo->out_color_space) {
    case JCS_GRAYSCALE:
      /* a single-sample grayscale pixel is expanded into equal R,G,B values */
      block.pixelSize = 1;
      block.offset[0] = 0;
      block.offset[1] = 0;
      block.offset[2] = 0;
      break;
    case JCS_RGB:
      /* note: this pixel layout assumes default configuration of libjpeg. */
      block.pixelSize = 3;
      block.offset[0] = 0;
      block.offset[1] = 1;
      block.offset[2] = 2;
      break;
    default:
      Tcl_AppendResult(interp, "Unsupported JPEG color space", (char *) NULL);
      return TCL_ERROR;
    }
    block.width = outWidth;
    block.height = 1;
    block.pitch = block.pixelSize * fileWidth;
    block.offset[3] = 0;

    Tk_PhotoExpand(imageHandle, destX + outWidth, destY + outHeight);

    /* Make a temporary one-row-high sample array */
    buffer = (*cinfo->mem->alloc_sarray)
		((j_common_ptr) cinfo, JPOOL_IMAGE,
		 cinfo->output_width * cinfo->output_components, 1);
    block.pixelPtr = (unsigned char *) buffer[0] + srcX * block.pixelSize;

    /* Read as much of the data as we need to */
    stopY = srcY + outHeight;
    outY = destY;
    for (curY = 0; curY < stopY; curY++) {
      jpeg.read_scanlines(cinfo, buffer, 1);
      if (curY >= srcY) {
	Tk_PhotoPutBlock(imageHandle, &block, destX, outY, outWidth, 1);
	outY++;
      }
    }

    /* Do normal cleanup if we read the whole image; else early abort */
    if (cinfo->output_scanline == cinfo->output_height)
	jpeg.finish_decompress(cinfo);
    else
	jpeg.abort_decompress(cinfo);

    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * FileWriteJPEG --
 *
 *	This procedure is invoked to write image data to a file in JPEG
 *	format.
 *
 * Results:
 *	A standard TCL completion code.  If TCL_ERROR is returned
 *	then an error message is left in interp->result.
 *
 * Side effects:
 *	Data is written to the file given by "fileName".
 *
 *----------------------------------------------------------------------
 */

static int
FileWriteJPEG(interp, fileName, formatString, blockPtr)
    Tcl_Interp *interp;
    char *fileName;
    char *formatString;
    Tk_PhotoImageBlock *blockPtr;
{
    struct jpeg_compress_struct cinfo; /* libjpeg's parameter structure */
    struct my_error_mgr jerror;	/* for controlling libjpeg error handling */
    Tcl_DString nameBuffer; 
    char *fullName;
    FILE *f;
    int result;

    if ((fullName=Tcl_TranslateFileName(interp,fileName,&nameBuffer))==NULL) {
	return TCL_ERROR;
    }
    if ((f = fopen(fullName, "wb")) == NULL) {
	Tcl_AppendResult(interp, fileName, ": ", Tcl_PosixError(interp),
			 (char *) NULL);
	Tcl_DStringFree(&nameBuffer);
	return TCL_ERROR;
    }
    Tcl_DStringFree(&nameBuffer);

    if (load_jpeg_library(interp) != TCL_OK) {
	return TCL_ERROR;
    }

    /* Initialize JPEG error handler */
    /* We set up the normal JPEG error routines, then override error_exit. */
    cinfo.err = jpeg.std_error(&jerror.pub);
    jerror.pub.error_exit = my_error_exit;
    jerror.pub.output_message = my_output_message;

    /* Establish the setjmp return context for my_error_exit to use. */
    if (setjmp(jerror.setjmp_buffer)) {
      /* If we get here, the JPEG code has signaled an error. */
      Tcl_AppendResult(interp, "couldn't write JPEG file \"", fileName,
		       "\": ", (char *) NULL);
      append_jpeg_message(interp, (j_common_ptr) &cinfo);
      jpeg.destroy_compress(&cinfo);
      fclose(f);
      return TCL_ERROR;
    }

    /* Now we can initialize libjpeg. */
    jpeg.CreateCompress(&cinfo, JPEG_LIB_VERSION,
			(size_t) sizeof(struct jpeg_compress_struct));
    jpeg.stdio_dest(&cinfo, f);

    /* Share code with StringWriteJPEG. */
    result = CommonWriteJPEG(interp, &cinfo, formatString, blockPtr);

    jpeg.destroy_compress(&cinfo);

    fclose(f);
    return result;
}

/*
 *----------------------------------------------------------------------
 *
 * StringWriteJPEG --
 *
 *	This procedure is called by the photo image type to write
 *	JPEG format data to a base-64 encoded string from the photo block.
 *
 * Results:
 *	A standard TCL completion code.  If TCL_ERROR is returned
 *	then an error message is left in interp->result.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */

static int
StringWriteJPEG(interp, dataPtr, formatString, blockPtr)
    Tcl_Interp *interp;
    Tcl_DString *dataPtr;
    char *formatString;
    Tk_PhotoImageBlock *blockPtr;
{
    struct jpeg_compress_struct cinfo; /* libjpeg's parameter structure */
    struct my_error_mgr jerror;	/* for controlling libjpeg error handling */
    int result;

    if (load_jpeg_library(interp) != TCL_OK) {
	return TCL_ERROR;
    }

    /* Initialize JPEG error handler */
    /* We set up the normal JPEG error routines, then override error_exit. */
    cinfo.err = jpeg.std_error(&jerror.pub);
    jerror.pub.error_exit = my_error_exit;
    jerror.pub.output_message = my_output_message;

    /* Establish the setjmp return context for my_error_exit to use. */
    if (setjmp(jerror.setjmp_buffer)) {
      /* If we get here, the JPEG code has signaled an error. */
      Tcl_AppendResult(interp, "couldn't write JPEG string: ", (char *) NULL);
      append_jpeg_message(interp, (j_common_ptr) &cinfo);
      jpeg.destroy_compress(&cinfo);
      return TCL_ERROR;
    }

    /* Now we can initialize libjpeg. */
    jpeg.CreateCompress(&cinfo, JPEG_LIB_VERSION,
	    (size_t) sizeof(struct jpeg_compress_struct));
    jpeg_string_dest(&cinfo, dataPtr);

    /* Share code with FileWriteJPEG. */
    result = CommonWriteJPEG(interp, &cinfo, formatString, blockPtr);

    jpeg.destroy_compress(&cinfo);

    return result;
}

/*
 *----------------------------------------------------------------------
 *
 * CommonWriteJPEG --
 *
 *	The common guts of FileWriteJPEG and StringWriteJPEG.
 *	The compress struct has already been set up and the
 *	appropriate data destination manager initialized.
 *	The caller should do jpeg_destroy_compress upon return,
 *	and also close the destination as necessary.
 *
 *----------------------------------------------------------------------
 */

static int
CommonWriteJPEG(interp, cinfo, formatString, blockPtr)
    Tcl_Interp *interp;
    j_compress_ptr cinfo;	
    char *formatString;
    Tk_PhotoImageBlock *blockPtr;
{
    JSAMPROW row_pointer[1];	/* pointer to original data scanlines */
    JSAMPARRAY buffer;		/* Intermediate row buffer */
    JSAMPROW bufferPtr;
    int w, h;
    int greenOffset, blueOffset, alphaOffset;
    unsigned char *pixelPtr, *pixLinePtr;

    grayscale = 0;
    greenOffset = blockPtr->offset[1] - blockPtr->offset[0];
    blueOffset = blockPtr->offset[2] - blockPtr->offset[0];
    alphaOffset = blockPtr->offset[0];
    if (alphaOffset < blockPtr->offset[2]) {
        alphaOffset = blockPtr->offset[2];
    }
    if (++alphaOffset < blockPtr->pixelSize) {
	alphaOffset -= blockPtr->offset[0];
    } else {
	alphaOffset = 0;
    }

    /* Set up JPEG compression parameters. */
    cinfo->image_width = blockPtr->width;
    cinfo->image_height = blockPtr->height;
    cinfo->input_components = 3;
    cinfo->in_color_space = JCS_RGB;

    jpeg.set_defaults(cinfo);

    /* Parse options, if any, and alter default parameters */
    if (formatString != NULL) {
      int argc;
      char **argv;
      if (Tcl_SplitList(interp, formatString, &argc, &argv) != TCL_OK)
	return TCL_ERROR;
      quality = 75;		/* default values */
      smooth = 0;
      optimize = 0;
      progressive = 0;
      if (Tk_ParseArgv(interp, (Tk_Window) NULL, &argc, argv,
		       writeOptTable, TK_ARGV_NO_LEFTOVERS|TK_ARGV_NO_DEFAULTS)
	  != TCL_OK) {
	ckfree((char *) argv);
	return TCL_ERROR;
      }
      if (jpeg.set_quality != NULL) {
	jpeg.set_quality(cinfo, quality, FALSE);
      }
      cinfo->smoothing_factor = smooth;
      if (grayscale && (jpeg.set_colorspace != NULL)) {
	/* Force a monochrome JPEG file to be generated. */
	jpeg.set_colorspace(cinfo, JCS_GRAYSCALE);
      }
      if (optimize) {
	/* Enable entropy parm optimization. */
	cinfo->optimize_coding = TRUE;
      }
      if (progressive  && (jpeg.simple_progression != NULL)) {
	/* Select simple progressive mode. */
	jpeg.simple_progression(cinfo);
      }
      ckfree((char *) argv);
    }

    pixLinePtr = blockPtr->pixelPtr + blockPtr->offset[0];
    greenOffset = blockPtr->offset[1] - blockPtr->offset[0];
    blueOffset = blockPtr->offset[2] - blockPtr->offset[0];
    if ((jpeg.set_colorspace != NULL) &&
	    (grayscale || (!greenOffset && !blueOffset))) {
	/* Generate monochrome JPEG file if source block is grayscale. */
	jpeg.set_colorspace(cinfo, JCS_GRAYSCALE);
    }

    jpeg.start_compress(cinfo, TRUE);
    
    /* note: we assume libjpeg is configured for standard RGB pixel order. */
    if ((greenOffset == 1) && (blueOffset == 2)
	&& (blockPtr->pixelSize == 3)) {
	/* No need to reformat pixels before passing data to libjpeg */
	for (h = blockPtr->height; h > 0; h--) {
	    row_pointer[0] = (JSAMPROW) pixLinePtr;
	    jpeg.write_scanlines(cinfo, row_pointer, 1);
	    pixLinePtr += blockPtr->pitch;
	}
    } else {
	/* Must convert data format.  Create a one-scanline work buffer. */
	buffer = (*cinfo->mem->alloc_sarray)
	  ((j_common_ptr) cinfo, JPOOL_IMAGE,
	   cinfo->image_width * cinfo->input_components, 1);
	for (h = blockPtr->height; h > 0; h--) {
	    pixelPtr = pixLinePtr;
	    bufferPtr = buffer[0];
	    for (w = blockPtr->width; w > 0; w--) {
		if (alphaOffset && !pixelPtr[alphaOffset]) {
		    /* if pixel is transparant, better use gray
		     * than the default black.
		     */
		    *bufferPtr++ = 0xd9;
		    *bufferPtr++ = 0xd9;
		    *bufferPtr++ = 0xd9;
		} else {
		    *bufferPtr++ = pixelPtr[0];
		    *bufferPtr++ = pixelPtr[greenOffset];
		    *bufferPtr++ = pixelPtr[blueOffset];
		}
		pixelPtr += blockPtr->pixelSize;
	    }
	    jpeg.write_scanlines(cinfo, buffer, 1);
	    pixLinePtr += blockPtr->pitch;
	}
    }

    jpeg.finish_compress(cinfo);

    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * Mwrite --
 *
 *  fwrite-like function to base64-encode data and add it to a Tcl DString.
 *
 * Results:
 *  The return value is the number of characters "written"
 *
 * Side effects:
 *  the base64 handle will change state.
 *
 *----------------------------------------------------------------------
 */

static size_t
Mwrite(src, count, dest)  
   char *src;		/* where to get the data */
   size_t count;	/* size of each transfer */
   str_dest_ptr dest;	/* "file" handle */
{
    size_t curcount = dest->handle.data - Tcl_DStringValue(dest->dstring);
    size_t bufcount = curcount + count + count/3 + count/52 + 10;
    size_t i;

    /* make sure that the DString contains enough space */
    if (bufcount >= (size_t) dest->dstring->spaceAvl) {
	/* We allocate 4k extra, so we don't have to */
	/* reallocate the buffer too often */
	Tcl_DStringSetLength(dest->dstring, bufcount + 4096);
	dest->handle.data = Tcl_DStringValue(dest->dstring) + curcount;
    }

    /* write the data */
    for (i=0; i<count; i++) {
      int c = (*src++) & 0x0ff;
      switch (dest->handle.state++) {
      case 0:
	*dest->handle.data++ = base64_table[(c>>2)&63];
	break;
      case 1:
	c |= dest->handle.c<<8;
	*dest->handle.data++ = base64_table[(c>>4)&63];
	break;
      case 2:
	dest->handle.state = 0;
	c |= dest->handle.c<<8;
	*dest->handle.data++ = base64_table[(c>>6)&63];
	*dest->handle.data++ = base64_table[c&63];
	break;
      }
      dest->handle.c = c;
      if (dest->linelength++ > 52) {
	dest->linelength = 0;
	*dest->handle.data++ = '\n';
      }
    }

    return count;
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
 *  the next byte (or JPG_DONE) is returned.
 *
 * Side effects:
 *  the base64 handle will change state.
 *
 *----------------------------------------------------------------------
 */

static int
Mgetc(handle)
   register MFile *handle;	/* handle containing decoder data and state */
{
    register int c, result = JPG_DONE;
     
    if (handle->state == JPG_DONE) {
	return(JPG_DONE);
    }

    do {
	c=char64(*handle->data);
	handle->data++;
    } while (c==JPG_SPACE);

    if (c>JPG_SPECIAL) {
	handle->state = JPG_DONE;
	return(handle->state ? handle->c : JPG_DONE);
    }

    switch (handle->state++) {
	case 0:
	   handle->c = c<<2;
	   result = Mgetc(handle);
	   break;
	case 1:
	   result = handle->c | (c>>4);
	   handle->c = (c&0x0F)<<4;
	   break;
	case 2:
	   result = handle->c | (c>>2);
	   handle->c = (c&0x03) << 6;
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
 * char64 --
 *
 *  This procedure converts a base64 ascii character into
 *  its binary equivalent.  This code is a slightly modified version
 *  of the char64 proc in N. Borenstein's metamail decoder.
 *
 * Results:
 *  the binary value, or an error code.
 *
 * Side effects:
 *  none
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

	case ' ': case '\t': case '\n': case '\r': case '\f': return(JPG_SPACE);
	case '=':  return(JPG_PAD);
	case '\0': return(JPG_DONE);
	default: return(JPG_BAD);
    }
}

/*
 * libjpeg source manager for reading from base64-encoded strings.
 */
static void
jpeg_string_src (cinfo, string)
    j_decompress_ptr cinfo;
    char* string;
{
  str_src_ptr src;

  src = (str_src_ptr)
      (*cinfo->mem->alloc_small) ((j_common_ptr) cinfo, JPOOL_PERMANENT,
				  sizeof(struct str_source_mgr));
  cinfo->src = (struct jpeg_source_mgr *) src;

  src->pub.init_source = str_init_source;
  src->pub.fill_input_buffer = str_fill_input_buffer;
  src->pub.skip_input_data = str_skip_input_data;
  src->pub.resync_to_restart = jpeg.resync_to_restart; /* use default method */
  src->pub.term_source = str_term_source;

  src->handle.data = string;
  src->handle.state = 0;

  src->pub.bytes_in_buffer = 0; /* forces fill_input_buffer on first read */
  src->pub.next_input_byte = NULL; /* until buffer loaded */
}

static void
str_init_source(cinfo)
    j_decompress_ptr cinfo;
{
  /* no work necessary here */
}

static boolean
str_fill_input_buffer(cinfo)
    j_decompress_ptr cinfo;
{
  str_src_ptr src = (str_src_ptr) cinfo->src;
  size_t nbytes;
  int c;

  nbytes = 0;
  while (nbytes < STRING_BUF_SIZE &&
	 (c = Mgetc(&src->handle)) != JPG_DONE) {
    src->buffer[nbytes++] = (JOCTET) c;
  }

  if (nbytes <= 0) {
    WARNMS(cinfo, JWRN_JPEG_EOF);
    /* Insert a fake EOI marker */
    src->buffer[0] = (JOCTET) 0xFF;
    src->buffer[1] = (JOCTET) JPEG_EOI;
    nbytes = 2;
  }

  src->pub.next_input_byte = src->buffer;
  src->pub.bytes_in_buffer = nbytes;

  return TRUE;
}

static void
str_skip_input_data(cinfo, num_bytes)
    j_decompress_ptr cinfo;
    long num_bytes;
{
  str_src_ptr src = (str_src_ptr) cinfo->src;

  if (num_bytes > 0) {
    while (num_bytes > (long) src->pub.bytes_in_buffer) {
      num_bytes -= (long) src->pub.bytes_in_buffer;
      str_fill_input_buffer(cinfo);
    }
    src->pub.next_input_byte += (size_t) num_bytes;
    src->pub.bytes_in_buffer -= (size_t) num_bytes;
  }
}

static void
str_term_source(cinfo)
    j_decompress_ptr cinfo;
{
  /* no work necessary here */
}

/*
 * libjpeg destination manager for writing to base64-encoded strings.
 */
static void
jpeg_string_dest (cinfo, dstring)
    j_compress_ptr cinfo;
    Tcl_DString* dstring;
{
  str_dest_ptr dest;

  if (cinfo->dest == NULL) {	/* first time for this JPEG object? */
    cinfo->dest = (struct jpeg_destination_mgr *)
      (*cinfo->mem->alloc_small) ((j_common_ptr) cinfo, JPOOL_PERMANENT,
				  sizeof(struct str_destination_mgr));
  }

  dest = (str_dest_ptr) cinfo->dest;
  dest->pub.init_destination = str_init_destination;
  dest->pub.empty_output_buffer = str_empty_output_buffer;
  dest->pub.term_destination = str_term_destination;
  dest->dstring = dstring;
}

static void
str_init_destination (cinfo)
    j_compress_ptr cinfo;
{
  str_dest_ptr dest = (str_dest_ptr) cinfo->dest;

  Tcl_DStringSetLength(dest->dstring, dest->dstring->spaceAvl);
  dest->handle.data = Tcl_DStringValue(dest->dstring);
  dest->handle.state = 0;
  dest->linelength = 0;
  dest->pub.next_output_byte = dest->buffer;
  dest->pub.free_in_buffer = STRING_BUF_SIZE;
}

static boolean
str_empty_output_buffer (cinfo)
    j_compress_ptr cinfo;
{
  str_dest_ptr dest = (str_dest_ptr) cinfo->dest;

  if (Mwrite(dest->buffer, STRING_BUF_SIZE, dest) != (size_t) STRING_BUF_SIZE)
    ERREXIT(cinfo, JERR_FILE_WRITE);

  dest->pub.next_output_byte = dest->buffer;
  dest->pub.free_in_buffer = STRING_BUF_SIZE;

  return TRUE;
}

static void
str_term_destination (cinfo)
    j_compress_ptr cinfo;
{
  str_dest_ptr dest = (str_dest_ptr) cinfo->dest;
  size_t datacount = STRING_BUF_SIZE - dest->pub.free_in_buffer;

  /* Write any data remaining in the buffer */
  if (datacount > 0) {
    if (Mwrite(dest->buffer, datacount, dest) != datacount)
      ERREXIT(cinfo, JERR_FILE_WRITE);
  }

  /* Empty any partial-byte from the base64 encoder */
  switch (dest->handle.state) {
  case 1:
    *dest->handle.data++ = base64_table[(dest->handle.c<<4)&63];
    *dest->handle.data++ = '=';
    *dest->handle.data++ = '=';
    break;
  case 2:
    *dest->handle.data++ = base64_table[(dest->handle.c<<2)&63];
    *dest->handle.data++ = '=';
    break;
  default:
    break;
  }

  /* Set the final output length */
  Tcl_DStringSetLength(dest->dstring,
		       dest->handle.data - Tcl_DStringValue(dest->dstring));
}


/*
 * Error handler to replace (or extend, really) libjpeg's default handler
 */

static void
my_error_exit (cinfo)
    j_common_ptr cinfo;
{
  struct my_error_mgr *myerr = (struct my_error_mgr *) cinfo->err;
  /* Exit back to outer level */
  longjmp(myerr->setjmp_buffer, 1);
}

static void
append_jpeg_message (interp, cinfo)
    Tcl_Interp *interp;
    j_common_ptr cinfo;
{
  /* Append libjpeg error message to interp->result */
  char buffer[JMSG_LENGTH_MAX];
  (*cinfo->err->format_message) (cinfo, buffer);
  Tcl_AppendResult(interp, buffer, (char *) NULL);
}

static void
my_output_message (cinfo)
    j_common_ptr cinfo;
{
  /* Override libjpeg's output_message to do nothing.
   * This ensures that warning messages will not appear on stderr,
   * even for a corrupted JPEG file.  Too bad there's no way
   * to report a "warning" message to the calling Tcl script.
   */
}
