/*
 * tkImgPNG.c --
 *
 * A photo image file handler for PNG files.
 *
 * Uses the libpng.so library, which is dynamically
 * loaded only when used.
 *
 */

/* Author : Jan Nijtmans */
/* Date   : 3/21/97        */
/* Original implementation : Joel Crisp     */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "tcl.h"
#include "tkInt.h"

#undef EXTERN

#ifdef MAC_TCL
#include "::compat:png.h"
#else
#ifdef HAVE_PNG_H
#   include <png.h>
#else
#   include "../compat/png.h"
#endif
#endif

#ifdef __WIN32__
#define PNG_LIB_NAME "png.dll"
#endif

#ifndef PNG_LIB_NAME
#define PNG_LIB_NAME "libpng.so"
#endif

#ifndef PNG_LIB_SUFFIX
#define PNG_LIB_SUFFIX "1"
#endif

#define COMPRESS_THRESHOLD 1024

/*
 * PNG's are represented as data in base64 format.
 * base64 strings consist of 4 6-bit characters -> 3 8 bit bytes.
 * A-Z, a-z, 0-9, + and / represent the 64 values (in order).
 * '=' is a trailing padding char when the un-encoded data is not a
 * multiple of 3 bytes.  We'll ignore white space when encountered.
 * Any other invalid character is treated as an EOF
 */

#define PNG_SPECIAL	(256)
#define PNG_PAD		(PNG_SPECIAL+1)
#define PNG_SPACE	(PNG_SPECIAL+2)
#define PNG_BAD		(PNG_SPECIAL+3)
#define PNG_DONE	(PNG_SPECIAL+4)

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

typedef struct mFile {
    Tcl_DString *buffer;	/* pointer to dynamical string for writing */
    char *data;			/* mmencoded source/destination string */
    int c;			/* bits left over from previous character */
    int state;			/* decoder state (0-4 or PNG_DONE) */
    int linelength;		/* length of phisical line already written */
} MFile;

/*
 * The format record for the PNG file format:
 */


static int FileMatchPNG _ANSI_ARGS_((FILE *f, char *fileName,
	char *formatString, int *widthPtr, int *heightPtr));
static int StringMatchPNG _ANSI_ARGS_((char *string, char *formatString,
	int *widthPtr, int *heightPtr));
static int FileReadPNG _ANSI_ARGS_((Tcl_Interp *interp, FILE *f,
	char *fileName, char *formatString, Tk_PhotoHandle imageHandle,
	int destX, int destY, int width, int height, int srcX, int srcY));
static int StringReadPNG _ANSI_ARGS_((Tcl_Interp *interp, char *string,
	char *formatString, Tk_PhotoHandle imageHandle,
	int destX, int destY, int width, int height, int srcX, int srcY));
static int FileWritePNG _ANSI_ARGS_((Tcl_Interp *interp, char *filename,
	char *formatString, Tk_PhotoImageBlock *blockPtr));
static int StringWritePNG _ANSI_ARGS_((Tcl_Interp *interp,
	Tcl_DString *dataPtr, char *formatString,
	Tk_PhotoImageBlock *blockPtr));

Tk_PhotoImageFormat tkImgFmtPNG = {
	"PNG",		/* name */
	FileMatchPNG,   /* fileMatchProc */
  	StringMatchPNG, /* stringMatchProc */
	FileReadPNG,    /* fileReadProc */
  	StringReadPNG,  /* stringReadProc */
	FileWritePNG,   /* fileWriteProc */
	StringWritePNG,	/* stringWriteProc */
};

/*
 * Prototypes for local procedures defined in this file:
 */

static int CommonReadPNG _ANSI_ARGS_((png_structp png_ptr, char *formatString,
	Tk_PhotoHandle imageHandle, int destX, int destY, int width,
	int height, int srcX, int srcY));
static int CommonWritePNG _ANSI_ARGS_((Tcl_Interp *interp, png_structp png_ptr,
	png_infop info_ptr, char *formatString,
	Tk_PhotoImageBlock *blockPtr));
static void tk_png_error _ANSI_ARGS_((png_structp, png_const_charp));
static void tk_png_warning _ANSI_ARGS_((png_structp, png_const_charp));

/*
 * these are for the BASE64 image reader code only
 */

static void	tk_png_memread _ANSI_ARGS_((png_structp, png_bytep,
		    png_uint_32));
static void	tk_png_memwrite _ANSI_ARGS_((png_structp, png_bytep,
		    png_uint_32));
static int	Mread _ANSI_ARGS_((char *dst, size_t size,
		    size_t count, MFile *handle));
static int	Mwrite _ANSI_ARGS_((char *src, size_t size,
		    size_t count, MFile *handle));
static int	Mgetc _ANSI_ARGS_((MFile *handle));
static int	Mputc _ANSI_ARGS_((int c, MFile *handle));
static int	char64 _ANSI_ARGS_((int c));
static void	mInit _ANSI_ARGS_((char *string,
		    MFile *handle));
static void	mWriteInit _ANSI_ARGS_((Tcl_DString *buffer,
		    MFile *handle));

static struct PngFunctions {
    VOID *handle;
    png_structp (* create_read_struct) _ANSI_ARGS_((png_const_charp,
	png_voidp, png_error_ptr, png_error_ptr));
    png_infop (* create_info_struct) _ANSI_ARGS_((png_structp));
    png_structp (* create_write_struct) _ANSI_ARGS_((png_const_charp,
	png_voidp, png_error_ptr, png_error_ptr));
    void (* destroy_read_struct) _ANSI_ARGS_((png_structpp,
	png_infopp, png_infopp));
    void (* destroy_write_struct) _ANSI_ARGS_((png_structpp, png_infopp));
    void (* error) _ANSI_ARGS_((png_structp, png_charp));
    png_voidp (* get_error_ptr) _ANSI_ARGS_((png_structp));
    png_voidp (* get_progressive_ptr) _ANSI_ARGS_((png_structp));
    void (* init_io) _ANSI_ARGS_((png_structp, FILE *));
    void (* read_image) _ANSI_ARGS_((png_structp, png_bytepp));
    void (* read_info) _ANSI_ARGS_((png_structp, png_infop));
    void (* read_update_info) _ANSI_ARGS_((png_structp, png_infop));
    void (* set_read_fn) _ANSI_ARGS_((png_structp, png_voidp, png_rw_ptr));
    void (* set_write_fn) _ANSI_ARGS_((png_structp, png_voidp,
	    png_rw_ptr, png_voidp));
    void (* write_end) _ANSI_ARGS_((png_structp, png_infop));
    void (* write_info) _ANSI_ARGS_((png_structp, png_infop));
    void (* write_rows) _ANSI_ARGS_((png_structp, png_bytepp, png_uint_32));
    void (* set_expand) _ANSI_ARGS_((png_structp));
    void (* set_filler) _ANSI_ARGS_((png_structp, png_byte, int));
    void (* set_strip_16) _ANSI_ARGS_((png_structp));
    int (* set_interlace_handling) _ANSI_ARGS_((png_structp));
} png = {0};

static char *symbols[] = {
    "png_create_read_struct",
    "png_create_info_struct",
    "png_create_write_struct",
    "png_destroy_read_struct",
    "png_destroy_write_struct",
    "png_error",
    "png_get_error_ptr",
    "png_get_progressive_ptr",
    "png_init_io",
    "png_read_image",
    "png_read_info",
    "png_read_update_info",
    "png_set_read_fn",
    "png_set_write_fn",
    "png_write_end",
    "png_write_info",
    "png_write_rows",
    /* The following symbols are not crucial. All of them
       are checked at runtime. */
    "png_set_expand",
    "png_set_filler",
    "png_set_strip_16",
    "png_set_interlace_handling",
    (char *) NULL
};

typedef struct cleanup_info {
    Tcl_Interp *interp;
    char **data;
} cleanup_info;

static void
tk_png_error(png_ptr, error_msg)
    png_structp png_ptr;
    png_const_charp error_msg;
{
    cleanup_info *info;

    info = (cleanup_info *) png.get_error_ptr(png_ptr);
    if (info->data) {
	ckfree((char *) info->data);
    }
    Tcl_AppendResult(info->interp,
	    error_msg, (char *) NULL);
    longjmp(*(jmp_buf *) png_ptr,1);
}

static void
tk_png_warning(png_ptr, error_msg)
    png_structp png_ptr;
    png_const_charp error_msg;
{
    return;
}

static void
tk_png_memread(png_ptr, data, length)
    png_structp png_ptr;
    png_bytep data;
    png_uint_32 length;
{
    png_uint_32 check;

    check = Mread(data, 1, (size_t)length,
	    (MFile *) png.get_progressive_ptr(png_ptr));
    if (check != length) {
	png.error(png_ptr, "Read Error");
    }
}

static void
tk_png_memwrite(png_ptr, data, length)
    png_structp png_ptr;
    png_bytep data;
    png_uint_32 length;
{
    png_uint_32 check;

    check = Mwrite(data, 1, (size_t)length,
	   (MFile *) png.get_progressive_ptr(png_ptr));
    if (check != length) {
	png.error(png_ptr, "Write Error");
    }
}

static int FileMatchPNG(f, fileName, formatString, widthPtr, heightPtr)
    FILE *f;
    char *fileName;
    char *formatString;
    int *widthPtr, *heightPtr;
{
    unsigned char buf[24];

    if ((fread(buf, 1, 24, f) != 24)
	    || (strncmp("\211PNG\15\12\32\12", (char *) buf, 8) != 0)
	    || (strncmp("IHDR", (char *) buf+12, 4) != 0)) {
	return 0;
    }
    *widthPtr = (buf[16]<<24) + (buf[17]<<16) + (buf[18]<<8) + buf[19];
    *heightPtr = (buf[20]<<24) + (buf[21]<<16) + (buf[22]<<8) + buf[23];
    return 1;
}

static int StringMatchPNG(string, formatString, widthPtr, heightPtr)
    char *string;
    char *formatString;
    int *widthPtr, *heightPtr;
{
    unsigned char buf[24];
    MFile handle;

    mInit(string,&handle);

    if ((Mread(buf, 1, 24, &handle) != 24)
	    || (strncmp("\211PNG\15\12\32\12", (char *) buf, 8) != 0)
	    || (strncmp("IHDR", (char *) buf+12, 4) != 0)) {
	return 0;
    }
    *widthPtr = (buf[16]<<24) + (buf[17]<<16) + (buf[18]<<8) + buf[19];
    *heightPtr = (buf[20]<<24) + (buf[21]<<16) + (buf[22]<<8) + buf[23];
    return 1;
}

static int FileReadPNG(interp, f, fileName, formatString, imageHandle,
	destX, destY, width, height, srcX, srcY)
    Tcl_Interp *interp;
    FILE *f;
    char *fileName;
    char *formatString;
    Tk_PhotoHandle imageHandle;
    int destX, destY;
    int width, height;
    int srcX, srcY;
{
    png_structp png_ptr;
    cleanup_info cleanup;

    if (TkLoadLib(interp, PNG_LIB_NAME, PNG_LIB_SUFFIX, &png.handle,
	    symbols, 17) != TCL_OK) {
	return TCL_ERROR;
    }

    cleanup.interp = interp;
    cleanup.data = NULL;

    png_ptr=png.create_read_struct(PNG_LIBPNG_VER_STRING,
	    (png_voidp) &cleanup,tk_png_error,tk_png_warning);
    if (!png_ptr) return(0); 

    png.init_io(png_ptr,f);

    return CommonReadPNG(png_ptr, formatString, imageHandle, destX, destY,
	    width,height,srcX,srcY);
}

static int StringReadPNG(interp, string, formatString, imageHandle,
	destX, destY, width, height, srcX, srcY)
    Tcl_Interp *interp;
    char *string;
    char *formatString;
    Tk_PhotoHandle imageHandle;
    int destX, destY;
    int width, height;
    int srcX, srcY;
{
    png_structp png_ptr;
    MFile handle;
    cleanup_info cleanup;

    cleanup.interp = interp;
    cleanup.data = NULL;

    if (TkLoadLib(interp, PNG_LIB_NAME, PNG_LIB_SUFFIX, &png.handle,
	    symbols, 17) != TCL_OK) {
	return TCL_ERROR;
    }

    png_ptr=png.create_read_struct(PNG_LIBPNG_VER_STRING,
	    (png_voidp) &cleanup,tk_png_error,tk_png_warning);
    if (!png_ptr) return(0); 

    mInit(string,&handle);

    png.set_read_fn(png_ptr,&handle, tk_png_memread);

    return CommonReadPNG(png_ptr, formatString, imageHandle, destX, destY,
	    width, height, srcX, srcY);
}

typedef struct myblock {
    Tk_PhotoImageBlock ck;
    int dummy; /* extra space for offset[3], in case it is not
		  included already in Tk_PhotoImageBlock */
} myblock;

#define block bl.ck

static int CommonReadPNG(png_ptr, formatString, imageHandle, destX, destY,
	width, height, srcX, srcY)
    png_structp png_ptr;
    char *formatString;
    Tk_PhotoHandle imageHandle;
    int destX, destY;
    int width, height;
    int srcX, srcY;
{
    png_infop info_ptr;
    png_infop end_info;
    char **png_data = NULL;
    myblock bl;
    unsigned int I;

    info_ptr=png.create_info_struct(png_ptr);
    if (!info_ptr) {
	png.destroy_read_struct(&png_ptr,NULL,NULL);
	return(TCL_ERROR);
    }

    end_info=png.create_info_struct(png_ptr);
    if (!end_info) {
	png.destroy_read_struct(&png_ptr,&info_ptr,NULL);
	return(TCL_ERROR);
    }

    if (setjmp(*(jmp_buf *) png_ptr)) {
	png.destroy_read_struct(&png_ptr, &info_ptr, &end_info);
	return TCL_ERROR;
    }

    png.read_info(png_ptr,info_ptr);

    if ((srcX + width) > (int) info_ptr->width) {
	width = info_ptr->width - srcX;
    }
    if ((srcY + height) > (int) info_ptr->height) {
	height = info_ptr->height - srcY;
    }
    if ((width <= 0) || (height <= 0)
	|| (srcX >= (int) info_ptr->width)
	|| (srcY >= (int) info_ptr->height)) {
	return TCL_OK;
    }

    Tk_PhotoExpand(imageHandle, destX + width, destY + height);

    if (png.set_expand != NULL) {
	png.set_expand(png_ptr);
    }

    Tk_PhotoGetImage(imageHandle, &block);

    if (info_ptr->bit_depth==16) {
	if (png.set_strip_16 == NULL) {
	    block.offset[1] = 2;
	    block.offset[2] = 4;
	} else {
	    png.set_strip_16(png_ptr);
	}
    }

    if (info_ptr->interlace_type
	    && (png.set_interlace_handling != NULL)) {
	png.set_interlace_handling(png_ptr);
    }

    png.read_update_info(png_ptr,info_ptr);
    png_data= (char **) ckalloc(sizeof(char *)*info_ptr->height +
	    info_ptr->height*info_ptr->rowbytes);
    ((cleanup_info *) png.get_error_ptr(png_ptr))->data = png_data;
    for(I=0;I<info_ptr->height;I++) {
	png_data[I]= ((char *) png_data) + (sizeof(char *)*info_ptr->height +
		I*info_ptr->rowbytes); 
    }
    png.read_image(png_ptr,(png_bytepp) png_data);

    block.pixelSize=info_ptr->pixel_depth/8;
    if (block.pixelSize < 3) {
	/* must be a grayscale image, with or without alpha channel */
	block.offset[1]=0;
	block.offset[2]=0;
    }
    block.width=width;
    block.height=height;
    block.pitch=info_ptr->rowbytes;
    block.pixelPtr=(unsigned char *) (png_data[srcY]+srcX*block.pixelSize);

    if (block.pixelSize & 1) {
	/* without alpha channel */
	block.offset[3] = 0;
    } else {
	/* with alpha channel */
	block.offset[3] = block.pixelSize - 1;
    }
    TkPhotoPutBlock(imageHandle,&block,destX,destY,width,height);

    ckfree((char *) png_data);
    png_data=NULL;

    png.destroy_read_struct(&png_ptr,&info_ptr,&end_info);

    return(TCL_OK);
}

static int FileWritePNG(interp, filename, formatString, blockPtr)
    Tcl_Interp *interp;
    char *filename;
    char *formatString;
    Tk_PhotoImageBlock *blockPtr;
{
    FILE *outfile = NULL;
    png_structp png_ptr;
    png_infop info_ptr;
    Tcl_DString nameBuffer; 
    char *fullname;
    int result;
    cleanup_info cleanup;

    if ((fullname=Tcl_TranslateFileName(interp,filename,&nameBuffer))==NULL) {
	return TCL_ERROR;
    }

    if (!(outfile=fopen(fullname,"wb"))) {
	Tcl_AppendResult(interp, filename, ": ", Tcl_PosixError(interp),
		(char *)NULL);
	Tcl_DStringFree(&nameBuffer);
	return TCL_ERROR;
    }

    Tcl_DStringFree(&nameBuffer);

    if (TkLoadLib(interp, PNG_LIB_NAME, PNG_LIB_SUFFIX, &png.handle,
	    symbols, 17) != TCL_OK) {
	return TCL_ERROR;
    }

    cleanup.interp = interp;
    cleanup.data = (char **) NULL;

    png_ptr=png.create_write_struct(PNG_LIBPNG_VER_STRING,
	    (png_voidp) &cleanup,tk_png_error,tk_png_warning);
    if (!png_ptr) return TCL_ERROR; 

    info_ptr=png.create_info_struct(png_ptr);
    if (!info_ptr) {
	png.destroy_write_struct(&png_ptr,NULL);
	fclose(outfile);
	return TCL_ERROR;
    }

    png.init_io(png_ptr,outfile);

    result = CommonWritePNG(interp, png_ptr, info_ptr, formatString, blockPtr);
    fclose(outfile);
    return result;
}

static int StringWritePNG(interp, dataPtr, formatString, blockPtr)
    Tcl_Interp *interp;
    Tcl_DString *dataPtr;
    char *formatString;
    Tk_PhotoImageBlock *blockPtr;
{
    png_structp png_ptr;
    png_infop info_ptr;
    MFile handle;
    int result;
    cleanup_info cleanup;

    if (TkLoadLib(interp, PNG_LIB_NAME, PNG_LIB_SUFFIX, &png.handle,
	    symbols, 17) != TCL_OK) {
	return TCL_ERROR;
    }

    cleanup.interp = interp;
    cleanup.data = (char **) NULL;

    png_ptr=png.create_write_struct(PNG_LIBPNG_VER_STRING,
	    (png_voidp) &cleanup,tk_png_error,tk_png_warning);
    if (!png_ptr) return TCL_ERROR; 

    info_ptr=png.create_info_struct(png_ptr);
    if (!info_ptr) {
	png.destroy_write_struct(&png_ptr,NULL);
	return TCL_ERROR;
    }

    png.set_write_fn(png_ptr,(png_voidp) &handle, tk_png_memwrite, (png_voidp) NULL);

    mWriteInit(dataPtr, &handle);

    result = CommonWritePNG(interp, png_ptr, info_ptr, formatString, blockPtr);

    switch(handle.state) {
	case 1:
	    *handle.data++ = base64_table[(handle.c<<4)&63];
	    *handle.data++ = '='; *handle.data++ = '='; break;
	case 2:
	    *handle.data++ = base64_table[(handle.c<<2)&63];
	    *handle.data++ = '='; break;
    }
    Tcl_DStringSetLength(dataPtr, handle.data - Tcl_DStringValue(dataPtr));
    return result;
}

static int CommonWritePNG(interp, png_ptr, info_ptr, formatString, blockPtr)
    Tcl_Interp *interp;
    png_structp png_ptr;
    png_infop info_ptr;
    char *formatString;
    Tk_PhotoImageBlock *blockPtr;
{
    int greenOffset, blueOffset, alphaOffset;
    int tagcount = 0;
    char **tags = NULL;
    int I, pass, number_passes;  
    int newPixelSize;
    png_bytep row_pointers;

    if (formatString != NULL) {
	if (Tcl_SplitList(interp,formatString,&tagcount,&tags)!=TCL_OK) {
	    Tcl_AppendResult(interp,"invalid format: \"",
		    formatString, "\"",(char *) NULL); 
	    return TCL_ERROR;
	}
	tagcount = tagcount/2 - 1;
    }

    if (setjmp(*(jmp_buf *)png_ptr)) {
	if (tags) {
	    ckfree((char *) tags);
	}
	for(I=0;I<tagcount;I++) {
	    ckfree((char *) info_ptr->text[I].key);
	    ckfree((char *) info_ptr->text[I].text);
	}
	png.destroy_write_struct(&png_ptr,&info_ptr);
	return TCL_ERROR;
    }
    info_ptr->width=blockPtr->width;
    info_ptr->height=blockPtr->height;
    info_ptr->bit_depth=8;
    greenOffset = blockPtr->offset[1] - blockPtr->offset[0];
    blueOffset = blockPtr->offset[2] - blockPtr->offset[0];
    alphaOffset = blockPtr->offset[0] + 1;
    if (alphaOffset <= blockPtr->offset[1]) alphaOffset = blockPtr->offset[1] + 1;
    if (alphaOffset <= blockPtr->offset[2]) alphaOffset = blockPtr->offset[2] + 1;
    if (alphaOffset < blockPtr->pixelSize) {
	alphaOffset -= blockPtr->offset[0];
    } else {
	alphaOffset = 0;
    }

    if (greenOffset || blueOffset) {
	info_ptr->color_type = PNG_COLOR_TYPE_RGB;
	newPixelSize = 3;
    } else {
	info_ptr->color_type = PNG_COLOR_TYPE_GRAY;
	newPixelSize = 1;
    }
    if (alphaOffset) {
	info_ptr->color_type |= PNG_COLOR_MASK_ALPHA;
	newPixelSize++;
    } else if ((blockPtr->pixelSize==4) && (newPixelSize == 3)
	    && (png.set_filler != NULL)) {
	png.set_filler(png_ptr,0,PNG_FILLER_AFTER);
	newPixelSize++;
    }
    info_ptr->interlace_type=1;
    info_ptr->valid=0;

    info_ptr->num_text=tagcount;
    if (tagcount>0) {
	info_ptr->text= (png_textp) ckalloc(tagcount*sizeof(png_text));
	for(I=0;I<tagcount;I++) {
	    info_ptr->text[I].compression = 0;
	    info_ptr->text[I].key = tags[2*I+1];
	    info_ptr->text[I].text = tags[2*I+2];
	    info_ptr->text[I].text_length=strlen(tags[2*I+2]);
	    if (info_ptr->text[I].text_length>COMPRESS_THRESHOLD) { 
		info_ptr->text[I].compression = -1;
	    } 
        }
    }

    png.write_info(png_ptr,info_ptr);

    if (tagcount>0) {
	ckfree((char *) info_ptr->text);
	ckfree((char *) tags);
	tags = (char **) NULL;
    }

    number_passes = png.set_interlace_handling(png_ptr);
    if (blockPtr->pixelSize != newPixelSize) {
	int J, oldPixelSize;
	png_bytep src, dst;
	oldPixelSize = blockPtr->pixelSize;
	row_pointers = (png_bytep)
		ckalloc(blockPtr->width * newPixelSize);
	for (pass = 0; pass < number_passes; pass++) {
	    for(I=0;I<blockPtr->height;I++) {
		src = (png_bytep) blockPtr->pixelPtr
			+ I * blockPtr->pitch + blockPtr->offset[0];
		dst = row_pointers;
		for (J = blockPtr->width; J > 0; J--) {
		    memcpy(dst, src, newPixelSize);
		    src += oldPixelSize;
		    dst += newPixelSize;
		}
		png.write_rows(png_ptr, &row_pointers, 1);
	    }
	}
	ckfree((char *) row_pointers);
    } else {
	for (pass = 0; pass < number_passes; pass++) {
	    for(I=0;I<blockPtr->height;I++) {
		row_pointers = (png_bytep) blockPtr->pixelPtr
			+ I * blockPtr->pitch + blockPtr->offset[0];
		png.write_rows(png_ptr, &row_pointers, 1);
	    }
	}
    }
    png.write_end(png_ptr,NULL);
    png.destroy_write_struct(&png_ptr,&info_ptr);

    return(TCL_OK);
}


/*
 *----------------------------------------------------------------------
 *
 * mInit -- --
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
mInit(string, handle)
    char *string;	/* string containing initial mmencoded data */
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
    Tcl_DStringSetLength(buffer, buffer->spaceAvl);
    handle->buffer = buffer;
    handle->data = Tcl_DStringValue(buffer);
    handle->state = 0;
    handle->linelength = 0;
}

/*
 *----------------------------------------------------------------------
 *
 * Mread --
 *
 *  This procedure is invoked by the PNG file reader as a 
 *  temporary replacement for "fread", to get PNG data out
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

   for(i=0; (i<count) && (c=Mgetc(handle)) != PNG_DONE; i++) {
	*dst++ = c;
   }
   return i;
}

/*
 *----------------------------------------------------------------------
 *
 * Mwrite --
 *
 *  This procedure is invoked by the PNG file reader as a 
 *  temporary replacement for "fwrite", to put PNG data
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
   char *src;		/* where to get the data */
   size_t chunkSize;	/* size of each transfer */
   size_t numChunks;	/* number of chunks */
   MFile *handle;	/* mmencode "file" handle */
{
    register int i;
    int count = chunkSize * numChunks;
    int curcount = handle->data - Tcl_DStringValue(handle->buffer);
    int bufcount = curcount + count + count/3 +count/52 + 10;

    /* make sure that the DString contains enough space */
    if (bufcount >= (handle->buffer->spaceAvl + 10)) {
	/* We allocate 4k extra, so we don't have to */
	/* reallocate the buffer too often */
	Tcl_DStringSetLength(handle->buffer, bufcount + 4096);
	handle->data = Tcl_DStringValue(handle->buffer) + curcount;
    }

    /* write the data */
    for(i=0; (i<count) && (Mputc(*src++,handle) != PNG_DONE); i++) {
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
 *  the next byte (or PNG_DONE) is returned.
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
    register int c, result = 0;
     
    if (handle->state == PNG_DONE) {
	return(PNG_DONE);
    }

    do {
	c=char64(*handle->data);
	handle->data++;
    } while (c==PNG_SPACE);

    if (c>PNG_SPECIAL) {
	handle->state = PNG_DONE;
	return(handle->state ? handle->c : PNG_DONE);
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

	case ' ': case '\t': case '\n': case '\r': case '\f': return(PNG_SPACE);
	case '=':  return(PNG_PAD);
	case '\0': return(PNG_DONE);
	default: return(PNG_BAD);
    }
}
