/*
 * Original version by Jean-loup Gailly. Modified for use by the
 * Erlang run-time system and efile_driver; names of all external
 * functions changed to avoid conflicts with the official gzio.c file.
 *
 * gzio.c -- IO on .gz files
 * Copyright (C) 1995-1996 Jean-loup Gailly.
 * For conditions of distribution and use, see copyright notice in zlib.h
 */

/* $Id$ */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif
#include <stdio.h>
#include <errno.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <ctype.h>
#include "erl_driver.h"

#ifdef VXWORKS
/* pull in FOPEN from zutil.h instead */
#undef F_OPEN
#endif
#ifdef __WIN32__
#include "sys.h"
#define HAVE_CONFLICTING_FREAD_DECLARATION
#endif

#if(defined(_OSE_) && defined(POWERPC))
#define HAVE_CONFLICTING_FREAD_DECLARATION
#endif

#ifdef STDC
#  define zstrerror(errnum) strerror(errnum)
#else
#  define zstrerror(errnum) ""
#endif

#include "zutil.h"
#include "gzio.h"

struct internal_state {int dummy;}; /* for buggy compilers */

#define Z_BUFSIZE 4096

#define ALLOC(size) driver_alloc(size)
#define TRYFREE(p) {if (p) driver_free(p);}

static int gz_magic[2] = {0x1f, 0x8b}; /* gzip magic header */

/* gzip flag byte */
#define ASCII_FLAG   0x01 /* bit 0 set: file probably ascii text */
#define HEAD_CRC     0x02 /* bit 1 set: header CRC present */
#define EXTRA_FIELD  0x04 /* bit 2 set: extra field present */
#define ORIG_NAME    0x08 /* bit 3 set: original file name present */
#define COMMENT      0x10 /* bit 4 set: file comment present */
#define RESERVED     0xE0 /* bits 5..7: reserved */

typedef unsigned (*FioFunc)OF((void*, unsigned, unsigned, FILE*));

typedef struct gz_stream {
    z_stream stream;
    int      z_err;   /* error code for last stream operation */
    int      z_eof;   /* set if end of input file */
    FILE     *file;   /* .gz file */
    Byte     *inbuf;  /* input buffer */
    Byte     *outbuf; /* output buffer */
    uLong    crc;     /* crc32 of uncompressed data */
    char     *msg;    /* error message */
    char     *path;   /* path name for debugging only */
    int      transparent; /* 1 if input file is not a .gz file */
    char     mode;    /* 'w' or 'r' */
    int      position; /* Position (for seek) */
    FioFunc  reader;   /* Function used for reading from files. */
    int (*destroy)OF((struct gz_stream*)); /* Function to destroy
					    *  this structure. */
} gz_stream;

/* gzFile erts_gzbufopen OF((char* bytes, int size)); */

local gzFile gz_open      OF((const char *path, const char *mode, int  fd));
local int    get_byte     OF((gz_stream *s));
local void   check_header OF((gz_stream *s));
local int    destroy      OF((gz_stream *s));
local void   putLong      OF((FILE *file, uLong x));
local uLong  getLong      OF((gz_stream *s));

local unsigned mem_reader_dummy OF((void*, unsigned, unsigned, FILE*));
local int mem_destroy OF((gz_stream *s));

/* ===========================================================================
     Opens a gzip (.gz) file for reading or writing. The mode parameter
   is as in fopen ("rb" or "wb"). The file is given either by file descriptor
   or path name (if fd == -1).
     gz_open return NULL if the file could not be opened or if there was
   insufficient memory to allocate the (de)compression state; errno
   can be checked to distinguish the two cases (if errno is zero, the
   zlib error is Z_MEM_ERROR).
*/
local gzFile gz_open (path, mode, fd)
    const char *path;
    const char *mode;
    int  fd;
{
    int err;
    int level = Z_DEFAULT_COMPRESSION; /* compression level */
    char *p = (char*)mode;
    gz_stream *s;
    char fmode[80]; /* copy of mode, without the compression level */
    char *m = fmode;

    if (!path || !mode) return Z_NULL;

    s = (gz_stream *)ALLOC(sizeof(gz_stream));
    if (!s) return Z_NULL;

    s->stream.zalloc = (alloc_func)0;
    s->stream.zfree = (free_func)0;
    s->stream.opaque = (voidpf)0;
    s->stream.next_in = s->inbuf = Z_NULL;
    s->stream.next_out = s->outbuf = Z_NULL;
    s->stream.avail_in = s->stream.avail_out = 0;
    s->file = NULL;
    s->z_err = Z_OK;
    s->z_eof = 0;
    s->crc = crc32(0L, Z_NULL, 0);
    s->msg = NULL;
    s->transparent = 0;
    s->position = 0;
    s->destroy = destroy;

    s->path = (char*)ALLOC(strlen(path)+1);
    if (s->path == NULL) {
        return s->destroy(s), (gzFile)Z_NULL;
    }
    strcpy(s->path, path); /* do this early for debugging */

    s->mode = '\0';
    do {
        if (*p == 'r')
	    s->mode = 'r';
	if (*p == 'w' || *p == 'a')
	    s->mode = 'w';
	if (isdigit((int)*p)) {
	    level = *p - '0';
	} else {
	    *m++ = *p;		/* Copy the mode */
	}
    } while (*p++ && m < fmode + sizeof(fmode) - 1);
    *m = '\0';
    if (s->mode == '\0')
	return s->destroy(s), (gzFile)Z_NULL;
    
    if (s->mode == 'w') {
        err = deflateInit2(&(s->stream), level,
                           Z_DEFLATED, -MAX_WBITS, DEF_MEM_LEVEL, 0);
        /* windowBits is passed < 0 to suppress zlib header */

        s->stream.next_out = s->outbuf = (Byte*)ALLOC(Z_BUFSIZE);

        if (err != Z_OK || s->outbuf == Z_NULL) {
            return s->destroy(s), (gzFile)Z_NULL;
        }
    } else {
#if (defined(_OSE_SFK_) && defined(fread))
	s->reader = zzfread;
#else
#ifndef HAVE_CONFLICTING_FREAD_DECLARATION
	extern int fread();
#endif
	s->reader = fread;
#endif
        err = inflateInit2(&(s->stream), -MAX_WBITS);
        s->stream.next_in  = s->inbuf = (Byte*)ALLOC(Z_BUFSIZE);

        if (err != Z_OK || s->inbuf == Z_NULL) {
            return s->destroy(s), (gzFile)Z_NULL;
        }
    }
    s->stream.avail_out = Z_BUFSIZE;

    errno = 0;
    s->file = fd < 0 ? F_OPEN(path, fmode) : (FILE*)fdopen(fd, fmode);

    if (s->file == NULL) {
        return s->destroy(s), (gzFile)Z_NULL;
    }
    if (s->mode == 'w') {
        /* Write a very simple .gz header:
         */
        fprintf(s->file, "%c%c%c%c%c%c%c%c%c%c", gz_magic[0], gz_magic[1],
             Z_DEFLATED, 0 /*flags*/, 0,0,0,0 /*time*/, 0 /*xflags*/, OS_CODE);
    } else {
	check_header(s); /* skip the .gz header */
    }
    return (gzFile)s;
}

/* ===========================================================================
   Rewind a gzfile back to the beginning.
*/

local int gz_rewind (gz_stream *s)
{
    TRYFREE(s->msg);

    fseek(s->file, 0L, SEEK_SET);
    inflateReset(&(s->stream));
    s->stream.next_in = Z_NULL;
    s->stream.next_out = Z_NULL;
    s->stream.avail_in = s->stream.avail_out = 0;
    s->z_err = Z_OK;
    s->z_eof = 0;
    s->crc = crc32(0L, Z_NULL, 0);
    s->msg = NULL;
    s->position = 0;
    s->stream.next_in = s->inbuf;

    s->stream.avail_out = Z_BUFSIZE;

    check_header(s);		/* skip the .gz header */
    return 1;
}

/* ===========================================================================
     Opens a gzip (.gz) file for reading or writing.
*/
gzFile erts_gzopen (path, mode)
    const char *path;
    const char *mode;
{
    return gz_open (path, mode, -1);
}

/* ===========================================================================
     Associate a gzFile with the file descriptor fd. fd is not dup'ed here
   to mimic the behavio(u)r of fdopen.
*/
gzFile erts_gzdopen (fd, mode)
    int fd;
    const char *mode;
{
    char name[20];

    if (fd < 0) return (gzFile)Z_NULL;
    sprintf(name, "<fd:%d>", fd); /* for debugging */

    return gz_open (name, mode, fd);
}

/* ===========================================================================
     Read a byte from a gz_stream; update next_in and avail_in. Return EOF
   for end of file.
   IN assertion: the stream s has been sucessfully opened for reading.
*/
local int get_byte(s)
    gz_stream *s;
{
    if (s->z_eof) return EOF;
    if (s->stream.avail_in == 0) {
	errno = 0;
	s->stream.avail_in = s->reader(s->inbuf, 1, Z_BUFSIZE, s->file);
	if (s->stream.avail_in == 0) {
	    s->z_eof = 1;
	    if (s->file && ferror(s->file))
		s->z_err = Z_ERRNO;
	    return EOF;
	}
	s->stream.next_in = s->inbuf;
    }
    s->stream.avail_in--;
    return *(s->stream.next_in)++;
}

/* ===========================================================================
      Check the gzip header of a gz_stream opened for reading. Set the stream
    mode to transparent if the gzip magic header is not present; set s->err
    to Z_DATA_ERROR if the magic header is present but the rest of the header
    is incorrect.
    IN assertion: the stream s has already been created sucessfully;
       s->stream.avail_in is zero for the first time, but may be non-zero
       for concatenated .gz files.
*/
local void check_header(s)
    gz_stream *s;
{
    int method; /* method byte */
    int flags;  /* flags byte */
    uInt len;
    int c;

    /* Check the gzip magic header */
    for (len = 0; len < 2; len++) {
	c = get_byte(s);
	if (c != gz_magic[len]) {
	    if (len != 0) s->stream.avail_in++, s->stream.next_in--;
	    if (c != EOF) {
		s->stream.avail_in++, s->stream.next_in--;
		s->transparent = 1;
	    }
	    s->z_err = s->stream.avail_in != 0 ? Z_OK : Z_STREAM_END;
	    return;
	}
    }
    method = get_byte(s);
    flags = get_byte(s);
    if (method != Z_DEFLATED || (flags & RESERVED) != 0) {
	s->z_err = Z_DATA_ERROR;
	return;
    }

    /* Discard time, xflags and OS code: */
    for (len = 0; len < 6; len++) (void)get_byte(s);

    if ((flags & EXTRA_FIELD) != 0) { /* skip the extra field */
	len  =  (uInt)get_byte(s);
	len += ((uInt)get_byte(s))<<8;
	/* len is garbage if EOF but the loop below will quit anyway */
	while (len-- != 0 && get_byte(s) != EOF) ;
    }
    if ((flags & ORIG_NAME) != 0) { /* skip the original file name */
	while ((c = get_byte(s)) != 0 && c != EOF) ;
    }
    if ((flags & COMMENT) != 0) {   /* skip the .gz file comment */
	while ((c = get_byte(s)) != 0 && c != EOF) ;
    }
    if ((flags & HEAD_CRC) != 0) {  /* skip the header crc */
	for (len = 0; len < 2; len++) (void)get_byte(s);
    }
    s->z_err = s->z_eof ? Z_DATA_ERROR : Z_OK;
}

 /* ===========================================================================
 * Cleanup then free the given gz_stream. Return a zlib error code.
   Try freeing in the reverse order of allocations.
 */
local int destroy (s)
    gz_stream *s;
{
    int err = Z_OK;

    if (!s) return Z_STREAM_ERROR;

    TRYFREE(s->msg);

    if (s->stream.state != NULL) {
       if (s->mode == 'w') {
           err = deflateEnd(&(s->stream));
       } else if (s->mode == 'r') {
           err = inflateEnd(&(s->stream));
       }
    }
    if (s->file != NULL && fclose(s->file)) {
        err = Z_ERRNO;
    }
    if (s->z_err < 0) err = s->z_err;

    TRYFREE(s->inbuf);
    TRYFREE(s->outbuf);
    TRYFREE(s->path);
    TRYFREE(s);
    return err;
}

/* ===========================================================================
     Reads the given number of uncompressed bytes from the compressed file.
   gzread returns the number of bytes actually read (0 for end of file).
*/
int
erts_gzread(gzFile file, voidp buf, unsigned len)
{
    gz_stream *s = (gz_stream*)file;
    Bytef *start = buf; /* starting point for crc computation */
    Byte  *next_out; /* == stream.next_out but not forced far (for MSDOS) */

    if (s == NULL || s->mode != 'r') return Z_STREAM_ERROR;

    if (s->z_err == Z_DATA_ERROR || s->z_err == Z_ERRNO) return -1;
    if (s->z_err == Z_STREAM_END) return 0;  /* EOF */

    s->stream.next_out = next_out = buf;
    s->stream.avail_out = len;

    while (s->stream.avail_out != 0) {

	if (s->transparent) {
	    /* Copy first the lookahead bytes: */
	    uInt n = s->stream.avail_in;
	    if (n > s->stream.avail_out) n = s->stream.avail_out;
	    if (n > 0) {
		zmemcpy(s->stream.next_out, s->stream.next_in, n);
		next_out += n;
		s->stream.next_out = next_out;
		s->stream.next_in   += n;
		s->stream.avail_out -= n;
		s->stream.avail_in  -= n;
	    }
	    if (s->stream.avail_out > 0) {
		s->stream.avail_out -= s->reader(next_out, 1, s->stream.avail_out,
						 s->file);
	    }
	    len -= s->stream.avail_out;
	    s->stream.total_in  += (uLong)len;
	    s->stream.total_out += (uLong)len;
            if (len == 0) s->z_eof = 1;
	    s->position += (int)len;
	    return (int)len;
	}
        if (s->stream.avail_in == 0 && !s->z_eof) {
            errno = 0;
            s->stream.avail_in = s->reader(s->inbuf, 1, Z_BUFSIZE, s->file);
            if (s->stream.avail_in == 0) {
                s->z_eof = 1;
		if (s->file && ferror(s->file)) {
		    s->z_err = Z_ERRNO;
		    break;
		}
            }
            s->stream.next_in = s->inbuf;
        }
        s->z_err = inflate(&(s->stream), Z_NO_FLUSH);

	if (s->z_err == Z_STREAM_END) {
	    /* Check CRC and original size */
	    s->crc = crc32(s->crc, start, (uInt)(s->stream.next_out - start));
	    start = s->stream.next_out;

	    if (getLong(s) != s->crc) {
		s->z_err = Z_DATA_ERROR;
	    } else {
	        (void)getLong(s);
                /* The uncompressed length returned by above getlong() may
                 * be different from s->stream.total_out) in case of
		 * concatenated .gz files. Check for such files:
		 */
		check_header(s);
		if (s->z_err == Z_OK) {
		    uLong total_in = s->stream.total_in;
		    uLong total_out = s->stream.total_out;

		    inflateReset(&(s->stream));
		    s->stream.total_in = total_in;
		    s->stream.total_out = total_out;
		    s->crc = crc32(0L, Z_NULL, 0);
		}
	    }
	}
	if (s->z_err != Z_OK || s->z_eof) break;
    }
    s->crc = crc32(s->crc, start, (uInt)(s->stream.next_out - start));

    s->position += (int)(len - s->stream.avail_out);

    return (int)(len - s->stream.avail_out);
}

/* ===========================================================================
     Writes the given number of uncompressed bytes into the compressed file.
   gzwrite returns the number of bytes actually written (0 in case of error).
*/
int
erts_gzwrite(gzFile file, voidpc buf, unsigned len)
{
    gz_stream *s = (gz_stream*)file;

    if (s == NULL || s->mode != 'w') return Z_STREAM_ERROR;

    s->stream.next_in = buf;
    s->stream.avail_in = len;

    while (s->stream.avail_in != 0) {

        if (s->stream.avail_out == 0) {

            s->stream.next_out = s->outbuf;
            if (fwrite(s->outbuf, 1, Z_BUFSIZE, s->file) != Z_BUFSIZE) {
                s->z_err = Z_ERRNO;
                break;
            }
            s->stream.avail_out = Z_BUFSIZE;
        }
        s->z_err = deflate(&(s->stream), Z_NO_FLUSH);
        if (s->z_err != Z_OK) break;
    }
    s->crc = crc32(s->crc, buf, len);

    s->position += (int)(len - s->stream.avail_in);
    return (int)(len - s->stream.avail_in);
}

/*
 * For use by Erlang file driver.
 *
 * XXX Limitations:
 *  - SEEK_END is not allowed (length of file is not known).
 *  - When writing, only forward seek is supported.
 */

int
erts_gzseek(gzFile file, int offset, int whence)
{
    int pos;
    gz_stream* s = (gz_stream *) file;

    if (s == NULL) {
	errno = EINVAL;
	return -1;
    }
    if (s->z_err == Z_DATA_ERROR || s->z_err == Z_ERRNO) {
	errno = EIO;
	return -1;
    }

    switch (whence) {
    case SEEK_SET: pos = offset; break;
    case SEEK_CUR: pos = s->position+offset; break;
    case SEEK_END: 
    default:
      errno = EINVAL; return -1;
    }

    if (pos == s->position) {
	return pos;
    }

    if (pos < s->position) {
	if (s->mode == 'w') {
	    errno = EINVAL;
	    return -1;
	}
	gz_rewind(s);
    }

    while (s->position < pos) {
	char buf[512];
	int n;

	n = pos - s->position;
	if (n > sizeof(buf))
	    n = sizeof(buf);

	if (s->mode == 'r') {
	    erts_gzread(file, buf, n);
	} else {
	    memset(buf, '\0', n);
	    erts_gzwrite(file, buf, n);
	}
    }

    return s->position;
}

/* ===========================================================================
     Flushes all pending output into the compressed file. The parameter
   flush is as in the deflate() function.
     gzflush should be called only when strictly necessary because it can
   degrade compression.
*/
int
erts_gzflush(gzFile file, int flush)
{
    uInt len;
    int done = 0;
    gz_stream *s = (gz_stream*)file;

    if (s == NULL || s->mode != 'w') return Z_STREAM_ERROR;

    s->stream.avail_in = 0; /* should be zero already anyway */

    for (;;) {
        len = Z_BUFSIZE - s->stream.avail_out;

        if (len != 0) {
            if ((uInt)fwrite(s->outbuf, 1, len, s->file) != len) {
                s->z_err = Z_ERRNO;
                return Z_ERRNO;
            }
            s->stream.next_out = s->outbuf;
            s->stream.avail_out = Z_BUFSIZE;
        }
        if (done) break;
        s->z_err = deflate(&(s->stream), flush);

        /* deflate has finished flushing only when it hasn't used up
         * all the available space in the output buffer: 
         */
        done = (s->stream.avail_out != 0 || s->z_err == Z_STREAM_END);
 
        if (s->z_err != Z_OK && s->z_err != Z_STREAM_END) break;
    }
    fflush(s->file);
    return  s->z_err == Z_STREAM_END ? Z_OK : s->z_err;
}

/* ===========================================================================
   Outputs a long in LSB order to the given file
*/
local void putLong (file, x)
    FILE *file;
    uLong x;
{
    int n;
    for (n = 0; n < 4; n++) {
        fputc((int)(x & 0xff), file);
        x >>= 8;
    }
}

/* ===========================================================================
   Reads a long in LSB order from the given gz_stream. Sets 
*/
local uLong getLong (s)
    gz_stream *s;
{
    uLong x = (uLong)get_byte(s);
    int c;

    x += ((uLong)get_byte(s))<<8;
    x += ((uLong)get_byte(s))<<16;
    c = get_byte(s);
    if (c == EOF) s->z_err = Z_DATA_ERROR;
    x += ((uLong)c)<<24;
    return x;
}

/* ===========================================================================
     Flushes all pending output if necessary, closes the compressed file
   and deallocates all the (de)compression state.
*/
int
erts_gzclose(gzFile file)
{
    int err;
    gz_stream *s = (gz_stream*)file;

    if (s == NULL) return Z_STREAM_ERROR;

    if (s->mode == 'w') {
        err = erts_gzflush (file, Z_FINISH);
        if (err != Z_OK) return s->destroy(file);

        putLong (s->file, s->crc);
        putLong (s->file, s->stream.total_in);

    }
    return s->destroy(file);
}


/* ===========================================================================
   Uncompresses the buffer given and returns a pointer to a binary.
   If the buffer was not compressed with gzip, the buffer contents
   will be copied unchanged into the binary.

   If a `gzip' header was found, but there were subsequent errors,
   a NULL pointer is returned.
*/

ErlDrvBinary*
erts_gzinflate_buffer(char* start, int size)
{
    ErlDrvBinary* bin;
    ErlDrvBinary* bin2;
    gzFile fd;
    int bytes_read = 0;

    if ((fd = erts_gzbufopen(start, size)) == NULL)
	return NULL;
    size *= 2;
    if ((bin = driver_alloc_binary(size)) == NULL) {
	return NULL;
    }

    for (;;) {
	int n = erts_gzread(fd, bin->orig_bytes + bytes_read, size-bytes_read);
	if (n == 0) {
	    erts_gzclose(fd);
	    if ((bin2 = driver_realloc_binary(bin, bytes_read)) == NULL) {
		driver_free_binary(bin);
	    }
	    return bin2;
	} else if (n == -1) {
	    driver_free_binary(bin);
	    erts_gzclose(fd);
	    return NULL;
	}
	bytes_read += n;
	size *= 2;
	if ((bin2 = driver_realloc_binary(bin, size)) == NULL) {
	    driver_free_binary(bin);
	    erts_gzclose(fd);
	    return NULL;
	}
	bin = bin2;
    }
}

/* ===========================================================================
   Compresses the buffer given and returns a pointer to a binary.
   A NULL pointer is returned if any error occurs.
   Writes a gzip header as well.
*/

#define GZIP_HD_SIZE 10
#define GZIP_TL_SIZE 8

#define GZIP_X_SIZE (GZIP_HD_SIZE+GZIP_TL_SIZE)

ErlDrvBinary*
erts_gzdeflate_buffer(char* start, int size)
{
    z_stream c_stream; /* compression stream */
    ErlDrvBinary* bin;
    ErlDrvBinary* bin2;
    uLong    crc;     /* crc32 of uncompressed data */
    uLong    szIn;
    Byte* ptr;
    int comprLen = size + (size/1000) + 1 + 12; /* see zlib.h */

    crc = crc32(0L, Z_NULL, 0);
    c_stream.zalloc = (alloc_func)0;
    c_stream.zfree = (free_func)0;
    c_stream.opaque = (voidpf)0;

    if (deflateInit2(&c_stream, Z_DEFAULT_COMPRESSION,
		     Z_DEFLATED, -MAX_WBITS, DEF_MEM_LEVEL, 0) != Z_OK)
	return NULL;

    if ((bin = driver_alloc_binary(comprLen+GZIP_X_SIZE)) == NULL)
	return NULL;
    sprintf(bin->orig_bytes, "%c%c%c%c%c%c%c%c%c%c", gz_magic[0], gz_magic[1],
	    Z_DEFLATED, 0 /*flags*/, 0,0,0,0 /*time*/, 0 /*xflags*/, OS_CODE);

    c_stream.next_out = ((Byte*) bin->orig_bytes)+GZIP_HD_SIZE;
    c_stream.avail_out = (uInt) bin->orig_size - GZIP_HD_SIZE;
    c_stream.next_in  = (Byte*) start;
    c_stream.avail_in = (uInt) size;

    if (deflate(&c_stream, Z_FINISH) != Z_STREAM_END) {
	driver_free_binary(bin);
	return NULL;	
    }
    crc = crc32(crc, (unsigned char*)start, size);
    ptr = c_stream.next_out;
    szIn = c_stream.total_in;

    *ptr++ = (crc & 0xff); crc >>= 8;
    *ptr++ = (crc & 0xff); crc >>= 8;
    *ptr++ = (crc & 0xff); crc >>= 8;
    *ptr++ = (crc & 0xff); crc >>= 8;

    *ptr++ = (szIn & 0xff); szIn >>= 8;
    *ptr++ = (szIn & 0xff); szIn >>= 8;
    *ptr++ = (szIn & 0xff); szIn >>= 8;
    *ptr++ = (szIn & 0xff); szIn >>= 8;

    if (deflateEnd(&c_stream) != Z_OK) {
	driver_free_binary(bin);
	return NULL;	
    }	
    size = ptr - (Byte*)bin->orig_bytes;

    if ((bin2 = driver_realloc_binary(bin, size)) == NULL)
	driver_free_binary(bin);
    return bin2;
}

/* ===========================================================================
   Like gzopen(), but opens a RAM file on the given the buffer.
*/

gzFile
erts_gzbufopen(bytes, size)
    char* bytes;		/* Start of buffer to read from. */
    int size;			/* Size of buffer. */
{
    int err;
    gz_stream *s;

    s = (gz_stream *)ALLOC(sizeof(gz_stream));
    if (!s)
	return Z_NULL;

    s->stream.zalloc = (alloc_func)0;
    s->stream.zfree = (free_func)0;
    s->stream.opaque = (voidpf)0;
    s->stream.next_in = s->inbuf = (unsigned char*)bytes;
    s->stream.next_out = s->outbuf = Z_NULL;
    s->stream.avail_in = size;
    s->stream.avail_out = 0;
    s->file = NULL;
    s->z_err = Z_OK;
    s->z_eof = 0;
    s->crc = crc32(0L, Z_NULL, 0);
    s->msg = NULL;
    s->transparent = 0;
    s->position = 0;

    s->path = (char*)ALLOC(1);
    s->path[0] = '\0';
    s->reader = mem_reader_dummy;
    s->destroy = mem_destroy;
    s->mode = 'r';
    
    err = inflateInit2(&(s->stream), -MAX_WBITS);
    if (err != Z_OK || s->inbuf == Z_NULL) {
	return s->destroy(s), (gzFile)Z_NULL;
    }
    s->stream.avail_out = Z_BUFSIZE;

    errno = 0;
    s->file = NULL;
    check_header(s);

    return (gzFile)s;
}

local int mem_destroy(s)
    gz_stream* s;
{
    int err = Z_OK;

    if (!s) return Z_STREAM_ERROR;

    TRYFREE(s->msg);

    if (s->stream.state != NULL) {
       if (s->mode == 'w') {
           err = deflateEnd(&(s->stream));
       } else if (s->mode == 'r') {
           err = inflateEnd(&(s->stream));
       }
    }
    if (s->z_err < 0)
	err = s->z_err;

    TRYFREE(s->path);
    TRYFREE(s->outbuf);
    TRYFREE(s);
    return err;
}

local unsigned mem_reader_dummy(buf, size, items, fp)
    void* buf;
    unsigned size;
    unsigned items;
    FILE* fp;
{
    return 0;			/* Always end of file. */
}

