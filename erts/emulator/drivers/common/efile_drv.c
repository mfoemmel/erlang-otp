/* ``The contents of this file are subject to the Erlang Public License,
 * Version 1.1, (the "License"); you may not use this file except in
 * compliance with the License. You should have received a copy of the
 * Erlang Public License along with this software. If not, it can be
 * retrieved via the world wide web at http://www.erlang.org/.
 * 
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
 * the License for the specific language governing rights and limitations
 * under the License.
 * 
 * The Initial Developer of the Original Code is Ericsson Utvecklings AB.
 * Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
 * AB. All Rights Reserved.''
 * 
 *     $Id$
 */
/*
 * Purpose: Provides file and directory operations.
 *
 * This file is generic, and does the work of decoding the commands
 * and encoding the responses.  System-specific functions are found in
 * the unix_efile.c and win_efile.c files.
 */

/* Operations */

#define FILE_OPEN		 1
#define FILE_READ		 2
#define FILE_LSEEK		 3
#define FILE_WRITE		 4
#define FILE_FSTAT		 5
#define FILE_PWD                 6
#define FILE_READDIR             7
#define FILE_CHDIR               8
#define FILE_FSYNC               9
#define FILE_MKDIR              10
#define FILE_DELETE             11
#define FILE_RENAME             12
#define FILE_RMDIR              13
#define FILE_TRUNCATE           14
#define FILE_READ_FILE          15
#define FILE_WRITE_INFO		16
#define FILE_PREAD              17
#define FILE_PWRITE             18
#define FILE_LSTAT            	19
#define FILE_READLINK        	20
#define FILE_LINK             	21
#define FILE_SYMLINK          	22


/* Return codes */

#define FILE_RESP_OK         0
#define FILE_RESP_ERROR      1
#define FILE_RESP_DATA       2
#define FILE_RESP_NUMBER     3
#define FILE_RESP_INFO       4

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif
#include "sys.h"
#include "driver.h"
#include "erl_efile.h"
#include "zlib.h"
#include "gzio.h"
#include <ctype.h>

#if     MAXPATHLEN >= BUFSIZ
#define    RESBUFSIZE  MAXPATHLEN+1
#else
#define    RESBUFSIZE  BUFSIZ
#endif

#define GET_TIME(i, b) \
    (i).year  = get_int32((b) + 0 * 4); \
    (i).month = get_int32((b) + 1 * 4); \
    (i).day   = get_int32((b) + 2 * 4); \
    (i).hour  = get_int32((b) + 3 * 4); \
    (i).minute = get_int32((b) + 4 * 4); \
    (i).second = get_int32((b) + 5 * 4)

#define PUT_TIME(i, b) \
  put_int32((i).year,  (b) + 0 * 4); \
  put_int32((i).month, (b) + 1 * 4); \
  put_int32((i).day,   (b) + 2 * 4); \
  put_int32((i).hour,  (b) + 3 * 4); \
  put_int32((i).minute,(b) + 4 * 4); \
  put_int32((i).second,(b) + 5 * 4)

typedef unsigned char uchar;

static long file_start();
static int file_init();
static int file_stop();
static int file_erlang_read();

typedef struct {
    int fd;
    unsigned int port;
    unsigned flags;		/* Original flags. */
} file_descriptor;


static FUNCTION(int, error_reply, (file_descriptor*, Efile_error* errInfo));


struct driver_entry efile_driver_entry = {
    file_init,
    file_start,
    file_stop,
    file_erlang_read,
    null_func,
    null_func,
    "efile"
};


static int file_init()
{
    return 0;
}

static long file_start(port, buf) 
int port;
uchar *buf; 
{
    file_descriptor* desc;

    if ((desc = (file_descriptor*)
	 sys_alloc_from(200, sizeof(file_descriptor))) == NULL)
	return -1;
    desc->fd = -1;
    desc->port = port;
    desc->flags = 0;
    return (long) desc;
}

static int file_stop(desc)
file_descriptor* desc;
{
    int fd = desc->fd;

    if (desc->flags & EFILE_COMPRESSED) {
	gzclose((gzFile)fd);
    } else if (fd >= 0) {
	efile_closefile(fd);
    }
    sys_free(desc);
    return 0;
}

/*
 * Sends back an error reply to Erlang.
 */

static int error_reply(desc, errInfo)
file_descriptor* desc;
Efile_error* errInfo;		/* The error codes. */
{
    char response[256];		/* Response buffer. */
    char* s;
    char* t;
    
    /*
     * Contents of buffer sent back:
     *
     * +-----------------------------------------+
     * | FILE_RESP_ERROR | Posix error id string |
     * +-----------------------------------------+
     */

    response[0] = FILE_RESP_ERROR;
    for (s = erl_errno_id(errInfo->posix_errno), t = response+1; *s; s++, t++)
	*t = tolower(*s);
    driver_output2(desc->port, response, t-response, NULL, 0);
    return 0;
}

static int reply(desc, ok, errInfo)
file_descriptor* desc;
int ok;
Efile_error* errInfo;
{
    if (!ok)
	error_reply(desc, errInfo);
    else {
	uchar c = FILE_RESP_OK;

        driver_output2(desc->port, &c, 1, NULL, 0);
    }
    return 0;
}

static int numeric_reply(desc, result)
file_descriptor* desc; 
int result;
{
    uchar tmp[5];

    /*
     * Contents of buffer sent back:
     *
     * +-----------------------------------------------+
     * | FILE_RESP_NUMBER | 32-bit number (big-endian) |
     * +-----------------------------------------------+
     */

    tmp[0] = FILE_RESP_NUMBER;
    put_int32(result, tmp+1);
    driver_output2(desc->port, tmp, sizeof(tmp), NULL, 0);
    return 0;
}
 
static int file_erlang_read(desc, buf, count)
file_descriptor* desc;
uchar *buf;
int count;
{
    Efile_error errInfo;	/* The error codes for the last operation. */
    int fd;			/* The file descriptor for this port, if any,
				 * -1 if none.
				 */
    uchar resbuf[RESBUFSIZE];	/* Result buffer. */
    char* name;			/* Points to the filename in buf. */
    int command;

    fd  = desc->fd;
    name = (char *) buf+1;
    command = *buf++;
    switch(command) {
    case FILE_MKDIR:
	return reply(desc, efile_mkdir(&errInfo, name), &errInfo);
    case FILE_RMDIR:
	return reply(desc, efile_rmdir(&errInfo, name), &errInfo);
    case FILE_DELETE:
	return reply(desc, efile_delete_file(&errInfo, name), &errInfo);
    case FILE_RENAME:
	{
	    char* new_name;	/* New name of file or directory. */

	    new_name = name+strlen(name)+1;
	    return reply(desc, efile_rename(&errInfo,name,new_name), &errInfo);
	}
    case FILE_CHDIR:
	return reply(desc, efile_chdir(&errInfo, name), &errInfo);

    case FILE_PWD:
	{
	    int length;
	    int drive;

	    drive = buf[0];
	    if (!efile_getdcwd(&errInfo,drive,(char *)resbuf+1,RESBUFSIZE-1))
		return error_reply(desc, &errInfo);
	    resbuf[0] = FILE_RESP_OK;
	    length = 1+strlen((char*) resbuf+1);
	    return driver_output2(desc->port, resbuf, length, NULL, 0);
	}

    case FILE_READDIR: 
	{
	    EFILE_DIR_HANDLE dir_handle; /* Handle to open directory. */

	    errInfo.posix_errno = 0;
	    dir_handle = NULL;
	    resbuf[0] = FILE_RESP_OK;

	    while (efile_readdir(&errInfo, name, &dir_handle,
				 (char *) resbuf+1, RESBUFSIZE-1)) {
		int length = 1 + strlen((char*) resbuf+1);
		driver_output2(desc->port, resbuf, length, NULL, 0);
	    }
	    if (errInfo.posix_errno != 0)
		return error_reply(desc, &errInfo);
	    return driver_output2(desc->port, resbuf, 1, NULL, 0);
	}

    case FILE_OPEN:
	{
	    unsigned flags;	/* Flags for opening file. */
	    unsigned size;	/* Size of file (not used). */
	    int status = 1;	/* Status of open call. */

	    flags = get_int32(buf);
	    name = (char *) buf+4;
	    if ((flags & EFILE_COMPRESSED) == 0) {
		status = efile_openfile(&errInfo, name, flags, &fd, &size);
	    } else {
		char* mode = NULL;

		if ((flags & (EFILE_MODE_READ|EFILE_MODE_WRITE)) ==
		    (EFILE_MODE_READ|EFILE_MODE_WRITE)) {
		    errInfo.posix_errno = EINVAL;
		    return error_reply(desc, &errInfo);
		}

		mode = (flags & EFILE_MODE_READ) ? "rb" : "wb";
		fd = (int) gzopen(name, mode);
		if ((gzFile)fd == NULL) {
		    if (errno == 0) {
			errno = ENOMEM;
		    }
		    errInfo.posix_errno = errno;
		    status = 0;
		}
	    }

	    if (!status) {
		return error_reply(desc, &errInfo);
	    } else {
		desc->flags = flags;
		desc->fd = fd;
		return numeric_reply(desc, fd);
	    }
	}

    case FILE_FSYNC:
	return reply(desc, efile_fsync(&errInfo, fd), &errInfo);

    case FILE_FSTAT: 
    case FILE_LSTAT:
    {
	Efile_info info;
	
	if (!efile_fileinfo(&errInfo, &info, (char *) buf, command == FILE_LSTAT)) {
	    return error_reply(desc, &errInfo);
	}

	resbuf[0] = FILE_RESP_INFO;

	put_int32(info.size_high,         &resbuf[1 + (0 * 4)]);
	put_int32(info.size_low,          &resbuf[1 + (1 * 4)]);
	put_int32(info.type,              &resbuf[1 + (2 * 4)]);

	PUT_TIME(info.accessTime, resbuf + 1 + 3*4);
	PUT_TIME(info.modifyTime, resbuf + 1 + 9*4);
	PUT_TIME(info.cTime, resbuf + 1 + 15*4);

	put_int32(info.mode,              &resbuf[1 + (21 * 4)]);
	put_int32(info.links,             &resbuf[1 + (22 * 4)]);
	put_int32(info.major_device,      &resbuf[1 + (23 * 4)]);
	put_int32(info.minor_device,      &resbuf[1 + (24 * 4)]);
	put_int32(info.inode,             &resbuf[1 + (25 * 4)]);
	put_int32(info.uid,               &resbuf[1 + (26 * 4)]);
	put_int32(info.gid,               &resbuf[1 + (27 * 4)]);
	put_int32(info.access,            &resbuf[1 + (28 * 4)]);


#define RESULT_SIZE (1 + (29 * 4))
	return driver_output2(desc->port, resbuf, RESULT_SIZE, NULL, 0);
#undef RESULT_SIZE
    }
    case FILE_PWRITE:
	{
	    int offset;		/* Offset for pwrite. */

	    offset = get_int32(buf);
	    if (efile_pwrite(&errInfo, fd, (char *) buf+4, count-5, offset))
		return numeric_reply(desc, count-5);
	    else
		return error_reply(desc, &errInfo);
	}

    case FILE_WRITE:
	{
	    int status;		/* Status of write operation. */

	    count--;
	    if (desc->flags & EFILE_COMPRESSED) {
		status = gzwrite((gzFile)fd, (char *) buf, count) == count;
	    } else {
		status = efile_write(&errInfo, desc->flags, fd, (char *) buf, count);
	    }
	    if (status) {
		return numeric_reply(desc, count);
	    } else {
		return error_reply(desc, &errInfo);
	    }
	}
	
    case FILE_LSEEK:
	{
	    int offset;		/* Offset for seek. */
	    int origin;		/* Origin of seek. */
	    unsigned location;	/* Resulting location. */
	    int status;		/* Status of seek operation. */

	    offset = get_int32(buf);
	    origin = get_int32(buf+4);
	    if (desc->flags & EFILE_COMPRESSED) {
		status = 1;
		location = gzseekk((gzFile)fd, offset, origin);
		if (location == -1) {
		    errInfo.posix_errno = errno;
		    status = 0;
		}
	    } else {
		status = efile_seek(&errInfo, fd, offset, origin, &location);
	    }
	    if (status)
		return numeric_reply(desc, location);
	    else
		return error_reply(desc, &errInfo);
	}

    case FILE_PREAD:
	{
	    int offset;		/* Offset for seek. */
	    unsigned bytesRead;	/* Bytes read from the file. */
	    DriverBinary* bin;	/* The binary data. */

	    offset = get_int32(buf);
	    count = get_int32(buf+4);
	    if ((bin = driver_alloc_binary(count)) == NULL) {
		errInfo.posix_errno = ENOMEM;
		errInfo.os_errno = 0;
		return error_reply(desc, &errInfo);
	    }
	    if (!efile_pread(&errInfo, fd, offset,  bin->orig_bytes, 
			     count, &bytesRead)) {
		error_reply(desc, &errInfo);
	    } else {
		char header[5];
		header[0] = FILE_RESP_DATA;
		put_int32(bytesRead, header+1);
		/* XXX consider driver_realloc_binary()? */
		driver_output_binary(desc->port, header, sizeof(header),
				     bin, 0, bytesRead);
	    }
	    driver_free_binary(bin);
	    return 0;
	}	

    case FILE_READ:
	{
	    int n;		/* Number of bytes to read. */
	    char header[5];     /* result code + count */
	    DriverBinary* bin;  /* The binary data */
	    unsigned bytesRead;	/* Bytes read from the file. */
	    int status;

	    n = get_int32(buf);
	    if ((bin = driver_alloc_binary(n)) == NULL) {
		errInfo.posix_errno = ENOMEM;
		errInfo.os_errno = 0;
		return error_reply(desc, &errInfo);
	    }
	    
	    if (desc->flags & EFILE_COMPRESSED) {
		bytesRead = gzread((gzFile)fd, bin->orig_bytes, n);
		status = (bytesRead != -1);
		if (!status) {
		    errInfo.posix_errno = EIO;
		}
	    } else {
		status = efile_read(&errInfo, desc->flags, fd,
				    bin->orig_bytes, n, &bytesRead);
	    }
	    if (!status) {
		error_reply(desc, &errInfo);
	    } else {
		header[0] = FILE_RESP_DATA;
		put_int32(bytesRead, header+1);
		driver_output_binary(desc->port, header, sizeof(header),
				     bin, 0, bytesRead);
	    }
	    driver_free_binary(bin);
	    return 0;
	}

    case FILE_TRUNCATE:
	return reply(desc, efile_truncate_file(&errInfo, &fd, desc->flags), 
		     &errInfo);

    case FILE_READ_FILE:
	{
	    DriverBinary* bin;  /* The binary data */
	    char header;        /* The result header */
	    unsigned size;	/* Size of file. */
	    unsigned bytesRead;	/* Bytes read from the file. */

	    if (!efile_openfile(&errInfo, name, EFILE_MODE_READ, &fd, &size)) {
		return error_reply(desc, &errInfo);
	    }

	    if ((bin = driver_alloc_binary(size)) == NULL) {
		errInfo.posix_errno = ENOMEM;
		errInfo.os_errno = 0;
		return error_reply(desc, &errInfo);
	    }

	    if (!efile_read(&errInfo, EFILE_MODE_READ, fd, bin->orig_bytes,
			    size, &bytesRead)) {
		error_reply(desc, &errInfo);
	    } else if (bytesRead != size) {
		errInfo.posix_errno = EIO;
		errInfo.os_errno = 0;
		error_reply(desc, &errInfo);
	    } else {
		header = FILE_RESP_OK;
		driver_output_binary(desc->port, &header, 1, 
				     bin, 0, bytesRead);
	    }

	    efile_closefile(fd);
	    driver_free_binary(bin);
	    return 0;
	}

    case FILE_WRITE_INFO:
	{
	    Efile_info info;
	
	    info.mode = get_int32(buf + 0 * 4);
	    info.uid = get_int32(buf + 1 * 4);
	    info.gid = get_int32(buf + 2 * 4);
	    GET_TIME(info.accessTime, buf + 3 * 4);
	    GET_TIME(info.modifyTime, buf + 9 * 4);
	    GET_TIME(info.cTime, buf + 15 * 4);
	    return reply(desc, efile_write_info(&errInfo, &info, buf+21*4),
			 &errInfo);
	}

    case FILE_READLINK:
	{
	    int length;

	    if (!efile_readlink(&errInfo, name, (char *) resbuf+1, RESBUFSIZE-1)) {
		return error_reply(desc, &errInfo);
	    }
	    resbuf[0] = FILE_RESP_OK;
	    length = 1+strlen((char*) resbuf+1);
	    return driver_output2(desc->port, resbuf, length, NULL, 0);
	}

    case FILE_LINK:
	{
	    char* new_name;

	    new_name = name+strlen(name)+1;
	    return reply(desc, efile_link(&errInfo, name, new_name), &errInfo);
	}

    case FILE_SYMLINK:
	{
	    char* new_name;

	    new_name = name+strlen(name)+1;
	    return reply(desc, efile_symlink(&errInfo, name, new_name), &errInfo);
	}

    }

    /*
     * Ignore anything else -- let the caller hang.
     */
     
    return 0;
}
