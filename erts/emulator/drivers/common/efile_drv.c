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
#define FILE_CLOSE		23

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

extern int erts_async_max_threads;

#if 0
/* Experimental, for forcing all file operations to use the same thread. */
int key = 1;
#define KEY1 (&key)
#else
#define KEY NULL
#endif

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

static void file_async_ready(file_descriptor *desc, void *data);

struct driver_entry efile_driver_entry = {
    file_init,
    file_start,
    file_stop,
    file_erlang_read,
    null_func,
    null_func,
    "efile",
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    file_async_ready
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

struct t_data
{
    Efile_error errInfo;
    Efile_info info;
    EFILE_DIR_HANDLE dir_handle; /* Handle to open directory. */
    int result_ok;
    DriverBinary *bin;
    char *buf;
    unsigned location;
    int drive;
    int flags;
    int fd;
    int n;
    int offset;
    int command;
    unsigned bytesRead;		/* Bytes read from the file. */
    char b[1];
};

static void free_data(void *data)
{
    sys_free(data);
}

static void invoke_close(void *data)
{
    struct t_data *d = (struct t_data *) data;
    int fd = d->fd;

    efile_closefile(fd);
}

static int file_stop(desc)
file_descriptor* desc;
{
    int fd = desc->fd;

    if (desc->flags & EFILE_COMPRESSED) {
	gzclose((gzFile)fd);
    } else if (fd >= 0) {
#if 0
	efile_closefile(fd);
#else
/* Threaded close */
    {
	struct t_data *d = sys_alloc(sizeof(struct t_data));
	d->fd = fd;
	d->command = FILE_CLOSE;
	driver_async(desc->port, KEY, invoke_close, (void *) d,
		     free_data);
	return 0;
    }
#endif
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
 
static void invoke_name(void *data, int (*f)(Efile_error *, char *))
{
    struct t_data *d = (struct t_data *) data;
    char *name = (char *) &d->b;

    d->result_ok = (*f)(&d->errInfo, name);
}

static void invoke_mkdir(void *data)
{
    invoke_name(data, efile_mkdir);
}

static void invoke_rmdir(void *data)
{
    invoke_name(data, efile_rmdir);
}

static void invoke_delete_file(void *data)
{
    invoke_name(data, efile_delete_file);
}

static void invoke_chdir(void *data)
{
    invoke_name(data, efile_chdir);
}

static void invoke_fsync(void *data)
{
    struct t_data *d = (struct t_data *) data;
    int fd = d->fd;

    d->result_ok = efile_fsync(&d->errInfo, fd);
}

static void invoke_truncate(void *data)
{
    struct t_data *d = (struct t_data *) data;
    int fd = d->fd;

    d->result_ok = efile_truncate_file(&d->errInfo, &fd, d->flags);
}

static void invoke_read(void *data)
{
    struct t_data *d = (struct t_data *) data;
    int status;

    if (d->flags & EFILE_COMPRESSED) {
	d->bytesRead = gzread((gzFile)d->fd, d->bin->orig_bytes, d->n);
	status = (d->bytesRead != -1);
	if (!status) {
	    d->errInfo.posix_errno = EIO;
	}
    } else {
	status = efile_read(&d->errInfo, d->flags, d->fd,
			    d->bin->orig_bytes, d->n, &d->bytesRead);
    }

    d->result_ok = status;
}

static void invoke_read_file(void *data)
{
    struct t_data *d = (struct t_data *) data;

    unsigned size;		/* Size of file. */
    int fd;

    d->result_ok = 0;
    d->bin = NULL;

    if (!efile_openfile(&d->errInfo, d->b, EFILE_MODE_READ, &fd, &size))
	return;

    if ((d->bin = driver_alloc_binary(size)) == NULL) {
	d->errInfo.posix_errno = ENOMEM;
	d->errInfo.os_errno = 0;
    } else if (!efile_read(&d->errInfo, EFILE_MODE_READ, fd, d->bin->orig_bytes,
			   size, &d->bytesRead)) {
	/* Nothing to do here. */
    } else if (d->bytesRead != size) {
	d->errInfo.posix_errno = EIO;
	d->errInfo.os_errno = 0;
    } else {
	d->result_ok = 1;
    }

    efile_closefile(fd);
}

static void invoke_pread(void *data)
{
    struct t_data *d = (struct t_data *) data;

    d->result_ok = efile_pread(&d->errInfo, d->fd, d->offset,
			       d->bin->orig_bytes, 
			       d->n, &d->bytesRead);
}

static void invoke_write(void *data)
{
    struct t_data *d = (struct t_data *) data;

    int status;			/* Status of write operation. */

    if (d->flags & EFILE_COMPRESSED) {
	status = gzwrite((gzFile)d->fd, (char *) d->b, d->n) == d->n;
    } else {
	status = efile_write(&d->errInfo, d->flags, d->fd,
			     (char *) d->b, d->n);
    }

    d->result_ok = status;
}

static void invoke_pwd(void *data)
{
    struct t_data *d = (struct t_data *) data;

    d->result_ok = efile_getdcwd(&d->errInfo,d->drive,(char *)&d->b+1,
				 RESBUFSIZE-1);
}

static void invoke_readlink(void *data)
{
    struct t_data *d = (struct t_data *) data;
    uchar resbuf[RESBUFSIZE];	/* Result buffer. */

    d->result_ok = efile_readlink(&d->errInfo, d->b, (char *) resbuf+1,
				  RESBUFSIZE-1);
    if (d->result_ok != 0)
	strcpy((char *) d->b + 1, resbuf+1);
}

static void invoke_pwrite(void *data)
{
    struct t_data *d = (struct t_data *) data;

    d->result_ok = efile_pwrite(&d->errInfo, d->fd, (char *) d->b,
				d->n, d->offset);
}

static void invoke_flstat(void *data)
{
    struct t_data *d = (struct t_data *) data;

    d->result_ok = efile_fileinfo(&d->errInfo, &d->info,
				  (char *) &d->b, d->command == FILE_LSTAT);
}

static void invoke_link(void *data)
{
    struct t_data *d = (struct t_data *) data;
    char *name = d->b;
    char *new_name;

    new_name = name+strlen(name)+1;
    d->result_ok = efile_link(&d->errInfo, name, new_name);
}

static void invoke_symlink(void *data)
{
    struct t_data *d = (struct t_data *) data;
    char *name = d->b;
    char *new_name;

    new_name = name+strlen(name)+1;
    d->result_ok = efile_symlink(&d->errInfo, name, new_name);
}

static void invoke_rename(void *data)
{
    struct t_data *d = (struct t_data *) data;
    char *name = d->b;
    char *new_name;

    new_name = name+strlen(name)+1;
    d->result_ok = efile_rename(&d->errInfo, name, new_name);
}

static void invoke_write_info(void *data)
{
    struct t_data *d = (struct t_data *) data;

    d->result_ok = efile_write_info(&d->errInfo, &d->info, d->b);
}

static void invoke_lseek(void *data)
{
    struct t_data *d = (struct t_data *) data;
    int status;

    if (d->flags & EFILE_COMPRESSED) {
	status = 1;
	d->location = gzseekk((gzFile)d->fd, d->offset, d->n);
	if (d->location == -1) {
	    d->errInfo.posix_errno = errno;
	    status = 0;
	}
    } else {
	status = efile_seek(&d->errInfo, d->fd, d->offset, d->n,
			    &d->location);
    }
    d->result_ok = status;
}

static void invoke_readdir(void *data)
{
    struct t_data *d = (struct t_data *) data;
    int s;
    uchar resbuf[RESBUFSIZE];	/* Result buffer. */

    resbuf[0] = FILE_RESP_OK;
    d->errInfo.posix_errno = 0;

    s = efile_readdir(&d->errInfo, d->b, &d->dir_handle,
		      (char *) resbuf+1, RESBUFSIZE-1);
    if (s) {
	d->n = 1 + strlen((char*) resbuf+1);
	d->result_ok = 1;
    }
    else {
	d->n = 1;
	d->result_ok = (d->errInfo.posix_errno == 0);
    }
    if (d->buf != NULL)
	sys_free(d->buf);
    d->buf = sys_alloc(d->n);
    memcpy(d->buf, resbuf, d->n);
}

static void invoke_open(void *data)
{
    struct t_data *d = (struct t_data *) data;

    unsigned size;		/* Size of file (not used). */
    int status = 1;		/* Status of open call. */

    if ((d->flags & EFILE_COMPRESSED) == 0) {
	status = efile_openfile(&d->errInfo, &d->b, d->flags, &d->fd, &size);
    } else {
	char* mode = NULL;

	if ((d->flags & (EFILE_MODE_READ|EFILE_MODE_WRITE)) ==
	    (EFILE_MODE_READ|EFILE_MODE_WRITE)) {
	    status = 0;
	    d->errInfo.posix_errno = EINVAL;
	} else {
	    mode = (d->flags & EFILE_MODE_READ) ? "rb" : "wb";
	    d->fd = (int) gzopen(&d->b, mode);
	    if ((gzFile)d->fd == NULL) {
		if (errno == 0) {
		    errno = ENOMEM;
		}
		d->errInfo.posix_errno = errno;
		status = 0;
	    }
	}
    }

    d->result_ok = status;
}

static void free_read(void *data)
{
    struct t_data *d = (struct t_data *) data;

    driver_free_binary(d->bin);
    sys_free(d);
}

static void free_readdir(void *data)
{
    struct t_data *d = (struct t_data *) data;

    if (d->buf != NULL)
	sys_free(d->buf);
    sys_free(d);
}

static void file_async_ready(file_descriptor *desc, void *data)
{
    struct t_data *d = (struct t_data *) data;
    char header[5];		/* result code + count */
    uchar resbuf[RESBUFSIZE];	/* Result buffer. */

    switch (d->command)
    {
      case FILE_READ:
      case FILE_PREAD:
	if (!d->result_ok) {
	    error_reply(desc, &d->errInfo);
	} else {
	    header[0] = FILE_RESP_DATA;
	    put_int32(d->bytesRead, header+1);
	    driver_output_binary(desc->port, header, sizeof(header),
				 d->bin, 0, d->bytesRead);
	}
	free_read(data);
	break;
      case FILE_READ_FILE:
	if (!d->result_ok)
	    error_reply(desc, &d->errInfo);
	else {
	    header[0] = FILE_RESP_OK;
	    driver_output_binary(desc->port, header, 1, 
				 d->bin, 0, d->bytesRead);
	}
	if (d->bin != NULL) {
	    driver_free_binary(d->bin);
	}
	free_data(data);
	break;
      case FILE_PWRITE:
      case FILE_WRITE:
	if (!d->result_ok) {
	    error_reply(desc, &d->errInfo);
	} else {
	    numeric_reply(desc, d->n);
	}
	free_data(data);
	break;
      case FILE_LSEEK:
	if (d->result_ok)
	    numeric_reply(desc, d->location);
	else
	    error_reply(desc, &d->errInfo);
	free_data(data);
	break;
      case FILE_MKDIR:
      case FILE_RMDIR:
      case FILE_CHDIR:
      case FILE_DELETE:
      case FILE_FSYNC:
      case FILE_TRUNCATE:
      case FILE_LINK:
      case FILE_SYMLINK:
      case FILE_RENAME:
      case FILE_WRITE_INFO:
	reply(desc, d->result_ok, &d->errInfo);
	free_data(data);
	break;
      case FILE_PWD:
      case FILE_READLINK:
        {
	    int length;
	    char *resbuf = &d->b;

	    if (!d->result_ok)
		error_reply(desc, &d->errInfo);
	    else {
		resbuf[0] = FILE_RESP_OK;
		length = 1+strlen((char*) resbuf+1);
		driver_output2(desc->port, resbuf, length, NULL, 0);
	    }
	    free_data(data);
	    break;
	}
      case FILE_OPEN:
	if (!d->result_ok) {
	    error_reply(desc, &d->errInfo);
	} else {
	    desc->fd = d->fd;
	    desc->flags = d->flags;
	    numeric_reply(desc, d->fd);
	}
	free_data(data);
	break;
      case FILE_FSTAT:
      case FILE_LSTAT:
        {
	    if (d->result_ok) {
		resbuf[0] = FILE_RESP_INFO;

		put_int32(d->info.size_high,         &resbuf[1 + (0 * 4)]);
		put_int32(d->info.size_low,          &resbuf[1 + (1 * 4)]);
		put_int32(d->info.type,              &resbuf[1 + (2 * 4)]);

		PUT_TIME(d->info.accessTime, resbuf + 1 + 3*4);
		PUT_TIME(d->info.modifyTime, resbuf + 1 + 9*4);
		PUT_TIME(d->info.cTime, resbuf + 1 + 15*4);

		put_int32(d->info.mode,              &resbuf[1 + (21 * 4)]);
		put_int32(d->info.links,             &resbuf[1 + (22 * 4)]);
		put_int32(d->info.major_device,      &resbuf[1 + (23 * 4)]);
		put_int32(d->info.minor_device,      &resbuf[1 + (24 * 4)]);
		put_int32(d->info.inode,             &resbuf[1 + (25 * 4)]);
		put_int32(d->info.uid,               &resbuf[1 + (26 * 4)]);
		put_int32(d->info.gid,               &resbuf[1 + (27 * 4)]);
		put_int32(d->info.access,            &resbuf[1 + (28 * 4)]);

#define RESULT_SIZE (1 + (29 * 4))
		driver_output2(desc->port, resbuf, RESULT_SIZE, NULL, 0);
#undef RESULT_SIZE
	    } else
		error_reply(desc, &d->errInfo);
	}
	free_data(data);
	break;
      case FILE_READDIR:
	if (!d->result_ok)
	    error_reply(desc, &d->errInfo);
	else {
	    driver_output2(desc->port, d->buf, d->n, NULL, 0);
	}
	if (d->n == 1)
	    free_readdir(data);
	else
	    driver_async(desc->port, KEY, invoke_readdir, (void *) d,
			 free_readdir);
	break;
	/* See file_stop */
      case FILE_CLOSE:
	free_data(data);
	break;
      default:
	abort();
    }
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
    char* name;			/* Points to the filename in buf. */
    int command;

    fd  = desc->fd;
    name = (char *) buf+1;
    command = *buf++;

    switch(command) {

    case FILE_MKDIR:
    {
	struct t_data *d = sys_alloc(sizeof(struct t_data) - 1
				     + strlen(name) + 1);
	
	strcpy((char *) &d->b, name);
	d->command = command;
	driver_async(desc->port, KEY, invoke_mkdir, (void *) d,
		     free_data);
	return 0;
    }
    case FILE_RMDIR:
    {
	struct t_data *d = sys_alloc(sizeof(struct t_data) - 1
				     + strlen(name) + 1);
	
	strcpy((char *) &d->b, name);
	d->command = command;
	driver_async(desc->port, KEY, invoke_rmdir, (void *) d,
		     free_data);
	return 0;
    }
    case FILE_DELETE:
    {
	struct t_data *d = sys_alloc(sizeof(struct t_data) - 1
				     + strlen(name) + 1);
	
	strcpy((char *) &d->b, name);
	d->command = command;
	driver_async(desc->port, KEY, invoke_delete_file, (void *) d,
		     free_data);
	return 0;
    }
    case FILE_RENAME:
	{
	    struct t_data *d;
	    char* new_name;

	    new_name = name+strlen(name)+1;
	    d = sys_alloc(sizeof(struct t_data) - 1
			  + strlen(name) + 1
			  + strlen(new_name) + 1);
	
	    strcpy((char *) &d->b, name);
	    strcpy((char *) &d->b + strlen(name) + 1, new_name);
	    d->flags = desc->flags;
	    d->fd = fd;
	    d->command = command;
	    driver_async(desc->port, KEY, invoke_rename, (void *) d,
			 free_data);
	    return 0;
	}
    case FILE_CHDIR:
    {
	struct t_data *d = sys_alloc(sizeof(struct t_data) - 1
				     + strlen(name) + 1);
	
	strcpy((char *) &d->b, name);
	d->command = command;
	driver_async(desc->port, KEY, invoke_chdir, (void *) d,
		     free_data);
	return 0;
    }
    case FILE_PWD:
        {
	    struct t_data *d = sys_alloc(sizeof(struct t_data) - 1
					 + RESBUFSIZE + 1);
	
	    d->drive = buf[0];
	    d->command = command;
	    driver_async(desc->port, KEY, invoke_pwd, (void *) d,
			 free_data);
	    return 0;
	}

    case FILE_READDIR: 
#ifdef USE_THREADS
	if (erts_async_max_threads > 0)
	{
	    struct t_data *d = sys_alloc(sizeof(struct t_data) - 1
					 + strlen(name) + 1);
	
	    strcpy((char *) &d->b, name);
	    d->dir_handle = NULL;
	    d->buf = NULL;
	    d->command = command;
	    driver_async(desc->port, KEY, invoke_readdir, (void *) d,
			 free_readdir);
	    return 0;
	}
	else   
#endif
	{
	    uchar resbuf[RESBUFSIZE];
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
	    struct t_data *d = sys_alloc(sizeof(struct t_data) - 1
					 + strlen(buf+4) + 1);
	
	    d->flags = get_int32(buf);
	    name = (char *) buf+4;
	    strcpy((char *) &d->b, name);
	    d->command = command;
	    driver_async(desc->port, KEY, invoke_open, (void *) d,
			 free_data);
	    return 0;
	}

    case FILE_FSYNC:
    {
	struct t_data *d = sys_alloc(sizeof(struct t_data));
	
	d->fd = fd;
	d->command = command;
	driver_async(desc->port, KEY, invoke_fsync, (void *) d,
		     free_data);
	return 0;
    }

    case FILE_FSTAT: 
    case FILE_LSTAT:
    {
	struct t_data *d = sys_alloc(sizeof(struct t_data) - 1
				     + strlen(name) + 1);
	
	strcpy((char *) &d->b, name);
	d->fd = fd;
	d->command = command;
	driver_async(desc->port, KEY, invoke_flstat, (void *) d,
		     free_data);
	return 0;
    }
    case FILE_PWRITE:
	{
	    int offset;		/* Offset for pwrite. */

	    struct t_data *d = sys_alloc(sizeof(struct t_data) - 1
					 + count-5);
	
	    offset = get_int32(buf);
	    d->offset = offset;
	    memcpy((char *) d->b, buf+4, count-5);
	    d->n = count-5;
	    d->fd = fd;
	    d->command = command;
	    driver_async(desc->port, KEY, invoke_pwrite, (void *) d,
			 free_data);
	    return 0;
	}

    case FILE_WRITE:
	{
	    struct t_data *d = sys_alloc(sizeof(struct t_data) - 1
					 +count-1);
	
	    count--;
	    memcpy(d->b, buf, count);
	    d->flags = desc->flags;
	    d->n = count;
	    d->fd = fd;
	    d->command = command;
	    driver_async(desc->port, KEY, invoke_write, (void *) d,
			 free_data);
	    return 0;
	}
	
    case FILE_LSEEK:
	{
	    int offset;		/* Offset for seek. */
	    int origin;		/* Origin of seek. */

	    struct t_data *d = sys_alloc(sizeof(struct t_data) - 1);
	
	    offset = get_int32(buf);
	    origin = get_int32(buf+4);

	    d->flags = desc->flags;
	    d->offset = offset;
	    d->n = origin;
	    d->fd = fd;
	    d->command = command;
	    driver_async(desc->port, KEY, invoke_lseek, (void *) d,
			 free_read);
	    return 0;
	}

    case FILE_PREAD:
	{
	    int offset;		/* Offset for seek. */
	    DriverBinary* bin;	/* The binary data. */

	    offset = get_int32(buf);
	    count = get_int32(buf+4);
	    if ((bin = driver_alloc_binary(count)) == NULL) {
		errInfo.posix_errno = ENOMEM;
		errInfo.os_errno = 0;
		return error_reply(desc, &errInfo);
	    }
	{
	    struct t_data *d = sys_alloc(sizeof(struct t_data) - 1);
	
	    d->flags = desc->flags;
	    d->bin = bin;
	    d->n = count;
	    d->offset = offset;
	    d->fd = fd;
	    d->command = command;
	    driver_async(desc->port, KEY, invoke_pread, (void *) d,
			 free_read);
	    return 0;
	}
	}	

    case FILE_READ:
	{
	    int n;		/* Number of bytes to read. */
	    DriverBinary* bin;  /* The binary data */

	    n = get_int32(buf);
	    if ((bin = driver_alloc_binary(n)) == NULL) {
		errInfo.posix_errno = ENOMEM;
		errInfo.os_errno = 0;
		return error_reply(desc, &errInfo);
	    }
	    
	{
	    struct t_data *d = sys_alloc(sizeof(struct t_data) - 1);
	
	    d->flags = desc->flags;
	    d->bin = bin;
	    d->n = n;
	    d->fd = fd;
	    d->command = command;
	    driver_async(desc->port, KEY, invoke_read, (void *) d,
			 free_read);
	    return 0;
	}
	}

    case FILE_TRUNCATE:
        {
	    struct t_data *d = sys_alloc(sizeof(struct t_data));
	
	    d->flags = desc->flags;
	    d->fd = fd;
	    d->command = command;
	    driver_async(desc->port, KEY, invoke_truncate, (void *) d,
			 free_data);
	    return 0;
	}
    case FILE_READ_FILE:
	{
	    struct t_data *d = sys_alloc(sizeof(struct t_data)
					 + strlen(name) + 1);
	
	    strcpy((char *) &d->b, name);
	    d->command = command;
	    driver_async(desc->port, KEY, invoke_read_file, (void *) d,
			 free_read);
	    return 0;
	}

    case FILE_WRITE_INFO:
	{
	    struct t_data *d = sys_alloc(sizeof(struct t_data) - 1
					 + strlen(buf+21*4) + 1);
	
	    d->info.mode = get_int32(buf + 0 * 4);
	    d->info.uid = get_int32(buf + 1 * 4);
	    d->info.gid = get_int32(buf + 2 * 4);
	    GET_TIME(d->info.accessTime, buf + 3 * 4);
	    GET_TIME(d->info.modifyTime, buf + 9 * 4);
	    GET_TIME(d->info.cTime, buf + 15 * 4);
	    strcpy((char *) &d->b, buf+21*4);
	    d->command = command;
	    driver_async(desc->port, KEY, invoke_write_info, (void *) d,
			 free_data);
	    return 0;
	}

    case FILE_READLINK:
	{
	    struct t_data *d = sys_alloc(sizeof(struct t_data) - 1
					 + RESBUFSIZE + 1);
	
	    strcpy((char *) &d->b, name);
	    d->command = command;
	    driver_async(desc->port, KEY, invoke_readlink, (void *) d,
			 free_data);
	    return 0;
	}

    case FILE_LINK:
	{
	    struct t_data *d;
	    char* new_name;

	    new_name = name+strlen(name)+1;
	    d = sys_alloc(sizeof(struct t_data) - 1
			  + strlen(name) + 1
			  + strlen(new_name) + 1);
	
	    strcpy((char *) &d->b, name);
	    strcpy((char *) &d->b + strlen(name) + 1, new_name);
	    d->flags = desc->flags;
	    d->fd = fd;
	    d->command = command;
	    driver_async(desc->port, KEY, invoke_link, (void *) d,
			 free_data);
	    return 0;
	}

    case FILE_SYMLINK:
	{
	    struct t_data *d;
	    char* new_name;

	    new_name = name+strlen(name)+1;
	    d = sys_alloc(sizeof(struct t_data) - 1
			  + strlen(name) + 1
			  + strlen(new_name) + 1);
	
	    strcpy((char *) &d->b, name);
	    strcpy((char *) &d->b + strlen(name) + 1, new_name);
	    d->flags = desc->flags;
	    d->fd = fd;
	    d->command = command;
	    driver_async(desc->port, KEY, invoke_symlink, (void *) d,
			 free_data);
	    return 0;
	}

    }

    /*
     * Ignore anything else -- let the caller hang.
     */
     
    return 0;
}
