/* 
 * tclWinChan.c
 *
 *	Channel drivers for Windows channels based on files, command
 *	pipes and TCP sockets.
 *
 * Copyright (c) 1995-1996 Sun Microsystems, Inc.
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 * SCCS: @(#) tclWinChan.c 1.64 96/10/11 15:39:43
 */

#include "tclWinInt.h"

/*
 * Static routines for this file:
 */

static int		FileBlockModeProc _ANSI_ARGS_((
    			    ClientData instanceData, int mode));
static int		FileCloseProc _ANSI_ARGS_((ClientData instanceData,
		            Tcl_Interp *interp));
static int		FileSeekProc _ANSI_ARGS_((ClientData instanceData,
			    long offset, int mode, int *errorCode));
static int		FileInputProc _ANSI_ARGS_((ClientData instanceData,
	            	    char *buf, int toRead, int *errorCode));
static int		FileOutputProc _ANSI_ARGS_((ClientData instanceData,
			    char *buf, int toWrite, int *errorCode));
static int		FileType _ANSI_ARGS_((HANDLE h));
static void		FileWatchProc _ANSI_ARGS_((ClientData instanceData,
		            int mask));
static int		FileReadyProc _ANSI_ARGS_((ClientData instanceData,
		            int mask));
static Tcl_File		FileGetProc _ANSI_ARGS_((ClientData instanceData,
		            int direction));

static int		PipeBlockModeProc _ANSI_ARGS_((
    			    ClientData instanceData, int mode));
static int		PipeCloseProc _ANSI_ARGS_((ClientData instanceData,
	            	    Tcl_Interp *interp));
static int		PipeInputProc _ANSI_ARGS_((ClientData instanceData,
	            	    char *buf, int toRead, int *errorCode));
static int		PipeOutputProc _ANSI_ARGS_((ClientData instanceData,
			    char *buf, int toWrite, int *errorCode));
static void		PipeWatchProc _ANSI_ARGS_((ClientData instanceData,
		            int mask));
static int		PipeReadyProc _ANSI_ARGS_((ClientData instanceData,
		            int mask));
static Tcl_File		PipeGetProc _ANSI_ARGS_((ClientData instanceData,
		            int direction));

/*
 * This structure describes the channel type structure for file based IO.
 */

static Tcl_ChannelType fileChannelType = {
    "file",			/* Type name. */
    FileBlockModeProc,		/* Set blocking or non-blocking mode.*/
    FileCloseProc,		/* Close proc. */
    FileInputProc,		/* Input proc. */
    FileOutputProc,		/* Output proc. */
    FileSeekProc,		/* Seek proc. */
    NULL,			/* Set option proc. */
    NULL,			/* Get option proc. */
    FileWatchProc,		/* Set up the notifier to watch the channel. */
    FileReadyProc,		/* Are events present? */
    FileGetProc,		/* Get a Tcl_File from channel. */
};

/*
 * This structure describes the channel type structure for command pipe
 * based IO.
 */

static Tcl_ChannelType pipeChannelType = {
    "pipe",			/* Type name. */
    PipeBlockModeProc,		/* Set blocking or non-blocking mode.*/
    PipeCloseProc,		/* Close proc. */
    PipeInputProc,		/* Input proc. */
    PipeOutputProc,		/* Output proc. */
    NULL,			/* Seek proc. */
    NULL,			/* Set option proc. */
    NULL,			/* Get option proc. */
    PipeWatchProc,		/* Set up notifier to watch the channel. */
    PipeReadyProc,		/* Are events present? */
    PipeGetProc,		/* Get a Tcl_File from channel. */
};

/*
 * This is the size of the channel name for File based channels
 */

#define CHANNEL_NAME_SIZE	64
static char channelName[CHANNEL_NAME_SIZE+1];

/*
 * Structure describing per-instance state for file based channels.
 *
 * IMPORTANT NOTE: If you modify this structure, make sure that the
 * "asynch" field remains the first field - FilePipeBlockMode depends
 * on this.
 */

typedef struct FileState {
    int asynch;			/* 1 if channel is in asynch mode. */
    int append;			/* 1 if channel is in append mode. */
    Tcl_File inFile;		/* Input file. */
    Tcl_File outFile;		/* Output file. */
} FileState;

/*
 * This structure describes per-instance state of a pipe based channel.
 *
 * IMPORTANT NOTE: If you modify this structure, make sure that the
 * "asynch" field remains the first field - FilePipeBlockMode depends
 * on this.
 */

typedef struct PipeState {
    int asynch;			/* 1 if channel is in asynch mode. */
    Tcl_File readFile;		/* Output from pipe. */
    Tcl_File writeFile;		/* Input from pipe. */
    Tcl_File errorFile;		/* Error output from pipe. */
    int numPids;		/* Number of processes attached to pipe. */
    int *pidPtr;		/* Pids of attached processes. */
} PipeState;

/*
 *----------------------------------------------------------------------
 *
 * FileBlockModeProc --
 *
 *	Set blocking or non-blocking mode on channel.
 *
 * Results:
 *	0 if successful, errno when failed.
 *
 * Side effects:
 *	Sets the device into blocking or non-blocking mode.
 *
 *----------------------------------------------------------------------
 */

static int
FileBlockModeProc(instanceData, mode)
    ClientData instanceData;		/* Instance state for channel. */
    int mode;				/* The mode to set. */
{
    FileState *fsPtr = (FileState *) instanceData;
    
    /*
     * Files on Windows can not be switched between blocking and nonblocking,
     * hence we have to emulate the behavior. This is done in the input
     * function by checking against a bit in the state. We set or unset the
     * bit here to cause the input function to emulate the correct behavior.
     */

    fsPtr->asynch = (mode == TCL_MODE_BLOCKING) ? 0 : 1;
    return 0;
}

/*
 *----------------------------------------------------------------------
 *
 * FileCloseProc --
 *
 *	Closes the IO channel.
 *
 * Results:
 *	0 if successful, the value of errno if failed.
 *
 * Side effects:
 *	Closes the physical channel
 *
 *----------------------------------------------------------------------
 */

static int
FileCloseProc(instanceData, interp)
    ClientData instanceData;	/* Pointer to FileState structure. */
    Tcl_Interp *interp;		/* Not used. */
{
    FileState *fsPtr = (FileState *) instanceData;
    HANDLE handle;
    int type, errorCode = 0;

    if (fsPtr->inFile != NULL) {
        handle = (HANDLE) Tcl_GetFileInfo(fsPtr->inFile, &type);

	/*
	 * Check for read/write file so we only close it once.
	 */

	if (fsPtr->inFile == fsPtr->outFile) {
	    fsPtr->outFile = NULL;
	}
        Tcl_FreeFile(fsPtr->inFile);

	if (CloseHandle(handle) == FALSE) {
	    TclWinConvertError(GetLastError());
	    errorCode = errno;
	}

    }
    if (fsPtr->outFile != NULL) {
        handle = (HANDLE) Tcl_GetFileInfo(fsPtr->outFile, &type);
	Tcl_FreeFile(fsPtr->outFile);

	if (CloseHandle(handle) == FALSE) {
	    TclWinConvertError(GetLastError());
	    if (errorCode == 0) {
		errorCode = errno;
	    }
	}
    }
    ckfree((char *) instanceData);
    return errorCode;
}

/*
 *----------------------------------------------------------------------
 *
 * FileSeekProc --
 *
 *	Seeks on a file-based channel. Returns the new position.
 *
 * Results:
 *	-1 if failed, the new position if successful. If failed, it
 *	also sets *errorCodePtr to the error code.
 *
 * Side effects:
 *	Moves the location at which the channel will be accessed in
 *	future operations.
 *
 *----------------------------------------------------------------------
 */

static int
FileSeekProc(instanceData, offset, mode, errorCodePtr)
    ClientData instanceData;			/* File state. */
    long offset;				/* Offset to seek to. */
    int mode;					/* Relative to where
                                                 * should we seek? */
    int *errorCodePtr;				/* To store error code. */
{
    FileState *fsPtr = (FileState *) instanceData;
    DWORD moveMethod;
    DWORD newPos;
    HANDLE handle;
    int type;

    *errorCodePtr = 0;
    if (fsPtr->inFile != (Tcl_File) NULL) {
        handle = (HANDLE) Tcl_GetFileInfo(fsPtr->inFile, &type);
    } else if (fsPtr->outFile != (Tcl_File) NULL) {
        handle = (HANDLE) Tcl_GetFileInfo(fsPtr->outFile, &type);
    } else {
        *errorCodePtr = EFAULT;
        return -1;
    }
    
    if (mode == SEEK_SET) {
        moveMethod = FILE_BEGIN;
    } else if (mode == SEEK_CUR) {
        moveMethod = FILE_CURRENT;
    } else {
        moveMethod = FILE_END;
    }

    newPos = SetFilePointer(handle, offset, NULL, moveMethod);
    if (newPos == 0xFFFFFFFF) {
        TclWinConvertError(GetLastError());
        return -1;
    }
    return newPos;
}

/*
 *----------------------------------------------------------------------
 *
 * FileInputProc --
 *
 *	Reads input from the IO channel into the buffer given. Returns
 *	count of how many bytes were actually read, and an error indication.
 *
 * Results:
 *	A count of how many bytes were read is returned and an error
 *	indication is returned in an output argument.
 *
 * Side effects:
 *	Reads input from the actual channel.
 *
 *----------------------------------------------------------------------
 */

static int
FileInputProc(instanceData, buf, bufSize, errorCode)
    ClientData instanceData;		/* File state. */
    char *buf;				/* Where to store data read. */
    int bufSize;			/* How much space is available
                                         * in the buffer? */
    int *errorCode;			/* Where to store error code. */
{
    FileState *statePtr;
    HANDLE handle;
    DWORD bytesRead;
    int type;

    *errorCode = 0;
    statePtr = (FileState *) instanceData;
    handle = (HANDLE) Tcl_GetFileInfo(statePtr->inFile, &type);

    /*
     * Note that we will block on reads from a console buffer until a
     * full line has been entered.  The only way I know of to get
     * around this is to write a console driver.  We should probably
     * do this at some point, but for now, we just block.
     */

    if (ReadFile(handle, (LPVOID) buf, (DWORD) bufSize, &bytesRead,
            (LPOVERLAPPED) NULL) == FALSE) {
	goto error;
    }
    
    return bytesRead;

error:
    TclWinConvertError(GetLastError());
    *errorCode = errno;
    if (errno == EPIPE) {
	return 0;
    }
    return -1;
}

/*
 *----------------------------------------------------------------------
 *
 * FileOutputProc --
 *
 *	Writes the given output on the IO channel. Returns count of how
 *	many characters were actually written, and an error indication.
 *
 * Results:
 *	A count of how many characters were written is returned and an
 *	error indication is returned in an output argument.
 *
 * Side effects:
 *	Writes output on the actual channel.
 *
 *----------------------------------------------------------------------
 */

static int
FileOutputProc(instanceData, buf, toWrite, errorCode)
    ClientData instanceData;		/* File state. */
    char *buf;				/* The data buffer. */
    int toWrite;			/* How many bytes to write? */
    int *errorCode;			/* Where to store error code. */
{
    FileState *statePtr = (FileState *) instanceData;
    int type;
    DWORD bytesWritten;
    HANDLE handle;
    
    *errorCode = 0;
    handle = (HANDLE) Tcl_GetFileInfo(statePtr->outFile, &type);

    /*
     * If we are writing to a file that was opened with O_APPEND, we need to
     * seek to the end of the file before writing the current buffer.
     */

    if (statePtr->append) {
        SetFilePointer(handle, 0, NULL, FILE_END);
    }

    if (WriteFile(handle, (LPVOID) buf, (DWORD) toWrite, &bytesWritten,
            (LPOVERLAPPED) NULL) == FALSE) {
        TclWinConvertError(GetLastError());
        *errorCode = errno;
        return -1;
    }
    FlushFileBuffers(handle);
    return bytesWritten;
}

/*
 *----------------------------------------------------------------------
 *
 * FileWatchProc --
 *
 *	Called by the notifier to set up to watch for events on this
 *	channel.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */

static void
FileWatchProc(instanceData, mask)
    ClientData instanceData;		/* File state. */
    int mask;				/* What events to watch for; OR-ed
                                         * combination of TCL_READABLE,
                                         * TCL_WRITABLE and TCL_EXCEPTION. */
{
    FileState *fsPtr = (FileState *) instanceData;

    if (mask & TCL_ACTIVE) {
        Tcl_WatchFile((Tcl_File) NULL, TCL_ACTIVE);
    }
    if ((mask & TCL_READABLE) && (fsPtr->inFile != (Tcl_File) NULL)) {
        Tcl_WatchFile(fsPtr->inFile, TCL_READABLE);
    }
    if ((mask & TCL_WRITABLE) && (fsPtr->outFile != (Tcl_File) NULL)) {
        Tcl_WatchFile(fsPtr->outFile, TCL_WRITABLE);
    }

    if (mask & TCL_EXCEPTION) {
        if (fsPtr->inFile != (Tcl_File) NULL) {
            Tcl_WatchFile(fsPtr->inFile, TCL_EXCEPTION);
        }
        if (fsPtr->outFile != (Tcl_File) NULL) {
            Tcl_WatchFile(fsPtr->outFile, TCL_EXCEPTION);
        }
    }
}

/*
 *----------------------------------------------------------------------
 *
 * FileReadyProc --
 *
 *	Called by the notifier to check whether events of interest are
 *	present on the channel.
 *
 * Results:
 *	Returns OR-ed combination of TCL_READABLE, TCL_WRITABLE and
 *	TCL_EXCEPTION to indicate which events of interest are present.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */

static int
FileReadyProc(instanceData, mask)
    ClientData instanceData;		/* The file state. */
    int mask;				/* Events of interest; an OR-ed
                                         * combination of TCL_READABLE,
                                         * TCL_WRITABLE and TCL_EXCEPTION. */
{
    FileState *fsPtr = (FileState *) instanceData;
    int present = 0;

    if ((mask & TCL_READABLE) && (fsPtr->inFile != (Tcl_File) NULL)) {
        present |= Tcl_FileReady(fsPtr->inFile, TCL_READABLE);
    }
    if ((mask & TCL_WRITABLE) && (fsPtr->outFile != (Tcl_File) NULL)) {
        present |= Tcl_FileReady(fsPtr->outFile, TCL_WRITABLE);
    }
    if (mask & TCL_EXCEPTION) {
        if (fsPtr->inFile != (Tcl_File) NULL) {
            present |= Tcl_FileReady(fsPtr->inFile, TCL_EXCEPTION);
        }
        if (fsPtr->outFile != (Tcl_File) NULL) {
            present |= Tcl_FileReady(fsPtr->outFile, TCL_EXCEPTION);
        }
    }
    return present;
}

/*
 *----------------------------------------------------------------------
 *
 * FileGetProc --
 *
 *	Called from Tcl_GetChannelFile to retrieve Tcl_Files from inside
 *	a file based channel.
 *
 * Results:
 *	The appropriate Tcl_File or NULL if not present. 
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */

static Tcl_File
FileGetProc(instanceData, direction)
    ClientData instanceData;		/* The file state. */
    int direction;			/* Which Tcl_File to retrieve? */
{
    FileState *fsPtr = (FileState *) instanceData;

    if (direction == TCL_READABLE) {
        return fsPtr->inFile;
    }
    if (direction == TCL_WRITABLE) {
        return fsPtr->outFile;
    }
    return (Tcl_File) NULL;
}

/*
 *----------------------------------------------------------------------
 *
 * PipeBlockModeProc --
 *
 *	Set blocking or non-blocking mode on channel.
 *
 * Results:
 *	0 if successful, errno when failed.
 *
 * Side effects:
 *	Sets the device into blocking or non-blocking mode.
 *
 *----------------------------------------------------------------------
 */

static int
PipeBlockModeProc(instanceData, mode)
    ClientData instanceData;		/* Instance state for channel. */
    int mode;				/* The mode to set. */
{
    PipeState *statePtr = (PipeState *) instanceData;
    
    /*
     * Files on Windows can not be switched between blocking and nonblocking,
     * hence we have to emulate the behavior. This is done in the input
     * function by checking against a bit in the state. We set or unset the
     * bit here to cause the input function to emulate the correct behavior.
     */

    statePtr->asynch = (mode == TCL_MODE_BLOCKING) ? 0 : 1;
    return 0;
}

/*
 *----------------------------------------------------------------------
 *
 * PipeCloseProc --
 *
 *	Closes a pipe based IO channel.
 *
 * Results:
 *	0 on success, errno otherwise.
 *
 * Side effects:
 *	Closes the physical channel.
 *
 *----------------------------------------------------------------------
 */

static int
PipeCloseProc(instanceData, interp)
    ClientData instanceData;	/* Pointer to PipeState structure. */
    Tcl_Interp *interp;		/* For error reporting. */
{
    PipeState *pipePtr = (PipeState *) instanceData;
    FileState *fsPtr;
    HANDLE handle;
    Tcl_Channel errChan;
    int errorCode, result, type;
    ClientData clientData;
    TclWinPipe *winPipePtr;

    errorCode = 0;
    if (pipePtr->readFile != NULL) {
	clientData = Tcl_GetFileInfo(pipePtr->readFile, &type);
        Tcl_FreeFile(pipePtr->readFile);
	if (type == TCL_WIN32S_PIPE) {
	    winPipePtr = (TclWinPipe *) clientData;

	    if (winPipePtr->otherPtr != NULL) {
		winPipePtr->otherPtr->otherPtr = NULL;
	    } else {
		if (winPipePtr->fileHandle != INVALID_HANDLE_VALUE) {
		    CloseHandle(winPipePtr->fileHandle);
		}
		DeleteFile(winPipePtr->fileName);
		ckfree((char *) winPipePtr->fileName);
	    }
	    ckfree((char *) winPipePtr);
	} else {
	    handle = (HANDLE) clientData;
	    if (CloseHandle(handle) == FALSE) {
		TclWinConvertError(GetLastError());
		errorCode = errno;
	    }
	}
    }
    if (pipePtr->writeFile != NULL) {
        clientData = Tcl_GetFileInfo(pipePtr->writeFile, &type);
	Tcl_FreeFile(pipePtr->writeFile);
	if (type == TCL_WIN32S_PIPE) {
	    winPipePtr = (TclWinPipe *) clientData;

	    if (winPipePtr->otherPtr != NULL) {
		winPipePtr->otherPtr->otherPtr = NULL;
	    } else {
		if (winPipePtr->fileHandle != INVALID_HANDLE_VALUE) {
		    CloseHandle(winPipePtr->fileHandle);
		}
		DeleteFile(winPipePtr->fileName);
		ckfree((char *) winPipePtr->fileName);
	    }
	    ckfree((char *) winPipePtr);
	} else {
	    handle = (HANDLE) clientData;
	    if (CloseHandle(handle) == FALSE) {
		TclWinConvertError(GetLastError());
		if (errorCode == 0) {
		    errorCode = errno;
		}
	    }
	}
    }
    
    /*
     * Wrap the error file into a channel and give it to the cleanup
     * routine.
     */

    if (pipePtr->errorFile != NULL) {
        fsPtr = (FileState *) ckalloc((unsigned) sizeof(FileState));

        fsPtr->inFile = pipePtr->errorFile;
        fsPtr->outFile = (Tcl_File) NULL;
        fsPtr->asynch = 0;
        fsPtr->append = 0;
        
	errChan = Tcl_CreateChannel(&fileChannelType, "pipeError",
                (ClientData) fsPtr, TCL_READABLE);
        if (Tcl_SetChannelOption(interp, errChan, "-translation", "auto") ==
                TCL_ERROR) {
            Tcl_Close((Tcl_Interp *) NULL, errChan);
            errChan = (Tcl_Channel) NULL;
        }
        if ((errChan != (Tcl_Channel) NULL) &&
                (Tcl_SetChannelOption(NULL, errChan, "-eofchar", "\032") ==
                        TCL_ERROR)) {
            Tcl_Close((Tcl_Interp *) NULL, errChan);
            errChan = (Tcl_Channel) NULL;
        }
    } else {
        errChan = NULL;
    }
    result = TclCleanupChildren(interp, pipePtr->numPids, pipePtr->pidPtr,
            errChan);
    if (pipePtr->numPids > 0) {
        ckfree((char *) pipePtr->pidPtr);
    }
    ckfree((char *) pipePtr);
    if (errorCode == 0) {
        return result;
    }
    return errorCode;
}

/*
 *----------------------------------------------------------------------
 *
 * PipeInputProc --
 *
 *	Reads input from the IO channel into the buffer given. Returns
 *	count of how many bytes were actually read, and an error indication.
 *
 * Results:
 *	A count of how many bytes were read is returned and an error
 *	indication is returned in an output argument.
 *
 * Side effects:
 *	Reads input from the actual channel.
 *
 *----------------------------------------------------------------------
 */

static int
PipeInputProc(instanceData, buf, bufSize, errorCode)
    ClientData instanceData;		/* Pipe state. */
    char *buf;				/* Where to store data read. */
    int bufSize;			/* How much space is available
                                         * in the buffer? */
    int *errorCode;			/* Where to store error code. */
{
    PipeState *statePtr;
    HANDLE handle;
    DWORD count;
    DWORD bytesRead;
    int type;
    ClientData clientData;
    TclWinPipe *pipePtr;

    *errorCode = 0;
    statePtr = (PipeState *) instanceData;
    clientData = Tcl_GetFileInfo(statePtr->readFile, &type);
    if (type == TCL_WIN32S_PIPE) {
	pipePtr = (TclWinPipe *) clientData;
	if (pipePtr->otherPtr != NULL) {
	    panic("PipeInputProc: child process isn't finished writing");
	}
	if (pipePtr->fileHandle == INVALID_HANDLE_VALUE) {
	    pipePtr->fileHandle = CreateFile(pipePtr->fileName, GENERIC_READ,
		    0, NULL, OPEN_ALWAYS, FILE_ATTRIBUTE_NORMAL, NULL);
	}
	handle = pipePtr->fileHandle;
	if (handle == INVALID_HANDLE_VALUE) {
	    goto error;
	}
    } else {
	handle = (HANDLE) clientData;

	/*
	 * Pipes will block until the requested number of bytes has been
	 * read.  To avoid blocking unnecessarily, we look ahead and only
	 * read as much as is available.
	 */

	if (PeekNamedPipe(handle, (LPVOID) NULL, (DWORD) 0, (LPDWORD) NULL,
		&count, (LPDWORD) NULL) == TRUE) {
	    if ((count != 0) && ((DWORD) bufSize > count)) {
		bufSize = (int) count;
	    } else if ((count == 0) && statePtr->asynch) {
		errno = *errorCode = EAGAIN;
		return 0;
	    } else if ((count == 0) && !statePtr->asynch) {
		bufSize = 1;
	    }
	} else {
	    goto error;
	}
    }

    /*
     * Note that we will block on reads from a console buffer until a
     * full line has been entered.  The only way I know of to get
     * around this is to write a console driver.  We should probably
     * do this at some point, but for now, we just block.
     */

    if (ReadFile(handle, (LPVOID) buf, (DWORD) bufSize, &bytesRead,
            (LPOVERLAPPED) NULL) == FALSE) {
	goto error;
    }
    
    return bytesRead;

    error:
    TclWinConvertError(GetLastError());
    if (errno == EPIPE) {
	return 0;
    }
    *errorCode = errno;
    return -1;
}

/*
 *----------------------------------------------------------------------
 *
 * PipeOutputProc --
 *
 *	Writes the given output on the IO channel. Returns count of how
 *	many characters were actually written, and an error indication.
 *
 * Results:
 *	A count of how many characters were written is returned and an
 *	error indication is returned in an output argument.
 *
 * Side effects:
 *	Writes output on the actual channel.
 *
 *----------------------------------------------------------------------
 */

static int
PipeOutputProc(instanceData, buf, toWrite, errorCode)
    ClientData instanceData;		/* Pipe state. */
    char *buf;				/* The data buffer. */
    int toWrite;			/* How many bytes to write? */
    int *errorCode;			/* Where to store error code. */
{
    PipeState *statePtr = (PipeState *) instanceData;
    int type;
    DWORD bytesWritten;
    HANDLE handle;
    
    *errorCode = 0;
    handle = (HANDLE) Tcl_GetFileInfo(statePtr->writeFile, &type);
    if (WriteFile(handle, (LPVOID) buf, (DWORD) toWrite, &bytesWritten,
            (LPOVERLAPPED) NULL) == FALSE) {
        TclWinConvertError(GetLastError());
        if (errno == EPIPE) {
            return 0;
        }
        *errorCode = errno;
        return -1;
    }
    return bytesWritten;
}

/*
 *----------------------------------------------------------------------
 *
 * PipeWatchProc --
 *
 *	Initialize the notifier to watch Tcl_Files from this channel.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Sets up the notifier so that a future event on the channel will
 *	be seen by Tcl.
 *
 *----------------------------------------------------------------------
 */

static void
PipeWatchProc(instanceData, mask)
    ClientData instanceData;		/* The pipe state. */
    int mask;				/* Events of interest; an OR-ed
                                         * combination of TCL_READABLE,
                                         * TCL_WRITABEL and TCL_EXCEPTION. */
{
    PipeState *psPtr = (PipeState *) instanceData;

    if (mask & TCL_ACTIVE) {
        Tcl_WatchFile((Tcl_File) NULL, TCL_ACTIVE);
    }
    if ((mask & TCL_READABLE) && (psPtr->readFile != (Tcl_File) NULL)) {
        Tcl_WatchFile(psPtr->readFile, TCL_READABLE);
    }
    if ((mask & TCL_WRITABLE) && (psPtr->writeFile != (Tcl_File) NULL)) {
        Tcl_WatchFile(psPtr->writeFile, TCL_WRITABLE);
    }

    if (mask & TCL_EXCEPTION) {
        if (psPtr->readFile != (Tcl_File) NULL) {
            Tcl_WatchFile(psPtr->readFile, TCL_EXCEPTION);
        }
        if (psPtr->writeFile != (Tcl_File) NULL) {
            Tcl_WatchFile(psPtr->writeFile, TCL_EXCEPTION);
        }
    }
}

/*
 *----------------------------------------------------------------------
 *
 * PipeReadyProc --
 *
 *	Called by the notifier to check whether events of interest are
 *	present on the channel.
 *
 * Results:
 *	Returns OR-ed combination of TCL_READABLE, TCL_WRITABLE and
 *	TCL_EXCEPTION to indicate which events of interest are present.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */

static int
PipeReadyProc(instanceData, mask)
    ClientData instanceData;		/* The pipe state. */
    int mask;				/* Events of interest; an OR-ed
                                         * combination of TCL_READABLE,
                                         * TCL_WRITABLE and TCL_EXCEPTION. */
{
    PipeState *psPtr = (PipeState *) instanceData;
    int present = 0;

    if ((mask & TCL_READABLE) && (psPtr->readFile != (Tcl_File) NULL)) {
        present |= Tcl_FileReady(psPtr->readFile, TCL_READABLE);
    }
    if ((mask & TCL_WRITABLE) && (psPtr->writeFile != (Tcl_File) NULL)) {
        present |= Tcl_FileReady(psPtr->writeFile, TCL_WRITABLE);
    }
    if (mask & TCL_EXCEPTION) {
        if (psPtr->readFile != (Tcl_File) NULL) {
            present |= Tcl_FileReady(psPtr->readFile, TCL_EXCEPTION);
        }
        if (psPtr->writeFile != (Tcl_File) NULL) {
            present |= Tcl_FileReady(psPtr->writeFile, TCL_EXCEPTION);
        }
    }
    return present;
}

/*
 *----------------------------------------------------------------------
 *
 * PipeGetProc --
 *
 *	Called from Tcl_GetChannelFile to retrieve Tcl_Files from inside
 *	a command pipeline based channel.
 *
 * Results:
 *	The appropriate Tcl_File or NULL if not present. 
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */

static Tcl_File
PipeGetProc(instanceData, direction)
    ClientData instanceData;		/* The pipe state. */
    int direction;			/* Which Tcl_File to retrieve? */
{
    PipeState *psPtr = (PipeState *) instanceData;

    if (direction == TCL_READABLE) {
        return psPtr->readFile;
    }
    if (direction == TCL_WRITABLE) {
        return psPtr->writeFile;
    }
    return (Tcl_File) NULL;
}

/*
 *----------------------------------------------------------------------
 *
 * Tcl_OpenFileChannel --
 *
 *	Open an File based channel on Unix systems.
 *
 * Results:
 *	The new channel or NULL. If NULL, the output argument
 *	errorCodePtr is set to a POSIX error.
 *
 * Side effects:
 *	May open the channel and may cause creation of a file on the
 *	file system.
 *
 *----------------------------------------------------------------------
 */

Tcl_Channel
Tcl_OpenFileChannel(interp, fileName, modeString, permissions)
    Tcl_Interp *interp;			/* Interpreter for error reporting;
                                         * can be NULL. */
    CONST char *fileName;		/* Name of file to open. */
    CONST char *modeString;		/* A list of POSIX open modes or
                                         * a string such as "rw". */
    int permissions;			/* If the open involves creating a
                                         * file, with what modes to create
                                         * it? */
{
    Tcl_File file;
    Tcl_Channel chan;
    FileState *sPtr;
    int seekFlag, mode, readWriteMode;
    HANDLE handle;
    DWORD accessMode, createMode, shareMode, flags;
    SECURITY_ATTRIBUTES sec;
    char *nativeName;
    Tcl_DString buffer;

    mode = TclGetOpenMode(interp, modeString, &seekFlag);
    if (mode == -1) {
        return NULL;
    }
    switch (mode & (O_RDONLY | O_WRONLY | O_RDWR)) {
	case O_RDONLY:
	    accessMode = GENERIC_READ;
	    break;
	case O_WRONLY:
	    accessMode = GENERIC_WRITE;
	    break;
	case O_RDWR:
	    accessMode = (GENERIC_READ | GENERIC_WRITE);
	    break;
	default:
	    panic("Tcl_OpenFileChannel: invalid mode value");
	    break;
    }

    /*
     * Map the creation flags to the NT create mode.
     */

    switch (mode & (O_CREAT | O_EXCL | O_TRUNC)) {
	case (O_CREAT | O_EXCL):
	case (O_CREAT | O_EXCL | O_TRUNC):
	    createMode = CREATE_NEW;
	    break;
	case (O_CREAT | O_TRUNC):
	    createMode = CREATE_ALWAYS;
	    break;
	case O_CREAT:
	    createMode = OPEN_ALWAYS;
	    break;
	case O_TRUNC:
	case (O_TRUNC | O_EXCL):
	    createMode = TRUNCATE_EXISTING;
	    break;
	default:
	    createMode = OPEN_EXISTING;
	    break;
    }

    /*
     * If the file is being created, get the file attributes from the
     * permissions argument, else use the existing file attributes.
     */

    if (mode & O_CREAT) {
        if (permissions & S_IWRITE) {
            flags = FILE_ATTRIBUTE_NORMAL;
        } else {
            flags = FILE_ATTRIBUTE_READONLY;
        }
    } else {
	flags = GetFileAttributes(fileName);
        if (flags == 0xFFFFFFFF) {
	    flags = 0;
	}
    }

    /*
     * Set up the security attributes so this file is not inherited by
     * child processes.
     */

    sec.nLength = sizeof(sec);
    sec.lpSecurityDescriptor = NULL;
    sec.bInheritHandle = 0;

    /*
     * Set up the file sharing mode.  We want to allow simultaneous access.
     */

    shareMode = FILE_SHARE_READ | FILE_SHARE_WRITE;

    /*
     * Now we get to create the file.
     */

    nativeName = Tcl_TranslateFileName(interp, fileName, &buffer);
    if (nativeName == NULL) {
	return NULL;
    }
    handle = CreateFile(nativeName, accessMode, shareMode, &sec, createMode,
            flags, (HANDLE) NULL);
    Tcl_DStringFree(&buffer);

    if (handle == INVALID_HANDLE_VALUE) {
	DWORD err = GetLastError();
	if ((err & 0xffffL) == ERROR_OPEN_FAILED) {
	    err = (mode & O_CREAT) ? ERROR_FILE_EXISTS : ERROR_FILE_NOT_FOUND;
	}
        TclWinConvertError(err);
	if (interp != (Tcl_Interp *) NULL) {
            Tcl_AppendResult(interp, "couldn't open \"", fileName, "\": ",
                    Tcl_PosixError(interp), (char *) NULL);
        }
        return NULL;
    }

    file = Tcl_GetFile((ClientData) handle, TCL_WIN_FILE);

    sPtr = (FileState *) ckalloc((unsigned) sizeof(FileState));
    sPtr->asynch = 0;
    sPtr->append = (mode & O_APPEND) ? 1 : 0;
    readWriteMode = 0;
    if (accessMode & GENERIC_READ) {
        readWriteMode |= TCL_READABLE;
        sPtr->inFile = file;
    } else {
        sPtr->inFile = (Tcl_File) NULL;
    }
    if (accessMode & GENERIC_WRITE) {
        readWriteMode |= TCL_WRITABLE;
        sPtr->outFile = file;
    } else {
        sPtr->outFile = (Tcl_File) NULL;
    }
    sprintf(channelName, "file%d", (int) Tcl_GetFileInfo(file, NULL));
    chan = Tcl_CreateChannel(&fileChannelType, channelName,
            (ClientData) sPtr, readWriteMode);
    if (chan == (Tcl_Channel) NULL) {
        if (interp != (Tcl_Interp *) NULL) {
            Tcl_AppendResult(interp, "could not open channel \"",
                    channelName, "\": ", Tcl_PosixError(interp),
                    (char *) NULL);
        }
        Tcl_FreeFile(file);
        CloseHandle(handle);
        ckfree((char *) sPtr);
        return NULL;
    }

    if (seekFlag) {
        if (Tcl_Seek(chan, 0, SEEK_END) < 0) {
            if (interp != (Tcl_Interp *) NULL) {
                Tcl_AppendResult(interp, "could not seek to end of file on \"",
                        channelName, "\": ", Tcl_PosixError(interp),
                        (char *) NULL);
            }
            Tcl_Close((Tcl_Interp *) NULL, chan);
            return NULL;
        }
    }

    /*
     * Files have default translation of AUTO and ^Z eof char, which
     * means that a ^Z will be appended to them at close.
     */
    
    if (Tcl_SetChannelOption(interp, chan, "-translation", "auto") ==
            TCL_ERROR) {
        Tcl_Close((Tcl_Interp *) NULL, chan);
        return (Tcl_Channel) NULL;
    }
    if (Tcl_SetChannelOption(NULL, chan, "-eofchar", "\032 {}") ==
            TCL_ERROR) {
        Tcl_Close((Tcl_Interp *) NULL, chan);
        return (Tcl_Channel) NULL;
    }
    return chan;
}

/*
 *----------------------------------------------------------------------
 *
 * FileType --
 *
 *	Converts a Windows handle type to a Tcl file type
 *
 * Results:
 *	The Tcl file type corresponding to the given Windows handle type
 *	or -1 on error.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */

static int
FileType(h)
    HANDLE h;		/* Convert the type of this handle to
                         * a Tcl file type. */
{
    switch (GetFileType(h)) {
    case FILE_TYPE_CHAR:
        return TCL_WIN_CONSOLE;
    case FILE_TYPE_DISK:
        return TCL_WIN_FILE;
    case FILE_TYPE_PIPE:
        return TCL_WIN_PIPE;
    default:
        return -1;
    }
}

/*
 *----------------------------------------------------------------------
 *
 * Tcl_MakeFileChannel --
 *
 *	Creates a Tcl_Channel from an existing platform specific file
 *	handle.
 *
 * Results:
 *	The Tcl_Channel created around the preexisting file.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */

Tcl_Channel
Tcl_MakeFileChannel(inFile, outFile, mode)
    ClientData inFile;		/* OS level handle used for input. */
    ClientData outFile;		/* OS level handle used for output. */
    int mode;			/* ORed combination of TCL_READABLE and
                                 * TCL_WRITABLE to indicate whether inFile
                                 * and/or outFile are valid. */
{
    Tcl_Channel chan;
    int fileUsed;
    Tcl_File inFd, outFd;
    char channelName[20];
    FileState *sPtr;

    if (mode & TCL_READABLE) {
        sprintf(channelName, "file%d", (int) inFile);
        inFd = Tcl_GetFile(inFile, FileType((HANDLE) inFile));
    } else {
        inFd = (Tcl_File) NULL;
    }

    if (mode & TCL_WRITABLE) {
        sprintf(channelName, "file%d", (int) outFile);
        outFd = Tcl_GetFile(outFile, FileType((HANDLE) outFile));
    } else {
        outFd = (Tcl_File) NULL;
    }

    /*
     * See if a channel with the right Tcl_Files in it already exists. If
     * so, return it.
     */
    
    chan = TclFindFileChannel(inFd, outFd, &fileUsed);
    if (chan != (Tcl_Channel) NULL) {
        return chan;
    }

    /*
     * If one of the Tcl_Files is already used by another channel, do not
     * create a new channel containing it. This will avoid core dumps later
     * when the Tcl_File would be freed twice.
     */

    if (fileUsed) {
        return (Tcl_Channel) NULL;
    }

    sPtr = (FileState *) ckalloc((unsigned) sizeof(FileState));
    sPtr->asynch = 0;
    sPtr->append = 0;
    sPtr->inFile = inFd;
    sPtr->outFile = outFd;

    chan = Tcl_CreateChannel(&fileChannelType, channelName,
            (ClientData) sPtr, mode);
    if (chan == (Tcl_Channel) NULL) {
        ckfree((char *) sPtr);
        return NULL;
    }

    /*
     * Windows files have AUTO translation mode and ^Z eof char on input.
     */
    
    if (Tcl_SetChannelOption((Tcl_Interp *) NULL, chan, "-translation",
            "auto") == TCL_ERROR) {
        Tcl_Close((Tcl_Interp *) NULL, chan);
        return (Tcl_Channel) NULL;
    }
    if (Tcl_SetChannelOption((Tcl_Interp *) NULL, chan, "-eofchar",
            "\032 {}") == TCL_ERROR) {
        Tcl_Close((Tcl_Interp *) NULL, chan);
        return (Tcl_Channel) NULL;
    }
    return chan;
}

/*
 *----------------------------------------------------------------------
 *
 * TclCreateCommandChannel --
 *
 *	This function is called by Tcl_OpenCommandChannel to perform
 *	the platform specific channel initialization for a command
 *	channel.
 *
 * Results:
 *	Returns a new channel or NULL on failure.
 *
 * Side effects:
 *	Allocates a new channel.
 *
 *----------------------------------------------------------------------
 */

Tcl_Channel
TclCreateCommandChannel(readFile, writeFile, errorFile, numPids, pidPtr)
    Tcl_File readFile;		/* If non-null, gives the file for reading. */
    Tcl_File writeFile;		/* If non-null, gives the file for writing. */
    Tcl_File errorFile;		/* If non-null, gives the file where errors
				 * can be read. */
    int numPids;		/* The number of pids in the pid array. */
    int *pidPtr;		/* An array of process identifiers. */
{
    Tcl_Channel channel;
    char channelName[20];
    int channelId;
    int permissions;
    PipeState *statePtr = (PipeState *) ckalloc((unsigned) sizeof(PipeState));

    statePtr->asynch = 0;
    statePtr->readFile = readFile;
    statePtr->writeFile = writeFile;
    statePtr->errorFile = errorFile;
    statePtr->numPids = numPids;
    statePtr->pidPtr = pidPtr;

    /*
     * Use one of the fds associated with the channel as the
     * channel id.
     */

    if (readFile) {
	channelId = (int) Tcl_GetFileInfo(readFile, NULL);
    } else if (writeFile) {
	channelId = (int) Tcl_GetFileInfo(writeFile, NULL);
    } else if (errorFile) {
	channelId = (int) Tcl_GetFileInfo(errorFile, NULL);
    } else {
	channelId = 0;
    }

    permissions = 0;
    if (readFile != (Tcl_File) NULL) {
        permissions |= TCL_READABLE;
    }
    if (writeFile != (Tcl_File) NULL) {
        permissions |= TCL_WRITABLE;
    }

    /*
     * For backward compatibility with previous versions of Tcl, we
     * use "file%d" as the base name for pipes even though it would
     * be more natural to use "pipe%d".
     */

    sprintf(channelName, "file%d", channelId);
    channel = Tcl_CreateChannel(&pipeChannelType, channelName,
            (ClientData) statePtr, permissions);

    if (channel == NULL) {
	ckfree((char *)statePtr);
        return NULL;
    }

    /*
     * Pipes have AUTO translation mode on Windows and ^Z eof char, which
     * means that a ^Z will be appended to them at close. This is needed
     * for Windows programs that expect a ^Z at EOF.
     */

    if (Tcl_SetChannelOption((Tcl_Interp *) NULL, channel, "-translation",
            "auto") == TCL_ERROR) {
        Tcl_Close((Tcl_Interp *) NULL, channel);
        return (Tcl_Channel) NULL;
    }
    if (Tcl_SetChannelOption((Tcl_Interp *) NULL, channel, "-eofchar",
            "\032 {}") == TCL_ERROR) {
        Tcl_Close((Tcl_Interp *) NULL, channel);
        return (Tcl_Channel) NULL;
    }
    return channel;
}

/*
 *----------------------------------------------------------------------
 *
 * Tcl_PidCmd --
 *
 *	This procedure is invoked to process the "pid" Tcl command.
 *	See the user documentation for details on what it does.
 *
 * Results:
 *	A standard Tcl result.
 *
 * Side effects:
 *	See the user documentation.
 *
 *----------------------------------------------------------------------
 */

	/* ARGSUSED */
int
Tcl_PidCmd(dummy, interp, argc, argv)
    ClientData dummy;			/* Not used. */
    Tcl_Interp *interp;			/* Current interpreter. */
    int argc;				/* Number of arguments. */
    char **argv;			/* Argument strings. */
{
    Tcl_Channel chan;			/* The channel to get pids for. */
    Tcl_ChannelType *typePtr;
    PipeState *pipePtr;			/* The pipe state. */
    int i;				/* Loops over PIDs attached to the
                                         * pipe. */
    char string[50];			/* Temp buffer for string rep. of
                                         * PIDs attached to the pipe. */

    if (argc > 2) {
	Tcl_AppendResult(interp, "wrong # args: should be \"",
		argv[0], " ?channelId?\"", (char *) NULL);
	return TCL_ERROR;
    }
    if (argc == 1) {
	sprintf(interp->result, "%lu", (unsigned long) getpid());
    } else {
        chan = Tcl_GetChannel(interp, argv[1], NULL);
        if (chan == (Tcl_Channel) NULL) {
	    return TCL_ERROR;
	}
	typePtr = Tcl_GetChannelType(chan);
	if (typePtr != &pipeChannelType) {
            return TCL_OK;
        }
        pipePtr = (PipeState *) Tcl_GetChannelInstanceData(chan);
        for (i = 0; i < pipePtr->numPids; i++) {
	    sprintf(string, "%lu", (unsigned long) pipePtr->pidPtr[i]);
	    Tcl_AppendElement(interp, string);
	}
    }
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * TclGetDefaultStdChannel --
 *
 *	Constructs a channel for the specified standard OS handle.
 *
 * Results:
 *	Returns the specified default standard channel, or NULL.
 *
 * Side effects:
 *	May cause the creation of a standard channel and the underlying
 *	file.
 *
 *----------------------------------------------------------------------
 */

Tcl_Channel
TclGetDefaultStdChannel(type)
    int type;			/* One of TCL_STDIN, TCL_STDOUT, TCL_STDERR. */
{
    Tcl_Channel channel;
    HANDLE handle;
    int mode;
    char *bufMode;
    DWORD handleId;		/* Standard handle to retrieve. */

#ifdef _MSC_VER
    /* 
     * If this code is compiled under Borland, the stdio handles for 
     * tclsh get screwed up and the program exits immediately.
     */

    static int hidden = 0;

    if (hidden == 0) {
	/*
	 * The stdio handles for this process are globally visible by all
	 * children of this process, which means that a badly behaved child
	 * process could write to its parent's handles.  Change the 
	 * permission on the handles so that they are not globally visible,
	 * then have to tell C that the standard file descriptors are to
	 * be associated with these handles
	 */

	HANDLE hProcess = GetCurrentProcess();
	HANDLE h1, h2;
	
	h1 = GetStdHandle(STD_INPUT_HANDLE);
	if (DuplicateHandle(hProcess, h1, hProcess, &h2, 0, FALSE, 
		DUPLICATE_SAME_ACCESS) != 0) {

	    /* 
	     * The following two commands have the side effects of 
	     * CloseHandle(h1) and SetStdHandle(STD_INPUT_HANDLE, h2).
	     */

	    _close(0);
	    _open_osfhandle((long) h2, _O_TEXT);
	}

	h1 = GetStdHandle(STD_OUTPUT_HANDLE);
	if (DuplicateHandle(hProcess, h1, hProcess, &h2, 0, FALSE, 
		DUPLICATE_SAME_ACCESS) != 0) {
	    /* 
	     * The following two commands have the side effects of 
	     * CloseHandle(h1) and SetStdHandle(STD_OUTPUT_HANDLE, h2).
	     */

	    _close(1);
	    _open_osfhandle((long) h2, _O_TEXT);
	}

	h1 = GetStdHandle(STD_ERROR_HANDLE);
	if (DuplicateHandle(hProcess, h1, hProcess, &h2, 0, FALSE, 
		DUPLICATE_SAME_ACCESS) != 0) {
	    /* 
	     * The following two commands have the side effects of 
	     * CloseHandle(h1) and SetStdHandle(STD_ERROR_HANDLE, h2).
	     */

	    _close(2);
	    _open_osfhandle((long) h2, _O_TEXT);
	}

	hidden = 1;
    }
#endif

    switch (type) {
	case TCL_STDIN:
	    handleId = STD_INPUT_HANDLE;
	    mode = TCL_READABLE;
	    bufMode = "line";
	    break;
	case TCL_STDOUT:
	    handleId = STD_OUTPUT_HANDLE;
	    mode = TCL_WRITABLE;
	    bufMode = "line";
	    break;
	case TCL_STDERR:
	    handleId = STD_ERROR_HANDLE;
	    mode = TCL_WRITABLE;
	    bufMode = "none";
	    break;
	default:
	    panic("TclGetDefaultStdChannel: Unexpected channel type");
	    break;
    }
    handle = GetStdHandle(handleId);

    /*
     * Note that we need to check for 0 because Windows will return 0 if this
     * is not a console mode application, even though this is not a valid
     * handle. 
     */

    if ((handle == INVALID_HANDLE_VALUE) || (handle == 0)) {
	return NULL;
    }

    channel = Tcl_MakeFileChannel(handle, handle, mode);

    /*
     * Set up the normal channel options for stdio handles.
     */

    if (Tcl_SetChannelOption((Tcl_Interp *) NULL, channel, "-translation",
            "auto") == TCL_ERROR) {
        Tcl_Close((Tcl_Interp *) NULL, channel);
        return (Tcl_Channel) NULL;
    }
    if (Tcl_SetChannelOption((Tcl_Interp *) NULL, channel, "-eofchar",
            "\032 {}") == TCL_ERROR) {
        Tcl_Close((Tcl_Interp *) NULL, channel);
        return (Tcl_Channel) NULL;
    }
    if (Tcl_SetChannelOption((Tcl_Interp *) NULL, channel, "-buffering",
            bufMode) == TCL_ERROR) {
        Tcl_Close((Tcl_Interp *) NULL, channel);
        return (Tcl_Channel) NULL;
    }
    return channel;
}

/*
 *----------------------------------------------------------------------
 *
 * TclGetAndDetachPids --
 *
 *	Stores a list of the command PIDs for a command channel in
 *	interp->result.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Modifies interp->result.
 *
 *----------------------------------------------------------------------
 */

void
TclGetAndDetachPids(interp, chan)
    Tcl_Interp *interp;
    Tcl_Channel chan;
{
    PipeState *pipePtr;
    Tcl_ChannelType *chanTypePtr;
    int i;
    char buf[20];

    /*
     * Punt if the channel is not a command channel.
     */

    chanTypePtr = Tcl_GetChannelType(chan);
    if (chanTypePtr != &pipeChannelType) {
        return;
    }

    pipePtr = (PipeState *) Tcl_GetChannelInstanceData(chan);
    for (i = 0; i < pipePtr->numPids; i++) {
        sprintf(buf, "%d", pipePtr->pidPtr[i]);
        Tcl_AppendElement(interp, buf);
        Tcl_DetachPids(1, &(pipePtr->pidPtr[i]));
    }
    if (pipePtr->numPids > 0) {
        ckfree((char *) pipePtr->pidPtr);
        pipePtr->numPids = 0;
    }
}

/*
 *----------------------------------------------------------------------
 *
 * TclClosePipeFile --
 *
 *	This function is a simple wrapper for close on a file or
 *	pipe handle.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Closes the HANDLE and frees the Tcl_File.
 *
 *----------------------------------------------------------------------
 */

void
TclClosePipeFile(file)
    Tcl_File file;
{
    int type;
    HANDLE handle = (HANDLE) Tcl_GetFileInfo(file, &type);
    switch (type) {
	case TCL_WIN_FILE:
	case TCL_WIN_PIPE:
	    CloseHandle(handle);
	    break;
	default:
	    break;
    }
    Tcl_FreeFile(file);
}
