/* 
 * tclMain.c --
 *
 *	Main program for Tcl shells and other Tcl-based applications.
 *
 * Copyright (c) 1988-1994 The Regents of the University of California.
 * Copyright (c) 1994-1996 Sun Microsystems, Inc.
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 * SCCS: @(#) tclMain.c 1.51 96/09/05 17:57:01
 */

#include "tcl.h"
#include "tclInt.h"

/*
 * The following code ensures that tclLink.c is linked whenever
 * Tcl is linked.  Without this code there's no reference to the
 * code in that file from anywhere in Tcl, so it may not be
 * linked into the application.
 */

extern int Tcl_LinkVar _ANSI_ARGS_((Tcl_Interp *interp,
		CONST char *varName, char *addr, int type));
int (*tclDummyLinkVarPtr) _ANSI_ARGS_((Tcl_Interp *interp,
		CONST char *varName, char *addr, int type)) = Tcl_LinkVar;

/*
 * Declarations for various library procedures and variables (don't want
 * to include tclPort.h here, because people might copy this file out of
 * the Tcl source directory to make their own modified versions).
 * Note:  "exit" should really be declared here, but there's no way to
 * declare it without causing conflicts with other definitions elsewher
 * on some systems, so it's better just to leave it out.
 */

extern int		isatty _ANSI_ARGS_((int fd));
extern char *		strcpy _ANSI_ARGS_((char *dst, CONST char *src));

static Tcl_Interp *interp;	/* Interpreter for application. */
static Tcl_DString command;	/* Used to buffer incomplete commands being
				 * read from stdin. */
static Tcl_DString line;	/* Used to read the next line from the
                                 * terminal input. */
static int tty;			/* Non-zero means standard input is a
				 * terminal-like device.  Zero means it's
				 * a file. */
#ifdef TCL_MEM_DEBUG
static char dumpFile[100];	/* Records where to dump memory allocation
				 * information. */
extern char *tclMemDumpFileName;
#endif

/*
 * Forward references for procedures defined later in this file:
 */

static void		Prompt _ANSI_ARGS_((Tcl_Interp *interp, int partial));
static void		StdinProc _ANSI_ARGS_((ClientData clientData,
			    int mask));
#ifdef TCL_MEM_DEBUG
static int		CheckmemCmd _ANSI_ARGS_((ClientData clientData,
			    Tcl_Interp *interp, int argc, char *argv[]));
#endif

/*
 *----------------------------------------------------------------------
 *
 * Tcl_Main --
 *
 *	Main program for tclsh and most other Tcl-based applications.
 *
 * Results:
 *	None. This procedure never returns (it exits the process when
 *	it's done.
 *
 * Side effects:
 *	This procedure initializes the Tk world and then starts
 *	interpreting commands;  almost anything could happen, depending
 *	on the script being interpreted.
 *
 *----------------------------------------------------------------------
 */


static char consoleScript[] = "\n\
set loaded [info loaded {}]\n\
if ![auto_load tkConInit] {\n\
    error \"cannot start tkcon\"\n\
}\n\
foreach pkg $loaded {\n\
  set name [lindex $pkg 1]\n\
  set version [package require $name]\n\
  slave eval package require $name $version\n\
  unset name version\n\
}\n\
catch {unset pkg}\n\
unset loaded\n\
";

void
Tcl_Main(argc, argv, appInitProc)
    int argc;				/* Number of arguments. */
    char **argv;			/* Array of argument strings. */
    Tcl_AppInitProc *appInitProc;	/* Application-specific initialization
					 * procedure to call after most
					 * initialization but before starting
					 * to execute commands. */
{
    char buffer[20], *args, *fileName;
    int code;
    int exitCode = 0;
    Tcl_Channel inChannel, outChannel, errChannel;

    Tcl_FindExecutable(argv[0]);
    interp = Tcl_CreateInterp();
#ifdef TCL_MEM_DEBUG
    Tcl_InitMemory(interp);
    Tcl_CreateCommand(interp, "checkmem", CheckmemCmd, (ClientData) 0,
	    (Tcl_CmdDeleteProc *) NULL);
#endif

    /*
     * Make command-line arguments available in the Tcl variables "argc"
     * and "argv".  If the first argument doesn't start with a "-" then
     * strip it off and use it as the name of a script file to process.
     */

    fileName = NULL;
    if ((argc > 1) && (argv[1][0] != '-')) {
	fileName = argv[1];
	argc--;
	argv++;
    }
    args = Tcl_Merge(argc-1, argv+1);
    Tcl_SetVar(interp, "argv", args, TCL_GLOBAL_ONLY);
    ckfree(args);
    sprintf(buffer, "%d", argc-1);
    Tcl_SetVar(interp, "argc", buffer, TCL_GLOBAL_ONLY);
    Tcl_SetVar(interp, "argv0", (fileName != NULL) ? fileName : argv[0],
	    TCL_GLOBAL_ONLY);

    /*
     * Set the "tcl_interactive" variable.
     */

    tty = isatty(0);
    Tcl_SetVar(interp, "tcl_interactive",
	    ((fileName == NULL) && tty) ? "1" : "0", TCL_GLOBAL_ONLY);
    
    /*
     * Invoke application-specific initialization.
     */

    if ((*appInitProc)(interp) != TCL_OK) {
	errChannel = Tcl_GetStdChannel(TCL_STDERR);
	if (errChannel) {
	    Tcl_Write(errChannel,
		    "application-specific initialization failed: ", -1);
	    Tcl_Write(errChannel, interp->result, -1);
	    Tcl_Write(errChannel, "\n", 1);
	}
    }

    /*
     * If a script file was specified then just source that file
     * and quit.
     */

    if (fileName != NULL) {
	code = Tcl_EvalFile(interp, fileName);
	if (code != TCL_OK) {
	    errChannel = Tcl_GetStdChannel(TCL_STDERR);
	    if (errChannel) {
		/*
		 * The following statement guarantees that the errorInfo
		 * variable is set properly.
		 */

		Tcl_AddErrorInfo(interp, "");
		Tcl_Write(errChannel,
			Tcl_GetVar(interp, "errorInfo", TCL_GLOBAL_ONLY), -1);
		Tcl_Write(errChannel, "\n", 1);
	    }
	    exitCode = 1;
	}
	goto done;
    }

    /*
     * We're running interactively.  Source a user-specific startup
     * file if the application specified one and if the file exists.
     */

    Tcl_SourceRCFile(interp);

    /*
     * If standard input is a terminal-like device and the variable
     * NoTkCon is not set, try to start up TkCon. If this fails
     * just continue with the original console.
     */

    if (tty && !Tcl_GetVar(interp, "NoTkCon", TCL_GLOBAL_ONLY)) {
        if (Tcl_GlobalEval(interp, consoleScript) == TCL_OK) {
	    goto done;
	}
    }

    /*
     * Process commands from stdin until there's an end-of-file.  Note
     * that we need to fetch the standard channels again after every
     * eval, since they may have been changed.
     */

    inChannel = Tcl_GetStdChannel(TCL_STDIN);
    if (inChannel) {
	Tcl_CreateChannelHandler(inChannel, TCL_READABLE|TCL_ACTIVE,
		StdinProc, (ClientData) inChannel);
    }
    if (tty) {
	Prompt(interp, 0);
    }
    outChannel = Tcl_GetStdChannel(TCL_STDOUT);
    if (outChannel) {
	Tcl_Flush(outChannel);
    }
    Tcl_DStringInit(&command);
    Tcl_CreateExitHandler((Tcl_ExitProc *) Tcl_DStringFree, (ClientData) &command);
    Tcl_DStringInit(&line);
    Tcl_ResetResult(interp);
 
    /*
     * Loop infinitely until all event handlers are passive. Then exit.
     * Rather than calling exit, invoke the "exit" command so that
     * users can replace "exit" with some other command to do additional
     * cleanup on exit.  The Tcl_Eval call should never return.
     */

done:
    while (Tcl_DoOneEvent(0)) {
#ifdef TCL_MEM_DEBUG 
	if (tclMemDumpFileName != (char *) NULL) {
	    Tcl_DeleteInterp(interp);
	    Tcl_Exit(0); 
	} 
#endif
    }
    sprintf(buffer, "exit %d", exitCode);
    Tcl_Eval(interp, buffer);
}

/*
 *----------------------------------------------------------------------
 *
 * StdinProc --
 *
 *	This procedure is invoked by the event dispatcher whenever
 *	standard input becomes readable.  It grabs the next line of
 *	input characters, adds them to a command being assembled, and
 *	executes the command if it's complete.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Could be almost arbitrary, depending on the command that's
 *	typed.
 *
 *----------------------------------------------------------------------
 */

    /* ARGSUSED */
static void
StdinProc(clientData, mask)
    ClientData clientData;		/* Not used. */
    int mask;				/* Not used. */
{
    static int gotPartial = 0;
    char *cmd;
    int code, count;
    Tcl_Channel newchan, chan = (Tcl_Channel) clientData;

    count = Tcl_Gets(chan, &line);

    if (count < 0) {
	if (!gotPartial) {
	    if (tty) {
		Tcl_Exit(0);
	    } else {
		Tcl_DeleteChannelHandler(chan, StdinProc, (ClientData) chan);
	    }
	    return;
	} else {
	    count = 0;
	}
    }

    (void) Tcl_DStringAppend(&command, Tcl_DStringValue(&line), -1);
    cmd = Tcl_DStringAppend(&command, "\n", -1);
    Tcl_DStringFree(&line);
    
    if (!Tcl_CommandComplete(cmd)) {
        gotPartial = 1;
        goto prompt;
    }
    gotPartial = 0;

    /*
     * Disable the stdin channel handler while evaluating the command;
     * otherwise if the command re-enters the event loop we might
     * process commands from stdin before the current command is
     * finished.  Among other things, this will trash the text of the
     * command being evaluated.
     */

    Tcl_CreateChannelHandler(chan, TCL_ACTIVE, StdinProc, (ClientData) chan);
    code = Tcl_RecordAndEval(interp, cmd, TCL_EVAL_GLOBAL);
    newchan = Tcl_GetChannel(interp, "stdin", NULL);
    if (newchan != chan) {
	Tcl_DeleteChannelHandler(chan, StdinProc, (ClientData) chan);
    }
    if (newchan) {
	Tcl_CreateChannelHandler(newchan, TCL_READABLE | TCL_ACTIVE, StdinProc,
		(ClientData) newchan);
    }
    Tcl_DStringFree(&command);
    if (*interp->result != 0) {
	if (code != TCL_OK) {
	    chan = Tcl_GetChannel(interp, "stderr", NULL);
	} else if (tty) {
	    chan = Tcl_GetChannel(interp, "stdout", NULL);
	} else {
	    chan = NULL;
	}
	if (chan) {
	    Tcl_Write(chan, interp->result, -1);
	    Tcl_Write(chan, "\n", 1);
	}
    }

    /*
     * Output a prompt.
     */

    prompt:
    if (tty) {
	Prompt(interp, gotPartial);
    }
    Tcl_ResetResult(interp);
}

/*
 *----------------------------------------------------------------------
 *
 * Prompt --
 *
 *	Issue a prompt on standard output, or invoke a script
 *	to issue the prompt.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	A prompt gets output, and a Tcl script may be evaluated
 *	in interp.
 *
 *----------------------------------------------------------------------
 */

static void
Prompt(interp, partial)
    Tcl_Interp *interp;			/* Interpreter to use for prompting. */
    int partial;			/* Non-zero means there already
					 * exists a partial command, so use
					 * the secondary prompt. */
{
    char *promptCmd;
    int code;
    Tcl_Channel outChannel, errChannel;


    promptCmd = Tcl_GetVar(interp,
	partial ? "tcl_prompt2" : "tcl_prompt1", TCL_GLOBAL_ONLY);
    if (promptCmd == NULL) {
	outChannel = Tcl_GetChannel(interp, "stdout", NULL);
defaultPrompt:
	if (!partial) {

            /*
             * We must check that outChannel is a real channel - it
             * is possible that someone has transferred stdout out of
             * this interpreter with "interp transfer".
             */

            if (outChannel != (Tcl_Channel) NULL) {
                Tcl_Write(outChannel, "% ", 2);
            }
	}
    } else {
	code = Tcl_Eval(interp, promptCmd);
	outChannel = Tcl_GetChannel(interp, "stdout", NULL);
	if (code != TCL_OK) {
	    Tcl_AddErrorInfo(interp,
		    "\n    (script that generates prompt)");
	    errChannel = Tcl_GetChannel(interp, "stderr", NULL);
            if (errChannel != (Tcl_Channel) NULL) {
                Tcl_Write(errChannel, interp->result, -1);
                Tcl_Write(errChannel, "\n", 1);
            }
	    goto defaultPrompt;
	} else if ((outChannel) && (*interp->result)) {
	    Tcl_Write(outChannel, interp->result, -1);
	}
    }
    if (outChannel != (Tcl_Channel) NULL) {
        Tcl_Flush(outChannel);
    }
}

/*
 *----------------------------------------------------------------------
 *
 * CheckmemCmd --
 *
 *	This is the command procedure for the "checkmem" command, which
 *	causes the application to exit after printing information about
 *	memory usage to the file passed to this command as its first
 *	argument.
 *
 * Results:
 *	Returns a standard Tcl completion code.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */
#ifdef TCL_MEM_DEBUG

	/* ARGSUSED */
static int
CheckmemCmd(clientData, interp, argc, argv)
    ClientData clientData;		/* Not used. */
    Tcl_Interp *interp;			/* Interpreter for evaluation. */
    int argc;				/* Number of arguments. */
    char *argv[];			/* String values of arguments. */
{
    if (argc != 2) {
	Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		" fileName\"", (char *) NULL);
	return TCL_ERROR;
    }
    strcpy(dumpFile, argv[1]);
    tclMemDumpFileName = dumpFile;
    return TCL_OK;
}
#endif
