/* 
 * tclCmdMZ.c --
 *
 *	This file contains the top-level command routines for most of
 *	the Tcl built-in commands whose names begin with the letters
 *	M to Z.  It contains only commands in the generic core (i.e.
 *	those that don't depend much upon UNIX facilities).
 *
 * Copyright (c) 1987-1993 The Regents of the University of California.
 * Copyright (c) 1994-1996 Sun Microsystems, Inc.
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 * SCCS: @(#) tclCmdMZ.c 1.68 96/10/12 17:05:57
 */

#include "tclInt.h"
#include "tclPort.h"

/*
 * Structure used to hold information about variable traces:
 */

typedef struct {
    int flags;			/* Operations for which Tcl command is
				 * to be invoked. */
    char *errMsg;		/* Error message returned from Tcl command,
				 * or NULL.  Malloc'ed. */
    int length;			/* Number of non-NULL chars. in command. */
    char command[4];		/* Space for Tcl command to invoke.  Actual
				 * size will be as large as necessary to
				 * hold command.  This field must be the
				 * last in the structure, so that it can
				 * be larger than 4 bytes. */
} TraceVarInfo;

/*
 * Forward declarations for procedures defined in this file:
 */

static char *		TraceVarProc _ANSI_ARGS_((ClientData clientData,
			    Tcl_Interp *interp, char *name1,
			    char *name2, int flags));

/*
 *----------------------------------------------------------------------
 *
 * Tcl_PwdCmd --
 *
 *	This procedure is invoked to process the "pwd" Tcl command.
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
Tcl_PwdCmd(dummy, interp, argc, argv)
    ClientData dummy;			/* Not used. */
    Tcl_Interp *interp;			/* Current interpreter. */
    int argc;				/* Number of arguments. */
    char **argv;			/* Argument strings. */
{
    char *dirName;

    if (argc != 1) {
	Tcl_AppendResult(interp, "wrong # args: should be \"",
		argv[0], "\"", (char *) NULL);
	return TCL_ERROR;
    }

    dirName = TclGetCwd(interp);
    if (dirName == NULL) {
	return TCL_ERROR;
    }
    interp->result = dirName;
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * Tcl_RegexpCmd --
 *
 *	This procedure is invoked to process the "regexp" Tcl command.
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
Tcl_RegexpCmd(dummy, interp, argc, argv)
    ClientData dummy;			/* Not used. */
    Tcl_Interp *interp;			/* Current interpreter. */
    int argc;				/* Number of arguments. */
    char **argv;			/* Argument strings. */
{
    int noCase = 0;
    int indices = 0;
    Tcl_RegExp regExpr;
    char **argPtr, *string, *pattern, *start, *end;
    int match = 0;			/* Initialization needed only to
					 * prevent compiler warning. */
    int i;
    Tcl_DString stringDString, patternDString;

    if (argc < 3) {
	wrongNumArgs:
	Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		" ?switches? exp string ?matchVar? ?subMatchVar ",
		"subMatchVar ...?\"", (char *) NULL);
	return TCL_ERROR;
    }
    argPtr = argv+1;
    argc--;
    while ((argc > 0) && (argPtr[0][0] == '-')) {
	if (strcmp(argPtr[0], "-indices") == 0) {
	    indices = 1;
	} else if (strcmp(argPtr[0], "-nocase") == 0) {
	    noCase = 1;
	} else if (strcmp(argPtr[0], "--") == 0) {
	    argPtr++;
	    argc--;
	    break;
	} else {
	    Tcl_AppendResult(interp, "bad switch \"", argPtr[0],
		    "\": must be -indices, -nocase, or --", (char *) NULL);
	    return TCL_ERROR;
	}
	argPtr++;
	argc--;
    }
    if (argc < 2) {
	goto wrongNumArgs;
    }

    /*
     * Convert the string and pattern to lower case, if desired, and
     * perform the matching operation.
     */

    if (noCase) {
	register char *p;

	Tcl_DStringInit(&patternDString);
	Tcl_DStringAppend(&patternDString, argPtr[0], -1);
	pattern = Tcl_DStringValue(&patternDString);
	for (p = pattern; *p != 0; p++) {
	    if (isupper(UCHAR(*p))) {
		*p = (char)tolower(UCHAR(*p));
	    }
	}
	Tcl_DStringInit(&stringDString);
	Tcl_DStringAppend(&stringDString, argPtr[1], -1);
	string = Tcl_DStringValue(&stringDString);
	for (p = string; *p != 0; p++) {
	    if (isupper(UCHAR(*p))) {
		*p = (char)tolower(UCHAR(*p));
	    }
	}
    } else {
	pattern = argPtr[0];
	string = argPtr[1];
    }
    regExpr = Tcl_RegExpCompile(interp, pattern);
    if (regExpr != NULL) {
	match = Tcl_RegExpExec(interp, regExpr, string, string);
    }
    if (noCase) {
	Tcl_DStringFree(&stringDString);
	Tcl_DStringFree(&patternDString);
    }
    if (regExpr == NULL) {
	return TCL_ERROR;
    }
    if (match < 0) {
	return TCL_ERROR;
    }
    if (!match) {
	interp->result = "0";
	return TCL_OK;
    }

    /*
     * If additional variable names have been specified, return
     * index information in those variables.
     */

    argc -= 2;
    for (i = 0; i < argc; i++) {
	char *result, info[50];

	Tcl_RegExpRange(regExpr, i, &start, &end);
	if (start == NULL) {
	    if (indices) {
		result = Tcl_SetVar(interp, argPtr[i+2], "-1 -1", 0);
	    } else {
		result = Tcl_SetVar(interp, argPtr[i+2], "", 0);
	    }
	} else {
	    if (indices) {
		sprintf(info, "%d %d", (int)(start - string),
			(int)(end - string - 1));
		result = Tcl_SetVar(interp, argPtr[i+2], info, 0);
	    } else {
		char savedChar, *first, *last;

		first = argPtr[1] + (start - string);
		last = argPtr[1] + (end - string);
		savedChar = *last;
		*last = 0;
		result = Tcl_SetVar(interp, argPtr[i+2], first, 0);
		*last = savedChar;
	    }
	}
	if (result == NULL) {
	    Tcl_AppendResult(interp, "couldn't set variable \"",
		    argPtr[i+2], "\"", (char *) NULL);
	    return TCL_ERROR;
	}
    }
    interp->result = "1";
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * Tcl_RegsubCmd --
 *
 *	This procedure is invoked to process the "regsub" Tcl command.
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
Tcl_RegsubCmd(dummy, interp, argc, argv)
    ClientData dummy;			/* Not used. */
    Tcl_Interp *interp;			/* Current interpreter. */
    int argc;				/* Number of arguments. */
    char **argv;			/* Argument strings. */
{
    int noCase = 0, all = 0;
    Tcl_RegExp regExpr;
    char *string, *pattern, *p, *firstChar, *newValue, **argPtr;
    int match, flags, code, numMatches;
    char *start, *end, *subStart, *subEnd;
    register char *src, c;
    Tcl_DString stringDString, patternDString;

    if (argc < 5) {
	wrongNumArgs:
	Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		" ?switches? exp string subSpec varName\"", (char *) NULL);
	return TCL_ERROR;
    }
    argPtr = argv+1;
    argc--;
    while (argPtr[0][0] == '-') {
	if (strcmp(argPtr[0], "-nocase") == 0) {
	    noCase = 1;
	} else if (strcmp(argPtr[0], "-all") == 0) {
	    all = 1;
	} else if (strcmp(argPtr[0], "--") == 0) {
	    argPtr++;
	    argc--;
	    break;
	} else {
	    Tcl_AppendResult(interp, "bad switch \"", argPtr[0],
		    "\": must be -all, -nocase, or --", (char *) NULL);
	    return TCL_ERROR;
	}
	argPtr++;
	argc--;
    }
    if (argc != 4) {
	goto wrongNumArgs;
    }

    /*
     * Convert the string and pattern to lower case, if desired.
     */

    if (noCase) {
	Tcl_DStringInit(&patternDString);
	Tcl_DStringAppend(&patternDString, argPtr[0], -1);
	pattern = Tcl_DStringValue(&patternDString);
	for (p = pattern; *p != 0; p++) {
	    if (isupper(UCHAR(*p))) {
		*p = (char)tolower(UCHAR(*p));
	    }
	}
	Tcl_DStringInit(&stringDString);
	Tcl_DStringAppend(&stringDString, argPtr[1], -1);
	string = Tcl_DStringValue(&stringDString);
	for (p = string; *p != 0; p++) {
	    if (isupper(UCHAR(*p))) {
		*p = (char)tolower(UCHAR(*p));
	    }
	}
    } else {
	pattern = argPtr[0];
	string = argPtr[1];
    }
    regExpr = Tcl_RegExpCompile(interp, pattern);
    if (regExpr == NULL) {
	code = TCL_ERROR;
	goto done;
    }

    /*
     * The following loop is to handle multiple matches within the
     * same source string;  each iteration handles one match and its
     * corresponding substitution.  If "-all" hasn't been specified
     * then the loop body only gets executed once.
     */

    flags = 0;
    numMatches = 0;
    for (p = string; *p != 0; ) {
	match = Tcl_RegExpExec(interp, regExpr, p, string);
	if (match < 0) {
	    code = TCL_ERROR;
	    goto done;
	}
	if (!match) {
	    break;
	}
	numMatches += 1;

	/*
	 * Copy the portion of the source string before the match to the
	 * result variable.
	 */

	Tcl_RegExpRange(regExpr, 0, &start, &end);
	src = argPtr[1] + (start - string);
	c = *src;
	*src = 0;
	newValue = Tcl_SetVar(interp, argPtr[3], argPtr[1] + (p - string),
		flags);
	*src = c;
	flags = TCL_APPEND_VALUE;
	if (newValue == NULL) {
	    cantSet:
	    Tcl_AppendResult(interp, "couldn't set variable \"",
		    argPtr[3], "\"", (char *) NULL);
	    code = TCL_ERROR;
	    goto done;
	}
    
	/*
	 * Append the subSpec argument to the variable, making appropriate
	 * substitutions.  This code is a bit hairy because of the backslash
	 * conventions and because the code saves up ranges of characters in
	 * subSpec to reduce the number of calls to Tcl_SetVar.
	 */
    
	for (src = firstChar = argPtr[2], c = *src; c != 0; src++, c = *src) {
	    int index;
    
	    if (c == '&') {
		index = 0;
	    } else if (c == '\\') {
		c = src[1];
		if ((c >= '0') && (c <= '9')) {
		    index = c - '0';
		} else if ((c == '\\') || (c == '&')) {
		    *src = c;
		    src[1] = 0;
		    newValue = Tcl_SetVar(interp, argPtr[3], firstChar,
			    TCL_APPEND_VALUE);
		    *src = '\\';
		    src[1] = c;
		    if (newValue == NULL) {
			goto cantSet;
		    }
		    firstChar = src+2;
		    src++;
		    continue;
		} else {
		    continue;
		}
	    } else {
		continue;
	    }
	    if (firstChar != src) {
		c = *src;
		*src = 0;
		newValue = Tcl_SetVar(interp, argPtr[3], firstChar,
			TCL_APPEND_VALUE);
		*src = c;
		if (newValue == NULL) {
		    goto cantSet;
		}
	    }
	    Tcl_RegExpRange(regExpr, index, &subStart, &subEnd);
	    if ((subStart != NULL) && (subEnd != NULL)) {
		char *first, *last, saved;
    
		first = argPtr[1] + (subStart - string);
		last = argPtr[1] + (subEnd - string);
		saved = *last;
		*last = 0;
		newValue = Tcl_SetVar(interp, argPtr[3], first,
			TCL_APPEND_VALUE);
		*last = saved;
		if (newValue == NULL) {
		    goto cantSet;
		}
	    }
	    if (*src == '\\') {
		src++;
	    }
	    firstChar = src+1;
	}
	if (firstChar != src) {
	    if (Tcl_SetVar(interp, argPtr[3], firstChar,
		    TCL_APPEND_VALUE) == NULL) {
		goto cantSet;
	    }
	}
	if (end == p) {
	    char tmp[2];

	    /*
	     * Always consume at least one character of the input string
	     * in order to prevent infinite loops.
	     */

	    tmp[0] = argPtr[1][p - string];
	    tmp[1] = 0;
	    newValue = Tcl_SetVar(interp, argPtr[3], tmp, flags);
	    if (newValue == NULL) {
		goto cantSet;
	    }
	    p = end + 1;
	} else {
	    p = end;
	}
	if (!all) {
	    break;
	}
    }

    /*
     * Copy the portion of the source string after the last match to the
     * result variable.
     */

    if ((*p != 0) || (numMatches == 0)) {
	if (Tcl_SetVar(interp, argPtr[3], argPtr[1] + (p - string), 
		flags) == NULL) {
	    goto cantSet;
	}
    }
    sprintf(interp->result, "%d", numMatches);
    code = TCL_OK;

    done:
    if (noCase) {
	Tcl_DStringFree(&stringDString);
	Tcl_DStringFree(&patternDString);
    }
    return code;
}

/*
 *----------------------------------------------------------------------
 *
 * Tcl_RenameCmd --
 *
 *	This procedure is invoked to process the "rename" Tcl command.
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
Tcl_RenameCmd(dummy, interp, argc, argv)
    ClientData dummy;			/* Not used. */
    Tcl_Interp *interp;			/* Current interpreter. */
    int argc;				/* Number of arguments. */
    char **argv;			/* Argument strings. */
{
    register Command *cmdPtr;
    Interp *iPtr = (Interp *) interp;
    Tcl_HashEntry *hPtr;
    int new;
    char *srcName, *dstName;

    if (argc != 3) {
	Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		" oldName newName\"", (char *) NULL);
	return TCL_ERROR;
    }
    if (argv[2][0] == '\0') {
	if (Tcl_DeleteCommand(interp, argv[1]) != 0) {
	    Tcl_AppendResult(interp, "can't delete \"", argv[1],
		    "\": command doesn't exist", (char *) NULL);
	    return TCL_ERROR;
	}
	return TCL_OK;
    }

    srcName = argv[1];
    dstName = argv[2];
    hPtr = Tcl_FindHashEntry(&iPtr->commandTable, dstName);
    if (hPtr != NULL) {
	Tcl_AppendResult(interp, "can't rename to \"", argv[2],
		"\": command already exists", (char *) NULL);
	return TCL_ERROR;
    }

    /*
     * The code below was added in 11/95 to preserve backwards compatibility
     * when "tkerror" was renamed "bgerror":  we guarantee that the hash
     * table entries for both commands refer to a single shared Command
     * structure.  This code should eventually become unnecessary.
     */

    if ((srcName[0] == 't') && (strcmp(srcName, "tkerror") == 0)) {
	srcName = "bgerror";
    }
    dstName = argv[2];
    if ((dstName[0] == 't') && (strcmp(dstName, "tkerror") == 0)) {
	dstName = "bgerror";
    }

    /*
     *  Look for the existing command:
     */

    cmdPtr = (Command *) Tcl_FindCommand(interp, srcName, 0);
    if (cmdPtr == NULL) {
	Tcl_AppendResult(interp, "can't ",
	    (*argv[2] == '\0') ? "delete" : "rename",
	    " \"", argv[1], "\": command doesn't exist",
	    (char*)NULL);
	return TCL_ERROR;
    }
    /*
     *  Either delete the existing command, or rename it.
     */
    /*
     * Prevent formation of alias loops through renaming.
     */
    
    if (TclPreventAliasLoop(interp, interp, dstName, cmdPtr->proc,
	    cmdPtr->clientData) != TCL_OK) {
        return TCL_ERROR;
    }

    /*
     *  Signal namespace changes first, then muck with
     *  the command hash table.  This avoids invalid memory
     *  references in the "synonyms" table.
     */

    Tcl_DeleteHashEntry(cmdPtr->hPtr);
    hPtr = Tcl_CreateHashEntry(&iPtr->commandTable, dstName, &new);
    Tcl_SetHashValue(hPtr, cmdPtr);
    cmdPtr->hPtr = hPtr;

    /*
     * The code below provides more backwards compatibility for the
     * "tkerror" => "bgerror" renaming.  As with the other compatibility
     * code above, it should eventually be removed.
     */

    if ((dstName[0] == 'b') && (strcmp(dstName, "bgerror") == 0)) {
	/*
	 * The destination command is "bgerror";  create a "tkerror"
	 * command that shares the same Command structure.
	 */

	hPtr = Tcl_CreateHashEntry(&iPtr->commandTable, "tkerror", &new);
	Tcl_SetHashValue(hPtr, cmdPtr);
    }
    if ((srcName[0] == 'b') && (strcmp(srcName, "bgerror") == 0)) {
	/*
	 * The source command is "bgerror":  delete the hash table
	 * entry for "tkerror" if it exists.
	 */

	Tcl_DeleteHashEntry(Tcl_FindHashEntry(&iPtr->commandTable, "tkerror"));
    }
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * Tcl_ReturnCmd --
 *
 *	This procedure is invoked to process the "return" Tcl command.
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
Tcl_ReturnCmd(dummy, interp, argc, argv)
    ClientData dummy;			/* Not used. */
    Tcl_Interp *interp;			/* Current interpreter. */
    int argc;				/* Number of arguments. */
    char **argv;			/* Argument strings. */
{
    Interp *iPtr = (Interp *) interp;
    int c, code;

    if (iPtr->errorInfo != NULL) {
	ckfree(iPtr->errorInfo);
	iPtr->errorInfo = NULL;
    }
    if (iPtr->errorCode != NULL) {
	ckfree(iPtr->errorCode);
	iPtr->errorCode = NULL;
    }
    code = TCL_OK;
    for (argv++, argc--; argc > 1; argv += 2, argc -= 2) {
	if (strcmp(argv[0], "-code") == 0) {
	    c = argv[1][0];
	    if ((c == 'o') && (strcmp(argv[1], "ok") == 0)) {
		code = TCL_OK;
	    } else if ((c == 'e') && (strcmp(argv[1], "error") == 0)) {
		code = TCL_ERROR;
	    } else if ((c == 'r') && (strcmp(argv[1], "return") == 0)) {
		code = TCL_RETURN;
	    } else if ((c == 'b') && (strcmp(argv[1], "break") == 0)) {
		code = TCL_BREAK;
	    } else if ((c == 'c') && (strcmp(argv[1], "continue") == 0)) {
		code = TCL_CONTINUE;
	    } else if (Tcl_GetInt(interp, argv[1], &code) != TCL_OK) {
		Tcl_ResetResult(interp);
		Tcl_AppendResult(interp, "bad completion code \"",
			argv[1], "\": must be ok, error, return, break, ",
			"continue, or an integer", (char *) NULL);
		return TCL_ERROR;
	    }
	} else if (strcmp(argv[0], "-errorinfo") == 0) {
	    iPtr->errorInfo = (char *) ckalloc((unsigned) (strlen(argv[1]) + 1));
	    strcpy(iPtr->errorInfo, argv[1]);
	} else if (strcmp(argv[0], "-errorcode") == 0) {
	    iPtr->errorCode = (char *) ckalloc((unsigned) (strlen(argv[1]) + 1));
	    strcpy(iPtr->errorCode, argv[1]);
	} else {
	    Tcl_AppendResult(interp, "bad option \"", argv[0],
		    ": must be -code, -errorcode, or -errorinfo",
		    (char *) NULL);
	    return TCL_ERROR;
	}
    }
    if (argc == 1) {
	Tcl_SetResult(interp, argv[0], TCL_VOLATILE);
    }
    iPtr->returnCode = code;
    return TCL_RETURN;
}

/*
 *----------------------------------------------------------------------
 *
 * Tcl_ScanCmd --
 *
 *	This procedure is invoked to process the "scan" Tcl command.
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
Tcl_ScanCmd(dummy, interp, argc, argv)
    ClientData dummy;			/* Not used. */
    Tcl_Interp *interp;			/* Current interpreter. */
    int argc;				/* Number of arguments. */
    char **argv;			/* Argument strings. */
{
#   define MAX_FIELDS 20
    typedef struct {
	char fmt;			/* Format for field. */
	int size;			/* How many bytes to allow for
					 * field. */
	char *location;			/* Where field will be stored. */
    } Field;
    Field fields[MAX_FIELDS];		/* Info about all the fields in the
					 * format string. */
    register Field *curField;
    int numFields = 0;			/* Number of fields actually
					 * specified. */
    int suppress;			/* Current field is assignment-
					 * suppressed. */
    int totalSize = 0;			/* Number of bytes needed to store
					 * all results combined. */
    char *results;			/* Where scanned output goes.
					 * Malloced; NULL means not allocated
					 * yet. */
    int numScanned;			/* sscanf's result. */
    register char *fmt;
    int i, widthSpecified, length, code;

    /*
     * The variables below are used to hold a copy of the format
     * string, so that we can replace format specifiers like "%f"
     * and "%F" with specifiers like "%lf"
     */

#   define STATIC_SIZE 5
    char copyBuf[STATIC_SIZE], *fmtCopy;
    register char *dst;

    if (argc < 3) {
	Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		" string format ?varName varName ...?\"", (char *) NULL);
	return TCL_ERROR;
    }

    /*
     * This procedure operates in four stages:
     * 1. Scan the format string, collecting information about each field.
     * 2. Allocate an array to hold all of the scanned fields.
     * 3. Call sscanf to do all the dirty work, and have it store the
     *    parsed fields in the array.
     * 4. Pick off the fields from the array and assign them to variables.
     */

    code = TCL_OK;
    results = NULL;
    length = strlen(argv[2]) * 2 + 1;
    if (length < STATIC_SIZE) {
	fmtCopy = copyBuf;
    } else {
	fmtCopy = (char *) ckalloc((unsigned) length);
    }
    dst = fmtCopy;
    for (fmt = argv[2]; *fmt != 0; fmt++) {
	*dst = *fmt;
	dst++;
	if (*fmt != '%') {
	    continue;
	}
	fmt++;
	if (*fmt == '%') {
	    *dst = *fmt;
	    dst++;
	    continue;
	}
	if (*fmt == '*') {
	    suppress = 1;
	    *dst = *fmt;
	    dst++;
	    fmt++;
	} else {
	    suppress = 0;
	}
	widthSpecified = 0;
	while (isdigit(UCHAR(*fmt))) {
	    widthSpecified = 1;
	    *dst = *fmt;
	    dst++;
	    fmt++;
	}
	if ((*fmt == 'l') || (*fmt == 'h') || (*fmt == 'L')) {
	    fmt++;
	}
	*dst = *fmt;
	dst++;
	if (suppress) {
	    continue;
	}
	if (numFields == MAX_FIELDS) {
	    interp->result = "too many fields to scan";
	    code = TCL_ERROR;
	    goto done;
	}
	curField = &fields[numFields];
	numFields++;
	switch (*fmt) {
	    case 'd':
	    case 'i':
	    case 'o':
	    case 'x':
		curField->fmt = 'd';
		curField->size = sizeof(int);
		break;

	    case 'u':
		curField->fmt = 'u';
		curField->size = sizeof(int);
		break;

	    case 's':
		curField->fmt = 's';
		curField->size = strlen(argv[1]) + 1;
		break;

	    case 'c':
                if (widthSpecified) {
	            interp->result = 
                         "field width may not be specified in %c conversion";
		    code = TCL_ERROR;
		    goto done;
                }
		curField->fmt = 'c';
		curField->size = sizeof(int);
		break;

	    case 'e':
	    case 'f':
	    case 'g':
		dst[-1] = 'l';
		dst[0] = 'f';
		dst++;
		curField->fmt = 'f';
		curField->size = sizeof(double);
		break;

	    case '[':
		curField->fmt = 's';
		curField->size = strlen(argv[1]) + 1;
		do {
		    fmt++;
		    if (*fmt == 0) {
			interp->result = "unmatched [ in format string";
			code = TCL_ERROR;
			goto done;
		    }
		    *dst = *fmt;
		    dst++;
		} while (*fmt != ']');
		break;

	    default:
		sprintf(interp->result, "bad scan conversion character \"%c\"",
			*fmt);
		code = TCL_ERROR;
		goto done;
	}
	curField->size = TCL_ALIGN(curField->size);
	totalSize += curField->size;
    }
    *dst = 0;

    if (numFields != (argc-3)) {
	interp->result =
		"different numbers of variable names and field specifiers";
	code = TCL_ERROR;
	goto done;
    }

    /*
     * Step 2:
     */

    results = (char *) ckalloc((unsigned) totalSize);
    for (i = 0, totalSize = 0, curField = fields;
	    i < numFields; i++, curField++) {
	curField->location = results + totalSize;
	totalSize += curField->size;
    }

    /*
     * Fill in the remaining fields with NULL;  the only purpose of
     * this is to keep some memory analyzers, like Purify, from
     * complaining.
     */

    for ( ; i < MAX_FIELDS; i++, curField++) {
	curField->location = NULL;
    }

    /*
     * Step 3:
     */

    numScanned = sscanf(argv[1], fmtCopy,
	    fields[0].location, fields[1].location, fields[2].location,
	    fields[3].location, fields[4].location, fields[5].location,
	    fields[6].location, fields[7].location, fields[8].location,
	    fields[9].location, fields[10].location, fields[11].location,
	    fields[12].location, fields[13].location, fields[14].location,
	    fields[15].location, fields[16].location, fields[17].location,
	    fields[18].location, fields[19].location);

    /*
     * Step 4:
     */

    if (numScanned < numFields) {
	numFields = numScanned;
    }
    for (i = 0, curField = fields; i < numFields; i++, curField++) {
	switch (curField->fmt) {
	    char string[TCL_DOUBLE_SPACE];

	    case 'd':
		sprintf(string, "%d", *((int *) curField->location));
		if (Tcl_SetVar(interp, argv[i+3], string, 0) == NULL) {
		    storeError:
		    Tcl_AppendResult(interp,
			    "couldn't set variable \"", argv[i+3], "\"",
			    (char *) NULL);
		    code = TCL_ERROR;
		    goto done;
		}
		break;

	    case 'u':
		sprintf(string, "%u", *((int *) curField->location));
		if (Tcl_SetVar(interp, argv[i+3], string, 0) == NULL) {
		    goto storeError;
		}
		break;

	    case 'c':
		sprintf(string, "%d", *((char *) curField->location) & 0xff);
		if (Tcl_SetVar(interp, argv[i+3], string, 0) == NULL) {
		    goto storeError;
		}
		break;

	    case 's':
		if (Tcl_SetVar(interp, argv[i+3], curField->location, 0)
			== NULL) {
		    goto storeError;
		}
		break;

	    case 'f':
		Tcl_PrintDouble(interp, *((double *) curField->location),
			string);
		if (Tcl_SetVar(interp, argv[i+3], string, 0) == NULL) {
		    goto storeError;
		}
		break;
	}
    }
    sprintf(interp->result, "%d", numScanned);
    done:
    if (results != NULL) {
	ckfree(results);
    }
    if (fmtCopy != copyBuf) {
	ckfree(fmtCopy);
    }
    return code;
}

/*
 *----------------------------------------------------------------------
 *
 * Tcl_DefineStaticFile --
 *
 *	This procedure is invoked to predefine scripts that can be
 *	executated with a modified source command, or get back with
 *	Tcl_GetStaticFile().
 *	See the user documentation for details on what it does.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	If necessary, memory is allocated for a new StaticFile structure.
 *
 *----------------------------------------------------------------------
 */

typedef struct StaticFileList {
    Tcl_StaticFile *entry;
    struct StaticFileList *next;
} StaticFileList;

static StaticFileList *firstStaticFile = NULL;

void
Tcl_DefineStaticFile(table)
    Tcl_StaticFile *table;
{
   StaticFileList *pointer = firstStaticFile;
   while (pointer && strcmp(table->name,pointer->entry->name)) {
	pointer = pointer->next;
   }
   if (!pointer) {
	pointer = (StaticFileList *) ckalloc(sizeof(StaticFileList));
	pointer->next = firstStaticFile;
	firstStaticFile = pointer;
   }
   pointer->entry = table;
}


/*
 *----------------------------------------------------------------------
 *
 * Tcl_GetStaticFile --
 *
 *	This procedure is invoked to get back scripts that are
 *	defined with Tcl_DefineStaticFile().
 *
 * Results:
 *	A pointer to an array of strings.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */

char **
Tcl_GetStaticFile(rsrcName)
    CONST char *rsrcName;
{
    StaticFileList *pointer = firstStaticFile;

    while (pointer) {
	Tcl_StaticFile *ptr = pointer->entry;
	while (ptr && ptr->name) {
	    if (!strcmp(rsrcName,ptr->name)) {
		return ptr->content;
	    }
	    ptr++;
	}

	pointer = pointer->next;
    }
    return (char **) NULL;
}


/*
 *----------------------------------------------------------------------
 *
 * Tcl_SourceCmd --
 *
 *	This procedure is invoked to process the "source" Tcl command.
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
Tcl_SourceCmd(dummy, interp, argc, argv)
    ClientData dummy;			/* Not used. */
    Tcl_Interp *interp;			/* Current interpreter. */
    int argc;				/* Number of arguments. */
    char **argv;			/* Argument strings. */
{
    int result = TCL_OK;

    if (argc == 3) {
	Tcl_DString data;
	char **p;

	if (strcmp(argv[1], "-rsrc")) {
	    Tcl_AppendResult(interp, "bad argument: should be \"", argv[0],
		    " fileName\" or \"", argv[0], " -rsrc name\"",
		    (char *) NULL);
	    return TCL_ERROR;
	}
	if ((p = Tcl_GetStaticFile(argv[2])) == (char **) NULL) {
	    Tcl_AppendResult(interp, "cannot source \"",
		    argv[2], "\": no such entry in resource", (char *) NULL);
	    return TCL_ERROR;
	}
	Tcl_DStringInit(&data);
	while(*p && (result == TCL_OK)) {
	    /* Copy the constant into a dynamic string. This */
	    /* is necessary because Tcl7.6 doesn't accept    */
	    /* constants as an argument to Tcl_Eval()        */
	    Tcl_DStringSetLength(&data,0);
	    Tcl_DStringAppend(&data,*p++,-1);
	    result = Tcl_Eval(interp,Tcl_DStringValue(&data));
	}
	Tcl_DStringFree(&data);
	if (result == TCL_RETURN) {
	    result = TclUpdateReturnInfo((Interp *) interp);
	} else if (result == TCL_ERROR) {
	    char msg[200];

	    /*
	     * Record information telling where the error occurred.
	     */

	    sprintf(msg, "\n    (resource \"%.150s\" line %d)", argv[2],
		    0);
	    Tcl_AddErrorInfo(interp, msg);
	}
	return result;
    }
    if (argc != 2) {
	Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		" fileName\" or \"", argv[0], " -rsrc name\"",
		(char *) NULL);
	return TCL_ERROR;
    }
    return Tcl_EvalFile(interp, argv[1]);
}

/*
 *----------------------------------------------------------------------
 *
 * Tcl_SplitCmd --
 *
 *	This procedure is invoked to process the "split" Tcl command.
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
Tcl_SplitCmd(dummy, interp, argc, argv)
    ClientData dummy;			/* Not used. */
    Tcl_Interp *interp;			/* Current interpreter. */
    int argc;				/* Number of arguments. */
    char **argv;			/* Argument strings. */
{
    char *splitChars;
    register char *p, *p2;
    char *elementStart;

    if (argc == 2) {
	splitChars = " \n\t\r";
    } else if (argc == 3) {
	splitChars = argv[2];
    } else {
	Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		" string ?splitChars?\"", (char *) NULL);
	return TCL_ERROR;
    }

    /*
     * Handle the special case of splitting on every character.
     */

    if (*splitChars == 0) {
	char string[2];
	string[1] = 0;
	for (p = argv[1]; *p != 0; p++) {
	    string[0] = *p;
	    Tcl_AppendElement(interp, string);
	}
	return TCL_OK;
    }

    /*
     * Normal case: split on any of a given set of characters.
     * Discard instances of the split characters.
     */

    for (p = elementStart = argv[1]; *p != 0; p++) {
	char c = *p;
	for (p2 = splitChars; *p2 != 0; p2++) {
	    if (*p2 == c) {
		*p = 0;
		Tcl_AppendElement(interp, elementStart);
		*p = c;
		elementStart = p+1;
		break;
	    }
	}
    }
    if (p != argv[1]) {
	Tcl_AppendElement(interp, elementStart);
    }
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * Tcl_StringCmd --
 *
 *	This procedure is invoked to process the "string" Tcl command.
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
Tcl_StringCmd(dummy, interp, argc, argv)
    ClientData dummy;			/* Not used. */
    Tcl_Interp *interp;			/* Current interpreter. */
    int argc;				/* Number of arguments. */
    char **argv;			/* Argument strings. */
{
    int length;
    register char *p;
    int match, c, first;
    int left = 0, right = 0;

    if (argc < 2) {
	Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		" option arg ?arg ...?\"", (char *) NULL);
	return TCL_ERROR;
    }
    c = argv[1][0];
    length = strlen(argv[1]);
    if ((c == 'c') && (strncmp(argv[1], "compare", length) == 0)) {
	if (argc != 4) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		    " compare string1 string2\"", (char *) NULL);
	    return TCL_ERROR;
	}
	match = strcmp(argv[2], argv[3]);
	if (match > 0) {
	    interp->result = "1";
	} else if (match < 0) {
	    interp->result = "-1";
	} else {
	    interp->result = "0";
	}
	return TCL_OK;
    } else if ((c == 'f') && (strncmp(argv[1], "first", length) == 0)) {
	if (argc != 4) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		    " first string1 string2\"", (char *) NULL);
	    return TCL_ERROR;
	}
	first = 1;

	firstLast:
	match = -1;
	c = *argv[2];
	length = strlen(argv[2]);
	for (p = argv[3]; *p != 0; p++) {
	    if (*p != c) {
		continue;
	    }
	    if (strncmp(argv[2], p, length) == 0) {
		match = p-argv[3];
		if (first) {
		    break;
		}
	    }
	}
	sprintf(interp->result, "%d", match);
	return TCL_OK;
    } else if ((c == 'i') && (strncmp(argv[1], "index", length) == 0)) {
	int index;

	if (argc != 4) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		    " index string charIndex\"", (char *) NULL);
	    return TCL_ERROR;
	}
	if (Tcl_GetInt(interp, argv[3], &index) != TCL_OK) {
	    return TCL_ERROR;
	}
	if ((index >= 0) && (index < (int) strlen(argv[2]))) {
	    interp->result[0] = argv[2][index];
	    interp->result[1] = 0;
	}
	return TCL_OK;
    } else if ((c == 'l') && (strncmp(argv[1], "last", length) == 0)
	    && (length >= 2)) {
	if (argc != 4) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		    " last string1 string2\"", (char *) NULL);
	    return TCL_ERROR;
	}
	first = 0;
	goto firstLast;
    } else if ((c == 'l') && (strncmp(argv[1], "length", length) == 0)
	    && (length >= 2)) {
	if (argc != 3) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		    " length string\"", (char *) NULL);
	    return TCL_ERROR;
	}
	sprintf(interp->result, "%d", strlen(argv[2]));
	return TCL_OK;
    } else if ((c == 'm') && (strncmp(argv[1], "match", length) == 0)) {
	if (argc != 4) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		    " match pattern string\"", (char *) NULL);
	    return TCL_ERROR;
	}
	if (Tcl_StringMatch(argv[3], argv[2]) != 0) {
	    interp->result = "1";
	} else {
	    interp->result = "0";
	}
	return TCL_OK;
    } else if ((c == 'r') && (strncmp(argv[1], "range", length) == 0)) {
	int last, stringLength;

	if (argc != 5) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		    " range string first last\"", (char *) NULL);
	    return TCL_ERROR;
	}
	stringLength = strlen(argv[2]);
	if ((*argv[3] == 'e')
		&& (strncmp(argv[3], "end", strlen(argv[3])) == 0)) {
	    first = stringLength-1;
	} else {
	    if (Tcl_GetInt(interp, argv[3], &first) != TCL_OK) {
		Tcl_ResetResult(interp);
		Tcl_AppendResult(interp,
			"expected integer or \"end\" but got \"",
			argv[3], "\"", (char *) NULL);
		return TCL_ERROR;
	    }
	}
	if ((*argv[4] == 'e')
		&& (strncmp(argv[4], "end", strlen(argv[4])) == 0)) {
	    last = stringLength-1;
	} else {
	    if (Tcl_GetInt(interp, argv[4], &last) != TCL_OK) {
		Tcl_ResetResult(interp);
		Tcl_AppendResult(interp,
			"expected integer or \"end\" but got \"",
			argv[4], "\"", (char *) NULL);
		return TCL_ERROR;
	    }
	}
	if (first < 0) {
	    first = 0;
	}
	if (last >= stringLength) {
	    last = stringLength-1;
	}
	if (last >= first) {
	    char saved;

	    p = argv[2] + last + 1;
	    saved = *p;
	    *p = 0;
	    Tcl_SetResult(interp, argv[2] + first, TCL_VOLATILE);
	    *p = saved;
	}
	return TCL_OK;
    } else if ((c == 't') && (strncmp(argv[1], "tolower", length) == 0)
	    && (length >= 3)) {
	if (argc != 3) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		    " tolower string\"", (char *) NULL);
	    return TCL_ERROR;
	}
	Tcl_SetResult(interp, argv[2], TCL_VOLATILE);
	for (p = interp->result; *p != 0; p++) {
	    if (isupper(UCHAR(*p))) {
		*p = (char)tolower(UCHAR(*p));
	    }
	}
	return TCL_OK;
    } else if ((c == 't') && (strncmp(argv[1], "toupper", length) == 0)
	    && (length >= 3)) {
	if (argc != 3) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		    " toupper string\"", (char *) NULL);
	    return TCL_ERROR;
	}
	Tcl_SetResult(interp, argv[2], TCL_VOLATILE);
	for (p = interp->result; *p != 0; p++) {
	    if (islower(UCHAR(*p))) {
		*p = (char) toupper(UCHAR(*p));
	    }
	}
	return TCL_OK;
    } else if ((c == 't') && (strncmp(argv[1], "trim", length) == 0)
	    && (length == 4)) {
	char *trimChars;
	register char *checkPtr;

	left = right = 1;

	trim:
	if (argc == 4) {
	    trimChars = argv[3];
	} else if (argc == 3) {
	    trimChars = " \t\n\r";
	} else {
	    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		    " ", argv[1], " string ?chars?\"", (char *) NULL);
	    return TCL_ERROR;
	}
	p = argv[2];
	if (left) {
	    for (c = *p; c != 0; p++, c = *p) {
		for (checkPtr = trimChars; *checkPtr != c; checkPtr++) {
		    if (*checkPtr == 0) {
			goto doneLeft;
		    }
		}
	    }
	}
	doneLeft:
	Tcl_SetResult(interp, p, TCL_VOLATILE);
	if (right) {
	    char *donePtr;

	    p = interp->result + strlen(interp->result) - 1;
	    donePtr = &interp->result[-1];
	    for (c = *p; p != donePtr; p--, c = *p) {
		for (checkPtr = trimChars; *checkPtr != c; checkPtr++) {
		    if (*checkPtr == 0) {
			goto doneRight;
		    }
		}
	    }
	    doneRight:
	    p[1] = 0;
	}
	return TCL_OK;
    } else if ((c == 't') && (strncmp(argv[1], "trimleft", length) == 0)
	    && (length > 4)) {
	left = 1;
	argv[1] = "trimleft";
	goto trim;
    } else if ((c == 't') && (strncmp(argv[1], "trimright", length) == 0)
	    && (length > 4)) {
	right = 1;
	argv[1] = "trimright";
	goto trim;
    } else if ((c == 'w') && (strncmp(argv[1], "wordend", length) == 0)
	    && (length > 4)) {
	int index, cur;
	char *string;

	if (argc != 4) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		    " ", argv[1], " string index\"", (char *) NULL);
	    return TCL_ERROR;
	}
	string = argv[2];
	if (Tcl_GetInt(interp, argv[3], &index) != TCL_OK) {
	    return TCL_ERROR;
	}
	length = strlen(argv[2]);
	if (index < 0) {
	    index = 0;
	}
	if (index >= (int) length) {
	    cur = length;
	    goto wordendDone;
	}
	for (cur = index ; cur < length; cur++) {
	    c = UCHAR(string[cur]);
	    if (!isalnum(c) && (c != '_')) {
		break;
	    }
	}
	if (cur == index) {
	    cur = index+1;
	}
	wordendDone:
	sprintf(interp->result, "%d", cur);
	return TCL_OK;
    } else if ((c == 'w') && (strncmp(argv[1], "wordstart", length) == 0)
	    && (length > 4)) {
	int index, cur;
	char *string;

	if (argc != 4) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		    " ", argv[1], " string index\"", (char *) NULL);
	    return TCL_ERROR;
	}
	string = argv[2];
	if (Tcl_GetInt(interp, argv[3], &index) != TCL_OK) {
	    return TCL_ERROR;
	}
	length = strlen(argv[2]);
	if (index >= (int) length) {
	    index = length-1;
	}
	if (index <= 0) {
	    cur = 0;
	    goto wordstartDone;
	}
	for (cur = index ; cur >= 0; cur--) {
	    c = UCHAR(string[cur]);
	    if (!isalnum(c) && (c != '_')) {
		break;
	    }
	}
	if (cur != index) {
	    cur += 1;
	}
	wordstartDone:
	sprintf(interp->result, "%d", cur);
	return TCL_OK;
    } else {
	Tcl_AppendResult(interp, "bad option \"", argv[1],
		"\": should be compare, first, index, last, length, match, ",
		"range, tolower, toupper, trim, trimleft, trimright, ",
		"wordend, or wordstart", (char *) NULL);
	return TCL_ERROR;
    }
}

/*
 *----------------------------------------------------------------------
 *
 * Tcl_SubstCmd --
 *
 *	This procedure is invoked to process the "subst" Tcl command.
 *	See the user documentation for details on what it does.  This
 *	command is an almost direct copy of an implementation by
 *	Andrew Payne.
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
Tcl_SubstCmd(dummy, interp, argc, argv)
    ClientData dummy;			/* Not used. */
    Tcl_Interp *interp;			/* Current interpreter. */
    int argc;				/* Number of arguments. */
    char **argv;			/* Argument strings. */
{
    Interp *iPtr = (Interp *) interp;
    Tcl_DString result;
    CONST char *p, *old;
    char *value;
    int code, count, doVars, doCmds, doBackslashes, i;
    size_t length;
    char c;

    /*
     * Parse command-line options.
     */

    doVars = doCmds = doBackslashes = 1;
    for (i = 1; i < (argc-1); i++) {
	p = argv[i];
	if (*p != '-') {
	    break;
	}
	length = strlen(p);
	if (length < 4) {
	    badSwitch:
	    Tcl_AppendResult(interp, "bad switch \"", p,
		    "\": must be -nobackslashes, -nocommands, ",
		    "or -novariables", (char *) NULL);
	    return TCL_ERROR;
	}
	if ((p[3] == 'b') && (strncmp(p, "-nobackslashes", length) == 0)) {
	    doBackslashes = 0;
	} else if ((p[3] == 'c') && (strncmp(p, "-nocommands", length) == 0)) {
	    doCmds = 0;
	} else if ((p[3] == 'v') && (strncmp(p, "-novariables", length) == 0)) {
	    doVars = 0;
	} else {
	    goto badSwitch;
	}
    }
    if (i != (argc-1)) {
	Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		" ?-nobackslashes? ?-nocommands? ?-novariables? string\"",
		(char *) NULL);
	return TCL_ERROR;
    }

    /*
     * Scan through the string one character at a time, performing
     * command, variable, and backslash substitutions.
     */

    Tcl_DStringInit(&result);
    old = p = argv[i];
    while (*p != 0) {
	switch (*p) {
	    case '\\':
		if (doBackslashes) {
		    if (p != old) {
			Tcl_DStringAppend(&result, old, p-old);
		    }
		    c = Tcl_Backslash((char *) p, &count);
		    Tcl_DStringAppend(&result, &c, 1);
		    p += count;
		    old = p;
		} else {
		    p++;
		}
		break;

	    case '$':
		if (doVars) {
		    if (p != old) {
			Tcl_DStringAppend(&result, old, p-old);
		    }
		    value = Tcl_ParseVar(interp, (char *) p, (char **) &p);
		    if (value == NULL) {
			Tcl_DStringFree(&result);
			return TCL_ERROR;
		    }
		    Tcl_DStringAppend(&result, value, -1);
		    old = p;
		} else {
		    p++;
		}
		break;

	    case '[':
		if (doCmds) {
		    if (p != old) {
			Tcl_DStringAppend(&result, old, p-old);
		    }
		    iPtr->evalFlags = TCL_BRACKET_TERM;
		    code = Tcl_Eval(interp, p+1);
		    if (code == TCL_ERROR) {
			Tcl_DStringFree(&result);
			return code;
		    }
		    old = p = iPtr->termPtr+1;
		    Tcl_DStringAppend(&result, iPtr->result, -1);
		    Tcl_ResetResult(interp);
		} else {
		    p++;
		}
		break;

	    default:
		p++;
		break;
	}
    }
    if (p != old) {
	Tcl_DStringAppend(&result, old, p-old);
    }
    Tcl_DStringResult(interp, &result);
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * Tcl_SwitchCmd --
 *
 *	This procedure is invoked to process the "switch" Tcl command.
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
Tcl_SwitchCmd(dummy, interp, argc, argv)
    ClientData dummy;			/* Not used. */
    Tcl_Interp *interp;			/* Current interpreter. */
    int argc;				/* Number of arguments. */
    char **argv;			/* Argument strings. */
{
#define EXACT	0
#define GLOB	1
#define REGEXP	2
    int i, code, mode, matched;
    int body;
    char *string;
    int switchArgc, splitArgs;
    char **switchArgv;

    switchArgc = argc-1;
    switchArgv = argv+1;
    mode = EXACT;
    while ((switchArgc > 0) && (*switchArgv[0] == '-')) {
	if (strcmp(*switchArgv, "-exact") == 0) {
	    mode = EXACT;
	} else if (strcmp(*switchArgv, "-glob") == 0) {
	    mode = GLOB;
	} else if (strcmp(*switchArgv, "-regexp") == 0) {
	    mode = REGEXP;
	} else if (strcmp(*switchArgv, "--") == 0) {
	    switchArgc--;
	    switchArgv++;
	    break;
	} else {
	    Tcl_AppendResult(interp, "bad option \"", switchArgv[0],
		    "\": should be -exact, -glob, -regexp, or --",
		    (char *) NULL);
	    return TCL_ERROR;
	}
	switchArgc--;
	switchArgv++;
    }
    if (switchArgc < 2) {
	Tcl_AppendResult(interp, "wrong # args: should be \"",
		argv[0], " ?switches? string pattern body ... ?default body?\"",
		(char *) NULL);
	return TCL_ERROR;
    }
    string = *switchArgv;
    switchArgc--;
    switchArgv++;

    /*
     * If all of the pattern/command pairs are lumped into a single
     * argument, split them out again.
     */

    splitArgs = 0;
    if (switchArgc == 1) {
	code = Tcl_SplitList(interp, switchArgv[0], &switchArgc, &switchArgv);
	if (code != TCL_OK) {
	    return code;
	}
	splitArgs = 1;
    }

    for (i = 0; i < switchArgc; i += 2) {
	if (i == (switchArgc-1)) {
	    interp->result = "extra switch pattern with no body";
	    code = TCL_ERROR;
	    goto cleanup;
	}

	/*
	 * See if the pattern matches the string.
	 */

	matched = 0;
	if ((*switchArgv[i] == 'd') && (i == switchArgc-2)
		&& (strcmp(switchArgv[i], "default") == 0)) {
	    matched = 1;
	} else {
	    switch (mode) {
		case EXACT:
		    matched = (strcmp(string, switchArgv[i]) == 0);
		    break;
		case GLOB:
		    matched = Tcl_StringMatch(string, switchArgv[i]);
		    break;
		case REGEXP:
		    matched = Tcl_RegExpMatch(interp, string, switchArgv[i]);
		    if (matched < 0) {
			code = TCL_ERROR;
			goto cleanup;
		    }
		    break;
	    }
	}
	if (!matched) {
	    continue;
	}

	/*
	 * We've got a match.  Find a body to execute, skipping bodies
	 * that are "-".
	 */

	for (body = i+1; ; body += 2) {
	    if (body >= switchArgc) {
		Tcl_AppendResult(interp, "no body specified for pattern \"",
			switchArgv[i], "\"", (char *) NULL);
		code = TCL_ERROR;
		goto cleanup;
	    }
	    if ((switchArgv[body][0] != '-') || (switchArgv[body][1] != 0)) {
		break;
	    }
	}
	code = Tcl_Eval(interp, switchArgv[body]);
	if (code == TCL_ERROR) {
	    char msg[100];
	    sprintf(msg, "\n    (\"%.50s\" arm line %d)", switchArgv[i],
		    interp->errorLine);
	    Tcl_AddErrorInfo(interp, msg);
	}
	goto cleanup;
    }

    /*
     * Nothing matched:  return nothing.
     */

    code = TCL_OK;

    cleanup:
    if (splitArgs) {
	ckfree((char *) switchArgv);
    }
    return code;
}

/*
 *----------------------------------------------------------------------
 *
 * Tcl_TclCmd --
 *
 *	This procedure is invoked to process the "tcl" Tcl command.
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
Tcl_TclCmd(dummy, interp, argc, argv)
    ClientData dummy;			/* Not used. */
    Tcl_Interp *interp;			/* Current interpreter. */
    int argc;				/* Number of arguments. */
    char **argv;			/* Argument strings. */
{
    size_t length;
    int c;

    if (argc < 2) {
	Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		" option arg ?arg ...?\"", (char *) NULL);
	return TCL_ERROR;
    }
    c = argv[1][0];
    length = strlen(argv[1]);
    if ((c == 'a') && (strncmp(argv[1], "append", length) == 0)
	    && (length >= 2)) {
	return Tcl_AppendCmd(dummy, interp, argc-1, argv+1);
    } else if ((c == 'a') && (strncmp(argv[1], "array", length) == 0)
	    && (length >= 2)) {
	return Tcl_ArrayCmd(dummy, interp, argc-1, argv+1);
    } else if ((c == 'b') && (strncmp(argv[1], "break", length) == 0)) {
	return Tcl_BreakCmd(dummy, interp, argc-1, argv+1);
    } else if ((c == 'c') && (strncmp(argv[1], "case", length) == 0)
	    && (length >= 3)) {
	return Tcl_CaseCmd(dummy, interp, argc-1, argv+1);
    } else if ((c == 'c') && (strncmp(argv[1], "catch", length) == 0)
	    && (length >= 3)) {
	return Tcl_CatchCmd(dummy, interp, argc-1, argv+1);
    } else if ((c == 'c') && (strncmp(argv[1], "clock", length) == 0)
	    && (length >= 4)) {
	return Tcl_ClockCmd(dummy, interp, argc-1, argv+1);
    } else if ((c == 'c') && (strncmp(argv[1], "concat", length) == 0)
	    && (length >= 4)) {
	return Tcl_ConcatCmd(dummy, interp, argc-1, argv+1);
    } else if ((c == 'c') && (strncmp(argv[1], "continue", length) == 0)
	    && (length >= 4)) {
	return Tcl_ContinueCmd(dummy, interp, argc-1, argv+1);
    } else if ((c == 'e') && (strncmp(argv[1], "error", length) == 0)
	    && (length >= 2)) {
	return Tcl_ErrorCmd(dummy, interp, argc-1, argv+1);
    } else if ((c == 'e') && (strncmp(argv[1], "eval", length) == 0)
	    && (length >= 2)) {
	return Tcl_EvalCmd(dummy, interp, argc-1, argv+1);
    } else if ((c == 'e') && (strncmp(argv[1], "exit", length) == 0)
	    && (length >= 3)) {
	return Tcl_ExitCmd(dummy, interp, argc-1, argv+1);
    } else if ((c == 'e') && (strncmp(argv[1], "expr", length) == 0)
	    && (length >= 3)) {
	return Tcl_ExprCmd(dummy, interp, argc-1, argv+1);
    } else if ((c == 'f') && (strncmp(argv[1], "fileevent", length) == 0)
	    && (length >= 5)) {
	return Tcl_FileEventCmd(dummy, interp, argc-1, argv+1);
    } else if ((c == 'f') && (strncmp(argv[1], "for", length) == 0)
	    && (length == 3)) {
	return Tcl_ForCmd(dummy, interp, argc-1, argv+1);
    } else if ((c == 'f') && (strncmp(argv[1], "foreach", length) == 0)
	    && (length >= 4)) {
	return Tcl_ForeachCmd(dummy, interp, argc-1, argv+1);
    } else if ((c == 'f') && (strncmp(argv[1], "format", length) == 0)
	    && (length >= 4)) {
	return Tcl_FormatCmd(dummy, interp, argc-1, argv+1);
    } else if ((c == 'g') && (strncmp(argv[1], "global", length) == 0)
	    && (length >= 5)) {
	return Tcl_GlobalCmd(dummy, interp, argc-1, argv+1);
    } else if ((c == 'h') && (strncmp(argv[1], "history", length) == 0)) {
	return Tcl_HistoryCmd(dummy, interp, argc-1, argv+1);
    } else if ((c == 'i') && (strncmp(argv[1], "if", length) == 0)
	    && (length == 2)) {
	return Tcl_IfCmd(dummy, interp, argc-1, argv+1);
    } else if ((c == 'i') && (strncmp(argv[1], "incr", length) == 0)
	    && (length >= 3)) {
	return Tcl_IncrCmd(dummy, interp, argc-1, argv+1);
    } else if ((c == 'i') && (strncmp(argv[1], "info", length) == 0)
	    && (length >= 3)) {
	return Tcl_InfoCmd(dummy, interp, argc-1, argv+1);
    } else if ((c == 'i') && (strncmp(argv[1], "interp", length) == 0)
	    && (length >= 3)) {
	return Tcl_InterpCmd(dummy, interp, argc-1, argv+1);
    } else if ((c == 'j') && (strncmp(argv[1], "join", length) == 0)) {
	return Tcl_JoinCmd(dummy, interp, argc-1, argv+1);
    } else if ((c == 'l') && (strncmp(argv[1], "lappend", length) == 0)
	    && (length >= 2)) {
	return Tcl_LappendCmd(dummy, interp, argc-1, argv+1);
    } else if ((c == 'l') && (strncmp(argv[1], "lindex", length) == 0)
	    && (length >= 4)) {
	return Tcl_LindexCmd(dummy, interp, argc-1, argv+1);
    } else if ((c == 'l') && (strncmp(argv[1], "linsert", length) == 0)
	    && (length >= 4)) {
	return Tcl_LinsertCmd(dummy, interp, argc-1, argv+1);
    } else if ((c == 'l') && (strncmp(argv[1], "list", length) == 0)
	    && (length >= 3)) {
	return Tcl_ListCmd(dummy, interp, argc-1, argv+1);
    } else if ((c == 'l') && (strncmp(argv[1], "llength", length) == 0)
	    && (length >= 2)) {
	return Tcl_LlengthCmd(dummy, interp, argc-1, argv+1);
    } else if ((c == 'l') && (strncmp(argv[1], "load", length) == 0)
	    && (length >= 2)) {
	return Tcl_LoadCmd(dummy, interp, argc-1, argv+1);
    } else if ((c == 'l') && (strncmp(argv[1], "lrange", length) == 0)
	    && (length >= 3)) {
	return Tcl_LrangeCmd(dummy, interp, argc-1, argv+1);
    } else if ((c == 'l') && (strncmp(argv[1], "lreplace", length) == 0)
	    && (length >= 3)) {
	return Tcl_LreplaceCmd(dummy, interp, argc-1, argv+1);
    } else if ((c == 'l') && (strncmp(argv[1], "lsearch", length) == 0)
	    && (length >= 3)) {
	return Tcl_LsearchCmd(dummy, interp, argc-1, argv+1);
    } else if ((c == 'l') && (strncmp(argv[1], "lsort", length) == 0)
	    && (length >= 3)) {
	return Tcl_LsortCmd(dummy, interp, argc-1, argv+1);
    } else if ((c == 'p') && (strncmp(argv[1], "package", length) == 0)
	    && (length >= 2)) {
	return Tcl_PackageCmd(dummy, interp, argc-1, argv+1);
    } else if ((c == 'p') && (strncmp(argv[1], "proc", length) == 0)
	    && (length >= 2)) {
	return Tcl_ProcCmd(dummy, interp, argc-1, argv+1);
    } else if ((c == 'r') && (strncmp(argv[1], "regexp", length) == 0)
	    && (length >= 4)) {
	return Tcl_RegexpCmd(dummy, interp, argc-1, argv+1);
    } else if ((c == 'r') && (strncmp(argv[1], "regsub", length) == 0)
	    && (length >= 4)) {
	return Tcl_RegsubCmd(dummy, interp, argc-1, argv+1);
    } else if ((c == 'r') && (strncmp(argv[1], "rename", length) == 0)
	    && (length >= 3)) {
	return Tcl_RenameCmd(dummy, interp, argc-1, argv+1);
    } else if ((c == 'r') && (strncmp(argv[1], "return", length) == 0)
	    && (length >= 3)) {
	return Tcl_ReturnCmd(dummy, interp, argc-1, argv+1);
    } else if ((c == 's') && (strncmp(argv[1], "scan", length) == 0)
	    && (length >= 2)) {
	return Tcl_ScanCmd(dummy, interp, argc-1, argv+1);
    } else if ((c == 's') && (strncmp(argv[1], "set", length) == 0)
	    && (length >= 3)) {
	return Tcl_SetCmd(dummy, interp, argc-1, argv+1);
    } else if ((c == 's') && (strncmp(argv[1], "split", length) == 0)
	    && (length >= 2)) {
	return Tcl_SplitCmd(dummy, interp, argc-1, argv+1);
    } else if ((c == 's') && (strncmp(argv[1], "string", length) == 0)
	    && (length >= 2)) {
	return Tcl_StringCmd(dummy, interp, argc-1, argv+1);
    } else if ((c == 's') && (strncmp(argv[1], "subst", length) == 0)
	    && (length >= 2)) {
	return Tcl_SubstCmd(dummy, interp, argc-1, argv+1);
    } else if ((c == 's') && (strncmp(argv[1], "switch", length) == 0)
	    && (length >= 2)) {
	return Tcl_SwitchCmd(dummy, interp, argc-1, argv+1);
    } else if ((c == 't') && (strncmp(argv[1], "trace", length) == 0)) {
	return Tcl_TraceCmd(dummy, interp, argc-1, argv+1);
    } else if ((c == 'u') && (strncmp(argv[1], "unset", length) == 0)
	    && (length >= 4)) {
	return Tcl_UnsetCmd(dummy, interp, argc-1, argv+1);
    } else if ((c == 'u') && (strncmp(argv[1], "unstack", length) == 0)
	    && (length >= 4)) {
	return Tcl_UnstackCmd(dummy, interp, argc-1, argv+1);
    } else if ((c == 'u') && (strncmp(argv[1], "upvar", length) == 0)
	    && (length >= 3)) {
	return Tcl_UpvarCmd(dummy, interp, argc-1, argv+1);
    } else if ((c == 'u') && (strncmp(argv[1], "uplevel", length) == 0)
	    && (length >= 3)) {
	return Tcl_UplevelCmd(dummy, interp, argc-1, argv+1);
    } else if ((c == 'w') && (strncmp(argv[1], "while", length) == 0)) {
	return Tcl_WhileCmd(dummy, interp, argc-1, argv+1);
#ifndef TCL_GENERIC_ONLY
    } else if ((c == 'a') && (strncmp(argv[1], "after", length) == 0)
	    && (length >= 2)) {
	return Tcl_AfterCmd(dummy, interp, argc-1, argv+1);
    } else if ((c == 'c') && (strncmp(argv[1], "cd", length) == 0)
	    && (length >= 2)) {
	return Tcl_CdCmd(dummy, interp, argc-1, argv+1);
    } else if ((c == 'c') && (strncmp(argv[1], "close", length) == 0)
	    && (length >= 4)) {
	return Tcl_CloseCmd(dummy, interp, argc-1, argv+1);
    } else if ((c == 'e') && (strncmp(argv[1], "eof", length) == 0)
	    && (length >= 2)) {
	return Tcl_CloseCmd(dummy, interp, argc-1, argv+1);
    } else if ((c == 'f') && (strncmp(argv[1], "fblocked", length) == 0)
	    && (length >= 2)) {
	return Tcl_FblockedCmd(dummy, interp, argc-1, argv+1);
    } else if ((c == 'f') && (strncmp(argv[1], "fconfigure", length) == 0)
	    && (length >= 2)) {
	return Tcl_FconfigureCmd(dummy, interp, argc-1, argv+1);
    } else if ((c == 'f') && (strncmp(argv[1], "file", length) == 0)
	    && (length >= 2)) {
	return Tcl_FileCmd(dummy, interp, argc-1, argv+1);
    } else if ((c == 'f') && (strncmp(argv[1], "flush", length) == 0)
	    && (length >= 2)) {
	return Tcl_FlushCmd(dummy, interp, argc-1, argv+1);
    } else if ((c == 'g') && (strncmp(argv[1], "gets", length) == 0)
	    && (length >= 2)) {
	return Tcl_GetsCmd(dummy, interp, argc-1, argv+1);
    } else if ((c == 'g') && (strncmp(argv[1], "glob", length) == 0)
	    && (length == 4)) {
	return Tcl_GlobCmd(dummy, interp, argc-1, argv+1);
    } else if ((c == 'o') && (strncmp(argv[1], "open", length) == 0)) {
	return Tcl_OpenCmd(dummy, interp, argc-1, argv+1);
    } else if ((c == 'p') && (strncmp(argv[1], "pid", length) == 0)
	    && (length >= 2)) {
	return Tcl_PidCmd(dummy, interp, argc-1, argv+1);
    } else if ((c == 'p') && (strncmp(argv[1], "puts", length) == 0)
	    && (length >= 2)) {
	return Tcl_PutsCmd(dummy, interp, argc-1, argv+1);
    } else if ((c == 'p') && (strncmp(argv[1], "pwd", length) == 0)
	    && (length >= 2)) {
	return Tcl_PwdCmd(dummy, interp, argc-1, argv+1);
    } else if ((c == 'r') && (strncmp(argv[1], "read", length) == 0)
	    && (length >= 3)) {
	return Tcl_ReadCmd(dummy, interp, argc-1, argv+1);
    } else if ((c == 's') && (strncmp(argv[1], "seek", length) == 0)
	    && (length >= 3)) {
	return Tcl_SeekCmd(dummy, interp, argc-1, argv+1);
    } else if ((c == 's') && (strncmp(argv[1], "socket", length) == 0)
	    && (length >= 2)) {
	return Tcl_SocketCmd(dummy, interp, argc-1, argv+1);
    } else if ((c == 't') && (strncmp(argv[1], "tell", length) == 0)
	    && (length >= 2)) {
	return Tcl_TellCmd(dummy, interp, argc-1, argv+1);
    } else if ((c == 't') && (strncmp(argv[1], "time", length) == 0)
	    && (length >= 2)) {
	return Tcl_TimeCmd(dummy, interp, argc-1, argv+1);
    } else if ((c == 'u') && (strncmp(argv[1], "update", length) == 0)) {
	return Tcl_UpdateCmd(dummy, interp, argc-1, argv+1);
    } else if ((c == 'v') && (strncmp(argv[1], "vwait", length) == 0)
	    && (length >= 2)) {
	return Tcl_VwaitCmd(dummy, interp, argc-1, argv+1);
    }
    Tcl_AppendResult(interp, "bad option \"", argv[1],
	    "\": should be after, append, array, break, case, catch, ",
	    "cd, clock, close, concat, continue, eof, error, eval, ",
	    "exit, expr, fblocked, fconfigure, fileevent, flush, for, ",
	    "foreach, format, gets, glob, global, history, if, incr, ",
	    "info, interp, join, lappend, lindex, linsert, list, ",
	    "llength, load, lrange, lreplace, lsearch, lsort, open, ",
	    "package, pid, proc, puts, pwd, read, regexp, regsub, ",
	    "rename, return, scan, seek, set, socket, split, string, ",
	    "subst, switch, tell, trace, unset, unstack, update, ",
	    "uplevel, upvar, vwait or while", (char *) NULL);
#else
    }
    Tcl_AppendResult(interp, "bad option \"", argv[1],
	    "\": should be append, array, break, case, catch, clock, ",
	    "concat, continue, error, eval, exit, expr, fileevent, for, ",
	    "foreach, format, global, history, if, incr, info, interp, ",
	    "join, lappend, lindex, linsert, list, llength, load, ",
	    "lrange, lreplace, lsearch, lsort, package, proc, regexp, ",
	    "regsub, rename, return, scan, set, split, string, subst, ",
	    "switch, trace, unset, unstack, uplevel, upvar or while",
	    (char *) NULL);
#endif
    return TCL_ERROR;
}

/*
 *----------------------------------------------------------------------
 *
 * Tcl_TimeCmd --
 *
 *	This procedure is invoked to process the "time" Tcl command.
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
Tcl_TimeCmd(dummy, interp, argc, argv)
    ClientData dummy;			/* Not used. */
    Tcl_Interp *interp;			/* Current interpreter. */
    int argc;				/* Number of arguments. */
    char **argv;			/* Argument strings. */
{
    int count, i, result;
    double timePer;
    Tcl_Time start, stop;

    if (argc == 2) {
	count = 1;
    } else if (argc == 3) {
	if (Tcl_GetInt(interp, argv[2], &count) != TCL_OK) {
	    return TCL_ERROR;
	}
    } else {
	Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		" command ?count?\"", (char *) NULL);
	return TCL_ERROR;
    }
    TclpGetTime(&start);
    for (i = count ; i > 0; i--) {
	result = Tcl_Eval(interp, argv[1]);
	if (result != TCL_OK) {
	    if (result == TCL_ERROR) {
		char msg[60];
		sprintf(msg, "\n    (\"time\" body line %d)",
			interp->errorLine);
		Tcl_AddErrorInfo(interp, msg);
	    }
	    return result;
	}
    }
    TclpGetTime(&stop);
    timePer = (stop.sec - start.sec)*1000000 + (stop.usec - start.usec);
    Tcl_ResetResult(interp);
    sprintf(interp->result, "%.0f microseconds per iteration",
	(count <= 0) ? 0 : timePer/count);
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * Tcl_TraceCmd --
 *
 *	This procedure is invoked to process the "trace" Tcl command.
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
Tcl_TraceCmd(dummy, interp, argc, argv)
    ClientData dummy;			/* Not used. */
    Tcl_Interp *interp;			/* Current interpreter. */
    int argc;				/* Number of arguments. */
    char **argv;			/* Argument strings. */
{
    int c;
    size_t length;

    if (argc < 2) {
	Tcl_AppendResult(interp, "too few args: should be \"",
		argv[0], " option [arg arg ...]\"", (char *) NULL);
	return TCL_ERROR;
    }
    c = argv[1][1];
    length = strlen(argv[1]);
    if ((c == 'a') && (strncmp(argv[1], "variable", length) == 0)
	    && (length >= 2)) {
	char *p;
	int flags;
	TraceVarInfo *tvarPtr;

	if (argc != 5) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"",
		    argv[0], " variable name ops command\"", (char *) NULL);
	    return TCL_ERROR;
	}

	flags = 0;
	for (p = argv[3] ; *p != 0; p++) {
	    if (*p == 'r') {
		flags |= TCL_TRACE_READS;
	    } else if (*p == 'w') {
		flags |= TCL_TRACE_WRITES;
	    } else if (*p == 'u') {
		flags |= TCL_TRACE_UNSETS;
	    } else {
		goto badOps;
	    }
	}
	if (flags == 0) {
	    goto badOps;
	}

	length = strlen(argv[4]);
	tvarPtr = (TraceVarInfo *) ckalloc((unsigned)
		(sizeof(TraceVarInfo) - sizeof(tvarPtr->command) + length + 1));
	tvarPtr->flags = flags;
	tvarPtr->errMsg = NULL;
	tvarPtr->length = length;
	flags |= TCL_TRACE_UNSETS;
	strcpy(tvarPtr->command, argv[4]);
	if (Tcl_TraceVar(interp, argv[2], flags, TraceVarProc,
		(ClientData) tvarPtr) != TCL_OK) {
	    ckfree((char *) tvarPtr);
	    return TCL_ERROR;
	}
    } else if ((c == 'd') && (strncmp(argv[1], "vdelete", length)
	    && (length >= 2)) == 0) {
	char *p;
	int flags;
	TraceVarInfo *tvarPtr;
	ClientData clientData;

	if (argc != 5) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"",
		    argv[0], " vdelete name ops command\"", (char *) NULL);
	    return TCL_ERROR;
	}

	flags = 0;
	for (p = argv[3] ; *p != 0; p++) {
	    if (*p == 'r') {
		flags |= TCL_TRACE_READS;
	    } else if (*p == 'w') {
		flags |= TCL_TRACE_WRITES;
	    } else if (*p == 'u') {
		flags |= TCL_TRACE_UNSETS;
	    } else {
		goto badOps;
	    }
	}
	if (flags == 0) {
	    goto badOps;
	}

	/*
	 * Search through all of our traces on this variable to
	 * see if there's one with the given command.  If so, then
	 * delete the first one that matches.
	 */

	length = strlen(argv[4]);
	clientData = 0;
	while ((clientData = Tcl_VarTraceInfo(interp, argv[2], 0,
		TraceVarProc, clientData)) != 0) {
	    tvarPtr = (TraceVarInfo *) clientData;
	    if ((tvarPtr->length == length) && (tvarPtr->flags == flags)
		    && (strncmp(argv[4], tvarPtr->command,
		    (size_t) length) == 0)) {
		Tcl_UntraceVar(interp, argv[2], flags | TCL_TRACE_UNSETS,
			TraceVarProc, clientData);
		if (tvarPtr->errMsg != NULL) {
		    ckfree(tvarPtr->errMsg);
		}
		ckfree((char *) tvarPtr);
		break;
	    }
	}
    } else if ((c == 'i') && (strncmp(argv[1], "vinfo", length) == 0)
	    && (length >= 2)) {
	ClientData clientData;
	char ops[4], *p;
	char *prefix = "{";

	if (argc != 3) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"",
		    argv[0], " vinfo name\"", (char *) NULL);
	    return TCL_ERROR;
	}
	clientData = 0;
	while ((clientData = Tcl_VarTraceInfo(interp, argv[2], 0,
		TraceVarProc, clientData)) != 0) {
	    TraceVarInfo *tvarPtr = (TraceVarInfo *) clientData;
	    p = ops;
	    if (tvarPtr->flags & TCL_TRACE_READS) {
		*p = 'r';
		p++;
	    }
	    if (tvarPtr->flags & TCL_TRACE_WRITES) {
		*p = 'w';
		p++;
	    }
	    if (tvarPtr->flags & TCL_TRACE_UNSETS) {
		*p = 'u';
		p++;
	    }
	    *p = '\0';
	    Tcl_AppendResult(interp, prefix, (char *) NULL);
	    Tcl_AppendElement(interp, ops);
	    Tcl_AppendElement(interp, tvarPtr->command);
	    Tcl_AppendResult(interp, "}", (char *) NULL);
	    prefix = " {";
	}
    } else {
	Tcl_AppendResult(interp, "bad option \"", argv[1],
		"\": should be variable, vdelete, or vinfo",
		(char *) NULL);
	return TCL_ERROR;
    }
    return TCL_OK;

    badOps:
    Tcl_AppendResult(interp, "bad operations \"", argv[3],
	    "\": should be one or more of rwu", (char *) NULL);
    return TCL_ERROR;
}

/*
 *----------------------------------------------------------------------
 *
 * TraceVarProc --
 *
 *	This procedure is called to handle variable accesses that have
 *	been traced using the "trace" command.
 *
 * Results:
 *	Normally returns NULL.  If the trace command returns an error,
 *	then this procedure returns an error string.
 *
 * Side effects:
 *	Depends on the command associated with the trace.
 *
 *----------------------------------------------------------------------
 */

	/* ARGSUSED */
static char *
TraceVarProc(clientData, interp, name1, name2, flags)
    ClientData clientData;	/* Information about the variable trace. */
    Tcl_Interp *interp;		/* Interpreter containing variable. */
    char *name1;		/* Name of variable or array. */
    char *name2;		/* Name of element within array;  NULL means
				 * scalar variable is being referenced. */
    int flags;			/* OR-ed bits giving operation and other
				 * information. */
{
    TraceVarInfo *tvarPtr = (TraceVarInfo *) clientData;
    char *result;
    int code;
    Interp dummy;
    Tcl_DString cmd;

    result = NULL;
    if (tvarPtr->errMsg != NULL) {
	ckfree(tvarPtr->errMsg);
	tvarPtr->errMsg = NULL;
    }
    if ((tvarPtr->flags & flags) && !(flags & TCL_INTERP_DESTROYED)) {

	/*
	 * Generate a command to execute by appending list elements
	 * for the two variable names and the operation.  The five
	 * extra characters are for three space, the opcode character,
	 * and the terminating null.
	 */

	if (name2 == NULL) {
	    name2 = "";
	}
	Tcl_DStringInit(&cmd);
	Tcl_DStringAppend(&cmd, tvarPtr->command, tvarPtr->length);
	Tcl_DStringAppendElement(&cmd, name1);
	Tcl_DStringAppendElement(&cmd, name2);
	if (flags & TCL_TRACE_READS) {
	    Tcl_DStringAppend(&cmd, " r", 2);
	} else if (flags & TCL_TRACE_WRITES) {
	    Tcl_DStringAppend(&cmd, " w", 2);
	} else if (flags & TCL_TRACE_UNSETS) {
	    Tcl_DStringAppend(&cmd, " u", 2);
	}

	/*
	 * Execute the command.  Be careful to save and restore the
	 * result from the interpreter used for the command.
	 */

	if (interp->freeProc == 0) {
	    dummy.freeProc = (Tcl_FreeProc *) 0;
	    dummy.result = "";
	    Tcl_SetResult((Tcl_Interp *) &dummy, interp->result, TCL_VOLATILE);
	} else {
	    dummy.freeProc = interp->freeProc;
	    dummy.result = interp->result;
	    interp->freeProc = (Tcl_FreeProc *) 0;
	}
	code = Tcl_Eval(interp, Tcl_DStringValue(&cmd));
	Tcl_DStringFree(&cmd);
	if (code != TCL_OK) {
	    tvarPtr->errMsg = (char *) ckalloc((unsigned) (strlen(interp->result) + 1));
	    strcpy(tvarPtr->errMsg, interp->result);
	    result = tvarPtr->errMsg;
	    Tcl_ResetResult(interp);		/* Must clear error state. */
	}
	Tcl_SetResult(interp, dummy.result,
		(dummy.freeProc == 0) ? TCL_VOLATILE : dummy.freeProc);
    }

    if (flags & TCL_TRACE_DESTROYED) {
	result = NULL;
	if (tvarPtr->errMsg != NULL) {
	    ckfree(tvarPtr->errMsg);
	}
	ckfree((char *) tvarPtr);
    }
    return result;
}

/*
 *----------------------------------------------------------------------
 *
 * Tcl_WhileCmd --
 *
 *	This procedure is invoked to process the "while" Tcl command.
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
Tcl_WhileCmd(dummy, interp, argc, argv)
    ClientData dummy;			/* Not used. */
    Tcl_Interp *interp;			/* Current interpreter. */
    int argc;				/* Number of arguments. */
    char **argv;			/* Argument strings. */
{
    int result, value;

    if (argc != 3) {
	Tcl_AppendResult(interp, "wrong # args: should be \"",
		argv[0], " test command\"", (char *) NULL);
	return TCL_ERROR;
    }

    while (1) {
	result = Tcl_ExprBoolean(interp, argv[1], &value);
	if (result != TCL_OK) {
	    return result;
	}
	if (!value) {
	    break;
	}
	result = Tcl_Eval(interp, argv[2]);
	if ((result != TCL_OK) && (result != TCL_CONTINUE)) {
	    if (result == TCL_ERROR) {
		char msg[60];
		sprintf(msg, "\n    (\"while\" body line %d)",
			interp->errorLine);
		Tcl_AddErrorInfo(interp, msg);
	    }
	    break;
	}
    }
    if (result == TCL_BREAK) {
	result = TCL_OK;
    }
    if (result == TCL_OK) {
	Tcl_ResetResult(interp);
    }
    return result;
}
