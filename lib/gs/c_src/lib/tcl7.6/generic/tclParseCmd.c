/* 
 * tclParseCmd.c --
 *
 *	This file contains a collection of procedures that are used
 *	to parse Tcl commands or parts of commands (like quoted
 *	strings or nested sub-commands).
 *
 * Copyright (c) 1987-1993 The Regents of the University of California.
 * Copyright (c) 1994-1995 Sun Microsystems, Inc.
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 * SCCS: @(#) tclParseCmd.c 1.51 96/09/06 09:47:29
 */

#include "tclInt.h"

/*
 * Function prototypes for procedures local to this file:
 */

static char *	QuoteEnd _ANSI_ARGS_((char *string, int term));
static char *	ScriptEnd _ANSI_ARGS_((char *p, int nested));
static char *	VarNameEnd _ANSI_ARGS_((char *string));

/*
 *----------------------------------------------------------------------
 *
 * Tcl_Backslash --
 *
 *	Figure out how to handle a backslash sequence.
 *
 * Results:
 *	The return value is the character that should be substituted
 *	in place of the backslash sequence that starts at src.  If
 *	readPtr isn't NULL then it is filled in with a count of the
 *	number of characters in the backslash sequence.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */

char
Tcl_Backslash(src, readPtr)
    char *src;			/* Points to the backslash character of
				 * a backslash sequence. */
    int *readPtr;		/* Fill in with number of characters read
				 * from src, unless NULL. */
{
    register char *p = src+1;
    char result;
    int count;

    count = 2;

    switch (*p) {
	/*
	 * Note: in the conversions below, use absolute values (e.g.,
	 * 0xa) rather than symbolic values (e.g. \n) that get converted
	 * by the compiler.  It's possible that compilers on some
	 * platforms will do the symbolic conversions differently, which
	 * could result in non-portable Tcl scripts.
	 */

	case 'a':
	    result = 0x7;
	    break;
	case 'b':
	    result = 0x8;
	    break;
	case 'f':
	    result = 0xc;
	    break;
	case 'n':
	    result = 0xa;
	    break;
	case 'r':
	    result = 0xd;
	    break;
	case 't':
	    result = 0x9;
	    break;
	case 'v':
	    result = 0xb;
	    break;
	case 'x':
	    if (isxdigit(UCHAR(p[1]))) {
		char *end;

		result = (char) strtoul(p+1, &end, 16);
		count = end - src;
	    } else {
		count = 2;
		result = 'x';
	    }
	    break;
	case '\n':
	    do {
		p++;
	    } while ((*p == ' ') || (*p == '\t'));
	    result = ' ';
	    count = p - src;
	    break;
	case 0:
	    result = '\\';
	    count = 1;
	    break;
	default:
	    if (isdigit(UCHAR(*p))) {
		result = (char)(*p - '0');
		p++;
		if (!isdigit(UCHAR(*p))) {
		    break;
		}
		count = 3;
		result = (char)((result << 3) + (*p - '0'));
		p++;
		if (!isdigit(UCHAR(*p))) {
		    break;
		}
		count = 4;
		result = (char)((result << 3) + (*p - '0'));
		break;
	    }
	    result = *p;
	    count = 2;
	    break;
    }

    if (readPtr != NULL) {
	*readPtr = count;
    }
    return result;
}

/*
 *----------------------------------------------------------------------
 *
 * TclWordEnd --
 *
 *	Given a pointer into a Tcl command, find the end of the next
 *	word of the command.
 *
 * Results:
 *	The return value is a pointer to the last character that's part
 *	of the word pointed to by "start".  If the word doesn't end
 *	properly within the string then the return value is the address
 *	of the null character at the end of the string.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */

char *
TclWordEnd(start, nested, semiPtr)
    char *start;		/* Beginning of a word of a Tcl command. */
    int nested;			/* Zero means this is a top-level command.
				 * One means this is a nested command (close
				 * bracket is a word terminator). */
    int *semiPtr;		/* Set to 1 if word ends with a command-
				 * terminating semi-colon, zero otherwise.
				 * If NULL then ignored. */
{
    register char *p;
    int count;

    if (semiPtr != NULL) {
	*semiPtr = 0;
    }

    /*
     * Skip leading white space (backslash-newline must be treated like
     * white-space, except that it better not be the last thing in the
     * command).
     */

    for (p = start; ; p++) {
	if (isspace(UCHAR(*p))) {
	    continue;
	}
	if ((p[0] == '\\') && (p[1] == '\n')) {
	    if (p[2] == 0) {
		return p+2;
	    }
	    continue;
	}
	break;
    }

    /*
     * Handle words beginning with a double-quote or a brace.
     */

    if (*p == '"') {
	p = QuoteEnd(p+1, '"');
	if (*p == 0) {
	    return p;
	}
	p++;
    } else if (*p == '{') {
	int braces = 1;
	while (braces != 0) {
	    p++;
	    while (*p == '\\') {
		(void) Tcl_Backslash(p, &count);
		p += count;
	    }
	    if (*p == '}') {
		braces--;
	    } else if (*p == '{') {
		braces++;
	    } else if (*p == 0) {
		return p;
	    }
	}
	p++;
    }

    /*
     * Handle words that don't start with a brace or double-quote.
     * This code is also invoked if the word starts with a brace or
     * double-quote and there is garbage after the closing brace or
     * quote.  This is an error as far as Tcl_Eval is concerned, but
     * for here the garbage is treated as part of the word.
     */

    while (1) {
	if (*p == '[') {
	    p = ScriptEnd(p+1, 1);
	    if (*p == 0) {
		return p;
	    }
	    p++;
	} else if (*p == '\\') {
	    if (p[1] == '\n') {
		/*
		 * Backslash-newline:  it maps to a space character
		 * that is a word separator, so the word ends just before
		 * the backslash.
		 */

		return p-1;
	    }
	    (void) Tcl_Backslash(p, &count);
	    p += count;
	} else if (*p == '$') {
	    p = VarNameEnd(p);
	    if (*p == 0) {
		return p;
	    }
	    p++;
	} else if (*p == ';') {
	    /*
	     * Include the semi-colon in the word that is returned.
	     */

	    if (semiPtr != NULL) {
		*semiPtr = 1;
	    }
	    return p;
	} else if (isspace(UCHAR(*p))) {
	    return p-1;
	} else if ((*p == ']') && nested) {
	    return p-1;
	} else if (*p == 0) {
	    if (nested) {
		/*
		 * Nested commands can't end because of the end of the
		 * string.
		 */
		return p;
	    }
	    return p-1;
	} else {
	    p++;
	}
    }
}

/*
 *----------------------------------------------------------------------
 *
 * QuoteEnd --
 *
 *	Given a pointer to a string that obeys the parsing conventions
 *	for quoted things in Tcl, find the end of that quoted thing.
 *	The actual thing may be a quoted argument or a parenthesized
 *	index name.
 *
 * Results:
 *	The return value is a pointer to the last character that is
 *	part of the quoted string (i.e the character that's equal to
 *	term).  If the quoted string doesn't terminate properly then
 *	the return value is a pointer to the null character at the
 *	end of the string.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */

static char *
QuoteEnd(string, term)
    char *string;		/* Pointer to character just after opening
				 * "quote". */
    int term;			/* This character will terminate the
				 * quoted string (e.g. '"' or ')'). */
{
    register char *p = string;
    int count;

    while (*p != term) {
	if (*p == '\\') {
	    (void) Tcl_Backslash(p, &count);
	    p += count;
	} else if (*p == '[') {
	    for (p++; *p != ']'; p++) {
		p = TclWordEnd(p, 1, (int *) NULL);
		if (*p == 0) {
		    return p;
		}
	    }
	    p++;
	} else if (*p == '$') {
	    p = VarNameEnd(p);
	    if (*p == 0) {
		return p;
	    }
	    p++;
	} else if (*p == 0) {
	    return p;
	} else {
	    p++;
	}
    }
    return p-1;
}

/*
 *----------------------------------------------------------------------
 *
 * VarNameEnd --
 *
 *	Given a pointer to a variable reference using $-notation, find
 *	the end of the variable name spec.
 *
 * Results:
 *	The return value is a pointer to the last character that
 *	is part of the variable name.  If the variable name doesn't
 *	terminate properly then the return value is a pointer to the
 *	null character at the end of the string.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */

static char *
VarNameEnd(string)
    char *string;		/* Pointer to dollar-sign character. */
{
    register char *p = string+1;

    if (*p == '{') {
	for (p++; (*p != '}') && (*p != 0); p++) {
	    /* Empty loop body. */
	}
	return p;
    }
    while (isalnum(UCHAR(*p)) || (*p == '_')) {
	p++;
    }
    if ((*p == '(') && (p != string+1)) {
	return QuoteEnd(p+1, ')');
    }
    return p-1;
}


/*
 *----------------------------------------------------------------------
 *
 * ScriptEnd --
 *
 *	Given a pointer to the beginning of a Tcl script, find the end of
 *	the script.
 *
 * Results:
 *	The return value is a pointer to the last character that's part
 *	of the script pointed to by "p".  If the command doesn't end
 *	properly within the string then the return value is the address
 *	of the null character at the end of the string.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */

static char *
ScriptEnd(p, nested)
    char *p;			/* Script to check. */
    int nested;			/* Zero means this is a top-level command.
				 * One means this is a nested command (the
				 * last character of the script must be
				 * an unquoted ]). */
{
    int commentOK = 1;
    int length;

    while (1) {
	while (isspace(UCHAR(*p))) {
	    if (*p == '\n') {
		commentOK = 1;
	    }
	    p++;
	}
	if ((*p == '#') && commentOK) {
	    do {
		if (*p == '\\') {
		    /*
		     * If the script ends with backslash-newline, then
		     * this command isn't complete.
		     */

		    if ((p[1] == '\n') && (p[2] == 0)) {
			return p+2;
		    }
		    Tcl_Backslash(p, &length);
		    p += length;
		} else {
		    p++;
		}
	    } while ((*p != 0) && (*p != '\n'));
	    continue;
	}
	p = TclWordEnd(p, nested, &commentOK);
	if (*p == 0) {
	    return p;
	}
	p++;
	if (nested) {
	    if (*p == ']') {
		return p;
	    }
	} else {
	    if (*p == 0) {
		return p-1;
	    }
	}
    }
}

/*
 *----------------------------------------------------------------------
 *
 * Tcl_CommandComplete --
 *
 *	Given a partial or complete Tcl command, this procedure
 *	determines whether the command is complete in the sense
 *	of having matched braces and quotes and brackets.
 *
 * Results:
 *	1 is returned if the command is complete, 0 otherwise.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */

int
Tcl_CommandComplete(cmd)
    char *cmd;				/* Command to check. */
{
    char *p;

    if (*cmd == 0) {
	return 1;
    }
    p = ScriptEnd(cmd, 0);
    return (*p != 0);
}
