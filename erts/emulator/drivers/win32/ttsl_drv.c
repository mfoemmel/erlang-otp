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
 * Tty driver that reads one character at the time and provides a
 * smart line for output.
 */

#include "sys.h"
#include <ctype.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <signal.h>

#include "driver.h"

#define TRUE 1
#define FALSE 0

static int cols;		/* Number of columns available. */
static int rows;		/* Number of rows available. */

/* The various opcodes. */
#define OP_PUTC 0
#define OP_MOVE 1
#define OP_INSC 2
#define OP_DELC 3
#define OP_BEEP 4

static int lbuf_size = BUFSIZ;
#define MAXSIZE (1 << 16)

#define ISPRINT(c) (isprint(c) || (128+32 <= (c) && (c) < 256))

/*
 * XXX These are used by win_con.c (for command history).
 * Should be cleaned up.
 */

byte *lbuf;		/* The current line buffer */
int llen;		/* The current line length */
byte *lc;		/* The current character pointer */
int lpos;

#define PAD '\n'
#define NL '\n'

/* Main interface functions. */
static int ttysl_init();
static long ttysl_start();
static int ttysl_stop();
static int ttysl_from_erlang();
static int ttysl_from_tty();
static sint16 get_sint16();

static int ttysl_port;

extern HANDLE console_input_event;
extern HANDLE console_thread;

static HANDLE ttysl_in = INVALID_HANDLE_VALUE; /* Handle for console input. */
static HANDLE ttysl_out = INVALID_HANDLE_VALUE;	/* Handle for console output */

/* Functions that work on the line buffer. */
static int start_lbuf();
static int stop_lbuf();
static int put_chars();
static int move_rel();
static int ins_chars();
static int del_chars();
static byte *step_over_chars(int n);
static int insert_buf();
static int write_buf();
static void move_cursor(int, int);

/* Define the driver table entry. */
struct driver_entry ttsl_driver_entry = {
    ttysl_init, ttysl_start, ttysl_stop, ttysl_from_erlang,
    ttysl_from_tty, null_func, "tty_sl"
};

static int ttysl_init()
{
    lbuf = NULL;		/* For line buffer handling */
    ttysl_port = -1;
    return TRUE;
}

static long ttysl_start(int port, char* buf)
{
    if (ttysl_port != -1 || console_thread == NULL) {
	return -1;
    }
    start_lbuf();
    driver_select(port, console_input_event, DO_READ, 1);
    ttysl_port = port;
    return ttysl_port;			/* Nothing important to return */
}

static int ttysl_stop(long ttysl_data)
{
    if (ttysl_port != -1) {
        driver_select(ttysl_port,console_input_event,DO_READ,0);
    }

    ttysl_in = ttysl_out = INVALID_HANDLE_VALUE;
    stop_lbuf();
    ttysl_port = -1;
    return TRUE;
}

/*
 * Check that there is enough room in all buffers to copy all pad chars
 * and stuff we need.  If not, realloc lbuf.
 */
static int check_buf_size(s,n)
byte *s;
int n;
{
    int size;
    byte *tmp, *old_lbuf;
    size = 10;
    for (tmp = s; n > 0; --n, tmp++) {
	if (ISPRINT(*tmp)) 
	    size++;
	else if (*tmp == '\t') 
	    size += 8;
	else if (*tmp >= 128) 
	    size += 4;
	else
	    size += 2;
    }
    if (size + lpos >= lbuf_size) {
	lbuf_size = size + lpos + BUFSIZ;
	old_lbuf = lbuf;
	if ((lbuf = realloc(lbuf, lbuf_size)) == NULL) {
	    driver_failure(ttysl_port, -1);
	    return(0);
	}
	lc = lbuf + (lc - old_lbuf);
    }
    return(1);
}

static int ttysl_from_erlang(ttysl_data, buf, count)
long ttysl_data;
byte *buf;
int count;
{
    if (lpos > MAXSIZE) 
	put_chars("\n", 1);
    
    if (check_buf_size(buf+1, count-1) == 0)
	return(-1);
    
    switch (buf[0]) {
    case OP_PUTC:
	put_chars(buf+1, count-1);
	break;
    case OP_MOVE:
	move_rel(get_sint16(buf+1));
	break;
    case OP_INSC:
	ins_chars(buf+1, count-1);
	break;
    case OP_DELC:
	del_chars(get_sint16(buf+1));
	break;
    case OP_BEEP:
	ConBeep();
	break;
    default:
	/* Unknown op, just ignore. */
	break;
    }
    return TRUE;
}

extern int read_inbuf(char *data, int n);
static int ttysl_from_tty(long ttysl_data, int fd)
{
   char inbuf[64];
   driver_output(ttysl_port,inbuf,ConReadInput(inbuf,1));   
   return TRUE;
}

/*
 * Gets signed 16 bit integer from binary buffer.
 */
static sint16 get_sint16(s)
char *s;
{
    return ((*s << 8) | ((byte*)s)[1]);
}


static int start_lbuf()
{
    if (!lbuf && !(lbuf = (byte*) malloc(lbuf_size)))
	return FALSE;
    llen = 0;
    lc = lbuf;
    lpos = 0;
    return TRUE;
}

static int stop_lbuf()
{
    if (lbuf)
	free(lbuf);
    lbuf = NULL;
    return TRUE;
}

/* Put l characters from s into the buffer and output them. */
static int put_chars(s, l)
byte *s;
int l;
{
    int n;

    n = insert_buf(s, l);
    if (n > 0)
	write_buf(lc - n, n);
    if (lpos > llen)
	llen = lpos;
    return TRUE;
}

/*
 * Move the current postition forwards or backwards within the current
 * line. We know about padding.
 */
static int move_rel(n)
int n;
{
    int npos;			/* The new position */
    byte *c;

    /* Step forwards or backwards over the buffer. */
    c = step_over_chars(n);

    /* Calculate move, updates pointers and move the cursor. */
    npos = c > lc ? lpos + (c - lc) : lpos - (lc - c);
    move_cursor(lpos, npos);
    lc = c;
    lpos = npos;
    return TRUE;
}

/* Insert characters into the buffer at the current position. */
static int ins_chars(s, l)
byte *s;
int l;
{
    int n, tl;
    byte *tbuf;
    
    /* Move tail of buffer to make space. */
    if ((tl = llen - lpos) > 0) {
	if ((tbuf = malloc(tl)) == NULL)
	    return FALSE;
	memcpy(tbuf, lc, tl);
    }
    n = insert_buf(s, l);
    if (tl > 0) {
	memcpy(lc, tbuf, tl);
	free(tbuf);
    }
    llen += n;
    write_buf(lc - n, llen - lpos + n);
    move_cursor(llen, lpos);
    return TRUE;
}

/*
 * Delete characters in the buffer. Can delete characters before (n < 0)
 * and after (n > 0) the current position. Cursor left at beginning of
 * deleted block.
 */
static int del_chars(n)
int n;
{
    int i, l, r;
    byte *c;

    /* Step forward or backwards over n logical characters. */
    c = step_over_chars(n);

    if (c > lc) {
	l = c - lc;		/* Buffer characters to delete */
	r = llen - lpos - l;	/* Characters after deleted */
	/* Fix up buffer and buffer pointers. */
	if (r > 0)
	    memcpy(lc, c, r);
	llen -= l;
	/* Write out characters after, blank the tail and jump back to lpos. */
	write_buf(lc, r);
	for (i = l ; i > 0; --i)
	  ConPutChar(' ');
	move_cursor(llen + l, lpos);
    }
    else if (c < lc) {
	l = lc - c;		/* Buffer characters */
	r = llen - lpos;	/* Characters after deleted */
	move_cursor(lpos, lpos-l);	/* Move back */
	/* Fix up buffer and buffer pointers. */
	if (r > 0)
	    memcpy(c, lc, r);
	lc = c;
	lpos -= l;
	llen -= l;
	/* Write out characters after, blank the tail and jump back to lpos. */
	write_buf(lc, r);
	for (i = l ; i > 0; --i)
	  ConPutChar(' ');
	move_cursor(llen + l, lpos);
    }
    return TRUE;
}

/* Step over n logical characters, check for overflow. */
static byte *step_over_chars(n)
int n;
{
    byte *c, *beg, *end;

    beg = lbuf;
    end = lbuf + llen;
    c = lc;
    for ( ; n > 0 && c < end; --n) {
	c++;
	while (c < end && *c == PAD)
	    c++;
    }
    for ( ; n < 0 && c > beg; n++) {
	--c;
	while (c > beg && *c == PAD )
	    --c;
    }
    return c;
}

/*
 * Insert n characters into the buffer at lc. Update pointers into buffer.
 * Know about pad characters and treat \n specially.
 */
static int insert_buf(byte* s, int n)
{
    int pos;
    byte *start;
    
    /* Copy the string to lbuf expanding control characters. */
    for (pos = lpos, start = lc; n > 0; --n, s++) {
	if (ISPRINT(*s)) {
	    *lc++ = *s;
	    pos++;
	}
	else if (*s == '\t') {
	    *lc++ = '\t';
	    for (pos++; pos % 8; pos++)
		*lc++ = PAD;
	}
	else if (*s == '\n' || *s == '\r') {
	    write_buf(start, lc - start);
	    ConPutChar('\r'); 
	    if (*s == '\n')
		ConPutChar('\n');
	    if (llen > pos) 
		memcpy(lbuf, lc, llen - pos);
		    lc = start = lbuf;
	    llen = lpos = pos = 0;
	}
	    
	else if (*s >= 128) {	/* "Meta" characters printed as \nnn */
	    *lc++ = *s;
	    *lc++ = PAD;
	    *lc++ = PAD;
	    *lc++ = PAD;
	    pos += 4;
	}
	else {
	    *lc++ = *s;
	    *lc++ = PAD;
	    pos += 2;
	}
    }
    lpos = pos;
    return lc - start;		/* Return characters written */
}

/*
 * Write n characters in line buffer starting at s. Be smart about
 * non-printables. Know about pad characters and that \n can never
 * occur normally.
 */
static int write_buf(s, n)
byte *s;
int n;
{
    while (n > 0) {
	if (ISPRINT(*s)) {
	    ConPutChar(*s);
	    --n;
	    s++;
	}
	else if (*s == '\t') {
	    do {
		ConPutChar(' ');
		--n; s++;
	    } while (*s == PAD);
	}
	else if (*s >= 128) {
	    ConPutChar('\\');
	    ConPutChar(((*s >> 6) & 07) + '0');
	    ConPutChar(((*s >> 3) & 07) + '0');
	    ConPutChar(((*s >> 0) & 07) + '0');
	    n -= 4;
	    s += 4;
	}
	else {
	    ConPutChar('^');
	    ConPutChar(*s == 0177 ? '?' : *s | 0x40);
	    n -= 2;
	    s += 2;
	}
    }
    return TRUE;
}

static void
move_cursor(int from, int to)
{
    ConSetCursor(from,to);
}
