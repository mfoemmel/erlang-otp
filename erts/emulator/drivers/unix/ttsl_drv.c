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

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "erl_driver.h"

static int ttysl_init(void);
static ErlDrvData ttysl_start(ErlDrvPort, char*);

#ifdef HAVE_TERMCAP  /* else make an empty driver that can not be opened */

#include "sys.h"
#include <ctype.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <signal.h>
#include <fcntl.h>
#include <locale.h>
#include <unistd.h>
#include <termios.h>
#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif
#ifdef HAVE_SYS_IOCTL_H
#include <sys/ioctl.h>
#endif

#define TRUE 1
#define FALSE 0

/* Termcap functions. */
int tgetent(char* bp, char *name);
int tgetnum(char* cap);
int tgetflag(char* cap);
char *tgetstr(char* cap, char** buf);
char *tgoto(char* cm, int col, int line);
int tputs(char* cp, int affcnt, int (*outc)(int c));

/* Terminal capabilites in which we are interested. */
static char *capbuf;
static char *up, *down, *left, *right;
static int cols, xn;
static volatile int cols_needs_update = FALSE;

/* The various opcodes. */
#define OP_PUTC 0
#define OP_MOVE 1
#define OP_INSC 2
#define OP_DELC 3
#define OP_BEEP 4
/* Control op */
#define CTRL_OP_GET_WINSIZE 100

static int lbuf_size = BUFSIZ;
#define MAXSIZE (1 << 16)

static byte *lbuf;		/* The current line buffer */
static int llen;		/* The current line length */
static byte *lc;		/* The current character pointer */
static int lpos;

#define COL(_l) ((_l) % cols)
#define LINE(_l) ((_l) / cols)

#define PAD '\n'
#define NL '\n'

/* Main interface functions. */
static void ttysl_stop(ErlDrvData);
static void ttysl_from_erlang(ErlDrvData, char*, int);
static void ttysl_from_tty(ErlDrvData, ErlDrvEvent);
static Sint16 get_sint16(char*);

static ErlDrvPort ttysl_port;
static int ttysl_fd;
static FILE *ttysl_out;

/* Functions that work on the line buffer. */
static int start_lbuf(void);
static int stop_lbuf(void);
static int put_chars(byte*,int);
static int move_rel(int);
static int ins_chars(byte*,int);
static int del_chars(int);
static byte *step_over_chars(int);
static int insert_buf(byte*,int);
static int write_buf(byte*,int);
static int outc(int c);
static int move_cursor(int,int);

/* Termcap functions. */
static int start_termcap(void);
static int stop_termcap(void);
static int move_left(int);
static int move_right(int);
static int move_up(int);
static int move_down(int);
static void update_cols(void);

/* Terminal setting functions. */
static int tty_init(int,int,int,int);
static int tty_set(int);
static int tty_reset(int);
static int ttysl_control(ErlDrvData, unsigned int, char *, int, char **, int);
static RETSIGTYPE suspend(int);
static RETSIGTYPE cont(int);
static RETSIGTYPE winch(int);

#  define IF_IMPL(x) x
#else
#  define IF_IMPL(x) NULL
#endif /* HAVE_TERMCAP */

/* Define the driver table entry. */
struct erl_drv_entry ttsl_driver_entry = {
    ttysl_init,
    ttysl_start,
    IF_IMPL(ttysl_stop),
    IF_IMPL(ttysl_from_erlang),
    IF_IMPL(ttysl_from_tty),
    NULL,
    "tty_sl",
    NULL,
    NULL,
    IF_IMPL(ttysl_control)
  };

static int ttysl_init(void)
{
#ifdef HAVE_TERMCAP
    ttysl_port = (ErlDrvPort)-1;
    ttysl_fd = -1;
    lbuf = NULL;		/* For line buffer handling */
    capbuf = NULL;		/* For termcap handling */
#endif
    return 0;
}

static ErlDrvData ttysl_start(ErlDrvPort port, char* buf)
{
#ifndef HAVE_TERMCAP
    return ERL_DRV_ERROR_GENERAL;
#else
    char *s, *t, c;
    int canon, echo, sig;	/* Terminal characteristics */
    int flag;
    extern int using_oldshell; /* set this to let the rest of erts know */

    if (ttysl_port != (ErlDrvPort)-1)
      return ERL_DRV_ERROR_GENERAL;

    if (!isatty(0) || !isatty(1))
	return ERL_DRV_ERROR_GENERAL;

    /* Set the terminal modes to default leave as is. */
    canon = echo = sig = 0;

    /* Parse the input parameters. */
    for (s = strchr(buf, ' '); s; s = t) {
	s++;
	/* Find end of this argument (start of next) and insert NUL. */
	if ((t = strchr(s, ' '))) {
	    c = *t;
	    *t = '\0';
	}
	if ((flag = ((*s == '+') ? 1 : ((*s == '-') ? -1 : 0)))) {
	    if (s[1] == 'c') canon = flag;
	    if (s[1] == 'e') echo = flag;
	    if (s[1] == 's') sig = flag;
	}
	else if ((ttysl_fd = open(s, O_RDWR, 0)) < 0)
	      return ERL_DRV_ERROR_GENERAL;
    }
    if (ttysl_fd < 0)
      ttysl_fd = 0;

    if (tty_init(ttysl_fd, canon, echo, sig) < 0 ||
	tty_set(ttysl_fd) < 0) {
	ttysl_port = (ErlDrvPort)-1;
	tty_reset(ttysl_fd);
	return ERL_DRV_ERROR_GENERAL;
    }

    /* Set up smart line and termcap stuff. */
    if (!start_lbuf() || !start_termcap()) {
	stop_lbuf();		/* Must free this */
	tty_reset(ttysl_fd);
	return ERL_DRV_ERROR_GENERAL;
    }

    /* Open the terminal and set the terminal */
    ttysl_out = fdopen(ttysl_fd, "w");

    setlocale(LC_CTYPE, "");	/* Set international environment */
    sys_sigset(SIGCONT, cont);
    sys_sigset(SIGWINCH, winch);

    driver_select(port, (ErlDrvEvent)(Uint)ttysl_fd, DO_READ, 1);
    ttysl_port = port;

    /* we need to know this when we enter the break handler */
    using_oldshell = 0;

    return (ErlDrvData)ttysl_port;	/* Nothing important to return */
#endif /* HAVE_TERMCAP */
}

#ifdef HAVE_TERMCAP

#define DEF_HEIGHT 24
#define DEF_WIDTH 80
static void ttysl_get_window_size(Uint32 *width, Uint32 *height)
{
#ifdef TIOCGWINSZ 
    struct winsize ws;
    if (ioctl(ttysl_fd,TIOCGWINSZ,&ws) == 0) {
	*width = (Uint32) ws.ws_col;
	*height = (Uint32) ws.ws_row;
	if (*width <= 0) 
	    *width = DEF_WIDTH;
	if (*height <= 0) 
	    *height = DEF_HEIGHT;
	return;
    }
#endif
    *width = DEF_WIDTH;
    *height = DEF_HEIGHT;
}
    
static int ttysl_control(ErlDrvData drv_data,
			 unsigned int command,
			 char *buf, int len,
			 char **rbuf, int rlen)
{
    char resbuff[2*sizeof(Uint32)];
    switch (command) {
    case CTRL_OP_GET_WINSIZE:
	{
	    Uint32 w,h;
	    ttysl_get_window_size(&w,&h);
	    memcpy(resbuff,&w,sizeof(Uint32));
	    memcpy(resbuff+sizeof(Uint32),&h,sizeof(Uint32));
	}
	break;
    default:
	return 0;
    }
    if (rlen < 2*sizeof(Uint32)) {
	*rbuf = driver_alloc(2*sizeof(Uint32));
    }
    memcpy(*rbuf,resbuff,2*sizeof(Uint32));
    return 2*sizeof(Uint32);
}


static void ttysl_stop(ErlDrvData ttysl_data)
{
    if (ttysl_port != (ErlDrvPort)-1) {
	stop_lbuf();
	stop_termcap();
	tty_reset(ttysl_fd);
	if (ttysl_fd != 0)
	  close(ttysl_fd);
	driver_select(ttysl_port, (ErlDrvEvent)(Uint)ttysl_fd, DO_READ, 0);
	sys_sigset(SIGCONT, SIG_DFL);
	sys_sigset(SIGWINCH, SIG_DFL);
    }
    ttysl_port = (ErlDrvPort)-1;
    ttysl_fd = -1;
    /* return TRUE; */
}

/*
 * Check that there is enough room in all buffers to copy all pad chars
 * and stiff we need If not, realloc lbuf.
 */
static int check_buf_size(byte *s, int n)
{
    int size;
    byte *tmp, *old_lbuf;
    size = 10;
    for (tmp = s; n > 0; --n, tmp++) {
	if (isprint(*tmp)) 
	    size++;
	else if (*tmp == '\t') 
	    size += 8;
	else if (*tmp >= 128) 
	    size += 4;
	else size += 2;
    }
    if (size + lpos >= lbuf_size) {

	lbuf_size = size + lpos + BUFSIZ;
	old_lbuf = lbuf;
	if ((lbuf = driver_realloc(lbuf, lbuf_size)) == NULL) {
	    driver_failure(ttysl_port, -1);
	    return(0);
	}
	lc = lbuf + (lc - old_lbuf);
    }
    return(1);
}

static void ttysl_from_erlang(ErlDrvData ttysl_data, char* buf, int count)
{
    if (lpos > MAXSIZE) 
	put_chars((byte*)"\n", 1);

    if (check_buf_size((byte*)buf+1, count-1) == 0)
	return; /*(-1); */

    switch (buf[0]) {
    case OP_PUTC:
	put_chars((byte*)buf+1, count-1);
	break;
    case OP_MOVE:
	move_rel(get_sint16(buf+1));
	break;
    case OP_INSC:
	ins_chars((byte*)buf+1, count-1);
	break;
    case OP_DELC:
	del_chars(get_sint16(buf+1));
	break;
    case OP_BEEP:
	outc('\007');
	break;
    default:
	/* Unknown op, just ignore. */
	break;
    }
    fflush(ttysl_out);
    return; /* TRUE; */
}

static void ttysl_from_tty(ErlDrvData ttysl_data, ErlDrvEvent fd)
{
    char b[1024];
    ssize_t i;

    if ((i = read((int)(Sint)fd, b, 1024)) >= 0)
      driver_output(ttysl_port, b, i);
    else
      driver_failure(ttysl_port, -1);
    /* return TRUE;*/
}

/* Procedures for putting and getting integers to/from strings. */
static Sint16 get_sint16(char *s)
{
    return ((*s << 8) | ((byte*)s)[1]);
}

static int start_lbuf(void)
{
    if (!lbuf && !(lbuf = (byte*) driver_alloc(lbuf_size)))
      return FALSE;
    llen = 0;
    lc = lbuf;
    lpos = 0;
    return TRUE;
}

static int stop_lbuf(void)
{
    if (lbuf) driver_free(lbuf);
    lbuf = NULL;
    return TRUE;
}

/* Put l characters from s into the buffer and output them. */
static int put_chars(byte *s, int l)
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
static int move_rel(int n)
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
static int ins_chars(byte *s, int l)
{
    int n, tl;
    byte *tbuf = NULL;		/* Suppress warning about use-before-set */

    /* Move tail of buffer to make space. */
    if ((tl = llen - lpos) > 0) {
	if ((tbuf = driver_alloc(tl)) == NULL)
	    return FALSE;
	memcpy(tbuf, lc, tl);
    }
    n = insert_buf(s, l);
    if (tl > 0) {
	memcpy(lc, tbuf, tl);
	driver_free(tbuf);
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
static int del_chars(int n)
{
    int i, l, r;
    byte *c;

    update_cols();

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
	  outc(' ');
	if (COL(llen+l) == 0 && xn)
	{
	   outc(' ');
	   move_left(1);
	}
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
	  outc(' ');
	if (COL(llen+l) == 0 && xn)
	{
	   outc(' ');
	   move_left(1);
	}
	move_cursor(llen + l, lpos);
    }
    return TRUE;
}

/* Step over n logical characters, check for overflow. */
static byte *step_over_chars(int n)
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
static int insert_buf(byte *s, int n)
{
    int pos;
    byte *start;

    /* Copy the string to lbuf expanding control characters. */
    for (pos = lpos, start = lc; n > 0; --n, s++) {
	if (isprint(*s)) {
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
	    outc('\r'); 
	    if (*s == '\n')
		outc('\n');
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
static int write_buf(byte *s, int n)
{
    update_cols();

    while (n > 0) {
	if (isprint(*s)) {
	    outc(*s);
	    --n; s++;
	}
	else if (*s == '\t') {
	    do {
		outc(' ');
		--n; s++;
	    } while (*s == PAD);
	}
	else if (*s >= 128) {
	    outc('\\');
	    outc(((*s >> 6) & 07) + '0');
	    outc(((*s >> 3) & 07) + '0');
	    outc(((*s >> 0) & 07) + '0');
	    n -= 4;
	    s += 4;
	}
	else {
	    outc('^');
	    outc(*s == 0177 ? '?' : *s | 0x40);
	    n -= 2;
	    s += 2;
	}
    }
    /* Check landed in first column of new line and have 'xn' bug. */
    n = s - lbuf;
    if (COL(n) == 0 && xn && n != 0) {
	outc(n >= llen ? ' ' : *s);
	move_left(1);
    }
    return TRUE;
}

/* The basic procedure for outputting one character. */
static int outc(int c)
{
    return (int)putc(c, ttysl_out);
}

static int move_cursor(int from, int to)
{
    int dc, dl;

    update_cols();

    dc = COL(to) - COL(from);
    dl = LINE(to) - LINE(from);
    if (dl > 0)
      move_down(dl);
    else if (dl < 0)
      move_up(-dl);
    if (dc > 0)
      move_right(dc);
    else if (dc < 0)
      move_left(-dc);
    return TRUE;
}

static int start_termcap(void)
{
    int eres;
    size_t envsz = 1024;
    char *env = NULL;
    char *c;

    capbuf = driver_alloc(1024);
    if (!capbuf)
	goto false;
    eres = erl_drv_getenv("TERM", capbuf, &envsz);
    if (eres == 0)
	env = capbuf;
    else if (eres < 0)
	goto false;
    else /* if (eres > 1) */ {
      char *envbuf = driver_alloc(envsz);
      if (!envbuf)
	  goto false;
      while (1) {
	  char *newenvbuf;
	  eres = erl_drv_getenv("TERM", envbuf, &envsz);
	  if (eres == 0)
	      break;
	  newenvbuf = driver_realloc(envbuf, envsz);
	  if (eres < 0 || !newenvbuf) {
	      env = newenvbuf ? newenvbuf : envbuf;
	      goto false;
	  }
	  envbuf = newenvbuf;
      }
      env = envbuf;
    }
    if (tgetent((char*)lbuf, env) <= 0)
      goto false;
    if (env != capbuf) {
	env = NULL;
	driver_free(env);
    }
    c = capbuf;
    cols = tgetnum("co");
    if (cols <= 0)
	cols = DEF_WIDTH;
    xn = tgetflag("xn");
    up = tgetstr("up", &c);
    if (!(down = tgetstr("do", &c)))
      down = "\n";
    if (!(left = tgetflag("bs") ? "\b" : tgetstr("bc", &c)))
      left = "\b";		/* Can't happen - but does on Solaris 2 */
    right = tgetstr("nd", &c);
    if (up && down && left && right)
      return TRUE;
 false:
    if (env && env != capbuf)
	driver_free(env);
    if (capbuf)
	driver_free(capbuf);
    capbuf = NULL;
    return FALSE;
}

static int stop_termcap(void)
{
    if (capbuf) driver_free(capbuf);
    capbuf = NULL;
    return TRUE;
}

static int move_left(int n)
{
    while (n-- > 0)
      tputs(left, 1, outc);
    return TRUE;
}

static int move_right(int n)
{
    while (n-- > 0)
      tputs(right, 1, outc);
    return TRUE;
}

static int move_up(int n)
{
    while (n-- > 0)
      tputs(up, 1, outc);
    return TRUE;
}

static int move_down(int n)
{
    while (n-- > 0)
      tputs(down, 1, outc);
    return TRUE;
}
		    

/*
 * Updates cols if terminal has resized (SIGWINCH). Should be called
 * at the start of any function that uses the COL or LINE macros. If
 * the terminal is resized after calling this function but before use
 * of the macros, then we may write to the wrong screen location.
 *
 * We cannot call this from the SIGWINCH handler because it uses
 * ioctl() which is not a safe function as listed in the signal(7)
 * man page.
 */
static void update_cols(void)
{
    Uint32 width, height;
 
    if (cols_needs_update) {
	cols_needs_update = FALSE;
	ttysl_get_window_size(&width, &height);
	cols = width;
    }
}
		    

/*
 * Put a terminal device into non-canonical mode with ECHO off.
 * Before doing so we first save the terminal's current mode,
 * assuming the caller will call the tty_reset() function
 * (also in this file) when it's done with raw mode.
 */

static struct termios tty_smode, tty_rmode;

static int tty_init(int fd, int canon, int echo, int sig)
{
    if (tcgetattr(fd, &tty_rmode) < 0)
      return -1;
    tty_smode = tty_rmode;

    /* Default characteristics for all usage including termcap output. */
    tty_smode.c_iflag &= ~ISTRIP;

    /* Turn canonical (line mode) on off. */
    if (canon > 0) {
	tty_smode.c_iflag |= ICRNL;
	tty_smode.c_lflag |= ICANON;
	tty_smode.c_oflag |= OPOST;
	tty_smode.c_cc[VEOF] = tty_rmode.c_cc[VEOF];
#ifdef VDSUSP
	tty_smode.c_cc[VDSUSP] = tty_rmode.c_cc[VDSUSP];
#endif
    }
    if (canon < 0) {
	tty_smode.c_iflag &= ~ICRNL;
	tty_smode.c_lflag &= ~ICANON;
	tty_smode.c_oflag &= ~OPOST;
	/* Must get these really right or funny effects can occur. */
	tty_smode.c_cc[VMIN] = 1;
	tty_smode.c_cc[VTIME] = 0;
#ifdef VDSUSP
	tty_smode.c_cc[VDSUSP] = 0;
#endif
    }

    /* Turn echo on or off. */
    if (echo > 0)
      tty_smode.c_lflag |= ECHO;
    if (echo < 0)
      tty_smode.c_lflag &= ~ECHO;

    /* Set extra characteristics for "RAW" mode, no signals. */
    if (sig > 0) {
	/* Ignore IMAXBEL as not POSIX. */
#ifndef QNX
	tty_smode.c_iflag |= (BRKINT|IGNPAR|ICRNL|IXON|IXANY);
#else
	tty_smode.c_iflag |= (BRKINT|IGNPAR|ICRNL|IXON);
#endif
	tty_smode.c_lflag |= (ISIG|IEXTEN);
    }
    if (sig < 0) {
	/* Ignore IMAXBEL as not POSIX. */
#ifndef QNX
	tty_smode.c_iflag &= ~(BRKINT|IGNPAR|ICRNL|IXON|IXANY);
#else
	tty_smode.c_iflag &= ~(BRKINT|IGNPAR|ICRNL|IXON);
#endif	
	tty_smode.c_lflag &= ~(ISIG|IEXTEN);
    }
    return 0;
}

/*
 * Set/restore a terminal's mode to whatever it was on the most
 * recent call to the tty_init() function above.
 */

static int tty_set(int fd)
{
    DEBUGF(("Setting tty...\n"));

    if (tcsetattr(fd, TCSANOW, &tty_smode) < 0)
	return(-1);
    return(0);
}

static int tty_reset(int fd)         /* of terminal device */
{
    DEBUGF(("Resetting tty...\n"));

    if (tcsetattr(fd, TCSANOW, &tty_rmode) < 0)
	return(-1);
    
    return(0);
}

/* 
 * Signal handler to cope with signals so that we can reset the tty
 * to the orignal settings
 */

static RETSIGTYPE suspend(int sig)
{
    if (tty_reset(ttysl_fd) < 0) {
	fprintf(stderr,"Can't reset tty \n");
	exit(1);
    }

    sys_sigset(sig, SIG_DFL);	/* Set signal handler to default */
    sys_sigrelease(sig);	/* Allow 'sig' to come through */
    kill(getpid(), sig);	/* Send ourselves the signal */
    sys_sigblock(sig);		/* Reset to old mask */
    sys_sigset(sig, suspend);	/* Reset signal handler */ 

    if (tty_set(ttysl_fd) < 0) {
	fprintf(stderr,"Can't set tty raw \n");
	exit(1);
    }
}

static RETSIGTYPE cont(int sig)
{
    if (tty_set(ttysl_fd) < 0) {
	fprintf(stderr,"Can't set tty raw\n");
	exit(1);
    }
}

static RETSIGTYPE winch(int sig)
{
    cols_needs_update = TRUE;
}
#endif /* HAVE_TERMCAP */
