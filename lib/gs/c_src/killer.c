
#include <stdio.h>
#include <signal.h>
/*
** Program that wait's for stdio to close (should be a pipe)
** Then it kills the process given as second argument
** with signal given as first argument
** if a 'Q' is received on stdin then the operation is canceled
*/
struct sig_str {
    char* sig_name;
    int   sig_no;
} sig_tab[] = {

"SIGHUP", SIGHUP,
"SIGINT", SIGINT,
"SIGQUIT", SIGQUIT,
"SIGILL", SIGILL,
"SIGTRAP", SIGTRAP,
"SIGABRT", SIGABRT,
"SIGIOT", SIGIOT,
#ifndef linux
"SIGEMT", SIGEMT,
#endif
"SIGFPE", SIGFPE,
"SIGKILL", SIGKILL,
"SIGBUS", SIGBUS,
"SIGSEGV", SIGSEGV,
#ifndef linux
"SIGSYS", SIGSYS,
#endif
"SIGPIPE", SIGPIPE,
"SIGALRM", SIGALRM,
"SIGTERM", SIGTERM,
"SIGURG", SIGURG,
"SIGSTOP", SIGSTOP,
"SIGTSTP", SIGTSTP,
"SIGCONT", SIGCONT,
"SIGCHLD", SIGCHLD,
"SIGTTIN", SIGTTIN,
"SIGTTOU", SIGTTOU,
"SIGIO"  , SIGIO  ,
"SIGXCPU", SIGXCPU,
"SIGXFSZ", SIGXFSZ,
"SIGVTALRM", SIGVTALRM,
"SIGPROF", SIGPROF,
"SIGWINCH", SIGWINCH,
/* "SIGINFO", SIGINFO, */
"SIGUSR1", SIGUSR1,
"SIGUSR2", SIGUSR2,
NULL, -1};

usage() 
{
    fprintf(stderr, "killer: usage <signal> <pid>\n");
    exit(1);
}

main(argc, argv)
int argc; char** argv;
{
    char* p;
    char buf[1];
    int sig = -1;
    int pid;
    int n;
    
    if (argc != 3)
	usage();

    p = argv[1];
    if (isdigit(p[0])) {
	sig = (p[0]-'0');
	if (isdigit(p[1]))
	    sig = sig*10 + (p[1]-'0');
	else if (p[1] != '\0')
	    usage();
    }
    else {
	int i = 0;

	while(sig_tab[i].sig_name != NULL) {
	    if (strcmp(sig_tab[i].sig_name, argv[1]) == 0) {
		sig = sig_tab[i].sig_no;
		break;
	    }
	    i++;
	}
	if (sig == -1) 
	    usage();
    }

    p = argv[2];
    while(*p != '\0') {
	if (!isdigit(*p))
	    usage();
	p++;
    }

    pid = atoi(argv[2]);


    if ((n = read(0, buf, 1)) == 0) {
	kill(pid, sig);
	exit(0);
    }
    else if (n == 1) {
	if (buf[0] == 'Q')
	    exit(0);
	else {
	    fprintf(stderr, "killer: got bad input (%d)\n", buf[0]);
	    exit(1);
	}
    }
    else {
	fprintf(stderr, "killer: bad read on input port\n");
	exit(1);
    }
}
