#include <stdio.h>

/*
** Write pid on output descriptor and exec the program
** given as argument
*/

main(argc, argv) 
int argc; char** argv;
{
    char pid_buf[8];

    /* emit the pid */
    sprintf(pid_buf, "%d", getpid());
    write(1, pid_buf, strlen(pid_buf));

    /* execute */
    if ( argv[1][0] == '/' )  /* Absolute path given ? */
      execv( argv[1] , &argv[1] );
    else 
      execvp( argv[1] , &argv[1] );
    exit(1);
}
