#include <string.h>
#include "ose.h"
#include "efs.h"
#include "efs_err.h"

#include "errno.h"
#include "stdio.h"
#include "unistd.h"

#ifdef USE_OSEDEF_H
#include "osedef.h"
#endif

#ifdef SHELL_SERUNIT
#ifdef SHELL_SERNAME
#include "login.sig"
#else
#error SHELL_SERUNIT is defined, but not SHELL_SERNAME!
#endif
#endif

#define TTYNAME "/tty"

#define STR(x) #x
#define STRSTR(x) STR(x)

struct Passwd
{
  const char *username;
  const char *password;
  OSUSER userid;
  OSENTRYPOINT *shell;
};

OS_PROCESS(no_shell)
{
  printf("Shell logins not allowed.\n");
  exit(0);
}

OS_PROCESS(no_telnet)
{
  printf("Telnet logins not allowed.\n");
  exit(0);
}

Boolean
validate_user(const char *username,
              const char *password,
              const char *termname,
	      OSUSER *user,
	      OSENTRYPOINT **shell)
{
  extern OSENTRYPOINT ose_shell;
  static const struct Passwd table[] =
  {
    /* For now the user id. must be zero. */
    { "",     "",      0, ose_shell },
    { "olli", "bolli", 0, ose_shell },
    { "ftp", "",       0, no_shell },
    { "anonymous", "", 0, no_shell },
    { NULL, NULL, 0, NULL }
  };
  unsigned i;

  printf("\nVALIDATE USER CALLED! %s, %s, %s\n", username, password, termname);

  for (i = 0; table[i].username != NULL; i++)
  {
    if (strcmp(table[i].username, username) == 0 &&
	( password == NULL ||
	  strcmp(table[i].password, password) == 0 ) )
    {
      if (strcmp(username, "root") == 0 &&
	  strncmp(termname, "/telnet", 7) == 0)
      {
	*user  = table[i].userid;
	*shell = no_telnet;
      }
      else
      {
	*user  = table[i].userid;
	*shell = table[i].shell;
      }
      return True;
    }
  }
  return False;
}

OS_PROCESS(start_shell)
{
#ifdef SHELL_SERUNIT

  /* Mount serial port and open it as stdin/stdout/stderr. */
  {
    int fd0, fd1, fd2;
    if (efs_mount(TTYNAME, "confm", "serdd",
		  "unit=0,hwname="SHELL_SERNAME",hwunit="STRSTR(SHELL_SERUNIT)",baudrate=9600") != 0)
    {
      error2(OSE_EFS_STARTEFS_EMOUNT_TTY_FAILED, (OSERRCODE)errno);
    }
    fd0 = open(TTYNAME, O_RDONLY, 0);
    fd1 = open(TTYNAME, O_WRONLY | O_TEXT, 0);
    fd2 = open(TTYNAME, O_WRONLY | O_TEXT, 0);
    if (fd0 != 0 || fd1 != 1 || fd2 != 2)
    {
      error2(OSE_EFS_STARTEFS_EOPEN_TTY_FAILED, (OSERRCODE)errno);
    }
  }

  /* Spawn the login process in the same block (main). */
  {
    extern OSENTRYPOINT ose_login;
    PROCESS login_ = create_process(OS_BG_PROC,
				    "ose_login",
				    ose_login,
				    1000,
				    (OSPRIORITY) 0,
				    (OSTIME) 0,
				    (PROCESS) 0,
				    NULL,
				    (OSVECTOR) 0,
				    (OSUSER) 0);
    efs_clone(login_);
    start(login_);		
    sendLoginInit(0, login_);
  }

  /* All initialisations done, clean up and hibernate. */
  close(0);
  close(1);
  close(2);
#endif

  stop(current_process());
}
