/*
 ****************************************************************************
 *                            PROGRAM MODULE
 *
 *     $Workfile:   startefs.c  $
 *     Document no: @(#) 511/OSE114-XX $Version:   /main/tb_ri24/18  $
 *     $Author:   integrat  $
 *     $Date:   07/19/01 18:48:00  $
 *
 *       Copyright (C) 1998 by ENEA OSE SYSTEMS AB. All rights reserved.
 *
 ****************************************************************************
 */

#include "string.h"
#include "ose.h"
#include "efs.h"
#include "efs.sig"
#include "efs_err.h"

#include "errno.h"
#include "stdio.h"
#include "unistd.h"

#include "malloc.h"
#include "dbgprintf.h"

#ifdef USE_OSEDEF_H
#include "osedef.h"
#endif

#ifdef SHELL_SERUNIT
#ifdef SHELL_SERNAME
#include "fm.sig"
#include "login.sig"
#else
#error SHELL_SERUNIT is defined, but not SHELL_SERNAME!
#endif
#endif

#ifdef USE_FFX
#define FFXDISKNAME "/ffx"
#endif

#ifdef USE_HOSTFM
#define HOSTDISKNAME "/host"
#endif

#define TTYNAME     "/tty"
#define DISKNAME    "/ram"
#define FLASH       "/flash"

#define STR(x) #x
#define STRSTR(x) STR(x)

struct Passwd
{
  const char *username;
  const char *password;
  OSUSER userid;
  OSENTRYPOINT *shell;
  const char *home_dir;
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
	      OSUSER * user,
	      OSENTRYPOINT ** shell)
{
  extern OSENTRYPOINT ose_shell;
  static const struct Passwd table[] =
  {
    /* For now the userid must be zero. */
    /* username, password, userid, entrypoint, homedir. */
    {  "",       "",       0,      ose_shell,  "/ram"},
    {  "olli",   "bolli",  0,      ose_shell,  "/ram"},
    {  "ftp",    "",       0,      no_shell,   "/ram"},
    {  "anonymous","",     0,      no_shell,   "/ram"},
    {  NULL,      NULL,    0,      NULL,       NULL}
  };
  unsigned i;

  for (i = 0; table[i].username != NULL; i++)
  {
    if (strcmp(table[i].username, username) == 0 &&
	(password == NULL || strcmp(table[i].password, password) == 0))
    {
      if (strcmp(username, "root") == 0 && strncmp(termname, "/telnet", 7) == 0)
      {
	*user = table[i].userid;
	*shell = no_telnet;
      }
      else
      {
	*user = table[i].userid;
	*shell = table[i].shell;
      }
      if (table[i].home_dir != NULL)
      {
	chdir(table[i].home_dir);
      }
      return True;
    }
  }
  return False;
}

OS_PROCESS(start_efs)
{
  EfsStatus status;
  int err;
  extern OSENTRYPOINT ose_ramdisk;
  PROCESS ramdisk_;

#ifdef SHELL_SERUNIT
  /* Mount serial port and open it as stdin/stdout/stderr. */
  {
    int fd0, fd1, fd2;

    status = efs_mount(TTYNAME, "confm", "serdd",
		       "unit=0,hwname=" SHELL_SERNAME ",hwunit=" STRSTR(SHELL_SERUNIT) ",baudrate=9600");

    if (status != EFS_SUCCESS)
    {
      error2(OSE_EFS_STARTEFS_EMOUNT_TTY_FAILED, (OSERRCODE) status);
    }
    fd0 = open(TTYNAME, O_RDONLY, 0);
    fd1 = open(TTYNAME, O_WRONLY | O_TEXT, 0);
    fd2 = open(TTYNAME, O_WRONLY | O_TEXT, 0);
    if (fd0 != 0 || fd1 != 1 || fd2 != 2)
    {
      error2(OSE_EFS_STARTEFS_EOPEN_TTY_FAILED, (OSERRCODE) errno);
    }
  }

  /* stdio enabled! */
#endif

#ifdef USE_RAMDISK
  ramdisk_ = create_process(OS_PRI_PROC,
			    "ose_ramdisk",
			    ose_ramdisk,
			    1000, 9, 0, 0, NULL, 0, 0);
  efs_clone(ramdisk_);
  start(ramdisk_);

  /* Mount and format the RAM disk */
  {
    status = efs_mount(DISKNAME, "extfat", "ramdisk", "unit=0");

    if (status != EFS_SUCCESS)
    {
      err = efs_status_to_errno(status);
      fprintf(stderr, "(mount) STATUS = %d, STATUS->ERRNO = %d, ERRNO = %d\r\n", status, err, errno);
      error2(OSE_EFS_STARTEFS_EMOUNT_DISK_FAILED, (OSERRCODE) status);
    }
    if (chdir(DISKNAME) != 0)
    {
      /*
       * A chdir() failiure means that the disk is not formatted.
       * Please remember that a FAT12 formated disk can contain
       * a maximum of 4078 availiable clusters. A FAT 16 formated
       * disk can contain a maximum of 65518 available clusters.
       * I.e adjust clustersize after the size of the partition.
       */
      status = efs_format(DISKNAME, "clustersize=128", False); /* 128*512 = 64k */        
      if (status != EFS_SUCCESS)
      {
	err = efs_status_to_errno(status);	
	fprintf(stderr, "(format) STATUS = %d, STATUS->ERRNO = %d, ERRNO = %d\r\n", status, err, errno);	
	error2(OSE_EFS_STARTEFS_EFORMAT_DISK_FAILED, (OSERRCODE) status);
      }
      /* Change current directory to the RAM disk, this will be
       inherited by the login and shell processes if not FFX is used */
      if (chdir(DISKNAME) != 0)
      {
	 error2(OSE_EFS_STARTEFS_ECHDIR_DISK_FAILED, (OSERRCODE) errno);
      }
    }
  }
#endif

#ifdef FLASH_ENABLED

  /* Mount and format the flash disk. */
  {
    status = efs_mount(FLASH, "extfat", "amd_29f032B", "unit=0");

    err = efs_status_to_errno(status);

    printf("STATUS = %d, STATUS->ERRNO = %d, ERRNO = %d\r\n", status, err, errno);
    
    if (status != EFS_SUCCESS)
    {
      error2(OSE_EFS_STARTEFS_EMOUNT_DISK_FAILED, (OSERRCODE) status);
    }
    if (chdir(FLASH) != 0)
    {
      /* chdir failed means disk is not formatted. */
      status = efs_format(FLASH, "quick, clustersize=1", False);
      if (status != EFS_SUCCESS)
      {
	error2(OSE_EFS_STARTEFS_EFORMAT_DISK_FAILED, (OSERRCODE) status);
      }
    }
    /* Change current directory to the flash disk, this will be
       inherited by the login and shell processes. */
    if (chdir(FLASH) != 0)
    {
      error2(OSE_EFS_STARTEFS_ECHDIR_DISK_FAILED, (OSERRCODE) errno);
    }
  }

#endif  

#ifdef USE_FFX

  /* Mount and format the FFX disk. */
  {
    status = efs_mount(FFXDISKNAME, "extfat", "ffxddb", "unit=0");
    if (status != EFS_SUCCESS)
    {
      error2(OSE_EFS_STARTEFS_EMOUNT_DISK_FAILED, (OSERRCODE) status);
    }
    if (chdir(FFXDISKNAME) != 0)
    {
      /* chdir failed means disk is not formatted. */
      status = efs_format(FFXDISKNAME, "quick, clustersize=1", False);
      if (status != EFS_SUCCESS)
      {
	error2(OSE_EFS_STARTEFS_EFORMAT_DISK_FAILED, (OSERRCODE) status);
      }
    }
    /* Change current directory to the FFX disk, this will be
       inherited by the login and shell processes. */
    if (chdir(FFXDISKNAME) != 0)
    {
      error2(OSE_EFS_STARTEFS_ECHDIR_DISK_FAILED, (OSERRCODE) errno);
    }
  }
  
#endif
  
#ifdef USE_HOSTFM
  /* Mount and format the HostFM disk (only soft kernels) */
  {
    status = efs_mount(HOSTDISKNAME, "hostfm", "/", "unit=0");
    if (status != EFS_SUCCESS)
    {
      error2(OSE_EFS_STARTEFS_EMOUNT_DISK_FAILED, (OSERRCODE) status);
    }
     if (chdir(HOSTDISKNAME) != 0)
    {
      error2(OSE_EFS_STARTEFS_ECHDIR_DISK_FAILED, (OSERRCODE) errno);
    }
  }
#endif

  {
    extern void set_clock(void);
    set_clock();
  }

#ifdef SHELL_SERUNIT
  /* Spawn the login process in the same block (main). */
  {      
    extern OSENTRYPOINT ose_login;
    PROCESS login_;
    
    login_ = create_process(OS_BG_PROC,
			    "ose_login",
			    ose_login,
			    2000,
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

