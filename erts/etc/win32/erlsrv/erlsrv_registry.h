#ifndef _ERLSRV_REGISTRY_H
#define _ERLSRV_REGISTRY_H

typedef struct _reg_entry {
  char *name;
  DWORD type;
  union {
    char *bytes;
    DWORD value;
    struct {
      char *bytes;
      char *unexpanded;
    } expand;
  } data;
} RegEntry;

typedef struct _reg_entry_desc {
  char *servicename;
  RegEntry *entries;
} RegEntryDesc;

enum {
#if defined(NOTYET)
  Display,
  Output,
  User,
  After,
#endif
  StopAction,
  OnFail,
  Machine,
  Env,
  WorkDir,
  Priority,
  SName,
  Name,
  Args,
  DebugType,
  InternalServiceName
};

#define ON_FAIL_IGNORE 0
#define ON_FAIL_RESTART 1
#define ON_FAIL_REBOOT 2
#define ON_FAIL_RESTART_ALWAYS 3 

#define DEBUG_TYPE_NO_DEBUG 0
#define DEBUG_TYPE_NEW 1
#define DEBUG_TYPE_REUSE 2
#define DEBUG_TYPE_CONSOLE 3

extern int num_reg_entries;

RegEntry *empty_reg_tab(void);
void free_keys(RegEntry *keys);
void free_all_keys(RegEntryDesc *descs);
RegEntry *get_keys(char *servicename);
int set_keys(char *servicename, RegEntry *keys);
RegEntryDesc *get_all_keys(void);
int remove_keys(char *servicename);
int register_logkeys(void);
#endif /* _ERLSRV_REGISTRY_H */

