#ifndef _ERLSRV_GLOBAL_H
#define _ERLSRV_GLOBAL_H

#define APP_NAME "ErlSrv"

#define ERLANG_MACHINE "erl.exe"

#define SERVICE_ENV "ERLSRV_SERVICE_NAME"
#define EXECUTABLE_ENV "ERLSRV_EXECUTABLE"
#define DEBUG_ENV "ERLSRV_DEBUG"

#ifdef _DEBUG
#define HARDDEBUG 1
#define DEBUG 1
#else
#define NDEBUG 1
#endif

#endif /* _ERLSRV_GLOBAL_H */
