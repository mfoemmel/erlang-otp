#ifndef _ERLSRV_UTIL_H
#define _ERLSRV_UTIL_H

extern char *service_name;
extern char *real_service_name;
void log_warning(char *mess);
void log_error(char *mess);
void log_info(char *mess);

char *envdup(char *env);
/*
** Call before env_to_arg to get a 'freeable' environment block.
*/

char *arg_to_env(char **arg);
/*
** Frees the argument list before returning!
*/

char **env_to_arg(char *env);
/*
** Frees the environment block before returning!
*/


#ifndef NDEBUG
void log_debug(char *mess);
#else
#define log_debug(mess) /* Debug removed */
#endif

#endif /* _ERLSRV_UTIL_H */
