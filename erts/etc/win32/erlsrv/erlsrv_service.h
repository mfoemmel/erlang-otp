#ifndef _ERLSRV_SERVICE_H
#define _ERLSRV_SERVICE_H

#define CYCLIC_RESTART_LIMIT 10 /* Seconds */
#define SUCCESS_WAIT_TIME (10*1000) /* Wait 5 s before reporting a service
				as really started */
#define NO_SUCCESS_WAIT 0
#define INITIAL_SUCCESS_WAIT 1
#define RESTART_SUCCESS_WAIT 2


int service_main(int argc, char **argv);

#endif /* _ERLSRV_SERVICE_H */
