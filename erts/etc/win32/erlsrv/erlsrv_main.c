#include <windows.h>
#include <winsvc.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "erlsrv_global.h"
#include "erlsrv_interactive.h"
#include "erlsrv_service.h"

int main(int argc, char **argv){
  if(argc > 1)
    return interactive_main(argc,argv);
  else
    return service_main(argc,argv);
}
   










