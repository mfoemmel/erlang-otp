/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2003-2009. All Rights Reserved.
 * 
 * The contents of this file are subject to the Erlang Public License,
 * Version 1.1, (the "License"); you may not use this file except in
 * compliance with the License. You should have received a copy of the
 * Erlang Public License along with this software. If not, it can be
 * retrieved online at http://www.erlang.org/.
 * 
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
 * the License for the specific language governing rights and limitations
 * under the License.
 * 
 * %CopyrightEnd%
 */
#include <windows.h>
#include <stdio.h>
#include <stdlib.h>
#include "init_file.h"

typedef int ErlexecFunction(int, char **, HANDLE, int); 

#define INI_FILENAME "erl.ini"
#define INI_SECTION "erlang"
#define ERLEXEC_BASENAME "erlexec.dll"

static void get_parameters(void);
static void error(char* format, ...);

static char *erlexec_name;
static char *erlexec_dir;

#ifdef WIN32_WERL
#define WERL 1
int WINAPI WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance,
		    PSTR szCmdLine, int iCmdShow)
{
    int argc = __argc;
    char **argv = __argv;
#else
#define WERL 0
int main(int argc, char **argv)
{
#endif
  HANDLE erlexec_handle; /* Instance */
  ErlexecFunction *win_erlexec;
  char *path = malloc(100);
  char *npath;
  int pathlen;

  get_parameters();

  if ((pathlen = GetEnvironmentVariable("PATH",path,100)) == 0) {
    error("No PATH variable (!)");
  } else if (pathlen > 100) {
    path = realloc(path,pathlen);
    GetEnvironmentVariable("PATH",path,pathlen);
  }
  npath = malloc(strlen(path) + strlen(erlexec_dir) + 2);
  sprintf(npath,"%s;%s",erlexec_dir,path);
  SetEnvironmentVariable("PATH",npath);

  if ((erlexec_handle = LoadLibrary(erlexec_name)) == NULL) {
    error("Could not load module %s.",erlexec_name);
  }

  if ((win_erlexec = (ErlexecFunction *) 
       GetProcAddress(erlexec_handle,"win_erlexec")) == NULL) {
    error("Could not find entry point \"win_erlexec\" in %s.", erlexec_name);
  }

  return (*win_erlexec)(argc,argv,erlexec_handle,WERL);
  
} 

static char *replace_filename(char *path, char *new_base) 
{
    int plen = strlen(path);
    char *res = malloc((plen+strlen(new_base)+1)*sizeof(char));
    char *p;

    strcpy(res,path);
    for (p = res+plen-1 ;p >= res && *p != '\\'; --p)
        ;
    *(p+1) ='\0';
    strcat(res,new_base);
    return res;
}

static char *do_lookup_in_section(InitSection *inis, char *name, 
				  char *section, char *filename)
{
    char *p = lookup_init_entry(inis, name);

    if (p == NULL) {
	error("Could not find key %s in section %s of file %s",
	      name,section,filename);
    }
    return _strdup(p);
}

static void get_parameters(void)
{
    char buffer[MAX_PATH];
    char *ini_filename;
    HANDLE module = GetModuleHandle(NULL);
    InitFile *inif;
    InitSection *inis;
    char *bindir;

    if (module = NULL) {
        error("Cannot GetModuleHandle()");
    }

    if (GetModuleFileName(module,buffer,MAX_PATH) == 0) {
        error("Could not GetModuleFileName");
    }

    ini_filename = replace_filename(buffer,INI_FILENAME);

    if ((inif = load_init_file(ini_filename)) == NULL) {
	error("Could not load init file %s",ini_filename);
    }

    if ((inis = lookup_init_section(inif,INI_SECTION)) == NULL) {
	error("Could not find section %s in init file %s",
	      INI_SECTION, ini_filename);
    }

    erlexec_dir = do_lookup_in_section(inis, "Bindir", INI_SECTION, ini_filename);

    erlexec_name = malloc(strlen(erlexec_dir) + strlen(ERLEXEC_BASENAME) + 2);
    strcpy(erlexec_name,erlexec_dir);
    strcat(erlexec_name, "\\" ERLEXEC_BASENAME);
    
    free_init_file(inif);
    free(ini_filename);
}


static void error(char* format, ...)
{
    char sbuf[2048];
    va_list ap;

    va_start(ap, format);
    vsprintf(sbuf, format, ap);
    va_end(ap);

#ifndef WIN32_WERL 
	fprintf(stderr, "%s\n", sbuf);
#else
	MessageBox(NULL, sbuf, "Werl", MB_OK|MB_ICONERROR);
#endif
    exit(1);
}

