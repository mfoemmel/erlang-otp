/* ``The contents of this file are subject to the Erlang Public License,
 * Version 1.1, (the "License"); you may not use this file except in
 * compliance with the License. You should have received a copy of the
 * Erlang Public License along with this software. If not, it can be
 * retrieved via the world wide web at http://www.erlang.org/.
 * 
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
 * the License for the specific language governing rights and limitations
 * under the License.
 * 
 * The Initial Developer of the Original Code is Ericsson Utvecklings AB.
 * Portions created by Ericsson are Copyright 2002, Ericsson Utvecklings
 * AB. All Rights Reserved.''
 * 
 *     $Id$
 */

#include "sys.h"
#include "erl_sys_driver.h"
#include "erl_alloc.h"

static char* merge_environment(char *current, char *add);
static char* arg_to_env(char **arg);
static char** env_to_arg(char *env);
static char** find_arg(char **arg, char *str);
static int compare(const void *a, const void *b);

char*
win_build_environment(char* new_env)
{
    if (new_env == NULL) {
	return NULL;
    } else {
	char *tmp = GetEnvironmentStrings();
	char *merged = merge_environment(tmp, new_env);

	FreeEnvironmentStrings(tmp);
	return merged;
    }
}

static char*
merge_environment(char *old, char *add)
{
    char **a_arg = env_to_arg(add);
    char **c_arg = env_to_arg(old);
    char *ret;
    int i, j;
    
    for(i = 0; c_arg[i] != NULL; ++i)
	;

    for(j = 0; a_arg[j] != NULL; ++j)
	;

    c_arg = erts_realloc(ERTS_ALC_T_TMP,
			 c_arg, (i+j+1) * sizeof(char *));

    for(j = 0; a_arg[j] != NULL; ++j){
	char **tmp;
	char *current = a_arg[j];

	if ((tmp = find_arg(c_arg, current)) != NULL) {
	    if (current[strlen(current)-1] != '=') {
		*tmp = current;
	    } else {
		*tmp = c_arg[--i];
		c_arg[i] = NULL;
	    }
	} else if (current[strlen(current)-1] != '=') {
	    c_arg[i++] = current;
	    c_arg[i] = NULL;
	}
    }
    ret = arg_to_env(c_arg);
    erts_free(ERTS_ALC_T_TMP, c_arg);
    erts_free(ERTS_ALC_T_TMP, a_arg);
    return ret;
}

static char**
find_arg(char **arg, char *str)
{
    char *tmp;
    int len;

    if ((tmp = strchr(str, '=')) != NULL) {
	tmp++;
	len = tmp - str;
	while (*arg != NULL){
	    if (_strnicmp(*arg, str, len) == 0){
		return arg;
	    }
	    ++arg;
	}
    }
    return NULL;
}

static int
compare(const void *a, const void *b)
{
    char *s1 = *((char **) a);
    char *s2 = *((char **) b);
    char *e1 = strchr(s1,'=');
    char *e2 = strchr(s2,'=');
    int ret;
    int len;
  
    if(!e1)
	e1 = s1 + strlen(s1);
    if(!e2)
	e2 = s2 + strlen(s2);
  
    if((e1 - s1) > (e2 - s2))
	len = (e2 - s2);
    else
	len = (e1 - s1);
  
    ret = _strnicmp(s1,s2,len);
    if (ret == 0)
	return ((e1 - s1) - (e2 - s2));
    else
	return ret;
}

static char**
env_to_arg(char *env)
{
    char **ret;
    char *tmp;
    int i;
    int num_strings = 0;

    for(tmp = env; *tmp != '\0'; tmp += strlen(tmp)+1) {
	++num_strings;
    }
    ret = erts_alloc(ERTS_ALC_T_TMP, sizeof(char *) * (num_strings + 1));
    i = 0;
    for(tmp = env; *tmp != '\0'; tmp += strlen(tmp)+1){
	ret[i++] = tmp;
    }
    ret[i] = NULL;
    return ret;
}

static char*
arg_to_env(char **arg)
{
    char *block;
    char *ptr;
    int i;
    int totlen = 1;		/* extra '\0' */

    for(i = 0; arg[i] != NULL; ++i) {
	totlen += strlen(arg[i])+1;
    }

    /* sort the environment vector */
    qsort(arg, i, sizeof(char *), &compare);

    if (totlen == 1){
	block = erts_alloc(ERTS_ALC_T_ENVIRONMENT, 2);
	block[0] = block[1] = '\0'; 
    } else {
	block = erts_alloc(ERTS_ALC_T_ENVIRONMENT, totlen);
	ptr = block;
	for(i=0; arg[i] != NULL; ++i){
	    strcpy(ptr, arg[i]);
	    ptr += strlen(ptr)+1;
	}
	*ptr = '\0';
    }
    return block;
}
