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
 * Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
 * AB. All Rights Reserved.''
 * 
 *     $Id$
 */
#include <ic.h>

void CORBA_free(void *storage)
{
    if (storage != NULL) 
	free(storage); 
}


CORBA_char *CORBA_string_alloc(CORBA_unsigned_long len) 
{
    return (CORBA_char *) malloc(len+1);
}


CORBA_wchar *CORBA_wstring_alloc(CORBA_unsigned_long len)
{
    return (CORBA_wchar *) malloc(len*(__OE_WCHAR_SIZE_OF__+1));
}


CORBA_Environment *CORBA_Environment_alloc(int inbufsz, int outbufsz)
{
    CORBA_Environment *ev;

    ev = malloc(sizeof(CORBA_Environment));

    if (ev != NULL) {

	/* CORBA */
	ev->_major = CORBA_NO_EXCEPTION;

	/* Set by user */
	ev->_fd= -1;
	ev->_inbufsz = inbufsz;
	ev->_inbuf = malloc(inbufsz);
	ev->_outbufsz = outbufsz;
	ev->_outbuf = malloc(outbufsz);
	ev->_memchunk = __OE_MEMCHUNK__;
	ev->_regname[0] = '\0';
	ev->_to_pid = NULL;
	ev->_from_pid = NULL;

	/* Set by client or server */
	ev->_iin = 0;
	ev->_iout = 0;
	ev->_operation[0] = '\0';
	ev->_received = 0;
	/* ev->_caller  */
	/* ev->_unique */
	ev->_exc_id = NULL;
	ev->_exc_value = NULL;
	ev->_ref_counter_1 = 0;
	ev->_ref_counter_2 = 0;
	ev->_ref_counter_3 = 0;
    }

    return ev;
}

#if 0
/* NOT EXPORTED SO FAR */
void CORBA_Environment_free(CORBA_Environment *ev)
{

    CORBA_free(ev->_inbuf);
    CORBA_free(ev->_outbuf);
    CORBA_exception_free(ev);
    CORBA_free(ev);
} 
#endif


CORBA_char *CORBA_exception_id(CORBA_Environment *ev)
{

    return ev->_exc_id;
}

void *CORBA_exception_value(CORBA_Environment *ev)
{

    return ev->_exc_value;
}

void CORBA_exception_free(CORBA_Environment *ev)
{
 
    /* Setting major value */
    ev->_major=CORBA_NO_EXCEPTION;

    /* Freeing storage */
    CORBA_free(ev->_exc_id);
    CORBA_free(ev->_exc_value);
}

void CORBA_exc_set(CORBA_Environment *ev, 
		   CORBA_exception_type Major, 
		   CORBA_char *Id, 
		   CORBA_char *Value)
{
    int ilen,vlen;

  /* Create exception ONLY if there where
     not allready one on the run */ 
    if (ev->_major == CORBA_NO_EXCEPTION) {
    
	/* Counting lengths */
	ilen=strlen(Id)+1;
	vlen=strlen(Value)+1;
    
	/* Allocating storage */
	ev->_exc_id=(CORBA_char *) malloc(ilen);
	ev->_exc_value=(CORBA_char *) malloc(vlen);
    
	/* Initiating */
	ev->_major=Major;
	strcpy(ev->_exc_id,Id);
	strcpy(ev->_exc_value,Value);
    }
}

#define ERLANG_REF_NUM_SIZE  18
#define ERLANG_REF_MASK      (~(~((unsigned int)0) << ERLANG_REF_NUM_SIZE))

/* Initiating message reference */
void ic_init_ref(CORBA_Environment *ev, erlang_ref *ref)
{

    strcpy(ref->node, erl_thisnodename());

    ref->len = 3;

    ++ev->_ref_counter_1;
    ev->_ref_counter_1 &= ERLANG_REF_MASK;
    if (ev->_ref_counter_1 == 0)
	if (++ev->_ref_counter_2 == 0) 
	    ++ev->_ref_counter_3;
    ref->n[0] = ev->_ref_counter_1;
    ref->n[1] = ev->_ref_counter_2;
    ref->n[2] = ev->_ref_counter_3;
    
    ref->creation = erl_thiscreation();
}

/* Comparing message references */
int ic_compare_refs(erlang_ref *ref1, erlang_ref *ref2)
{
    int i;

    if(strcmp(ref1->node, ref2->node) != 0) 
	return -1;
 
    if (ref1->len != ref2->len) 
	return -1;

    for (i = 0; i < ref1->len; i++)
	if (ref1->n[i] != ref2->n[i])
	    return -1;
    
    return 0; 
}

/* Length counter for wide strings */
int ic_wstrlen(CORBA_wchar * p)
{
    int len = 0;

    while(1) {
	if (p[len] == 0)
	    return len;

	len+=1;
    }
}


/* Wide string compare function */
int ic_wstrcmp(CORBA_wchar * ws1, CORBA_wchar * ws2)
{
    int index = 0;

    while(1) {
	if (ws1[index] == ws2[index]) {

	    if (ws1[index] == 0)
		return 0;
      
	    index += 1;

	} else 
	    return -1;
    }
}

/* Generic call information extractor */
int ___call_info___(CORBA_Object obj, CORBA_Environment *env)
{
    char gencall_atom[10];
    int error = 0;
    int rec_version = 0;
    env->_iin = 0;
    env->_received = 0;
    
    memset(gencall_atom, 0, 10);
    ei_decode_version(env->_inbuf, &env->_iin, &rec_version);
    ei_decode_tuple_header(env->_inbuf, &env->_iin, &env->_received);
    ei_decode_atom(env->_inbuf, &env->_iin, gencall_atom);
    
    if (strcmp(gencall_atom, "$gen_cast") == 0) {
	
	if ((error = ei_decode_atom(env->_inbuf, &env->_iin, 
				    env->_operation)) < 0) {
	    ei_decode_tuple_header(env->_inbuf, &env->_iin, &env->_received);
	    if ((error = ei_decode_atom(env->_inbuf, &env->_iin, 
					env->_operation)) < 0) { 
		CORBA_exc_set(env, 
			      CORBA_SYSTEM_EXCEPTION, 
			      BAD_OPERATION, 
			      "Bad Message, cannot extract operation");
		return error;
	    }
	    env->_received -= 1;
	} else
	    env->_received -= 2;
	
	return 0;
    }
    
    if (strcmp(gencall_atom, "$gen_call") == 0) {
	
	ei_decode_tuple_header(env->_inbuf, &env->_iin, &env->_received);
	
	if ((error = ei_decode_pid(env->_inbuf, &env->_iin, 
				   &env->_caller)) < 0) {
	    CORBA_exc_set(env, 
			  CORBA_SYSTEM_EXCEPTION, 
			  MARSHAL, 
			  "Bad Message, bad caller identity");
	    return error;
	}
	
	if ((error = ei_decode_ref(env->_inbuf, &env->_iin, 
				   &env->_unique)) < 0) {
	    CORBA_exc_set(env, 
			  CORBA_SYSTEM_EXCEPTION, 
			  MARSHAL, 
			  "Bad Message, bad message reference");
	    return error;
	}
	
	if ((error = ei_decode_atom(env->_inbuf, &env->_iin, 
				    env->_operation)) < 0) {
	    
	    ei_decode_tuple_header(env->_inbuf, &env->_iin, &env->_received);
	    
	    if ((error = ei_decode_atom(env->_inbuf, &env->_iin, 
					env->_operation)) < 0) { 
		CORBA_exc_set(env, 
			      CORBA_SYSTEM_EXCEPTION, 
			      BAD_OPERATION, 
			      "Bad Message, cannot extract operation");
		return error;
	    }
	    
	    env->_received -= 1;
	    return 0;	  
	}
	else {
	    env->_received -= 2;
	    return 0;
	}
    }
    
    CORBA_exc_set(env, 
		  CORBA_SYSTEM_EXCEPTION, 
		  MARSHAL, 
		  "Bad message, neither cast nor call");
    return -1;
}

/* #define DEBUG_MAP */

#if defined(DEBUG_MAP)

#define PRINT_MAPS(P, M, S) print_maps(P, M, S)
#define PRINT_MAP(T, M)     print_map(T, "", M)

static void print_map(char *title, char *prefix, ___map___ *map)
{
    if (map == NULL) {
	fprintf(stdout, "%s => NULL\n", title);
	return;
    }

    fprintf(stdout, "%s%s\n", prefix, title);

    {
	int j, len = map->length; 

	fprintf(stdout, "%s  length:     %d\n", prefix, len);
	fprintf(stdout, "%s  operations: 0x%X%d\n", prefix, map->operations);
    
	for (j = 0 ; j < len ; j++) {
	    fprintf(stdout, "%s  operation[%d]:\n", prefix, j);
      
	    if (map->operations[j].interface != NULL) {
		fprintf(stdout, "%s    intf: %s\n", prefix, 
			map->operations[j].interface);
	    } else {
		fprintf(stdout, "%s    intf: NULL\n", prefix);
	    }
	    fprintf(stdout, "%s    name: %s\n", prefix, 
		    map->operations[j].name);
	    fprintf(stdout, "%s    func: 0x%X\n", prefix, 
		    map->operations[j].function);
	}
    }
    fflush(stdout);
}

static void print_maps(char* title, ___map___ * maps, int size)
{
    int  i;
    char p[64];

    fprintf(stdout, "%s\n", title);

    for (i = 0 ; i < size ; i++) {
	sprintf(p, "map[%d]:", i);
	print_map(p, "  ", &maps[i]);
    }
    fprintf(stdout, "\n");
    fflush(stdout);
}

#else

#define PRINT_MAPS(P, M, S) 
#define PRINT_MAP(T, M)     

#endif /* if defined(DEBUG_MAP) */


/* Generic switch */
int ___switch___(CORBA_Object obj, CORBA_Environment *env, ___map___ *map)
{
    /* Setting local variables */
    int status = 0;
    int index = 0;

    /* XXX map may be NULL !! */
    int length = map->length;
    char* op = env->_operation;
    
    PRINT_MAP("switching on map", map);

    /* Initiating exception indicator */
    env->_major = CORBA_NO_EXCEPTION;
    
    /* Call switch */
    if ((status = ___call_info___(obj, env)) < 0)
	return status;
#if defined(DEBUG_MAP)
    fprintf(stdout, "looking for operation: %s\n", op); fflush(stdout);
#endif
    for (index = 0; index < length; index++) {
#if defined(DEBUG_MAP)
	fprintf(stdout, "map->operations[%d].name: %s\n",  
		index, map->operations[index].name);  
	fflush(stdout); 
#endif
	if(strcmp(map->operations[index].name, op) == 0) {
#if defined(DEBUG_MAP)
	    fprintf(stdout, "calling map->operations[%d].function: 0x%X\n",
		    index, map->operations[index].function);  
	    fflush(stdout); 
#endif
	    return map->operations[index].function(obj, env);
	}
    }
    /* Bad call */
    CORBA_exc_set(env, CORBA_SYSTEM_EXCEPTION, BAD_OPERATION, 
		  "Invalid operation");
    return -1;
}

___map___* ___merge___(___map___ *maps, int size)
{ 
    int i, j, length, len, maplen, malloc_size;
    void *memp;
    ___map___ *merged;

    if ((maps == NULL) || (size <= 0))
	return NULL;

    PRINT_MAPS("merging maps", maps, size);
	
    length = 0;
    for (i = 0; i < size; i++)
	length += (maps[i].length);

    maplen = OE_ALIGN(sizeof(___map___));
    malloc_size = maplen + OE_ALIGN(length*sizeof(___operation___));
    if ((memp = malloc(malloc_size)) == NULL)
	return NULL;

    merged = memp;
    merged->length = length;
    merged->operations = (___operation___ *)((char*)memp + maplen);
    	
    for (i = 0, len = 0; i < size; i++) {
	for(j = 0 ; j < maps[i].length; j++)
	    merged->operations[len+j] = maps[i].operations[j];
	len += maps[i].length;
    }
    PRINT_MAP("merged map", merged);
    return merged;
}


