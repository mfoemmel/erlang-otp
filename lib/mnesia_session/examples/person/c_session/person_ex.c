/* 
   %%%----------------------------------------------------------------------
   %%% File    : person_ex.c
   %%% Author  : Dan Gudmundsson <dgud@erix.ericsson.se>
   %%% Purpose : Examples of mnesia_sesion interface to mnesia
   %%% Created : 24 Apr 1998 by Dan Gudmundsson <dgud@erix.ericsson.se>
   %%%----------------------------------------------------------------------
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

//Includes
#include <erl_interface.h>
#include "mnesia_session.h"
#include "mnesia_connector.h"
#include "persons.h"
#include <ic.h>
//Includes

#define COOKIE "example"
#define MAX_SIZE 10000

void decode_person(ETERM * tuple, persons_person * p);


void handle_error(char * func, CORBA_Environment * env)
{
    fprintf(stderr, "\nFailed in %s, reason : %s\n", 
	   func, (char *) CORBA_exception_value(env));
    CORBA_exception_free(env);
    exit(-1);
}

//Init
erlang_pid start_session(CORBA_Environment * env)
{
    erlang_pid session_pid;
    
    session_pid = mnesia_connector_connect(NULL, env);
    
    if(env->_major != CORBA_NO_EXCEPTION) 
	handle_error("connector_connect", env);
    return session_pid;
}
//Init

//create_table
void create_person_table(CORBA_Environment * env)
{
    int err;
    char * reason;
    mnesia_Status result;    
    mnesia_TableDef tabdef;
    mnesia_Indices idxs;
    
    long idx_list[1] = {4};
    char * attrs[4] = {"name", "personData", "married_to", "children"};
    
    tabdef.type = mnesia_bag;
    tabdef.mode = mnesia_read_write;

    tabdef.ram_copies._maximum = 0;
    tabdef.ram_copies._length = 0;
    tabdef.ram_copies._buffer = NULL;
    
    tabdef.disc_copies._maximum = 0;
    tabdef.disc_copies._length = 0;
    tabdef.disc_copies._buffer = NULL;

    tabdef.disc_only_copies._maximum = 0;
    tabdef.disc_only_copies._length = 0;
    tabdef.disc_only_copies._buffer = NULL;
    
    tabdef.index_list._maximum = 5;
    tabdef.index_list._length = 1;
    tabdef.index_list._buffer = idx_list;

    tabdef.attributes._maximum = 5;
    tabdef.attributes._length = 4;
    tabdef.attributes._buffer = attrs;
   
    tabdef.record_name = "persons_person"; /* The name of the stored 
					      type/struct */

    result = mnesia_session_create_table(NULL, "persons", &tabdef, &reason, env);

    if(env->_major != CORBA_NO_EXCEPTION)
    {   
	fprintf(stderr,"\n error in create_table: %d\n", env->_major);
	exit(1);
    }
    else if(result != mnesia_ok)
    {
	fprintf(stderr, "Create table failed with reason %s", reason);
    }
    CORBA_free(reason);
}
//create_table

//dirty_write
void insert_person(CORBA_Environment * env,
		   char *name, persons_Sex sex, int age, 
		   int phone, char * mt, persons_person_children * ch)
{
    ETERM *person, *children, *temp;
    int err, i;
    char * reason;
    mnesia_Status result;
    extern char * oe_persons_Sex[];
    
    children = erl_mk_empty_list();
    
    if(ch != NULL)
	for(i = 0; i < ch->_length; i++)
	{
	    temp = erl_mk_string(ch->_buffer[i]);
	    children = erl_cons(temp, children);
	    erl_free_term(temp);
	};
    
    person = erl_format("{persons_person,~s,{persons_data,~a,~i,~i}, ~s, ~w}",
			name, oe_persons_Sex[sex], age, phone, mt, children);
    
    result = mnesia_session_dirty_write(NULL, "persons", person, &reason, env);
    
    if(env->_major != CORBA_NO_EXCEPTION) 
    {   
	fprintf(stderr,"\n error in insert_person: %d\n", env->_major);
	exit(1);
    }
    else if(result != mnesia_ok)
    {
	fprintf(stderr, "Insert person failed with reason %s", reason);	
	exit(-1);
    }
    
    erl_free_term(children);
    erl_free_term(person);    
    
    CORBA_free(reason);
}
//dirty_write

//dirty_read
void get_person(CORBA_Environment * env, char * name, persons_person * person)
{
    int err, i;
    char * reason;
    mnesia_Status result;
    mnesia_Recordlist *rec_list;
    ETERM * key;

    key = erl_mk_string(name);
    
    result = mnesia_session_dirty_read(NULL, "persons", key, &rec_list, 
                                       &reason, env);
    if(env->_major != CORBA_NO_EXCEPTION) 
    {   
	fprintf(stderr,"\n error in get_person: %d\n", env->_major);
	exit(1);
    }
    else if(result != mnesia_ok)
    {
	fprintf(stderr, "Get person failed with reason %s", reason);	
	exit(-1);
    }
    
    if(rec_list->_length > 0)
    {
	/* Only interrested in the first match */
	decode_person(rec_list->_buffer[0], person);
	for(i=0; i < rec_list->_length; i++)
	    erl_free_term(rec_list->_buffer[i]);
    }
    else 
    {
	fprintf(stderr, "Insert person failed empty_list returned ");	
	exit(-1);
    }
    
    CORBA_free(rec_list);
    CORBA_free(reason);
    erl_free_term(key);
}
//dirty_read

void decode_person(ETERM * tuple, persons_person * p)
{
    ETERM * etemp, * etemp2;
    char * name;
    int i, count, size;
    extern char * oe_persons_Sex[];
    
    etemp = ERL_TUPLE_ELEMENT(tuple, 1);
    if(!ERL_IS_LIST(etemp))
    {
	fprintf(stderr, "Failed decoding atom element 2");
	erl_print_term(stderr, etemp);
	exit(-1);
    }
    p->name = erl_iolist_to_string(etemp);
    
    etemp = ERL_TUPLE_ELEMENT(tuple, 2);
    if(!ERL_IS_TUPLE(etemp))
    {
	fprintf(stderr, "Failed decoding tuple element 3");
	erl_print_term(stderr, etemp);
	exit(-1);
    }
    
    etemp2 = ERL_TUPLE_ELEMENT(etemp, 1);
    if(!ERL_IS_ATOM(etemp2))
    {
	fprintf(stderr, "Failed decoding 'sex' element 1");
	erl_print_term(stderr, etemp2);
	exit(-1);	
    }
    if(!strncmp(oe_persons_Sex[0], ERL_ATOM_PTR(etemp2), ERL_ATOM_SIZE(etemp2)))
	p->personData.sex = 0;
    else 
	p->personData.sex = 1;
    
    etemp2 = ERL_TUPLE_ELEMENT(etemp, 2);
    if(!ERL_IS_INTEGER(etemp2))
    {
	fprintf(stderr, "Failed decoding age element 2");
	erl_print_term(stderr, etemp2);
	exit(-1);
    }
    p->personData.age = ERL_INT_VALUE(etemp2);
    
    etemp2 = ERL_TUPLE_ELEMENT(etemp, 3);
    if(!ERL_IS_INTEGER(etemp2))
    {
	fprintf(stderr, "Failed decoding phone element 3");
	erl_print_term(stderr, etemp2);
	exit(-1);
    }
    p->personData.phone = ERL_INT_VALUE(etemp2);
    
    etemp = ERL_TUPLE_ELEMENT(tuple, 3); 
    p->married_to = erl_iolist_to_string(etemp);
    
    etemp2 = etemp = ERL_TUPLE_ELEMENT(tuple, 4);
    count = 0;

    while(!ERL_IS_EMPTY_LIST(etemp2))
    {
	count++;
	etemp2 = ERL_CONS_TAIL(etemp2);
    };

    p->children._length  = count;
    p->children._maximum = count;    
    p->children._buffer  = (char **) malloc(sizeof(char*) * count);
    etemp2 = etemp;
    
    for(i = 0; i < count; i++)
    {
	p->children._buffer[i] = erl_iolist_to_string(ERL_CONS_HEAD(etemp));
	etemp2 = ERL_CONS_TAIL(etemp2);
    }
}

char * add_host_to_name(char * nodename)
{
    char *host, *temp;
    temp = (char *) malloc(255 * sizeof(char));
    for(host = nodename; *host != '@'; host++);
    sprintf(temp, "c47%s", host);
    return temp;
}

int connect_node(char * nodename, char * cookie)
{
    int erl_node;
    
    /* connect to erlang node */ 
    erl_init(NULL,0);
    
    erl_connect_init(47, cookie, 0);
    erl_node = erl_connect(nodename);
    
    if(erl_node < 0) 
    {
	fprintf(stderr,"Cannot connect to erlang, exiting\n");
	exit(1);
    }
    
    return erl_node;
}

int main(int argc, char **argv)
{
    if(argc != 3)
    {
	printf("No nodename specified\n");
	printf("Usage: person_ex node@host cookie\n");
	exit(-1);
    }
    else
    {
	char * mynodename;
	char * nodename;
	char buff[MAX_SIZE];
	erlang_pid session, my_pid;
	CORBA_Environment *env;
	
	persons_person result;
	extern char* oe_persons_Sex[];
	
	nodename = argv[1];
	
	fprintf(stderr, "Connecting to node %s \n", nodename);
	mynodename = add_host_to_name(nodename);
	fprintf(stderr, "Using nodename %s\n", mynodename);
	
	/* Initialization */
	env = CORBA_Environment_alloc(MAX_SIZE,0);
	env->_fd= connect_node(nodename, argv[2]);
	env->_inbufsz = MAX_SIZE;
	env->_from_pid = &my_pid;
	
	sprintf(my_pid.node, "%s", mynodename);
 	my_pid.num = env->_fd; 
 	my_pid.serial = 0;
 	my_pid.creation = 0; 
	/* ---- */
	
	/* Make first connection to the registered process mnesia_session */
	strcpy(env->_regname, "mnesia_connector");

	fprintf(stderr, "Ok initialized\n");
	session = start_session(env);
	/*
	  The rest of connections should be made to the process 
	  returned from start_session 
	*/
	strcpy(env->_regname, "");
	env->_to_pid = &session;
	create_person_table(env);
	fprintf(stderr, "Ok created\n");
	insert_person(env, "dan", persons_male, 27, 97185, "none", NULL);
	get_person(env, "dan", &result);

	fprintf(stderr, "\nRead person Dan from mnesia \n");
	fprintf(stderr, " Sex %s",   oe_persons_Sex[result.personData.sex]);
	fprintf(stderr, " Age %d",   result.personData.age);
	fprintf(stderr, " Phone %d\n\n", result.personData.phone);

	close(env->_fd);

	  /* Free env & company */
	CORBA_free(env->_inbuf);
	CORBA_free(env->_outbuf);
	CORBA_free(env);
	free(mynodename);
	free(result.name);
	free(result.children._buffer);
    }
}


