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

/* ----------------------------- CONSTANTS ------------------------------*/

#define MAXCOLSIZE 4096 
#define MAX_ERR_MSG 1024
#define ERRMSG_HEADR_SIZE 20
#define MAX_CONN_STR_OUT 1024
#define TRUNCATED "01004"
#define SQL_STATE_SIZE 6
#define TRUE 1
#define FALSE 0
#define WAIT_FOR_NEW_MSG 0
#define NEW_MSG_ARRIVED  1
#define TIME_OUT 10
#define DUMMY_OFFSET 0

/* Constats defining the command protocol between the Erlang control process
   and the port program. These constants must also be defined in the same
   way in Erlang. */ 
#define OPEN_CONNECTION		1
#define CLOSE_CONNECTION	2
#define COMMIT_TRANSACTION	3
#define COMMIT			4
#define ROLLBACK		5
#define QUERY			6
#define SELECT_COUNT		7
#define SELECT_FIRST		8
#define SELECT_LAST		9
#define SELECT_NEXT		10
#define SELECT_PREV		11
#define SELECT			12
#define SELECT_RELATIVE		13
#define SELECT_ABSOLUTE		14
#define SELECT_N_NEXT		15
#define DEBUG			16
#define LENGTH_INDICATOR_SIZE	4
#define INT_VALUE		1
#define STR_VALUE		2
#define ON		        1
#define OFF		        2


/*%%%%%%%%%%%%%%% DEPRECATED CONSTANTS START %%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
#define OPEN_DB			17
#define CLOSE_DB		18
#define BIND_COLUMN		19
#define DESCRIBE_COLUMN		20
#define END_TRANSACTION		21
#define EXEDIR			22
#define FETCH_DATA		23
#define NUMBER_RESULT_COLUMNS	24
#define ROW_COUNT		25
#define SET_ATTRIBUTE		26
#define READ_BUFFER		27
#define CLOSE_HANDLE		28
#define EXECDB			29
/*%%%%%%%%%%%%%%% DEPRECATED CONSTANTS END %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

/*------------------------   TYPDEFS  ----------------------------------*/

typedef unsigned char byte;
typedef int Boolean;

typedef struct {
  char columnName[100];
  char *buf;
  SQLSMALLINT type;
  char *typename;                 /* columntype as readible name */
  SQLINTEGER len;
  SQLINTEGER resultLen;
} ColumnDef;

typedef struct {
  int length;
  byte *buffer;
  Boolean dyn_alloc; 
} DbResultMsg;

typedef struct {
  SQLCHAR sqlState[SQL_STATE_SIZE];
  char *error_msg;
} Diagnos;

typedef struct {
  SQLHDBC connection_handle;     
  SQLHENV environment_handle;    
  SQLHSTMT statement_handle;
  ColumnDef *columns;
  int number_of_columns;
  ei_x_buff dynamic_buffer;
  Boolean associated_result_set;
} DBState;

#define connection_handle(DBState) (DBState -> connection_handle)
#define environment_handle(DBState) (DBState -> environment_handle)
#define statement_handle(DBState) (DBState -> statement_handle)
#define columns(DBState) (DBState -> columns)
#define nr_of_columns(DBState) (DBState -> number_of_columns)
#define dynamic_buffer(DBState) (DBState -> dynamic_buffer)
#define associated_result_set(DBState) (DBState -> associated_result_set)

#define DBG( proto ) if (dbgflag) Log proto
