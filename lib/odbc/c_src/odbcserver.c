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

/*
  Erlang ODBC (Open Database Connectivity) application.  Erlang sends byte
  streams to the c-program and the c program sends erlang terms encoded in
  erlang external binary format using ei-functions to do the encoding.  The
  size of the input/output buffert is dynamic.
*/

#include<stdio.h>
#include<stdlib.h>
#include<string.h>
#include<stdarg.h>

#if defined WIN32 || defined MULTITHREAD_WIN32
#include<windows.h> 
#include<io.h>
#include<fcntl.h>
#include<memory.h>
#else
#include<unistd.h>
#endif
#ifdef MULTITHREAD_UNIX
#include<pthread.h>
#endif

#define ODBCVER 0x0300
#ifdef MULTITHREAD_WIN32
#include<sql.h>
#include<sqlext.h>
#else 
#include "sql.h"
#include "sqlext.h"
#endif
#include "ei.h"
#include "ei_x_encode.h"


/* Debug */
#define DBG( proto ) if (dbgflag) Log proto


#define MAXCOLSIZE 4096 /* Size of a column with a datatypes without */
/* length. */

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

/* Constats defining the command protocol between the Erlang control process
   and the port program. These constants must also be defined in the same way
   in Erlang. */
#define OPEN_DB 1
#define CLOSE_DB 2
#define BIND_COLUMN 3
#define DESCRIBE_COLUMN 4
#define END_TRANSACTION 5
#define EXEDIR 6
#define FETCH_DATA 7
#define NUMBER_RESULT_COLUMNS 8
#define ROW_COUNT 9
#define SET_ATTRIBUTE 10
#define READ_BUFFER 11
#define CLOSE_HANDLE 12
#define EXECDB 13
#define DEBUG  14
#define LENGTH_INDICATOR_SIZE 4
#define INT_VALUE 1
#define STR_VALUE 2
#define DEBUG_ON 1
#define DEBUG_OFF 2

typedef unsigned char byte;
typedef int Boolean;

typedef struct {
  int selected;                   /* The column has been selected */
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
} DBState;

#define connection_handle(DBState) (DBState -> connection_handle)
#define environment_handle(DBState) (DBState -> environment_handle)
#define statement_handle(DBState) (DBState -> statement_handle)
#define columns(DBState) (DBState -> columns)
#define nr_of_columns(DBState) (DBState -> number_of_columns)
#define dynamic_buffer(DBState) (DBState -> dynamic_buffer)

/* Global variabel definition */
volatile static int dbgflag = 0;           /* debug flag */
volatile static int reclen = 0;                /* received messages len */

/* Thread handling */
volatile static int state = WAIT_FOR_NEW_MSG;  
#ifdef MULTITHREAD_WIN32
HANDLE threadh;
HANDLE Mutex;
HANDLE EventArrived;
HANDLE EventRecived;
#elif MULTITHREAD_UNIX
pthread_mutex_t Mutex = PTHREAD_MUTEX_INITIALIZER;
pthread_cond_t EventArrived = PTHREAD_COND_INITIALIZER;
pthread_cond_t EventRecived = PTHREAD_COND_INITIALIZER;
#endif

/* Main functions */
void main();
#ifdef MULTITHREAD_WIN32
DWORD WINAPI supervisor(LPVOID n);
#else
void database_handler();
#endif
static DbResultMsg handleRequest(byte *reqstring, DBState *state);

/* ODBC functions */
static DbResultMsg dbOpen(byte *connStrIn, DBState *state);
static DbResultMsg dbExecExt(byte *sql, DBState *state);
static DbResultMsg dbExecDir(byte *sql,DBState *state );
static DbResultMsg dbEndTran(byte compleationtype, DBState *state);
static DbResultMsg dbNumResultCols(DBState *state);
static DbResultMsg dbRowCount(DBState *state);
static DbResultMsg dbSetConnectAttr(byte *args, DBState *state);
static DbResultMsg dbBindCol(byte *ASCCI_column_nr, DBState *state);
static DbResultMsg dbDescribeCol(byte *ASCCI_column_nr, DBState *state);
static DbResultMsg dbFetch(DBState *state);
static DbResultMsg dbreadbuffer(byte *ASCCI_column_nr, DBState *state);
static DbResultMsg dbCloseHandle(DBState *state);
static DbResultMsg dbCloseConnection(DBState *state);

/* Help functions */
static void * safe_malloc(int size);
static void * safe_realloc(void * ptr, int size);
static int read_exact(byte *buf, int len);
static int write_exact(byte *buf, int len);
static void exit_on_failure(char *error_msg, ...);
static int received_length();
static void send_msg(byte *buf, int len);
static void receive_msg(byte *buffer, int len);
static DbResultMsg create_error_message(char *mes, char *result,
					int sqlRestult);
static DbResultMsg create_ok_message(int sqlRestult);

static DbResultMsg encode_column_name_list(SQLSMALLINT NumColumn,
					   DBState *state);
static DbResultMsg encode_value_list(SQLSMALLINT NumColumn, DBState *state);
static int encode_column(byte *buffer, int index, ColumnDef column,
			 int column_nr, DBState *state);  
static void encode_column_dyn(ColumnDef column, int column_nr, DBState *state);
static ColumnDef retrive_binary_data(ColumnDef column, int column_nr,
			      DBState *state);
static DbResultMsg encode_result_set(SQLSMALLINT NumColumn, DBState *state);
static ColumnDef * alloc_column_buffer(int n);
static void free_column_buffer(ColumnDef *columns, int n);



static Boolean sql_success(SQLRETURN result);
static DbResultMsg sql2c_column(int typ, int sz, int decimals, ColumnDef* column);
static Diagnos getDiagnos(SQLSMALLINT handleType, SQLHANDLE handle);


/* Help functions for thread handling */
static void main_init();
static void main_clean();
static void wait_for_request_received();
static void signal_request_arrived();
static void wait_for_request_arrival();
static void signal_request_recived();

/* Debug functions */
static DbResultMsg dbDebug(byte in);
static void Log(char *s, ...);
static void printbuffer(char *buffer, int len);

/* ----------------- Main functions ----------------------------------*/

#ifdef MULTITHREAD_WIN32
DWORD WINAPI supervisor(LPVOID n) {
#else
void main() {
#endif  
  int msg_len;

#ifdef MULTITHREAD_UNIX
  main_init();
#endif  
  DBG(("supervisor thread-> Start\n"));
  
  while ((msg_len = received_length()) > 0) {
    DBG(("supervisor thread -> Received message length  %d \n", msg_len));
    
    reclen = msg_len; /* Set global variable */
    
    signal_request_arrived(); /* Wake up the database handler thread */
    
    /* Wait until the database handler thread has received the request */
    wait_for_request_received(); 
  }
  DBG(("supervisor thread-> Stopped %d \n", msg_len));
  reclen = 0;
  
#ifdef MULTITHREAD_UNIX
  main_clean();
  exit(EXIT_SUCCESS);
}
#else
 exit(EXIT_SUCCESS); 
}
#endif

#ifdef MULTITHREAD_WIN32
void main() {
#else
void database_handler() { 
#endif
  DbResultMsg msg;
  byte *request_buffer = NULL;
  DBState state = {NULL, NULL, NULL, NULL, 0, {NULL, 0, 0}};
  byte cmd;

#ifdef MULTITHREAD_WIN32
  main_init();
#endif
  
  do { 
    wait_for_request_arrival();
    request_buffer = (byte *)safe_malloc(reclen);
    receive_msg(request_buffer, reclen);
    
    DBG(("Db-thread -> Request recived: %s\n", request_buffer));
    
    cmd  = request_buffer[0];
   
    if(cmd != OPEN_DB) {
      signal_request_recived(); /* Wake up the supervisor */
    }
    
    msg = handleRequest(request_buffer, &state);
    if(msg.length > 0) {
      send_msg(msg.buffer, msg.length); /* Send answer to erlang */
    }
    
    if (msg.dyn_alloc) {
      ei_x_free(&(state.dynamic_buffer));
    } else {
      free(msg.buffer);
      msg.buffer = NULL;
    }   
    
    free(request_buffer);
    request_buffer = NULL;
    
    if(cmd == OPEN_DB)
      {
	signal_request_recived();
      }
  } while(reclen > 0);
  
  DBG(("Db-thread -> End\n"));
#ifdef MULTITHREAD_WIN32
  main_clean();
#endif  
  exit(EXIT_SUCCESS);
}

/* Description: Calls the appropriate function to handle the database request
  recived from the erlang-process. Returns a message to send back to erlang. */
  
static DbResultMsg handleRequest(byte *reqstring, DBState *state) {
  byte *args;
  byte cmd_index;

  /*First byte is an index that identifies the requested command the
    rest is the argument string. */
  cmd_index = reqstring[0]; 
  args = reqstring + sizeof(byte);
  
  switch(cmd_index) {
  case OPEN_DB:
    return dbOpen(args, state);
  case CLOSE_DB:
    return dbCloseConnection(state);
  case BIND_COLUMN:
    return dbBindCol(args, state);
  case DESCRIBE_COLUMN:
    return dbDescribeCol(args, state);
  case END_TRANSACTION:
    /* In this case the args[0] is a byte that has the value
       SQL_COMMIT | SQL_ROLLBACK */
    return dbEndTran(args[0], state);
  case EXEDIR:
    return dbExecDir(args, state);
  case FETCH_DATA:
    return dbFetch(state);
  case NUMBER_RESULT_COLUMNS:
    return dbNumResultCols(state);
  case ROW_COUNT:
    return dbRowCount(state);
  case SET_ATTRIBUTE:
    return dbSetConnectAttr(args, state);
  case READ_BUFFER:
    return dbreadbuffer(args, state);
  case CLOSE_HANDLE:
    return dbCloseHandle(state);
  case EXECDB:
    return dbExecExt(args, state);
  case DEBUG:
    /* In this case the args[0] is a byte that has the value
       DEBUG_ON | DEBUG_OFF  */
    return dbDebug(args[0]);
  default:
    exit_on_failure("Unknown command %d\n", cmd_index);
    exit(EXIT_FAILURE); /* To make compiler happy */
  }  
}

/* ----------------- ODBC-functions  ----------------------------------*/

/* Description: Tries to open a connection to the database using <connStrIn>,
   returns a message indicating the outcome. */
static DbResultMsg dbOpen(byte *connStrIn, DBState *state) {
  SQLCHAR connStrOut[MAX_CONN_STR_OUT];
  SQLRETURN result;
  SQLSMALLINT stringlength2ptr, connlen;
  DbResultMsg msg;
  Diagnos diagnos;
  
  DBG(("dbOpen -> The connection string is: %s\n",connStrIn));
  
  if(!sql_success(SQLAllocHandle(SQL_HANDLE_ENV,
				 SQL_NULL_HANDLE,
				 &environment_handle(state))))
    exit_on_failure("SQLAllocHandle (environment) in dbOpen failed");
  
  if(!sql_success(SQLSetEnvAttr(environment_handle(state),
				SQL_ATTR_ODBC_VERSION,
				(SQLPOINTER)SQL_OV_ODBC3, 0)))
    exit_on_failure("SQLSetEnvAttr in dbOpen failed");
  
  if(!sql_success(SQLAllocHandle(SQL_HANDLE_DBC, environment_handle(state),
				 &connection_handle(state))))
    exit_on_failure("SQLAllocHandle (connection) in dbOpen failed");
  
  if(!sql_success(SQLSetConnectAttr(connection_handle(state),
				    SQL_ATTR_CONNECTION_TIMEOUT,
				    (SQLPOINTER)TIME_OUT, 0)))
    exit_on_failure("SQLSetConnectAttr in dbOpen failed");
  
  /* Connect to the database */
  connlen = (SQLSMALLINT)strlen((const char*)connStrIn);
  result = SQLDriverConnect(connection_handle(state), NULL,
			    (SQLCHAR *)connStrIn, 
			    connlen, 
			    connStrOut, (SQLSMALLINT)MAX_CONN_STR_OUT,
			    &stringlength2ptr, SQL_DRIVER_NOPROMPT);
  
  if (!sql_success(result)) {
    diagnos = getDiagnos(SQL_HANDLE_STMT, statement_handle(state));
    strcat((char *)diagnos.error_msg, " Connection to database failed.");
    msg = create_error_message("error", diagnos.error_msg, result);

    if(!sql_success(SQLFreeHandle(SQL_HANDLE_DBC, connection_handle(state))))
      exit_on_failure("SQLFreeHandle (connection) failed");
    if(!sql_success(SQLFreeHandle(SQL_HANDLE_ENV, environment_handle(state))))
      exit_on_failure("SQLFreeHandle (connection) failed");

    DBG(("dbOpen -> Return error message %s\n", msg));
    return msg;
  }
  DBG(("dbOpen -> Return ok message \n"));
  return create_ok_message(result);
}

/* Description: Executes an sql query and encodes the result set as an erlang
   term into the message buffer of the returned message-struct. */
static DbResultMsg dbExecExt(byte *sql, DBState *state)
{
  char *atom;
  int rowCount, elements, update;
  SQLSMALLINT NumColumn;
  SQLRETURN result;
  SQLINTEGER RowCountPtr;
  DbResultMsg msg;
  Diagnos diagnos;

  DBG(("dbExecExt -> SQL-query: %s\n", sql));
  
  msg.length = 0;
  msg.buffer = NULL;
  ei_x_new(&dynamic_buffer(state));
    
  if(!sql_success(SQLAllocHandle(SQL_HANDLE_STMT, connection_handle(state),
				 &statement_handle(state))))
    exit_on_failure("SQLAllocHandle in dbExecExt failed");
  
  result = SQLExecDirect(statement_handle(state), sql, SQL_NTS);
  if (!sql_success(result)) {
    diagnos =  getDiagnos(SQL_HANDLE_STMT, statement_handle(state));
    msg = create_error_message("error", diagnos.error_msg, result);
    return msg;
  }

  if(!sql_success(SQLNumResultCols(statement_handle(state), &NumColumn)))
    exit_on_failure("SQLNumResultCols in dbExecExt failed");
  
  DBG(("dbExecExt -> Number of columns: %d\n", NumColumn));

  if (NumColumn == 0) {
    elements = 2;
    atom = "updated";
    update = TRUE;
  } else {
    elements = 3;
    atom = "selected";
    update = FALSE;
  }

  if(!sql_success(SQLRowCount(statement_handle(state), &RowCountPtr)))
    exit_on_failure("SQLRowCount in dbExecExt failed"); 
  rowCount = (int)RowCountPtr;
  
  ei_x_new_with_version(&dynamic_buffer(state));
  ei_x_encode_tuple_header(&dynamic_buffer(state), elements);
  ei_x_encode_atom(&dynamic_buffer(state), atom);
  if (update) {
    DBG(("dbExecExt -> updated %d\n", rowCount));
    ei_x_encode_long(&dynamic_buffer(state), rowCount);
  } else {
    DBG(("dbExecExt -> selected %d\n", NumColumn));
    msg = encode_result_set(NumColumn, state);
  }

  if (statement_handle(state) != NULL) {
    if(!sql_success(SQLFreeHandle(SQL_HANDLE_STMT, statement_handle(state))))
      exit_on_failure("SQLFreeHandle in dbExecExt failed");
    statement_handle(state) = NULL;
  }
  
  if (msg.length != 0) { /* An error has occurred */
    ei_x_free(&(dynamic_buffer(state))); 
    return msg;
  } else {
    msg.buffer = (byte *)dynamic_buffer(state).buff;
    msg.length = dynamic_buffer(state).index;
    msg.dyn_alloc = TRUE;
    return msg;
  }
} 

/* Description: Executes an sql query. An ok or error message is returned */
static DbResultMsg dbExecDir(byte *sql, DBState *state) {
  SQLSMALLINT NumColumn;
  SQLRETURN result;
  Diagnos diagnos;

  DBG(("dbExecDir -> SQL-query: %s\n",sql));
  
  if(!sql_success(SQLAllocHandle(SQL_HANDLE_STMT, connection_handle(state),
				 &statement_handle(state))))
    exit_on_failure("SQLAllocHandle in dbExecDir failed");

  result = SQLExecDirect(statement_handle(state), sql, SQL_NTS);
  
  if(!sql_success(result)) {
    diagnos =  getDiagnos(SQL_HANDLE_STMT, statement_handle(state));
    DBG(("dbExecDir -> Return error message %s\n", diagnos.error_msg));
    return create_error_message("error", diagnos.error_msg, result);
  }
  
  if(!sql_success(SQLNumResultCols(statement_handle(state), &NumColumn)))
    exit_on_failure("SQLNumResultCols in dbExecDir failed");

  nr_of_columns(state) = (int)NumColumn;

  DBG(("dbExecDir -> Return ok message \n"));
  return create_ok_message(result);
}

/* Description: Requests a commit or rollback operation for all active
   operations on all statements associated with the connection
   handle <connection_handle(state)>. Returns an ok or error message. */
static DbResultMsg dbEndTran(byte compleationtype, DBState *state) {
  SQLRETURN result;
  Diagnos diagnos;

  DBG(("dbEndTran -> compleationtype: %d\n", compleationtype));
  
  result = SQLEndTran(SQL_HANDLE_DBC, connection_handle(state),
		      (SQLSMALLINT)compleationtype);

  if (!sql_success(result)) {
    diagnos = getDiagnos(SQL_HANDLE_DBC, connection_handle(state));
    DBG(("dbEndTran -> Return error message %s\n", diagnos.error_msg));
    return create_error_message("error", diagnos.error_msg, result);
  } else {
    DBG(("dbEndTran -> Return ok message \n"));
    return create_ok_message(result);
  }
}

/* Description: Finds out the number of columns in a result set and encodes the
   it as an erlang term into the message buffer of the returned
   message-struct.*/
static DbResultMsg dbNumResultCols(DBState *state) {
  SQLSMALLINT NumColumn;
  int index;
  SQLRETURN result;	
  DbResultMsg msg;
  Diagnos diagnos;
  
  result = SQLNumResultCols(statement_handle(state), &NumColumn);

  if (sql_success(result)) {
    DBG(("dbNumResultCols -> success\n"));
    /* Calculate needed buffer size for result */
    index = 0;
    ei_encode_version(NULL, &index);
    ei_encode_tuple_header(NULL, &index, 2);
    ei_encode_long(NULL, &index, result);
    ei_encode_long(NULL, &index, (int)NumColumn);
    msg.length = index;
    msg.buffer = (byte *)safe_malloc(index); 
    msg.dyn_alloc = FALSE;
    
    nr_of_columns(state) = (int)NumColumn;

    /* Encode result */
    index = 0;
    ei_encode_version((char*)msg.buffer, &index);
    ei_encode_tuple_header((char *)msg.buffer, &index, 2);
    ei_encode_long((char *)msg.buffer, &index, result);
    ei_encode_long((char *)msg.buffer, &index, (int)NumColumn);
    return msg;
  } else {
    DBG(("dbNumResultCols -> failed\n"));
    diagnos =  getDiagnos(SQL_HANDLE_STMT, statement_handle(state));
    msg = create_error_message("error", diagnos.error_msg, result);
    
    if(!sql_success(SQLFreeHandle(SQL_HANDLE_STMT, statement_handle(state))))
      exit_on_failure("SQLFreeHandle in dbNumResultCols failed");
    statement_handle(state) = NULL;
    return msg;
  }
}

/* Description: Finds out the number of rows affected by an UPDATE, INSERT, or
   DELETE statement and encodes the it as an erlang term into the message
   buffer of the returned message-struct */
static DbResultMsg dbRowCount(DBState *state) {
  int index;
  SQLINTEGER rowCount;
  SQLRETURN result;
  DbResultMsg msg;
  Diagnos diagnos;
  
  result = SQLRowCount(statement_handle(state), &rowCount);

  if (sql_success(result)) {
    DBG(("dbRowCount -> success\n"));
    /* Calculate needed buffer size for result */
    index = 0;
    ei_encode_version(NULL, &index);
    ei_encode_tuple_header(NULL, &index, 2);
    ei_encode_long(NULL, &index, result);
    ei_encode_long(NULL, &index, rowCount);
    msg.length = index;
    msg.buffer = (byte *)safe_malloc(index);
    msg.dyn_alloc = FALSE;
    
    /* Encode result */
    index = 0;
    ei_encode_version((char *)msg.buffer, &index);
    ei_encode_tuple_header((char *)msg.buffer, &index, 2);
    ei_encode_long((char *)msg.buffer, &index, result);
    ei_encode_long((char *)msg.buffer, &index, rowCount);
    return msg;
  } else {
    DBG(("dbRowCount -> failed\n"));
    diagnos = getDiagnos(SQL_HANDLE_STMT, statement_handle(state));
    msg = create_error_message("error", diagnos.error_msg, result);
    if(!sql_success(SQLFreeHandle(SQL_HANDLE_STMT, statement_handle(state))))
      exit_on_failure("SQLFreeHandle in dbRowCount failed");
    statement_handle(state) = NULL;
    return msg;
  }
}

/* Description: Sets connection attributes. Returns an ok or error message. */
static DbResultMsg dbSetConnectAttr(byte *args, DBState *state) {
  int index;
  SQLRETURN result;
  SQLINTEGER attribute, intValue;
  char *strValue;
  Diagnos diagnos;
  
  DBG(("dbSetConnectAttr -> inparameter:%s\n", args));
  attribute = atoi(strtok((char *)(args + sizeof(byte)), ";"));
  strValue = strtok(NULL, ";");
		     
  if(args[0] == INT_VALUE)
    {
      intValue = atoi(strValue);
      result = SQLSetConnectAttr(connection_handle(state), attribute,
				 (SQLPOINTER)intValue, 0);
    } else {  /* args[0] == STR_VALUE */
      result = SQLSetConnectAttr(connection_handle(state), attribute,
				 (SQLPOINTER)strValue, SQL_NTS);
    }
  
  if (!sql_success(result)) {
    diagnos = getDiagnos(SQL_HANDLE_DBC, connection_handle(state));
    DBG(("dbSetConnectAttr -> Return error message %s\n", diagnos.error_msg));
    return create_error_message("error", diagnos.error_msg, result);
  }
  DBG(("dbSetConnectAttr -> Return ok message \n"));
  return create_ok_message(result);
}


/* Description: Binds data buffers to columns in the result set. An ok or error
   message is returned */
static DbResultMsg dbBindCol(byte *ASCCI_column_nr, DBState *state) {
  int index, i, j;
  SQLSMALLINT NumColumn;
  SQLTCHAR cName[255];
  SQLSMALLINT cNameLen, cType, cDecDigits, cNullable;
  SQLUINTEGER cSize; 
  SQLRETURN result;	
  DbResultMsg list_result;
  DbResultMsg msg;
  Diagnos diagnos;
  
  DBG(("dbBindCol -> parameter:%s\n", ASCCI_column_nr));

  /* The columnarray starts at index 0 */
  j =  atoi((char *)ASCCI_column_nr);
  i =  j - 1;

  /* If the number of columns are not known */
  if (nr_of_columns(state) == 0) {
    result = SQLNumResultCols(statement_handle(state), &NumColumn);
    
    if(!sql_success(result)) {
      diagnos =  getDiagnos(SQL_HANDLE_STMT, statement_handle(state));
      DBG(("dbBindCol-> Return error message %s\n", diagnos.error_msg));
      return create_error_message("error", diagnos.error_msg, result);
    }
    nr_of_columns(state) = (int)NumColumn;
  }

  /* Create the column buffer it does not already exist */
  if(columns(state) == NULL)
    columns(state) = alloc_column_buffer(nr_of_columns(state));
  
  columns(state)[i].selected = 1;
  index = 0;
  result = SQLDescribeCol(statement_handle(state), (SQLSMALLINT)j, cName,
			  sizeof(cName), &cNameLen, &cType, &cSize,
			  &cDecDigits, &cNullable);
  if(!sql_success(result)) {
    free_column_buffer(columns(state), nr_of_columns(state));
    diagnos =  getDiagnos(SQL_HANDLE_STMT, statement_handle(state));
    DBG(("dbBindCol-> Return error message %s\n", diagnos.error_msg));
    return create_error_message("error", diagnos.error_msg, result);
  }
 
  /* The cType SQL_LONGVARCHAR or SQL_LONGVARBINARY has a */
  /* Size 2 GByte which must be limited to MAXCOLSIZE */
  if(cType == SQL_LONGVARCHAR || cType == SQL_LONGVARBINARY) {
    cSize = MAXCOLSIZE;
  }
  
  msg = sql2c_column(cType, cSize, cDecDigits, &columns(state)[i]);
  if(msg.length > 0) {
    free_column_buffer(columns(state), nr_of_columns(state));
    DBG(("dbBindCol-> Return error message %s\n", msg));
    return msg;
  }
  if (columns(state)[i].len > 0) {
    columns(state)[i].buf = (char *)safe_malloc(columns(state)[i].len);
    if(columns(state)[i].type == SQL_C_BINARY) {
    } else {
      result =
	SQLBindCol(statement_handle(state), (SQLSMALLINT)(i+1),
		   columns(state)[i].type, columns(state)[i].buf,
		   columns(state)[i].len, &columns(state)[i].resultLen);
      if (!sql_success(result)) {
	diagnos = getDiagnos(SQL_HANDLE_STMT, statement_handle(state));
	msg = create_error_message("error", diagnos.error_msg, result);
	free_column_buffer(columns(state), nr_of_columns(state));
	if(!sql_success(SQLFreeHandle(SQL_HANDLE_STMT,
				      statement_handle(state))))
	  exit_on_failure("SQLFreeHandle in dbBindCol failed");
	statement_handle(state) = NULL;
	DBG(("dbBindCol-> Return error message %s\n", msg));
	return msg;
      }
    } 
  }
  else {
    columns(state)[i].len = 0;
    columns(state)[i].buf = NULL;
  }
  DBG(("dbBindCol-> Return ok message \n"));
  return create_ok_message(result); 
}

/* Description: Returns the result descriptor-column name, and nullability-for
   one column in the result set.*/
static DbResultMsg dbDescribeCol(byte *ASCCI_column_nr, DBState *state) {
  int index, j;
  SQLTCHAR cName[255];
  SQLSMALLINT cNameLen, cType, cDecDigits, cNullable;
  SQLUINTEGER cSize; 
  SQLRETURN result;	
  DbResultMsg msg;
  Diagnos diagnos;
  
  DBG(("dbDescribeCol -> parameter:%s\n", ASCCI_column_nr));

  /* selected column */
  j = atoi((char *)ASCCI_column_nr);

  result = SQLDescribeCol(statement_handle(state), (SQLSMALLINT)j, cName,
			  sizeof(cName), &cNameLen, &cType, &cSize,
			  &cDecDigits, &cNullable);

  if (sql_success(result)) {
     /* Calculate needed buffer size for result */
    index = 0;
    ei_encode_version(NULL, &index);
    ei_encode_tuple_header(NULL, &index, 3);
    ei_encode_long(NULL, &index, result);
    ei_encode_string(NULL, &index, (char *)cName);
    ei_encode_long(NULL, &index, cNullable);
    msg.length = index;
    msg.buffer = (byte *)safe_malloc(index); 
    msg.dyn_alloc = FALSE;
    
    DBG(("dbDescribeCol -> columnname %s\n",cName));

    /* Encode result */
    index = 0;
    ei_encode_version((char*)msg.buffer, &index);
    ei_encode_tuple_header((char *)msg.buffer, &index, 3);
    ei_encode_long((char *)msg.buffer, &index, result);
    ei_encode_string((char *)msg.buffer, &index, (char *)cName);
    ei_encode_long((char *)msg.buffer, &index, cNullable);

    return msg;
  } else { /* error */
    diagnos = getDiagnos(SQL_HANDLE_STMT, statement_handle(state));
    msg = create_error_message("error", diagnos.error_msg, result);
  
    if(!sql_success(SQLFreeHandle(SQL_HANDLE_STMT, statement_handle(state))))
      exit_on_failure("SQLFreeHandle in dbDescribeCol failed"); 
    statement_handle(state) = NULL;
    DBG(("dbDescribeCol-> Return error message %s\n", msg));
    return msg;
  }
}

/* Description: Fetches the next rowset of data from the result set and writes
   data in the bound columns buffers. Returns an ok or error message. */
static DbResultMsg dbFetch(DBState *state) {
  DbResultMsg msg;
  SQLRETURN result;
  Diagnos diagnos;
  
  result = SQLFetch(statement_handle(state));

  if(sql_success(result) || result == SQL_NO_DATA) {
    DBG(("dbFetch -> Return ok message \n"));
    return create_ok_message(result);
  } else {
    diagnos = getDiagnos(SQL_HANDLE_STMT, statement_handle(state));
    msg = create_error_message("error", diagnos.error_msg, result);
    free_column_buffer(columns(state), nr_of_columns(state));
    if(!sql_success(SQLFreeHandle(SQL_HANDLE_STMT, statement_handle(state))))
      exit_on_failure("SQLFreeHandle in dbFetch failed");
    statement_handle(state) = NULL;
    DBG(("dbFetch-> Return error message %s\n", msg));
    return msg;
  }
}

/* Description: Reads the databuffer for a column previously bound by
   dbBindCol and fetched by dbFetch. Encodes the data as an erlang
   term into the message buffer of the returned message-struct.*/
static DbResultMsg dbreadbuffer(byte *ASCCI_column_nr, DBState *state) {
  int index, i;
  int ret;
  ColumnDef column;
  DbResultMsg msg;
  
  /* Columnarray start at index 0 */
  i = atoi((char *)ASCCI_column_nr) - 1;
  
  DBG(("dbreadbuffer -> columntype: %d \n", columns(state)[i].type));
  DBG(("ddreadbuffer -> columntypename %s\n",columns(state)[i].typename));
  DBG(("dbreadbuffer -> columnbufferlength: %d \n", columns(state)[i].len));
  
  /* Calculate needed buffer size for result */
  index = 0;
  ei_encode_version(NULL, &index);
  ei_encode_tuple_header(NULL, &index, 2);
  ei_encode_atom(NULL, &index, "ok");
  index = encode_column(NULL, index, columns(state)[i], i, state);
  msg.length = index;
  msg.buffer = (byte *)safe_malloc(index);
  msg.dyn_alloc = FALSE;
  
  /* Encode result */
  index = 0;
  ei_encode_version((char *)msg.buffer, &index);
  ei_encode_tuple_header((char *)msg.buffer, &index, 2);
  ei_encode_atom((char *)msg.buffer, &index, "ok");
  index = encode_column(msg.buffer, index, columns(state)[i], i, state);

  return msg;
}

/* Description: Frees resources associated with the current 
   statement handle keeped in the state.Returns an ok or error message.*/
static DbResultMsg dbCloseHandle(DBState *state) {
  DbResultMsg msg;
  SQLRETURN result;
  Diagnos diagnos;
  
  if (statement_handle(state) != NULL) {
    result = SQLFreeHandle(SQL_HANDLE_STMT, statement_handle(state));
    
    if (!sql_success(result)) {
      diagnos = getDiagnos(SQL_HANDLE_STMT, statement_handle(state));
      msg = create_error_message("error", diagnos.error_msg, result);
      free_column_buffer(columns(state), nr_of_columns(state));
      DBG(("dbCloseHandle -> Return error message %s\n", msg));
      return msg;
    }
  }
  statement_handle(state) = NULL;
  free_column_buffer(columns(state), nr_of_columns(state));
  DBG(("dbCloseHandle -> Return ok message\n"));
  return create_ok_message(result); 
}

/* Close the connection to the database. Returns an ok or error message. */
static DbResultMsg dbCloseConnection(DBState *state) {
  int index;
  SQLRETURN result;
  Diagnos diagnos;
  
  result = SQLDisconnect(connection_handle(state));
  
  if (!sql_success(result)) {
    diagnos = getDiagnos(SQL_HANDLE_DBC, connection_handle(state));
    DBG(("dbCloseConnection -> Return error message %s\n", diagnos.error_msg));
    return create_error_message("error", diagnos.error_msg, result);
  }

  if(!sql_success(SQLFreeHandle(SQL_HANDLE_DBC, connection_handle(state))))
    exit_on_failure("SQLFreeHandle (connection) in dbCloseConnection failed"); 
  if(!sql_success(SQLFreeHandle(SQL_HANDLE_ENV, environment_handle(state))))
    exit_on_failure("SQLFreeHandle (environment) in dbCloseConnection failed");

  DBG(("dbCloseConnection -> Return ok message\n"));
  return create_ok_message(result);
}

/* ----------------- Help functions ----------------------------------*/

static void *safe_malloc(int size) {
  void *memory;
  
  memory = (void *)malloc(size);
  if (memory == NULL) 
    exit_on_failure("malloc failed to allocate memory.");

  return memory;
}

static void *safe_realloc(void *ptr, int size) {
  void *memory;

  memory = (void *)realloc(ptr, size);

  if (memory == NULL)
    {
      free(ptr);
      exit_on_failure("realloc failed to allocate memory.");
    }
  return memory;
}
  
static void exit_on_failure(char *error_msg, ...) {
  /* a pointer to the variable argument list */
  va_list arg_pointer;
  
  /*Init the argument pointer */
  va_start(arg_pointer, error_msg);

  vfprintf(stderr, error_msg, arg_pointer);
  
  va_end(arg_pointer);

  exit(EXIT_FAILURE);
}

/* read from stdin */ 
static int read_exact(byte *buffer, int len) {
  int i, got = 0;

  do {
    if ((i = read(0, buffer+got, len-got)) <= 0)
      return(i);
    got += i;
  } while (got<len);
  return len;
}

/* write to stdout */
static int write_exact(byte *buffer, int len) {
  int i, wrote = 0;

  do {
    if ((i = write(1, buffer+wrote, len-wrote)) <= 0)
      return i;
    wrote += i;
  } while (wrote<len);
  return len;
}

/* Receive the length-header of a message and
   change the received length from big endian to little endian */
static int received_length() {
  int len, i;
  byte buffer[LENGTH_INDICATOR_SIZE];

  DBG(("received_length -> Entered\n"));
  
  len = 0;
  if(read_exact(buffer,LENGTH_INDICATOR_SIZE) != LENGTH_INDICATOR_SIZE)
    {
      DBG(("received_length -> FOOBAR\n"));
      return(-1);
    }
  for(i=0; i < LENGTH_INDICATOR_SIZE; i++) {
    len <<= 8;
    len |= buffer[i];
  }
  return len;
}
 
/* Send (write) data to erlang on stdout */
static void send_msg(byte *buffer, int len) {
  int result;
  unsigned char lengthstr[LENGTH_INDICATOR_SIZE];

  lengthstr[0] = (len >> 24) & 0x000000FF;
  lengthstr[1] = (len >> 16) & 0x000000FF;
  lengthstr[2] = (len >> 8) & 0x000000FF;
  lengthstr[3] = len &  0x000000FF;

  result = write_exact(lengthstr, LENGTH_INDICATOR_SIZE);

  if (result != LENGTH_INDICATOR_SIZE) 
    exit_on_failure("Could not write to stdout");

  result = write_exact(buffer, len);
  if (result != len)
    exit_on_failure("Could not write to stdout");
}

/* Recieive (read) data from erlang on stdin */
static void receive_msg(byte *buffer, int len) {
  if (read_exact(buffer, len) <= 0) 
    exit_on_failure("Could not read from stdin");    
}
     

/* Description: Encode an error-message to send back to erlang*/
static DbResultMsg create_error_message(char *mes, char *result,
					int sqlRestult) {
  int index;
  DbResultMsg msg;
  
  index = 0;
  ei_encode_version(NULL, &index);
  ei_encode_tuple_header(NULL, &index, 3);
  ei_encode_atom(NULL, &index, mes);
  ei_encode_string(NULL, &index, result);
  ei_encode_long(NULL, &index, sqlRestult);

  msg.length = index;
  msg.buffer = (byte *)safe_malloc(index);
  msg.dyn_alloc = FALSE;
  
  index = 0;
  ei_encode_version((char *)msg.buffer, &index);
  ei_encode_tuple_header((char *)msg.buffer, &index, 3);
  ei_encode_atom((char *)msg.buffer, &index, mes);
  ei_encode_string((char *)msg.buffer, &index, result);
  ei_encode_long((char *)msg.buffer, &index, sqlRestult);
  
  return msg;
}

/* sqlResult - SQL_SUCCESS | SQL_SUCCESS_WITH_INFO 
   Description: Encode a messge telling erlang the operation was a sucess */
static DbResultMsg create_ok_message(int sqlResult) {
  int index;
  DbResultMsg msg;
  
  index = 0;
  ei_encode_version(NULL, &index);
  ei_encode_long(NULL, &index, sqlResult);

  msg.length = index;
  msg.buffer = (byte *)safe_malloc(index);
  msg.dyn_alloc = FALSE;
  
  index = 0;
  ei_encode_version((char *)msg.buffer, &index);
  ei_encode_long((char *)msg.buffer, &index, sqlResult);
  
  return msg;
}
 
/* Description: Allocate memory for n columns */
static ColumnDef * alloc_column_buffer(int n) {
  int i;
  ColumnDef *columns;
  
  columns = (ColumnDef *)safe_malloc(n * sizeof(ColumnDef));
  for(i = 0; i < n; i++)
    columns[i].buf = NULL;
  
  return columns;
}

/* Description: Deallocate memory allocated by alloc_column_buffer */
static void free_column_buffer(ColumnDef *columns, int n) {
  int i;
  if(columns != NULL) {
    for (i = 0; i < n; i++) {
      free(columns[i].buf);
    }
    free(columns);
    columns = NULL;
  }
}

static Boolean sql_success(SQLRETURN result) {
  return result == SQL_SUCCESS || result == SQL_SUCCESS_WITH_INFO;
}

/* Description: Transform SQL columntype to C columntype. Returns a dummy
 DbResultMsg with length 0 on success and an errormessage otherwise.*/
static DbResultMsg sql2c_column(int typ, int size, int decimals,
				ColumnDef* column) {
  DbResultMsg msg;

  msg.length = 0;
  msg.buffer = NULL;
  
  switch(typ) {
  case SQL_UNKNOWN_TYPE:
    msg = create_error_message("error", "Unknown column type", SQL_ERROR);
    break;
  case SQL_CHAR:
    column->len = size + 1;
    column->type = SQL_C_CHAR;
    column->typename = "SQL_CHAR";
     break;
  case SQL_NUMERIC:
    column->len = sizeof(double);
    column->type = SQL_C_DOUBLE;
    column->typename = "SQL_NUMERIC";
     break;
  case SQL_DECIMAL:
    column->len = 50;
    column->type = SQL_C_CHAR;
    column->typename = "SQL_DECIMAL";
     break;
  case SQL_INTEGER:
    column->len = sizeof(long);
    column->type = SQL_C_SLONG;
    column->typename = "SQL_INTEGER";
     break;
  case SQL_SMALLINT:
    column->len = sizeof(long);
    column->type = SQL_C_SLONG;
    column->typename = "SQL_SMALLINT";
     break;
  case SQL_FLOAT:
    column->len = sizeof(double);
    column->type = SQL_C_DOUBLE;
    column->typename = "SQL_FLOAT";
     break;
  case SQL_REAL:
    column->len = sizeof(double);
    column->type = SQL_C_DOUBLE;
    column->typename = "SQL_REAL";
     break;
  case SQL_DOUBLE:
    column->len = sizeof(double);
    column->type = SQL_C_DOUBLE;
    column->typename = "SQL_DOUBLE";
     break;
  case SQL_VARCHAR:
    column->len = size + 1;
    column->type = SQL_C_CHAR;
    column->typename = "SQL_VARCHAR";
    break;
  case SQL_TYPE_DATE:
  case SQL_TYPE_TIME:
  case SQL_TYPE_TIMESTAMP:
    /* erlang dates instead? */
    column->len= 30;
    column->type = SQL_C_CHAR;
    column->typename = "SQL_TYPE_DATE/TIME";
     break;
  case SQL_LONGVARCHAR:
    column->len = size + 1;
    column->type = SQL_C_BINARY;
    column->typename = "SQL_LONGVARCHAR";
     break;
  case SQL_BINARY:
    column->len = size + 1;
    column->type = SQL_C_BINARY;
    column->typename = "SQL_BINARY";
     break;
  case SQL_VARBINARY:
    column->len = size + 1;
    column->type = SQL_C_BINARY;
    column->typename = "SQL_VARBINARY";
     break;
  case SQL_LONGVARBINARY:
    column->len = size + 1;
    column->type = SQL_C_BINARY;
    column->typename = "SQL_LONGVARBINARY";
     break;
  case SQL_BIGINT:
    column->len = 100;
    column->type = SQL_C_CHAR;
    column->typename = "SQL_BIGINT";
     break;
  case SQL_TINYINT:
    column->len = sizeof(int);
    column->type = SQL_C_SLONG;
    column->typename = "SQL_TINYINT";
     break;
  case SQL_BIT:
    column->len = 7;
    column->type = SQL_C_BIT;
    column->typename = "SQL_BIT";
     break;
  default:
    msg = create_error_message("error", "Column type not supported",
			       SQL_ERROR);
    break;
  }
  return msg;
}

/* Description: An ODBC function can post zero or more diagnostic records each
   time it is called. This function loops through the current set of diagnostic
   records scaning for error messages and the sqlstate. If this function is
   called when no error has ocurred only the sqlState field may be
   referenced.*/
static Diagnos getDiagnos(SQLSMALLINT handleType, SQLHANDLE handle) {
  Diagnos diagnos;
  SQLINTEGER nativeError;
  SQLSMALLINT errmsg_buffer_size, record_nr, errmsg_size;
  int acc_errmsg_size;
  byte errmsg_buffer[MAX_ERR_MSG];
  
  DBG(("getDiagnos -> entered\n"));
  /* number bytes free in error message buffer */
  errmsg_buffer_size = MAX_ERR_MSG - ERRMSG_HEADR_SIZE;  
  acc_errmsg_size = 0;   /* number bytes used in the error message buffer */

  /* Foreach diagnostic record in the current set of diagnostic records
     the error message is obtained */
  for(record_nr = 1; ;record_nr++) {    
    if(SQLGetDiagRec(handleType, handle, record_nr, diagnos.sqlState,
		     &nativeError, errmsg_buffer,
		     (SQLSMALLINT)errmsg_buffer_size, &errmsg_size)
       != SQL_SUCCESS) {
      break;
    } else {
      DBG(("getDiagnos -> errormsg found\n")); 
      errmsg_buffer_size = errmsg_buffer_size - errmsg_size;
      acc_errmsg_size = acc_errmsg_size + errmsg_size;
    }
  }
    
  if(acc_errmsg_size == 0) {
    DBG(("getDiagnos -> Acc error empty: \n"));
    strcat((char *)errmsg_buffer, "No SQL-driver information available.");
    diagnos.error_msg = (char *)errmsg_buffer;
    DBG(("getDiagnos -> Msg: %s\n", diagnos.error_msg));
  }
  else {
    DBG(("getDiagnos -> Acc error: %s\n", errmsg_buffer));
    strcat(strcat((char *)errmsg_buffer, " SQLSTATE IS: "),
	   (char *)diagnos.sqlState);
    diagnos.error_msg = (char *)errmsg_buffer;
  }
  return diagnos;
}

/* Description: Encodes the result set into the "ei_x" - dynamic_buffer held by
   the state variable */
static DbResultMsg encode_result_set(SQLSMALLINT NumColumn, DBState *state) {

  DbResultMsg msg;

  DBG(("encode_result_set -> start \n"));
  
  columns(state) = alloc_column_buffer(NumColumn);
    
  msg = encode_column_name_list(NumColumn, state);
  if (msg.length == 0) { /* If no error has occurred */   
    msg = encode_value_list(NumColumn, state);
  }

  free_column_buffer(columns(state), NumColumn);
  
  DBG(("encode_result_set -> end \n"));
  return msg;
}  

/* Description: Encodes the list of column names into the "ei_x" -
   dynamic_buffer held by the state variable */
static DbResultMsg encode_column_name_list(SQLSMALLINT NumColumn,
					   DBState *state) {
  int i;
  DbResultMsg msg;
  SQLTCHAR cName[255];
  SQLSMALLINT cNameLen, cType, cDecDigits, cNullable;
  SQLUINTEGER cSize; 
  SQLRETURN result;

  msg.length = 0;
  msg.buffer = NULL;

  DBG(("encode_column_name_list -> start \n"));
  
  ei_x_encode_list_header(&dynamic_buffer(state), NumColumn);
  
  for (i = 0; i < NumColumn; ++i) {

    if(!sql_success(SQLDescribeCol(statement_handle(state), (SQLSMALLINT)(i+1),
				   cName, sizeof(cName), &cNameLen, &cType,
				   &cSize, &cDecDigits, &cNullable)))
      exit_on_failure("SQLDescribeCol in encode_column_name_list failed");

    if(cType == SQL_LONGVARCHAR || cType == SQL_LONGVARBINARY)
      cSize = MAXCOLSIZE;
    
    msg = sql2c_column(cType, cSize, cDecDigits, &columns(state)[i]);
    if (msg.length > 0) {
      return msg; /* An error has occurred */
    } else {
      if (columns(state)[i].len > 0) {
	columns(state)[i].buf = (char *)safe_malloc(columns(state)[i].len);
	
	if (columns(state)[i].type == SQL_BINARY) {
	  /* retrived later by retrive_binary_data */
	}else {
	  if(!sql_success(SQLBindCol(statement_handle(state),
				     (SQLSMALLINT)(i+1),
				     columns(state)[i].type,
				     columns(state)[i].buf,
				     columns(state)[i].len,
				     &columns(state)[i].resultLen)))
	    exit_on_failure("SQLBindCol in encode_column_name_list failed");
	}
	ei_x_encode_string_len(&dynamic_buffer(state),
			       (char *)cName, cNameLen);
      }
      else {
	columns(state)[i].len = 0;
	columns(state)[i].buf = NULL;
      }
    }  
  }
  ei_x_encode_empty_list(&dynamic_buffer(state)); 

  DBG(("encode_column_name_list -> end \n"));
  return msg;
}

/* Description: Encodes the list(s) of column values into the "ei_x" -
   dynamic_buffer held by the state variable */
static DbResultMsg encode_value_list(SQLSMALLINT NumColumn, DBState *state) {
  int i, msg_len;
  SQLRETURN result;
  DbResultMsg list_result;
  DbResultMsg msg;

  msg.length = 0;
  msg.buffer = NULL;

  DBG(("encode_value_list -> start, nr of columns: %d\n", NumColumn));
  
  for (;;) {
    /* fetch the next row */
    result = SQLFetch(statement_handle(state)); 
    
    if (result == SQL_NO_DATA) /* Reached end of result set */
      {
	DBG(("encode_value_list -> Reached end of result set \n"));
	break;
      }

    ei_x_encode_list_header(&dynamic_buffer(state), 1);
    ei_x_encode_list_header(&dynamic_buffer(state), NumColumn);
    
    for (i = 0; i < NumColumn; i++) {
      encode_column_dyn(columns(state)[i], i, state);
    }
    
    ei_x_encode_empty_list(&dynamic_buffer(state));
  } 
  ei_x_encode_empty_list(&dynamic_buffer(state));
  return msg;
}

/* Description: Encodes a column value into the buffer <buffer>.*/
static int encode_column(byte *buffer, int index, ColumnDef column,
			 int column_nr, DBState *state) {
  DBG(("encode_column -> Column type %d\n", column.type));
  
  if(column.len == 0 || column.resultLen == SQL_NULL_DATA)
    ei_encode_atom((char *)buffer, &index, "null");
  else {
    switch(column.type) {
    case SQL_C_CHAR:
      ei_encode_string((char *)buffer, &index, column.buf);
      break;
    case SQL_C_DOUBLE:
      ei_encode_double((char *)buffer, &index, *(double *)column.buf);
      break;
    case SQL_C_SLONG:
      ei_encode_long((char *)buffer, &index, *(long *)column.buf);
      break;
    case SQL_C_BIT:
      ei_encode_atom((char *)buffer, &index,  column.buf[0]?"true":"false");
      break;
    case SQL_C_BINARY:
      if (buffer == NULL) {  
      	column = retrive_binary_data(column, column_nr, state);
      }
      ei_encode_string((char *)buffer, &index,  column.buf);
      break;
    default:
      ei_encode_atom((char *)buffer, &index, "error");
      break;
    } 
  } 
  return index;
}

/* Description: Encodes the a column value into the "ei_x" - dynamic_buffer
   held by the state variable */
static void encode_column_dyn(ColumnDef column, int column_nr, DBState *state){
  DBG(("encode_column_dyn -> Column type %d\n", column.type));
  
  if (column.len == 0 || column.resultLen == SQL_NULL_DATA) {
    ei_x_encode_atom(&dynamic_buffer(state), "null");
  } else {
    switch(column.type) {
    case SQL_C_CHAR:
      ei_x_encode_string(&dynamic_buffer(state), column.buf);
      break;
    case SQL_C_SLONG:
      ei_x_encode_long(&dynamic_buffer(state), *(long*)column.buf);
      break;
    case SQL_C_DOUBLE:
      ei_x_encode_double(&dynamic_buffer(state), *(double*)column.buf);
      break;
    case SQL_C_BIT:
      ei_x_encode_atom(&dynamic_buffer(state), column.buf[0]?"true":"false");
      break;
    case SQL_C_BINARY:
      column = retrive_binary_data(column, column_nr, state);
      ei_x_encode_string(&dynamic_buffer(state), column.buf);
      break;
    default:
      ei_x_encode_atom(&dynamic_buffer(state), "error");
      break;
    }
  } 
}

/* Description: More than one call to SQLGetData may be required to retrieve
   data from a single column with  binary data. SQLGetData then returns
   SQL_SUCCESS_WITH_INFO and the SQLSTATE will have the value 01004 (Data
   truncated). The application can then use the same column number to retrieve
   subsequent parts of the data until SQLGetData returns SQL_SUCCESS,
   indicating that all data for the column has been retrieved. */

static ColumnDef retrive_binary_data(ColumnDef column, int column_nr,
				     DBState *state) { 
  char *outputptr;
  char *sqlState;
  int blocklen, outputlen, result;
  Diagnos diagnos;
  
  blocklen = column.len;
  outputptr = column.buf;
  result = SQLGetData(statement_handle(state), (SQLSMALLINT)(column_nr+1),
		      SQL_C_CHAR, outputptr,
		      blocklen, &column.resultLen);
  
  while (result == SQL_SUCCESS_WITH_INFO) {
    
    diagnos = getDiagnos(SQL_HANDLE_STMT, statement_handle(state));
    
    if(strcmp((char *)diagnos.sqlState, TRUNCATED) == 0) {
      outputlen = column.len - 1;
      column.len = outputlen + blocklen;
      column.buf = safe_realloc((void *)column.buf, column.len);
      outputptr = column.buf + outputlen;
      result = SQLGetData(statement_handle(state),
			  (SQLSMALLINT)(column_nr+1), SQL_C_CHAR,
			  outputptr, blocklen, &column.resultLen);
    }
  }
  
  if (result == SQL_SUCCESS) {
    return column;
  } else {
    exit_on_failure("Failed to retrive binary data");
    exit(EXIT_FAILURE); /* to make compiler happy */
  }
}

/* ---------- Help functions for thread handling ----------------------- */

static void main_init() {
  int result;
#ifdef MULTITHREAD_WIN32
  DWORD threadId;
#elif MULTITHREAD_UNIX
  pthread_t thread;
#endif

#if defined WIN32 || defined MULTITHREAD_WIN32 
  _setmode(_fileno( stdin),  _O_BINARY);
  _setmode(_fileno( stdout), _O_BINARY);
#endif
  
#ifdef MULTITHREAD_WIN32
  EventArrived = CreateEvent(NULL, FALSE, FALSE, NULL);
  EventRecived = CreateEvent(NULL, FALSE, FALSE, NULL);
  threadh = (HANDLE)_beginthreadex(NULL, 0, supervisor, 0, 0, &threadId);
  Mutex = CreateMutex(NULL, FALSE, NULL);
#elif MULTITHREAD_UNIX
  result = pthread_create(&thread, NULL,
			  (void *(*)(void *))database_handler, NULL);
  if (result != 0)
    exit_on_failure("Failed to create thread");
#endif
}

static void main_clean() {
#ifdef MULTITHREAD_WIN32
  CloseHandle(EventArrived);
  CloseHandle(EventRecived);
  CloseHandle(Mutex);
#elif MULTITHREAD_UNIX
  pthread_cond_destroy(&EventArrived);
  pthread_cond_destroy(&EventRecived);
  pthread_mutex_destroy(&Mutex);
#endif
}

static void wait_for_request_received() {
  DBG((" supervisor -> Wait for request to be recived \n"));
#ifdef MULTITHREAD_WIN32
  WaitForSingleObject(Mutex, INFINITE);
  while (TRUE) {
    if (state != WAIT_FOR_NEW_MSG) {
      ReleaseMutex(Mutex);
      WaitForSingleObject(EventRecived, INFINITE);
      continue; 
    }
    break;
  }
#elif MULTITHREAD_UNIX
  pthread_mutex_lock(&Mutex);
  while (state != WAIT_FOR_NEW_MSG) {
    /* As spurious wakeups from the pthread_cond_wait()  
       function  may occur. */
    while(pthread_cond_wait(&EventRecived, &Mutex) != 0);
  }
#endif
}
  
static void signal_request_arrived() {
  DBG(("supervisor -> signal request arrived \n")); 
#ifdef MULTITHREAD_WIN32
  state = NEW_MSG_ARRIVED;
  ReleaseMutex(Mutex);
  PulseEvent(EventArrived);
#elif MULTITHREAD_UNIX
  state = NEW_MSG_ARRIVED;
  pthread_cond_signal(&EventArrived);
  pthread_mutex_unlock(&Mutex);
#endif 
}

static void wait_for_request_arrival()
{
  DBG(("DB-thread -> Wait for request to arrive \n"));
#ifdef MULTITHREAD_WIN32
  WaitForSingleObject(Mutex, INFINITE);
  while(TRUE) {
    if (state != NEW_MSG_ARRIVED) {
      ReleaseMutex(Mutex);
      WaitForSingleObject(EventArrived, INFINITE);
      continue;
    }
    break;
  }
#elif MULTITHREAD_UNIX
  pthread_mutex_lock(&Mutex);  
  while(state != NEW_MSG_ARRIVED) {
    /* As spurious wakeups from the pthread_cond_wait()  
       function  may occur. */
    while(pthread_cond_wait(&EventArrived, &Mutex) != 0);
  }
#endif   
}

static void signal_request_recived()
{
  DBG(("DB-thread -> Signal request recived\n"));
#ifdef MULTITHREAD_WIN32
  state = WAIT_FOR_NEW_MSG;
  ReleaseMutex(Mutex);
  PulseEvent(EventRecived);  
#elif  MULTITHREAD_UNIX
  state = WAIT_FOR_NEW_MSG;
  pthread_cond_signal(&EventRecived);
  pthread_mutex_unlock(&Mutex);
#endif
  
}

/* ------------------------- Debug functions ---------------------------- */

/* Description: Switch on or off the debuging */
static DbResultMsg dbDebug(byte state) {
  DbResultMsg msg;
  msg.length = 0;
  msg.buffer = NULL;
  msg.dyn_alloc = FALSE;
  
  switch(state) {
  case DEBUG_ON:
    dbgflag = TRUE;
    break;
  default: /* state == DEBUG_OFF */
    dbgflag = FALSE;
  }
  return msg; /* return dummy message */
}

/* Description: Write debug info to the file odbc_debug.log */
static void Log(char *s, ...){
  FILE *f;
  va_list args;
  va_start(args, s); /* The first arg without name */
  if ((f = fopen("odbc_debug.log", "a"))) {
    vfprintf(f, s, args);
    fclose(f);
  }
  else
    vfprintf(stderr, s, args);
  va_end(args);
}

static void printbuffer(char *buffer, int len) {
  int i;

  DBG((" The buffer contains: "));
  for (i = 0; i < len; i++)
    DBG(("%02x ",(unsigned char)buffer[i]));
  DBG(("\n"));
}
