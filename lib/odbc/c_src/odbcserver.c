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
 * Purpose: Implementation of ODBC (Open Database Connectivity).
 * This version of odbc use open_port communication to erlang 
 * and encode information with ei-functions. 
 * The size of the input/output buffert is dynamic
*/

#ifdef WIN32
#include<windows.h> 
#include<io.h>
#endif

#include<stdio.h>
#include<fcntl.h>
#include<stdlib.h>
#include<string.h>
#include<memory.h>
#include<stdarg.h>
#ifndef WIN32
#include<unistd.h>
#endif
#include "ei.h"

#define ODBCVER 0x0300
#ifdef WIN32
#include<sql.h>
#include<sqlext.h>
#else
#include "sql.h"
#include "sqlext.h"
#endif

#if defined(DEBUG)
#define DBG( proto ) Log proto
#else
#define DBG( proto ) ((void) 0)
#endif

#define COMMANDLEN 6 /* Maximum characters in odbc-command */
#define COMMANDS 14  /* number of odbc-commands */
#ifdef WIN32
#define MAX_BUFFER_SIZE 2000000 /* Maximum size of a column */
#else
#define MAX_BUFFER_SIZE 2000000
#endif
typedef unsigned char byte;
typedef enum {_FALSE, _TRUE} MYBOOL;

typedef int (*RHANDLER)(char *);

typedef struct {
  MYBOOL selected;                   /* The column is selected */
  char *columnName;       
  char *buf;
  SQLSMALLINT type;
  SQLINTEGER len;
  SQLINTEGER resultLen;
} ColumnDef;

typedef struct {
  char name[COMMANDLEN];
  RHANDLER handler;
} HandlerRecord;

/* Global variabel definition */
static char *buf;
SQLHDBC hdbc1;       /* ConnectionHandle */
SQLHENV henv1;       /* EnvironementHandle */
SQLHSTMT hstmt;      /* StatementHandle */
ColumnDef *columns; 
MYBOOL connected;
int numberColumn;

int dbOpen(char *in);
int dbExecExt(char *in);
int dbCloseConnection(char *in);
int dbEndTran(char *in);
int dbExecDir(char *in);
int dbNumResultCols(char *in);
int dbRowCount(char *in);
int dbBindCol(char *in);
int dbDescribeCol(char *in);
int dbFetch(char *in);
int dbreadbuffer(char *in);
int dbSetConnectAttr(char *in);
int dbCloseCursor(char *in);

#ifdef DEBUG
void Log(char *s, ...){
  FILE *f;
  va_list args;
  va_start(args, s); /* The first arg without name */
  f = fopen("odbc_debug.log", "a");
  vfprintf(f, s, args);
  fclose(f);
  va_end(args);
}
#endif


/* Translete the requested instruction to a function */ 
HandlerRecord requestHandlers[] = {
  {"execdb", dbExecExt},
  {"opendb", dbOpen},
  {"closdb", dbCloseConnection},
  {"bincol", dbBindCol},
  {"descol", dbDescribeCol},
  {"endtra", dbEndTran},
  {"exedir", dbExecDir},
  {"fetchd", dbFetch},
  {"rescol", dbNumResultCols},
  {"rowcon", dbRowCount},
  {"setatr", dbSetConnectAttr},
  {"readbu", dbreadbuffer},
  {"closec", dbCloseCursor}
};

int handleRequest(char *reqstring) {
  char request[COMMANDLEN], *tmp1;
  int i, result;

  for(i = 0; i < COMMANDLEN; i++) 
    request[i] = reqstring[i];
  request[COMMANDLEN] = '\0';
  /* move the pointer steps up in reqstring */
  tmp1 = reqstring + COMMANDLEN;
  DBG(("handleRequest ->requeststring: %s \n", request));
  i = 0;
  while( i < COMMANDS) {
    if(strcmp(requestHandlers[i].name, request) == 0) {
      result = requestHandlers[i].handler(tmp1);
      return result;
    }
    i++;
  }
  result = -90;
  return result;
}

int read_exact(byte *buf, int len) {
  int i, got=0;

  do {
    if ((i = read(0, buf+got, len-got)) <= 0)
      return(i);
    got += i;
  } while (got<len);
  return len;
}

int write_exact(byte *buf, int len) {
  int i, wrote = 0;

  do {
    if ((i = write(1, buf+wrote, len-wrote)) <= 0)
      return (i);
    wrote += i;
  } while (wrote<len);
  return len;
}

/* change the received length from big endian to little endian */
int receivedlength() {
  int len, i, tmp;
  byte lbuf[4];

  len = 0;
  tmp = read(0,lbuf, 4);
  if(tmp != 4)
    return -1;
  for(i=0; i < 4; i++) {
    len <<= 8;
    len |= lbuf[i];
  }
  return len;
}

/* Write buffer to stdout */
int write_cmd(byte *buf, int len) {
  byte li;

  li = (len >> 24) & 0x000000FF;
  write_exact(&li, 1);
  li = (len >> 16) & 0x000000FF;
  write_exact(&li, 1);
  li = (len >> 8) & 0x000000FF;
  write_exact(&li, 1);
  li = len &  0x000000FF;
  write_exact(&li, 1);
  return write_exact(buf, len);
}

int getDiagnosis(SQLSMALLINT handleType, SQLHANDLE handle, 
		 char *msg, int len) {
  SQLCHAR sqlState[6];
  SQLINTEGER nativeError;
  SQLSMALLINT msgLen;
  SQLSMALLINT i;
  int j;

  sqlState[5] = '\0';
  i = 1;
  j = 0;
  while(SQLGetDiagRec(handleType, handle, i, sqlState, &nativeError,
		      msg, len, &msgLen) == SQL_SUCCESS) {
    if (j < (len - 2)) {
      j = j + msgLen;
      msg[j] = '\r';
      j++;
      msg[j] = '\0';
      DBG(("getDiagnosis -> error Message number %d\n",i));
      DBG(("getDiagnosis -> error Message %s\n",msg));
      msg[j] = '\n';
      j++;
      i++;
    }
    i++;
    DBG(("getDiagnosis -> sqlState %s\n",sqlState));
  }
  msg[j] = '\0';
  if(j == 0)
    strcpy(msg, "unknown error\r\n");
  return j;
}


/* connect to a database */
int dbOpen(char *in) {
  char connStr[256];
  SQLRETURN	ret;
  SQLSMALLINT si, connlen;
  int eimes, index,errMsgLen;
  char *conn, *mes, *tmp;
  
  errMsgLen = 0;
  conn = in;
  DBG(("dbOpen -> The connection string is: %s\n",conn));
  if(connected) {
    mes = "error";
    tmp ="already_connected";
    goto exit;
  }

  ret = SQLAllocHandle(SQL_HANDLE_ENV, SQL_NULL_HANDLE, &henv1);
  if (ret == SQL_ERROR || ret == SQL_INVALID_HANDLE) {
    mes = "error";
    tmp = "SQLAllocHandle";
    goto exit;
  }

  ret = SQLSetEnvAttr(henv1, SQL_ATTR_ODBC_VERSION, 
		      (SQLPOINTER)SQL_OV_ODBC3,0);
  if (ret == SQL_ERROR || ret == SQL_INVALID_HANDLE) {
    mes = "error";
    tmp = "SQLSetEnvAttr";
    goto exit;			
  }

  SQLAllocHandle(SQL_HANDLE_DBC, henv1, &hdbc1);

  connlen = (SQLSMALLINT)strlen(conn);
  ret = SQLDriverConnect(hdbc1, NULL, conn, connlen, connStr, 
			 (SQLSMALLINT)sizeof(connStr), &si, 
			 SQL_DRIVER_NOPROMPT);

  switch(ret) {
  case SQL_SUCCESS:
  case SQL_SUCCESS_WITH_INFO:
    mes = "ok";
    tmp = "connected";
    connected = _TRUE;
    break;
    
  default:
    mes = "error";
    tmp = malloc(1024 * sizeof(char));
    errMsgLen = getDiagnosis(SQL_HANDLE_DBC, hdbc1, tmp, 1024);
    SQLFreeHandle(SQL_HANDLE_DBC, hdbc1);
    SQLFreeHandle(SQL_HANDLE_ENV, henv1);
    connected = _FALSE;
    break;
  }

 exit:
  index = 0;
  buf = malloc((200 + errMsgLen) * sizeof(char));
  eimes = ei_encode_version(buf, &index);
  eimes = ei_encode_tuple_header(buf, &index, 3);
  eimes = ei_encode_atom(buf, &index, mes);
  eimes = ei_encode_string(buf, &index, tmp);
  eimes = ei_encode_long(buf, &index, ret);
  DBG(("dbOpen -> returned mes %s\n", mes));
  DBG(("dbOpen -> returned returncode %d\n", ret));
  if(errMsgLen > 0)
    free(tmp);
  DBG(("dbOpen -> returned index %d\n", index));
  return index;
}

int dbExecExt(char *in) {
  char *mes, *result, *rowbuf;
  char *sql;
  int index, j,k, n, rowCount;
  SQLUSMALLINT i;
  SQLTCHAR cName[255];
  SQLSMALLINT cNameLen, cType, cDecDigits, cNullable, NumColumn;
  SQLUINTEGER cSize; 
  int resultListLen;
  int eimes, writerr, errMsgLen;
  SQLRETURN loopret, ret;
  SQLINTEGER RowCountPtr;	
  
  errMsgLen = 0;
  ret = SQLAllocHandle(SQL_HANDLE_STMT, hdbc1, &hstmt);
  DBG(("dbExecExt -> SQLAllocHandle returnvalue: %d\n", ret));
  if(ret == SQL_ERROR) {
    mes = "error";
    result = "SQLAllocHandle_error";
    goto exit;
  }
  if(ret == SQL_INVALID_HANDLE) {
    mes = "error";
    result = "SQLAllocHandle_Invalid_handle";
    goto exit;
  }

  sql = in;
  DBG(("dbExecExt -> the command string is: %s\n",sql));
  ret = SQLExecDirect(hstmt, sql, SQL_NTS);
  DBG(("dbExecExt -> SQLExecDirect returnvalue: %d\n", ret));
  if(ret == SQL_ERROR) {
    mes = "error";
    result = malloc(1024 * sizeof(char)); 
    errMsgLen = getDiagnosis(SQL_HANDLE_STMT, hstmt, result, 1024);
    DBG(("dbExecExt -> SQLExecDirect error: %s\n",result));
    goto exit;
  } 
  if(ret == SQL_INVALID_HANDLE) {
    mes = "error";
    result = "SQLExecDirect_Invalid_handle";
    goto exit;
  }

  DBG(("dbExecExt -> translate columns to sql values\n"));
  ret = SQLNumResultCols(hstmt, &NumColumn);
  if(ret == SQL_ERROR || ret == SQL_INVALID_HANDLE) {
    mes = "error";
    result = "SQLNumResultCols";
    goto exit;
  }

  ret = SQLRowCount(hstmt, &RowCountPtr); 
  if(ret == SQL_ERROR || ret == SQL_INVALID_HANDLE) {
    mes = "error";
    result = "SQLRowCount";
    goto exit;
  }
  rowCount =(int)RowCountPtr;
  DBG(("dbExecExt -> rowCount: %d\n",rowCount));
  DBG(("dbExecExt -> Number of columns: %d\n",NumColumn));
  if(NumColumn == 0) {
    mes = "updated";
    goto exit;
  }
  
  DBG(("dbExecExt -> translate...\n"));
  columns = malloc(NumColumn * sizeof(ColumnDef));
  for(i=0; i < NumColumn; i++) {
    ret = SQLDescribeCol(hstmt, i+1, cName,
		      (SQLSMALLINT)sizeof(cName), &cNameLen, &cType, 
		      &cSize, &cDecDigits, &cNullable);
    if(ret != SQL_SUCCESS && ret != SQL_SUCCESS_WITH_INFO) {
      mes = "error";
      result = "SQLDescribeCol";
      goto exit;
    }
    else {
      /* Limit the buffer of one column */
      if(cSize > MAX_BUFFER_SIZE)
	cSize = MAX_BUFFER_SIZE;
      switch(cType) {
      case SQL_CHAR:
      case SQL_VARCHAR:
      case SQL_LONGVARCHAR:
	columns[i].len = cSize + 1;
	columns[i].type = SQL_C_CHAR;
	break;
      case SQL_DECIMAL:
	columns[i].len = 50;
	columns[i].type = SQL_C_CHAR;
	break;
      case SQL_SMALLINT:
	columns[i].len = 50;
	columns[i].type = SQL_C_SSHORT;
	break;
      case SQL_INTEGER:
	columns[i].len = 50;
	columns[i].type = SQL_C_SLONG;
	break;
      case SQL_TINYINT:
	columns[i].len = 50;
	columns[i].type = SQL_C_TINYINT;
	break;
      case SQL_TYPE_DATE:
      case SQL_TYPE_TIME:
      case SQL_TYPE_TIMESTAMP:
	columns[i].len= 30;
	columns[i].type = SQL_C_CHAR;
	break;
      case SQL_FLOAT:
	columns[i].len = 100;
	columns[i].type = SQL_C_FLOAT;
	break;
      case SQL_DOUBLE:
	columns[i].len = 100;
	columns[i].type = SQL_C_DOUBLE;
	break;	
      case SQL_BIGINT:
	columns[i].len = 100;
	columns[i].type = SQL_C_CHAR;
	break;
      case SQL_BIT:
	columns[i].len = sizeof(SQL_C_BIT);
	columns[i].type = SQL_C_BIT;
	break;
	
      default:
	columns[i].len = 0;
	columns[i].buf = NULL;
	break;
      } /* switch */
      if (columns[i].len > 0) {
	columns[i].buf = malloc(columns[i].len);
      	ret = SQLBindCol(hstmt, i+1, columns[i].type, columns[i].buf, 
		   columns[i].len, &columns[i].resultLen);
	if (ret != SQL_SUCCESS && ret != SQL_SUCCESS_WITH_INFO) {
	  mes = "error";
	  result = "SQLBindCol";
	  goto exit;
	}
      }
      else
	columns[i].buf = NULL;
    } /* if */
    /* save columnname in column vector */
    columns[i].columnName = malloc(strlen(cName) * sizeof(char));
    columns[i].columnName = strcpy(columns[i].columnName,cName);
    DBG(("dbExecExt -> columnname %s\n",columns[i].columnName));
    DBG(("dbExecExt -> columntype  %d\n",cType));    
  }  /* for */
  
  /* Send a list of column names to erlang */
  index = 0;
  for(i=0; i < NumColumn; i++)
    eimes = ei_encode_string(NULL,&index, columns[i].columnName);
  resultListLen = index + 100;
  rowbuf = malloc(resultListLen * sizeof(char));
  index = 0;
  eimes = ei_encode_version(rowbuf, &index);
  eimes = ei_encode_tuple_header(rowbuf, &index, 2);
  eimes = ei_encode_atom(rowbuf, &index, "column_name");
  eimes = ei_encode_list_header(rowbuf, &index, NumColumn);
  for(i=0; i < NumColumn; i++)
    eimes = ei_encode_string(rowbuf, &index, columns[i].columnName);  
  eimes = ei_encode_empty_list(rowbuf, &index);
  if ((writerr = write_cmd(rowbuf,index)) < 0)
    return writerr;
  index = 0;
  free(rowbuf);

  j = 0;
  loopret = SQLFetch(hstmt);
  if(loopret == SQL_ERROR ) {
    mes = "error";
    result = "SQLFetch";
    goto exit;
  }
  DBG(("dbExecExt -> SQLFetch returns: %d\n", loopret));
  while(loopret != SQL_NO_DATA) {
    DBG(("dbExecExt -> fetch value from the table...\n"));
    /* First find the length of the buf */
    index = 0;
    for(i=0; i < NumColumn; i++) {
      if(columns[i].len == 0 || columns[i].resultLen == SQL_NULL_DATA) {
	eimes = ei_encode_atom(NULL, &index,"null");
      }
      else {
	switch(columns[i].type) {
	case SQL_C_CHAR:
	  n = strlen(columns[i].buf);
	  /* Strip string of trailing spaces */
	  if(columns[i].buf[n-1] == ' ') {
	    for(k=n-2; k >= 0; k--) {
	      if(columns[i].buf[k] != ' ') {
		columns[i].buf[k+1] = '\0';
		n = k+1;
		break;
	      }
	      if(k == 0 && columns[i].buf[0]) {
		columns[i].buf[0] = '\0';
		n = 0;
	      }
	    }
	  }
	  eimes = ei_encode_string(NULL, &index, columns[i].buf);
	  break;
	case SQL_C_SSHORT:
	  eimes = ei_encode_long(NULL, &index, 
				 *((SQLSMALLINT *) columns[i].buf));
	  break;
	case SQL_C_DOUBLE:
	  eimes = ei_encode_double(NULL, &index, 
				   *((SQLDOUBLE *) columns[i].buf));
	  break;
	case SQL_C_SLONG:
	  eimes = ei_encode_long(NULL, &index, 
				 *((SQLINTEGER *) columns[i].buf));
	  break;
	case SQL_C_FLOAT:
	  eimes = ei_encode_double(NULL, &index,
				 *((SQLREAL *) columns[i].buf));
	  break;
	case SQL_C_BIT:
	  if(*(SQLCHAR *)columns[i].buf)
	    eimes = ei_encode_atom(NULL, &index, "true");
	  else
	    eimes = ei_encode_atom(NULL, &index, "false");
	  break;

	default:
	  eimes = ei_encode_atom(NULL, &index,"null");
	  break;
	} /*switch */
      } /* else */
    } /* for(i=0; i < NumColumn; i++) */

    /* Putting data into the buffer */
    DBG(("dbExecExt -> calculated length of rowbuf %d\n", index));
    resultListLen = index + 84;
    rowbuf = malloc(resultListLen * sizeof(char));
    index = 0;
    eimes = ei_encode_version(rowbuf, &index);
    eimes = ei_encode_tuple_header(rowbuf, &index, 2);
    eimes = ei_encode_atom(rowbuf, &index, "row");
    eimes = ei_encode_list_header(rowbuf, &index, NumColumn);
    for(i=0; i < NumColumn; i++) {
      if(columns[i].len == 0 || columns[i].resultLen == SQL_NULL_DATA) {
	eimes = ei_encode_atom(rowbuf, &index,"null");
      }
      else {
	switch(columns[i].type) {
	case SQL_C_CHAR:
	  eimes = ei_encode_string(rowbuf,&index, columns[i].buf);
	  break;
		
	case SQL_C_DOUBLE:
	  eimes = ei_encode_double(rowbuf,&index, 
				   *((SQLDOUBLE *) columns[i].buf));
	  break;
		
	case SQL_C_SLONG:
	  eimes = ei_encode_long(rowbuf,&index, 
				 *((SQLINTEGER *) columns[i].buf));
	  break;

	case SQL_C_SSHORT:
	  eimes = ei_encode_long(rowbuf, &index,
				 *((SQLSMALLINT *) columns[i].buf));
	  break;
	case SQL_C_FLOAT:
	  eimes = ei_encode_double(rowbuf, &index,
				   *((SQLREAL *) columns[i].buf));
	  break;
	case SQL_C_BIT:
	  if(*(SQLCHAR *)columns[i].buf)
	    eimes = ei_encode_atom(rowbuf, &index, "true");
	  else
	    eimes = ei_encode_atom(rowbuf, &index, "false");
	  break;
		
	default:
	  eimes = ei_encode_atom(rowbuf, &index,"null");
	  break;
	} /* switch */
      } /* else */
    } /* for(i=0; i < NumColumn; i++) */
    eimes = ei_encode_empty_list(rowbuf, &index);
    if ((writerr = write_cmd(rowbuf,index)) < 0)
      return writerr;
    DBG(("dbExecExt -> index in (column) %d\n", index));
    DBG(("dbExecExt -> eimes in (column) %d\n", eimes));
    DBG(("dbExecExt -> j row in (column) %d\n", j));
    DBG(("dbExecExt -> writerr in (column) %d\n", writerr));
    free(rowbuf);
    j++;
    loopret = SQLFetch(hstmt);
  } /* while */

  DBG(("dbExecExt -> dealloc...\n"));
  for(i=0; i < NumColumn; i++) {
    free(columns[i].columnName);
    if(columns[i].len)
      free(columns[i].buf);
  }
  free(columns);
  mes = "selected";
  result = "lastrow";
 exit:
  DBG(("dbExecExt -> exit: ...\n"));
  SQLCloseCursor(hstmt); 
  SQLFreeHandle(SQL_HANDLE_STMT, hstmt);
  DBG(("dbExecExt -> mes %s\n", mes));
  buf = malloc((250 + errMsgLen) * sizeof(char)); 
  index = 0;
  eimes = ei_encode_version(buf, &index);
  eimes = ei_encode_tuple_header(buf, &index, 2);
  eimes = ei_encode_atom(buf, &index, mes);
  n = strcmp(mes,"updated");
  if(n == 0)
    eimes = ei_encode_long(buf, &index, rowCount);
  else {
    DBG(("dbExecExt -> returned result %s \n", result));
    n = strcmp(mes, "error");
    if(n == 0)
      eimes = ei_encode_string(buf, &index, result);
    else 
      eimes = ei_encode_atom(buf, &index, result);
  }
  if(errMsgLen > 0)
    free(result);
  DBG(("dbExecExt -> returned index %d\n", index));
  return index;
}

int dbExecDir(char *in) {
  char *mes, *result;
  char *sql;
  int index, errMsgLen; 
  int eimes;
  SQLRETURN ret;	
  
  errMsgLen = 0;
  ret = SQLAllocHandle(SQL_HANDLE_STMT, hdbc1, &hstmt);
  DBG(("dbExecDir -> SQLAllocHandle returnvalue: %d\n", ret));
  if(ret == SQL_ERROR) {
    result = malloc(1024 * sizeof(char));
    errMsgLen = getDiagnosis(SQL_HANDLE_DBC, hdbc1, result, 1024);
    mes = "error";
    goto exit;
  }
  if(ret == SQL_INVALID_HANDLE) {
    mes = "error";
    result = "SQLAllocHandle_Invalid_handle";
    goto exit;
  }

  sql = in;
  DBG(("dbExecDir -> the command string is: %s\n",sql));
  ret = SQLExecDirect(hstmt, sql, SQL_NTS);
  DBG(("dbExecDir -> SQLExecDirect returnvalue: %d\n", ret)); 
  if(ret == SQL_ERROR) {
    result = malloc(1024 * sizeof(char));
    errMsgLen = getDiagnosis(SQL_HANDLE_STMT, hstmt, result, 1024);
    mes = "error";
    goto exit;
  }
  if(ret == SQL_INVALID_HANDLE) {
    mes = "error";
    result = "SQLExecDirect_Invalid_handle";
    goto exit;
  }

  numberColumn = 0;
  mes = "ok";
  result = "dbExecDir"; 
 exit:
  DBG(("dbExecDir -> mes %s\n", mes));
  buf = malloc((250 + errMsgLen) * sizeof(char)); 
  index = 0;
  eimes = ei_encode_version(buf, &index);
  eimes = ei_encode_tuple_header(buf, &index, 3);
  eimes = ei_encode_atom(buf, &index, mes);
  eimes = ei_encode_string(buf, &index, result);
  eimes = ei_encode_long(buf, &index, ret); 
  DBG(("dbExecDir -> result %s\n", result));
  if (errMsgLen > 0)
    free(result);
  DBG(("dbExecDir -> returned index %d\n", index));
  return index;
}

int dbEndTran(char *in) {
  char *mes, *result, attr[20];
  int index, eimes;
  SQLRETURN ret;
  SQLSMALLINT intattr;
  int i;

  DBG(("dbEndTran -> inparameter:%s\n", in));

  i = 0;   /* string index */
  while(in[i] != '\0' && i < 20) {
    if(in[i] == 59) /* if in[i] == ";" */
      attr[i] = '\0';
    i++;
  }

  intattr = atoi(attr);
  ret = SQLEndTran(SQL_HANDLE_DBC, hdbc1, intattr);
  DBG(("dbEndTran -> SQLEndTrans returned %d\n", ret));
  buf = malloc(200 * sizeof(char));
  index = 0;
  if ((ret == SQL_ERROR)|| (ret == SQL_INVALID_HANDLE)) {
    mes = "error";
    result = "SQLEndTran";
    eimes = ei_encode_version(buf, &index);
    eimes = ei_encode_tuple_header(buf, &index, 3);
    eimes = ei_encode_atom(buf, &index, mes);
    eimes = ei_encode_string(buf, &index, result);
    eimes = ei_encode_long(buf, &index, ret);
    DBG(("dbEndTran -> mes %s\n", mes));
    DBG(("dbEndTran -> returned result %s \n", result));
    DBG(("dbEndTran -> returned index %d\n", index));
    return index;
  }
  else {
    mes = "ok";
    result = "dbEndTran";
    eimes = ei_encode_version(buf, &index);
    eimes = ei_encode_tuple_header(buf, &index, 3);
    eimes = ei_encode_atom(buf, &index, mes);
    eimes = ei_encode_string(buf, &index, result);
    eimes = ei_encode_long(buf, &index, ret);
    DBG(("dbEndTran -> mes %s\n", mes));
    DBG(("dbEndTran -> rowcount %s\n",result)); 
    DBG(("dbEndTran -> returned index %d\n", index));
    return index;
  }
}

int dbNumResultCols(char *in) {
  char *mes, *result;
  int index;
  SQLSMALLINT NumColumn;
  int eimes;
  SQLRETURN ret;	
  
  buf = malloc(200 * sizeof(char)); 
  index = 0;
  ret = SQLNumResultCols(hstmt, &NumColumn);
  DBG(("dbNumResultCols -> returned ret %d\n", ret));
  if (ret == SQL_ERROR || ret == SQL_INVALID_HANDLE) {
    mes = "error";
    result = "SQLNumResultCols";
    SQLCloseCursor(hstmt); 
    SQLFreeHandle(SQL_HANDLE_STMT, hstmt);
    eimes = ei_encode_version(buf, &index);
    eimes = ei_encode_tuple_header(buf, &index, 3);
    eimes = ei_encode_atom(buf, &index, mes);
    eimes = ei_encode_string(buf, &index, result);
    eimes = ei_encode_long(buf, &index, ret);
    DBG(("dbNumResultCols -> mes %s\n", mes));
    DBG(("dbNumResultCols -> returned result %s \n", result));
    DBG(("dbNumResultCols -> returned index %d\n", index));
    return index;
  }
  else {
    numberColumn = (int)NumColumn;
    mes = "ok";
    eimes = ei_encode_version(buf, &index);
    eimes = ei_encode_tuple_header(buf, &index, 3);
    eimes = ei_encode_atom(buf, &index, mes);
    eimes = ei_encode_long(buf, &index, numberColumn);
    eimes = ei_encode_long(buf, &index, ret);
    DBG(("dbNumResultCols -> mes %s\n", mes));
    DBG(("dbNumResultCols -> Number of Column %d\n", numberColumn)); 
    DBG(("dbNumResultCols -> returned index %d\n", index));
  return index;
  }
}

int dbRowCount(char *in) {
  char *mes, *result;
  int index;
  SQLINTEGER rowCount;
  int eimes;
  SQLRETURN ret;
  
  buf = malloc(200 * sizeof(char)); 
  index = 0;
  ret = SQLRowCount(hstmt, &rowCount);
  DBG(("dbRowCount -> returned ret %d\n", ret));
  if (ret == SQL_ERROR || ret == SQL_INVALID_HANDLE) {
    mes = "error";
    result = "SQLRowCount";
    SQLCloseCursor(hstmt); 
    SQLFreeHandle(SQL_HANDLE_STMT, hstmt);
    eimes = ei_encode_version(buf, &index);
    eimes = ei_encode_tuple_header(buf, &index, 3);
    eimes = ei_encode_atom(buf, &index, mes);
    eimes = ei_encode_string(buf, &index, result);
    eimes = ei_encode_long(buf, &index, ret);
    DBG(("dbRowCount -> mes %s\n", mes));
    DBG(("dbRowCount -> returned result %s \n", result));
    DBG(("dbRowCount -> returned index %d\n", index));
    return index;
  }
  else {
    mes = "ok";
    eimes = ei_encode_version(buf, &index);
    eimes = ei_encode_tuple_header(buf, &index, 3);
    eimes = ei_encode_atom(buf, &index, mes);
    eimes = ei_encode_long(buf, &index, rowCount);
    eimes = ei_encode_long(buf, &index, ret);
    DBG(("dbRowCount -> mes %s\n", mes));
    DBG(("dbRowCount -> rowcount %d\n",rowCount)); 
    DBG(("dbRowCount -> returned index %d\n", index));
    return index;
  }
}

/* Set connectionattributes */
int dbSetConnectAttr(char *in) {
  char *mes, *result;
  int index, eimes;
  SQLRETURN ret;
  SQLINTEGER attr1, intattr2;
  int sel;
  char *array[3];
  int i,j,k;

  DBG(("dbSetConnectAttr -> inparameter:%s\n", in));
  /* This need some comments: */
  /* SQLSetConnectAttr needs two attr from Erlang. */
  /* The first arguments is a integer, second a decoder for the third */
  /* argument and finaly the third argument which */
  /* can be string or integer. Between each argument is delimeter */
  /* character ;. The decoder decode 0 for integer and 1 for string. */
  /* So a string as third argument can look like "0;1;nisse;" and */
  /* and integer like "0;0;12;" */
  i = 0;   /* in string index */
  j = 0;   /* which attr */
  k = 0;   /* which char in the attr */
  /* Reserve memory for each argument */
  while(in[i] != '\0' && i < 40) {
    if(in[i] == 59) { /* if in[i] == ";" */
      array[j] = malloc((k + 1) * sizeof(char));
      j++;
      k = 0;
    }
    else
      k++;
    i++;
  }
  i = 0; j = 0; k = 0;
  while(in[i] != '\0' && i < 40) {
    if(in[i] == 59) { /* if in[i] == ";" */
      array[j][k] = '\0';
      j++;
      k = 0;
    }
    else {
      array[j][k] = in[i];
      k++;
    }
    i++;
  }
  attr1 = atoi(array[0]);
  sel = atoi(array[1]);
  if(sel == 0) { /* second attribute integer */
    intattr2 = atoi(array[2]);
    ret = SQLSetConnectAttr(hdbc1, attr1, (SQLPOINTER)intattr2, 0);
  }
  else 
    ret = SQLSetConnectAttr(hdbc1, attr1, (SQLPOINTER)array[2], SQL_NTS);
  DBG(("dbSetConnectAttr -> SQLSetConnectAttr returned %d\n", ret));
  for(i = 0; i < 3; i++) 
    free(array[i]);
  buf = malloc(200 * sizeof(char));
  index = 0;
  if (ret == SQL_ERROR || ret == SQL_INVALID_HANDLE) {
    mes = "error";
    result = "SQLSetConnectAttr";
    eimes = ei_encode_version(buf, &index);
    eimes = ei_encode_tuple_header(buf, &index, 3);
    eimes = ei_encode_atom(buf, &index, mes);
    eimes = ei_encode_string(buf, &index, result);
    eimes = ei_encode_long(buf, &index, ret);
    DBG(("dbSetConnectAttr -> mes %s\n", mes));
    DBG(("dbSetConnectAttr -> returned result %s \n", result));
    DBG(("dbSetConnectAttr -> returned index %d\n", index));
    return index;
  }
  else {
    mes = "ok";
    result = "dbSetConnectAttr";
    eimes = ei_encode_version(buf, &index);
    eimes = ei_encode_tuple_header(buf, &index, 3);
    eimes = ei_encode_atom(buf, &index, mes);
    eimes = ei_encode_string(buf, &index, result);
    eimes = ei_encode_long(buf, &index, ret);
    DBG(("dbSetConnectAttr -> mes %s\n", mes));
    DBG(("dbSetConnectAttr -> rowcount %s\n",result)); 
    DBG(("dbSetConnectAttr -> returned index %d\n", index));
    return index;
  }
}

/* dbBindCol creates memory for the columns */
/* Inparmeter: Column number (starts with 1) */
int dbBindCol(char *in) {
  char *mes, *result;
  int index, j;
  SQLUSMALLINT i, NumColumn;
  SQLTCHAR cName[255];
  SQLSMALLINT cNameLen, cType, cDecDigits, cNullable;
  SQLUINTEGER cSize; 
  int resultListLen;
  int eimes;
  SQLRETURN ret;	

  DBG(("dbBindCol -> translate...\n"));
  DBG(("dbBindCol -> inparameter:%s\n", in));
  /* selected column */
  j = atoi(in);
  DBG(("dbBindCol -> selected column:%d\n", j));
  /* The columnarray starts at index 0 */
  i = j - 1;
  /* If Number of Column are not known */
  if (numberColumn == 0) {
    ret = SQLNumResultCols(hstmt, &NumColumn);
    DBG(("dbBindCol -> SQLNumResultCols return %d\n", ret));
    if(ret == SQL_ERROR || ret == SQL_INVALID_HANDLE) {
      mes = "error";
      result = "SQLNumResultCols";
      goto exit;
    }
    numberColumn = (int)NumColumn;
    DBG(("dbBindCol -> number of Columns %d\n", numberColumn));
  }
  if (columns == NULL) {
    columns = malloc(numberColumn * sizeof(ColumnDef));
    DBG(("dbBindCol -> allocate memory for one row \n"));
  }
  columns[i].selected = _TRUE;
  index = 0;
  ret = SQLDescribeCol(hstmt, j, cName, (SQLSMALLINT)sizeof(cName),
		       &cNameLen, &cType, &cSize, &cDecDigits, 
		       &cNullable);
  DBG(("dbBindCol -> SQLDescribeCol return %d\n", ret));
  DBG(("dbBindCol -> columnname %s\n", cName));
  DBG(("dbBindCol -> columntype %d\n", cType));
  if(ret == SQL_ERROR || ret == SQL_INVALID_HANDLE) {
    mes = "error";
    mes = "SQLDescribeCol";
    goto exit;
  }
  /* Limit the buffer of one column */
  if(cSize > MAX_BUFFER_SIZE)
    cSize = MAX_BUFFER_SIZE;
  if ((ret == SQL_SUCCESS)||(ret == SQL_SUCCESS_WITH_INFO)) {
    switch(cType) {
    case SQL_CHAR:
    case SQL_VARCHAR:
      columns[i].len = cSize + 1;
      columns[i].type = SQL_C_CHAR;
    case SQL_LONGVARCHAR:
      columns[i].len = cSize +1;
      columns[i].type = SQL_C_CHAR;
      break;
    case SQL_INTEGER:
      columns[i].len = 50;
      columns[i].type = SQL_C_SLONG;
      break;
    case SQL_TINYINT:
      columns[i].len = 50;
      columns[i].type = SQL_C_STINYINT;
      break;
    case SQL_SMALLINT:
      columns[i].len = 50;
      columns[i].type = SQL_C_SSHORT;
    case SQL_TYPE_DATE:
    case SQL_TYPE_TIME:
    case SQL_TYPE_TIMESTAMP:
      columns[i].len= 30;
      columns[i].type = SQL_C_CHAR;
      break;
    case SQL_DECIMAL:
      columns[i].len = 50;
      columns[i].type = SQL_C_CHAR;
      break;
    case SQL_FLOAT:
      columns[i].len = 100;
      columns[i].type = SQL_C_FLOAT;
      break;
    case SQL_DOUBLE:
      columns[i].len = 100;
      columns[i].type = SQL_C_DOUBLE;
      break;
    case SQL_BIT:
      columns[i].len = sizeof(SQL_C_BIT);
      columns[i].type = SQL_C_BIT;
      break;
      
    default:
      columns[i].len = 0;
      columns[i].buf = NULL;
      break;
    } /* switch */
    if (columns[i].len > 0) {
      columns[i].buf = malloc(columns[i].len);
      ret = SQLBindCol(hstmt, j, columns[i].type, columns[i].buf, 
		       columns[i].len, &columns[i].resultLen);
      if (ret == SQL_ERROR || ret == SQL_INVALID_HANDLE) {
	mes = "error";
	result = "SQLBindCol";
	goto exit;
      }
    }
    else
      columns[i].buf = NULL;
    eimes = ei_encode_string(NULL, &index, cName);
    resultListLen = index + 200;
    mes = "ok";
    buf = malloc(resultListLen * sizeof(char)); 
    index = 0;
    eimes = ei_encode_version(buf, &index);
    eimes = ei_encode_tuple_header(buf, &index, 3);
    eimes = ei_encode_atom(buf, &index, mes);
    eimes = ei_encode_string(buf, &index, "dbBindCol");
    eimes = ei_encode_long(buf, &index, ret); 
    DBG(("dbBindCol -> returned index %d\n", index));
    return index;
  } /* if */

 exit:
  DBG(("dbBindCol -> mes %s\n", mes));
  SQLCloseCursor(hstmt); 
  SQLFreeHandle(SQL_HANDLE_STMT, hstmt);
  buf = malloc(200 * sizeof(char)); 
  eimes = ei_encode_version(buf, &index);
  eimes = ei_encode_tuple_header(buf, &index, 3);
  eimes = ei_encode_atom(buf, &index, mes);
  eimes = ei_encode_string(buf, &index, result);
  eimes = ei_encode_long(buf, &index, ret);
  DBG(("dbBindCol -> returned result %s \n", result));
  DBG(("dbBindCol -> returned index %d\n", index));
  return index;
}

/* dbDescribeCol returns column name for one column in */
/* the result set. */
/* Inparmeter: Column number (starts with 1) */
int dbDescribeCol(char *in) {
  char *mes, *result;
  int index, j, resultListLen;
  SQLTCHAR cName[255];
  SQLSMALLINT cNameLen, cType, cDecDigits, cNullable;
  SQLUINTEGER cSize; 
  int eimes;
  SQLRETURN ret;	

  DBG(("dbDescribeCol -> translate...\n"));
  DBG(("dbDescribeCol -> inparameter:%s\n", in));
  /* selected column */
  j = atoi(in);
  DBG(("dbDescribeCol -> selected column:%d\n", j));
  /* The columnarray starts at index 0 */

  index = 0;
  ret = SQLDescribeCol(hstmt, j, cName, (SQLSMALLINT)sizeof(cName),
		       &cNameLen, &cType, &cSize, &cDecDigits, 
		       &cNullable);
  DBG(("dbDescribeCol -> SQLDescribeColumn return %d\n", ret));
  if(ret == SQL_ERROR || ret == SQL_INVALID_HANDLE){ /* error */
    mes = "error";
    result = "SQLDescribeCol";
    goto exit;
  }
  if (ret == SQL_SUCCESS || ret == SQL_SUCCESS_WITH_INFO) {
    DBG(("dbDescribeCol -> columnname %s\n",cName));
    eimes = ei_encode_string(NULL, &index, cName);
    resultListLen = index + 200;
    mes = "ok";
    buf = malloc(resultListLen * sizeof(char)); 
    index = 0;
    eimes = ei_encode_version(buf, &index);
    eimes = ei_encode_tuple_header(buf, &index, 4);
    eimes = ei_encode_atom(buf, &index, mes);
    eimes = ei_encode_long(buf, &index, ret);
    eimes = ei_encode_string(buf, &index, cName);
    eimes = ei_encode_long(buf, &index, cNullable);
    DBG(("dbDescribeCol -> Name of the column %s\n", cName)); 
    DBG(("dbDescribeCol -> returned index %d\n", index));
    return index;
  } /* if */
  

 exit:
  DBG(("dbDescribeCol -> mes %s\n", mes));
  SQLCloseCursor(hstmt); 
  SQLFreeHandle(SQL_HANDLE_STMT, hstmt);
  buf = malloc(200 * sizeof(char)); 
  eimes = ei_encode_version(buf, &index);
  eimes = ei_encode_tuple_header(buf, &index, 3);
  eimes = ei_encode_atom(buf, &index, mes);
  eimes = ei_encode_string(buf, &index, result);
  eimes = ei_encode_long(buf, &index, ret);
  DBG(("dbDescribeCol -> returned result %s \n", result));
  DBG(("dbDescribeCol -> returned index %d\n", index));
  return index;
}

/* Fetch data from database */  
int dbFetch(char *in) {
  char *mes, *result;
  int eimes, index;
  SQLRETURN ret;	

  index = 0;
  buf = malloc(150 * sizeof(char)); 
  ret = SQLFetch(hstmt);
  DBG(("dbFetch -> SQLFetch return: %d \n", ret));
  if(ret == SQL_ERROR || ret == SQL_INVALID_HANDLE) {
    mes = "error";
    result = "SQLFetch";
    goto exit;
  }
  if((ret == SQL_SUCCESS)||(ret == SQL_SUCCESS_WITH_INFO) ||
     (ret == SQL_NO_DATA)) {
    mes = "ok";
    eimes = ei_encode_version(buf, &index);
    eimes = ei_encode_tuple_header(buf, &index, 2);
    eimes = ei_encode_atom(buf, &index, mes);
    eimes = ei_encode_long(buf, &index, ret);
    DBG(("dbFetch -> mes %s\n", mes));
    DBG(("dbFetch -> returned index %d\n", index));
    return index;
  }

 exit:
  DBG(("dbFetch -> exit: ...\n"));
  SQLCloseCursor(hstmt); 
  SQLFreeHandle(SQL_HANDLE_STMT, hstmt);
  eimes = ei_encode_version(buf, &index);
  eimes = ei_encode_tuple_header(buf, &index, 3);
  eimes = ei_encode_atom(buf, &index, mes);
  eimes = ei_encode_string(buf, &index, result);
  eimes = ei_encode_long(buf, &index, ret);
  DBG(("dbFetch -> mes %s\n", mes)); 
  DBG(("dbFetch -> returned result %s \n", result));
  DBG(("dbFetch -> returned index %d\n", index));
  return index;
  }


/* Read data from Database */
int dbreadbuffer(char *in) {
  int index,j, k, n;
  SQLUSMALLINT i;
  int resultListLen;
  int eimes;

  DBG(("dbreadbuffer -> start \n"));
  /* First find the length of the buf */
  index = 0;
  /* Column number */
  j = atoi(in);
  /* Columnarray start at index 0 */
  i = j - 1;
  DBG(("dbreadbuffer -> selected column %d \n", j));
  DBG(("dbreadbuffer -> columntype: %d \n", columns[i].type));
  DBG(("dbreadbuffer -> columnbufferlength: %d \n", columns[i].len));
  if(columns[i].len == 0 || columns[i].resultLen == SQL_NULL_DATA) {
    eimes = ei_encode_atom(NULL, &index,"null");
  }
  else {
    switch(columns[i].type) {
    case SQL_C_CHAR:
      n = strlen(columns[i].buf);
      /* Strip string of trailing spaces */
      if(columns[i].buf[n-1] == ' ') {
	for(k=n-2; k >= 0; k--) {
	  if(columns[i].buf[k] != ' ') {
	    columns[i].buf[k+1] = '\0';
	    n = k+1;
	    break;
	  }
	  if(k == 0 && columns[i].buf[0]) {
	    columns[i].buf[0] = '\0';
	    n = 0;
	  }
	}
      }
      eimes = ei_encode_string(NULL, &index, columns[i].buf);
      break;
    case SQL_C_STINYINT:
      eimes = ei_encode_long(NULL, &index, 
			     *((SQLCHAR *) columns[i].buf));
      break;
    case SQL_C_SSHORT:
      eimes = ei_encode_long(NULL, &index, 
			     *((SQLSMALLINT *) columns[i].buf));
      break;
    case SQL_C_FLOAT:
      eimes = ei_encode_double(NULL, &index,
			       *((SQLREAL *) columns[i].buf));
      break;
    case SQL_C_DOUBLE:
      eimes = ei_encode_double(NULL, &index, 
			       *((SQLDOUBLE *) columns[i].buf));
      break;
    case SQL_C_SLONG:
      eimes = ei_encode_long(NULL, &index, 
			     *((SQLINTEGER *) columns[i].buf));
      break;
    case SQL_C_BIT:
      if(*(SQLCHAR *)columns[i].buf)
	eimes = ei_encode_atom(NULL, &index, "true");
      else
	eimes = ei_encode_atom(NULL, &index, "false");
      break;
	
    default:
      eimes = ei_encode_atom(NULL, &index,"null");
      break;
    } /*switch */
  } /* else */
  
  /* Putting data into the buffer */
  DBG(("dbreadbuffer -> calculated length of buf %d\n", index));
  resultListLen = index + 84;
  buf = malloc(resultListLen * sizeof(char));
  index = 0;
  eimes = ei_encode_version(buf, &index);
  eimes = ei_encode_tuple_header(buf, &index, 2);
  eimes = ei_encode_atom(buf, &index, "ok");

  if(columns[i].len == 0 || columns[i].resultLen == SQL_NULL_DATA) {
    eimes = ei_encode_atom(buf, &index,"null");
  }
  else {
    switch(columns[i].type) {
    case SQL_C_CHAR:
      eimes = ei_encode_string(buf,&index, columns[i].buf);
      break;
    case SQL_C_STINYINT:
      eimes = ei_encode_long(buf, &index, 
			     *((SQLCHAR *) columns[i].buf));
      break;        
    case SQL_C_SLONG:
      eimes = ei_encode_long(buf, &index, 
			     *((SQLINTEGER *) columns[i].buf));
      break;
    case SQL_C_SSHORT:
      eimes = ei_encode_long(buf, &index,
			     *((SQLSMALLINT *) columns[i].buf));
      break;
   case SQL_C_DOUBLE:
      eimes = ei_encode_double(buf, &index, 
			       *((SQLDOUBLE *) columns[i].buf));
      break;
    case SQL_C_FLOAT:
      eimes = ei_encode_double(buf, &index,
			       *((SQLREAL *) columns[i].buf));
      break;
    case SQL_C_BIT:
      if(*(SQLCHAR *)columns[i].buf)
	eimes = ei_encode_atom(buf, &index, "true");
      else
	eimes = ei_encode_atom(buf, &index, "false");
      break;
      
    default:
      eimes = ei_encode_atom(buf, &index,"null");
      break;
    } /* switch */
  } /* else */
  DBG(("dbreadbuffer -> index  %d\n", index));
  return index;
}

int dbCloseCursor(char *in) {
  char *mes, *result;
  int i, index;
  int eimes;
  SQLRETURN ret;	

  ret = SQLCloseCursor(hstmt);
  DBG(("dbCloseCursor -> SQLCloseCursor returns %d\n", ret));
  if (ret == SQL_ERROR || ret == SQL_INVALID_HANDLE) {
    mes = "error";
    result = "SQLCloseCursor";
    goto exit;
  }
  ret = SQLFreeHandle(SQL_HANDLE_STMT, hstmt);
  DBG(("dbCloseCursor -> SQLFreeHandle returns %d\n", ret));
  if (ret == SQL_ERROR || ret == SQL_INVALID_HANDLE) {
    mes = "error";
    result = "SQLSQLFreeHandle";
    goto exit;
  }
  mes = "ok";
  result = "dbCloseCursor";
 exit:
  DBG(("dbCloseCursor -> exit: ...\n"));
  DBG(("dbCloseCursor -> deallocate memory \n"));
  if (columns != NULL) {
    for (i = 0; i < numberColumn; i++) {
      if((columns[i].selected == _TRUE) && (columns[i].len > 0) 
	 && (columns[i].buf != NULL)) 
	free(columns[i].buf);
    }
    free(columns);
  }
  numberColumn = 0;
  DBG(("dbCloseCursor -> returned mes %s\n", mes));
  index = 0;
  buf = malloc(200 * sizeof(char));
  eimes = ei_encode_version(buf, &index);
  eimes = ei_encode_tuple_header(buf, &index, 3);
  eimes = ei_encode_atom(buf, &index, mes);
  eimes = ei_encode_string(buf, &index, result);
  eimes = ei_encode_long(buf, &index, ret); 
  DBG(("dbCloseCursor -> returned result %s \n", result));
  DBG(("dbCloseCursor -> returned index %d\n", index));
  return index;
}

/* close the connection to the database */
int dbCloseConnection(char *in) {

  char *mes, *result;
  int index;
  int eimes;
  SQLRETURN ret;

  if(connected == _FALSE) {
    mes = "error";
    result = "allready_disconnected";
    ret = -1;
    goto exit;
  }
    ret = SQLDisconnect(hdbc1);
    DBG(("dbCloseConnection -> SQLDisconnect return %d \n", ret));
    if (ret == SQL_ERROR || ret == SQL_INVALID_HANDLE) {
      mes = "error";
      result = "SQLDisconnect";
      goto exit;
    }
    ret = SQLFreeHandle(SQL_HANDLE_DBC, hdbc1);
    DBG(("dbCloseConnection -> SQLFreeHandle return %d \n", ret));
    if (ret == SQL_ERROR || ret == SQL_INVALID_HANDLE) {
      mes = "error";
      result = "SQLFreeHandle";
      goto exit;
    }
    ret = SQLFreeHandle(SQL_HANDLE_ENV, henv1);
    DBG(("dbCloseConnection -> SQLFreeHandle return %d \n", ret));
    if (ret == SQL_ERROR || ret == SQL_INVALID_HANDLE) {
      mes = "error";
      result = "SQLFreeHandle";
      goto exit;
    }
    connected = _FALSE;
    mes = "ok";
    result = "disconnected";
 exit:
    index = 0;
    buf = malloc(200 * sizeof(char));
    eimes = ei_encode_version(buf, &index);
    eimes = ei_encode_tuple_header(buf, &index, 3);
    eimes = ei_encode_atom(buf, &index, mes);
    eimes = ei_encode_string(buf, &index, result);
    eimes = ei_encode_long(buf, &index, ret); 
    DBG(("dbCloseConnection -> mes %s \n", mes));
    DBG(("dbCloseConnection -> returned result %s \n", result));
    DBG(("dbCloseConnection -> returned index %d\n", index));
    return index;
}


int main() {
  int len,lensend, index, ermes,writerr;
  char *cmdbuf;

#ifdef WIN32
  _setmode(_fileno( stdin),  _O_BINARY);
  _setmode(_fileno( stdout), _O_BINARY);
#endif

  DBG(("main() -> we start mow \n"));
  /* As long as there is something to read from Erlang */
  /* Read first two bytes, length indicator */
  while ((len = receivedlength()) > 0) {
    /* make the buffert of size len byte */
    buf = malloc(len * sizeof(char)); 
    DBG(("main() -> alloc buf of size  %d \n", len));
    cmdbuf = malloc(len * sizeof(char));
    len = read_exact(buf, len);
    DBG(("main() -> received messages lenght: %d \n", len));
    index = 0;
    ermes = ei_decode_version(buf, &index, &len);
    ermes = ei_decode_string(buf, &index, cmdbuf); 
	  
    DBG(("main() -> received messages: %s \n",cmdbuf));
    free(buf); 
    lensend = handleRequest(cmdbuf);
    DBG(("main() -> Lensend is %d\n",lensend));
    if (lensend > 0) {
      if((writerr = write_cmd(buf, lensend)) <= 0) {
	DBG(("main() -> command reply write error: %d\n",writerr));
	goto stop;
      }
      DBG(("main() -> command reply write result: %d\n",writerr));
      free(buf);
    }
    free(cmdbuf);
    
    DBG(("main() -> done for now\n"));
  } 
  DBG(("main() -> after while loop: len; %d\n",len));
  /* free(buf); */
 stop:
  DBG(("main() -> stopped!\n"));
  return 0;
}


