/* File: create_odbc_hrl.c
 * Author: Joakim Hirsch
 *
 * Purpose:
 * To translate a set of c macros used in ODBC into erlang macros
 * and write them to a file. Macros that are not integer literals
 * are handled separately in the program (defined "by hand").
 *
 * Usage:
 * Compile his program in the same context as the ODBC c-server
 * is compiled (same flags, libs, etc)Then run the program. It
 * takes the name of the output file as argument.
 * All include files of interest must of course be #included for this
 * program. 
 */

#ifdef __WIN32__
#include <windows.h>
#endif
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

/* The files containing the macros of interest */
#include "sqlext.h"
#include "odbclli_cb_codes.h"



int
main(int argc, char** argv)
{
  FILE* fd;

  if (argc != 2)
  {
    fprintf(stderr, "Usage: convert_macros <outfile>\n");
    exit(0);
  }

  fd = fopen(argv[1], "w");

  fprintf(fd, "%%%%%% This file is generated. DO NOT EDIT!!! %%%%%%\n\n");

  /* Non ODBC macros */
  fprintf(fd, "%%%% Non ODBC macros\n");
  fprintf(fd, "%%%% _______________\n\n");

  fprintf(fd, "-define(NULL_REF, %ld).\n", (long) NULL);

  fprintf(fd, "\n\n");

  /* Boolean */
  fprintf(fd, "%%%% Boolean\n");
  fprintf(fd, "%%%% _______\n\n");

  fprintf(fd, "-define(SQL_TRUE, %ld).\n", (long) SQL_TRUE);
  fprintf(fd, "-define(SQL_FALSE, %ld).\n", (long) SQL_FALSE);

  fprintf(fd, "\n\n");

  /* Handles */
  fprintf(fd, "%%%% Handles\n");
  fprintf(fd, "%%%% _______\n\n");

  fprintf(fd, "-define(SQL_HANDLE_ENV, %ld).\n", (long) SQL_HANDLE_ENV);
  fprintf(fd, "-define(SQL_HANDLE_DBC, %ld).\n", (long) SQL_HANDLE_DBC);
  fprintf(fd, "-define(SQL_HANDLE_STMT, %ld).\n", (long) SQL_HANDLE_STMT);
  fprintf(fd, "-define(SQL_NULL_HANDLE, %ld).\n", (long) SQL_NULL_HANDLE);
  fprintf(fd, "-define(SQL_NULL_HENV, %ld).\n", (long) SQL_NULL_HENV);
  fprintf(fd, "-define(SQL_NULL_HDBC, %ld).\n", (long) SQL_NULL_HDBC);
  fprintf(fd, "-define(SQL_NULL_HSTMT, %ld).\n", (long) SQL_NULL_HSTMT);

  fprintf(fd, "\n\n");

  /* Result codes */
  fprintf(fd, "%%%% Result codes\n");
  fprintf(fd, "%%%% ____________\n\n");

  fprintf(fd, "-define(SQL_SUCCESS, %ld).\n", (long) SQL_SUCCESS);
  fprintf(fd,
	  "-define(SQL_SUCCESS_WITH_INFO, %ld).\n",
	  (long) SQL_SUCCESS_WITH_INFO);
  fprintf(fd, "-define(SQL_INVALID_HANDLE, %ld).\n", (long) SQL_INVALID_HANDLE);
  fprintf(fd, "-define(SQL_ERROR, %ld).\n", (long) SQL_ERROR);
  fprintf(fd, "-define(SQL_NO_DATA, %ld).\n", (long) SQL_NO_DATA);
  fprintf(fd, "-define(SQL_NEED_DATA, %ld).\n", (long) SQL_NEED_DATA);

  fprintf(fd, "\n\n");

  /* Parameter types */
  fprintf(fd, "%%%% Parameter types\n");
  fprintf(fd, "%%%% _______________\n\n");

  fprintf(fd, "-define(SQL_PARAM_INPUT, %ld).\n", (long) SQL_PARAM_INPUT);

  fprintf(fd, "\n\n");

  /* Length/indicator values */
  fprintf(fd, "%%%% Length/indicator values\n");
  fprintf(fd, "%%%% _______________________\n\n");

  fprintf(fd, "-define(SQL_NTS, %ld).\n", (long) SQL_NTS);
  fprintf(fd, "-define(SQL_NULL_DATA, %ld).\n", (long) SQL_NULL_DATA);
  fprintf(fd, "-define(SQL_DATA_AT_EXEC, %ld).\n", (long) SQL_DATA_AT_EXEC);
  fprintf(fd, "-define(SQL_LEN_DATA_AT_EXEC(L), -L-100).\n");
  fprintf(fd, "-define(SQL_NO_TOTAL, %ld).\n", (long) SQL_NO_TOTAL);

  fprintf(fd, "\n\n");

  /* C data types */
  fprintf(fd, "%%%% C data types\n");
  fprintf(fd, "%%%% ____________\n\n");

  fprintf(fd, "-define(SQL_C_CHAR, %ld).\n", (long) SQL_C_CHAR);
  fprintf(fd, "-define(SQL_C_BINARY, %ld).\n", (long) SQL_C_BINARY);
  fprintf(fd, "-define(SQL_C_SLONG, %ld).\n", (long) SQL_C_SLONG);
  fprintf(fd, "-define(SQL_C_USHORT, %ld).\n", (long) SQL_C_USHORT);
  fprintf(fd, "-define(SQL_C_ULONG, %ld).\n", (long) SQL_C_ULONG);
  fprintf(fd, "-define(SQL_IS_UINTEGER, %ld).\n", (long) SQL_IS_UINTEGER);
  fprintf(fd, "-define(SQL_IS_INTEGER, %ld).\n", (long) SQL_IS_INTEGER);

  fprintf(fd, "\n\n");

  /* SQL data types */
  fprintf(fd, "%%%% SQL data types\n");
  fprintf(fd, "%%%% ______________\n\n");

  fprintf(fd, "-define(SQL_CHAR, %ld).\n", (long) SQL_CHAR);
  fprintf(fd, "-define(SQL_VARCHAR, %ld).\n", (long) SQL_VARCHAR);
  fprintf(fd, "-define(SQL_LONGVARCHAR, %ld).\n", (long) SQL_LONGVARCHAR);
  fprintf(fd, "-define(SQL_DECIMAL, %ld).\n", (long) SQL_DECIMAL);
  fprintf(fd, "-define(SQL_NUMERIC, %ld).\n", (long) SQL_NUMERIC);
  fprintf(fd, "-define(SQL_SMALLINT, %ld).\n", (long) SQL_SMALLINT);
  fprintf(fd, "-define(SQL_INTEGER, %ld).\n", (long) SQL_INTEGER);
  fprintf(fd, "-define(SQL_REAL, %ld).\n", (long) SQL_REAL);
  fprintf(fd, "-define(SQL_FLOAT, %ld).\n", (long) SQL_FLOAT);
  fprintf(fd, "-define(SQL_DOUBLE, %ld).\n", (long) SQL_DOUBLE);
  fprintf(fd, "-define(SQL_BIT, %ld).\n", (long) SQL_BIT);
  fprintf(fd, "-define(SQL_TINYINT, %ld).\n", (long) SQL_TINYINT);
  fprintf(fd, "-define(SQL_BIGINT, %ld).\n", (long) SQL_BIGINT);
  fprintf(fd, "-define(SQL_BINARY, %ld).\n", (long) SQL_BINARY);
  fprintf(fd, "-define(SQL_VARBINARY, %ld).\n", (long) SQL_VARBINARY);
  fprintf(fd, "-define(SQL_LONGVARBINARY, %ld).\n", (long) SQL_LONGVARBINARY);
  fprintf(fd, "-define(SQL_TYPE_DATE, %ld).\n", (long) SQL_TYPE_DATE);
  fprintf(fd, "-define(SQL_TYPE_TIME, %ld).\n", (long) SQL_TYPE_TIME);
  fprintf(fd, "-define(SQL_TYPE_TIMESTAMP, %ld).\n", (long) SQL_TYPE_TIMESTAMP);
  fprintf(fd, "-define(SQL_INTERVAL_MONTH, %ld).\n", (long) SQL_INTERVAL_MONTH);
  fprintf(fd, "-define(SQL_INTERVAL_YEAR, %ld).\n", (long) SQL_INTERVAL_YEAR);
  fprintf(fd,
	  "-define(SQL_INTERVAL_YEAR_TO_MONTH, %ld).\n",
	  (long) SQL_INTERVAL_YEAR_TO_MONTH);
  fprintf(fd, "-define(SQL_INTERVAL_DAY, %ld).\n", (long) SQL_INTERVAL_DAY);
  fprintf(fd, "-define(SQL_INTERVAL_HOUR, %ld).\n", (long) SQL_INTERVAL_HOUR);
  fprintf(fd, "-define(SQL_INTERVAL_MINUTE, %ld).\n", (long) SQL_INTERVAL_MINUTE);
  fprintf(fd, "-define(SQL_INTERVAL_SECOND, %ld).\n", (long) SQL_INTERVAL_SECOND);
  fprintf(fd,
	  "-define(SQL_INTERVAL_DAY_TO_HOUR, %ld).\n",
	  (long) SQL_INTERVAL_DAY_TO_HOUR);
  fprintf(fd,
	  "-define(SQL_INTERVAL_DAY_TO_MINUTE, %ld).\n",
	  (long) SQL_INTERVAL_DAY_TO_MINUTE);
  fprintf(fd,
	  "-define(SQL_INTERVAL_DAY_TO_SECOND, %ld).\n",
	  (long) SQL_INTERVAL_DAY_TO_SECOND);
  fprintf(fd,
	  "-define(SQL_INTERVAL_HOUR_TO_MINUTE, %ld).\n",
	  (long) SQL_INTERVAL_HOUR_TO_MINUTE);
  fprintf(fd,
	  "-define(SQL_INTERVAL_HOUR_TO_SECOND, %ld).\n",
	  (long) SQL_INTERVAL_HOUR_TO_SECOND);
  fprintf(fd,
	  "-define(SQL_INTERVAL_MINUTE_TO_SECOND, %ld).\n",
	  (long) SQL_INTERVAL_MINUTE_TO_SECOND);
  fprintf(fd, "-define(SQL_ALL_TYPES, %ld).\n", (long) SQL_ALL_TYPES);

  fprintf(fd, "\n\n");

  /* Values of "Nullable" */
  fprintf(fd, "%%%% Values of \"Nullable\"\n");
  fprintf(fd, "%%%% ____________________\n\n");

  fprintf(fd, "-define(SQL_NO_NULLS, %ld).\n", (long) SQL_NO_NULLS);
  fprintf(fd, "-define(SQL_NULLABLE, %ld).\n", (long) SQL_NULLABLE);
  fprintf(fd, "-define(SQL_NULLABLE_UNKNOWN, %ld).\n", (long) SQL_NULLABLE_UNKNOWN);

  fprintf(fd, "\n\n");

  /* Commit/rollback macros */
  fprintf(fd, "%%%% Commit/rollback macros\n");
  fprintf(fd, "%%%% ______________________\n\n");

  fprintf(fd, "-define(SQL_COMMIT, %ld).\n", (long) SQL_COMMIT);
  fprintf(fd, "-define(SQL_ROLLBACK, %ld).\n", (long) SQL_ROLLBACK);

  fprintf(fd, "\n\n");

  /* Environment attributes and values */
  fprintf(fd, "%%%% Environment attributes and values\n");
  fprintf(fd, "%%%% _________________________________\n\n");

  fprintf(fd,
	  "-define(SQL_ATTR_ODBC_VERSION, %ld).\n",
	  (long) SQL_ATTR_ODBC_VERSION);
  fprintf(fd, "-define(SQL_OV_ODBC3, %ld).\n", (long) SQL_OV_ODBC3);
  fprintf(fd, "-define(SQL_OV_ODBC2, %ld).\n", (long) SQL_OV_ODBC2);

  fprintf(fd, "\n\n");

  /* Connection attributes and values */
  fprintf(fd, "%%%% Connection attributes and values\n");
  fprintf(fd, "%%%% ________________________________\n\n");

  fprintf(fd, "-define(SQL_ATTR_ACCESS_MODE, %ld).\n", (long) SQL_ATTR_ACCESS_MODE);
  fprintf(fd, "-define(SQL_MODE_READ_ONLY, %ld).\n", (long) SQL_MODE_READ_ONLY);
  fprintf(fd, "-define(SQL_MODE_READ_WRITE, %ld).\n", (long) SQL_MODE_READ_WRITE);

  fprintf(fd, "-define(SQL_ATTR_AUTOCOMMIT, %ld).\n", (long) SQL_ATTR_AUTOCOMMIT);
  fprintf(fd, "-define(SQL_AUTOCOMMIT_OFF, %ld).\n", (long) SQL_AUTOCOMMIT_OFF);
  fprintf(fd, "-define(SQL_AUTOCOMMIT_ON, %ld).\n", (long) SQL_AUTOCOMMIT_ON);

  fprintf(fd,
	  "-define(SQL_ATTR_ODBC_CURSORS, %ld).\n",
	  (long) SQL_ATTR_ODBC_CURSORS);
  fprintf(fd,
	  "-define(SQL_CUR_USE_IF_NEEDED, %ld).\n",
	  (long) SQL_CUR_USE_IF_NEEDED);
  fprintf(fd, "-define(SQL_FETCH_PRIOR, %ld).\n", (long) SQL_FETCH_PRIOR);
  fprintf(fd, "-define(SQL_CUR_USE_ODBC, %ld).\n", (long) SQL_CUR_USE_ODBC);
  fprintf(fd, "-define(SQL_CUR_USE_DRIVER, %ld).\n", (long) SQL_CUR_USE_DRIVER);

  fprintf(fd, "-define(SQL_ATTR_TRACE, %ld).\n", (long) SQL_ATTR_TRACE);
  fprintf(fd, "-define(SQL_OPT_TRACE_OFF, %ld).\n", (long) SQL_OPT_TRACE_OFF);
  fprintf(fd, "-define(SQL_OPT_TRACE_ON, %ld).\n", (long) SQL_OPT_TRACE_ON);

  fprintf(fd, "-define(SQL_ATTR_TRACEFILE, %ld).\n", (long) SQL_ATTR_TRACEFILE);

  fprintf(fd,
	  "-define(SQL_ATTR_TRANSLATE_LIB, %ld).\n",
	  (long) SQL_ATTR_TRANSLATE_LIB);

  fprintf(fd,
	  "-define(SQL_ATTR_TRANSLATE_OPTION, %ld).\n",
	  (long) SQL_ATTR_TRANSLATE_OPTION);

  fprintf(fd, "\n\n");

  /* Statement attributes and values */
  fprintf(fd, "%%%% Statement attributes and values\n");
  fprintf(fd, "%%%% ________________________________\n\n");

  fprintf(fd, "-define(SQL_ATTR_METADATA_ID, %ld).\n", (long) SQL_ATTR_METADATA_ID);

  fprintf(fd, "-define(SQL_ATTR_NOSCAN, %ld).\n", (long) SQL_ATTR_NOSCAN);
  fprintf(fd, "-define(SQL_NOSCAN_OFF, %ld).\n", (long) SQL_NOSCAN_OFF);
  fprintf(fd, "-define(SQL_NOSCAN_ON, %ld).\n", (long) SQL_NOSCAN_ON);

#ifdef SQL_ATTR_MAX_LENGTH
  fprintf(fd, "-define(SQL_ATTR_MAX_LENGTH, %ld).\n", (long) SQL_ATTR_MAX_LENGTH);
#endif

  fprintf(fd, "\n\n");

  /* GetFunctions macros */
  fprintf(fd, "%%%% sql_get_functions macros\n");
  fprintf(fd, "%%%% ________________________\n\n");

  fprintf(fd,
	  "-define(SQL_API_ODBC3_ALL_FUNCTIONS, %ld).\n",
	  (long) SQL_API_ODBC3_ALL_FUNCTIONS);
  fprintf(fd,
	  "-define(SQL_API_ODBC3_ALL_FUNCTIONS_SIZE, %ld).\n",
	  (long) SQL_API_ODBC3_ALL_FUNCTIONS_SIZE);
  fprintf(fd,
	  "-define(SQL_API_SQLALLOCHANDLE, %ld).\n",
	  (long) SQL_API_SQLALLOCHANDLE);
  fprintf(fd, "-define(SQL_API_SQLBINDCOL, %ld).\n", (long) SQL_API_SQLBINDCOL);
  fprintf(fd, "-define(SQL_API_SQLCANCEL, %ld).\n", (long) SQL_API_SQLCANCEL);
  fprintf(fd,
	  "-define(SQL_API_SQLCLOSECURSOR, %ld).\n",
	  (long) SQL_API_SQLCLOSECURSOR);
  fprintf(fd,
	  "-define(SQL_API_SQLCOLATTRIBUTE, %ld).\n",
	  (long) SQL_API_SQLCOLATTRIBUTE);
  fprintf(fd, "-define(SQL_API_SQLCONNECT, %ld).\n", (long) SQL_API_SQLCONNECT);
  fprintf(fd,
	  "-define(SQL_API_SQLDATASOURCES, %ld).\n",
	  (long) SQL_API_SQLDATASOURCES);
  fprintf(fd,
	  "-define(SQL_API_SQLDESCRIBECOL, %ld).\n",
	  (long) SQL_API_SQLDESCRIBECOL);
  fprintf(fd,
	  "-define(SQL_API_SQLDISCONNECT, %ld).\n",
	  (long) SQL_API_SQLDISCONNECT);
  fprintf(fd, "-define(SQL_API_SQLDRIVERS, %ld).\n", (long) SQL_API_SQLDRIVERS);
  fprintf(fd, "-define(SQL_API_SQLENDTRAN, %ld).\n", (long) SQL_API_SQLENDTRAN);
  fprintf(fd,
	  "-define(SQL_API_SQLGETDIAGREC, %ld).\n",
	  (long) SQL_API_SQLGETDIAGREC);
  fprintf(fd,
	  "-define(SQL_API_SQLGETENVATTR, %ld).\n",
	  (long) SQL_API_SQLGETENVATTR);
  fprintf(fd,
	  "-define(SQL_API_SQLGETFUNCTIONS, %ld).\n",
	  (long) SQL_API_SQLGETFUNCTIONS);
  fprintf(fd, "-define(SQL_API_SQLGETINFO, %ld).\n", (long) SQL_API_SQLGETINFO);
  fprintf(fd,
	  "-define(SQL_API_SQLGETSTMTATTR, %ld).\n",
	  (long) SQL_API_SQLGETSTMTATTR);
  fprintf(fd,
	  "-define(SQL_API_SQLGETTYPEINFO, %ld).\n",
	  (long) SQL_API_SQLGETTYPEINFO);
  fprintf(fd,
	  "-define(SQL_API_SQLNUMRESULTCOLS, %ld).\n",
	  (long) SQL_API_SQLNUMRESULTCOLS);
  fprintf(fd, "-define(SQL_API_SQLPARAMDATA, %ld).\n", (long) SQL_API_SQLPARAMDATA);
  fprintf(fd, "-define(SQL_API_SQLPREPARE, %ld).\n", (long) SQL_API_SQLPREPARE);
  fprintf(fd,
	  "-define(SQL_API_SQLEXECDIRECT, %ld).\n",
	  (long) SQL_API_SQLEXECDIRECT);
  fprintf(fd, "-define(SQL_API_SQLEXECUTE, %ld).\n", (long) SQL_API_SQLEXECUTE);
  fprintf(fd, "-define(SQL_API_SQLFETCH, %ld).\n", (long) SQL_API_SQLFETCH);
  fprintf(fd,
	  "-define(SQL_API_SQLFREEHANDLE, %ld).\n",
	  (long) SQL_API_SQLFREEHANDLE);
  fprintf(fd, "-define(SQL_API_SQLFREESTMT, %ld).\n", (long) SQL_API_SQLFREESTMT);
  fprintf(fd,
	  "-define(SQL_API_SQLGETCONNECTATTR, %ld).\n",
	  (long) SQL_API_SQLGETCONNECTATTR);
  fprintf(fd,
	  "-define(SQL_API_SQLGETCURSORNAME, %ld).\n",
	  (long) SQL_API_SQLGETCURSORNAME);
  fprintf(fd,
	  "-define(SQL_API_SQLGETDATA, %ld).\n",
	  (long) SQL_API_SQLGETDATA);
  fprintf(fd, "-define(SQL_API_SQLPUTDATA, %ld).\n", (long) SQL_API_SQLPUTDATA);
  fprintf(fd, "-define(SQL_API_SQLROWCOUNT, %ld).\n", (long) SQL_API_SQLROWCOUNT);
  fprintf(fd,
	  "-define(SQL_API_SQLSETCONNECTATTR, %ld).\n",
	  (long) SQL_API_SQLSETCONNECTATTR);
  fprintf(fd,
	  "-define(SQL_API_SQLSETCURSORNAME, %ld).\n",
	  (long) SQL_API_SQLSETCURSORNAME);
  fprintf(fd,
	  "-define(SQL_API_SQLSETENVATTR, %ld).\n",
	  (long) SQL_API_SQLSETENVATTR);
  fprintf(fd,
	  "-define(SQL_API_SQLSETSTMTATTR, %ld).\n",
	  (long) SQL_API_SQLSETSTMTATTR);
  fprintf(fd,
	  "-define(SQL_API_SQLCOLUMNS, %ld).\n",
	  (long) SQL_API_SQLCOLUMNS);
  fprintf(fd,
	  "-define(SQL_API_SQLSPECIALCOLUMNS, %ld).\n",
	  (long) SQL_API_SQLSPECIALCOLUMNS);
  fprintf(fd,
	  "-define(SQL_API_SQLSTATISTICS, %ld).\n",
	  (long) SQL_API_SQLSTATISTICS);
  fprintf(fd, "-define(SQL_API_SQLTABLES, %ld).\n", (long) SQL_API_SQLTABLES);
  fprintf(fd,
	  "-define(SQL_API_SQLBINDPARAMETER, %ld).\n",
	  (long) SQL_API_SQLBINDPARAMETER);
  fprintf(fd,
	  "-define(SQL_API_SQLDRIVERCONNECT, %ld).\n",
	  (long) SQL_API_SQLDRIVERCONNECT);
  fprintf(fd, "-define(SQL_API_SQLNATIVESQL, %ld).\n", (long) SQL_API_SQLNATIVESQL);
  fprintf(fd, "-define(SQL_API_SQLNUMPARAMS, %ld).\n", (long) SQL_API_SQLNUMPARAMS);

  fprintf(fd, "\n\n");

  /* GetInfo macros */
  fprintf(fd, "%%%% sql_get_info macros\n");
  fprintf(fd, "%%%% ___________________\n\n");

  fprintf(fd,
	  "-define(SQL_ACCESSIBLE_PROCEDURES, %ld).\n",
	  (long) SQL_ACCESSIBLE_PROCEDURES);

  fprintf(fd,
	  "-define(SQL_ACCESSIBLE_TABLES, %ld).\n",
	  (long) SQL_ACCESSIBLE_TABLES);

  fprintf(fd,
	  "-define(SQL_ACTIVE_ENVIRONMENTS, %ld).\n",
	  (long) SQL_ACTIVE_ENVIRONMENTS);

  fprintf(fd,
	  "-define(SQL_AGGREGATE_FUNCTIONS, %ld).\n",
	  (long) SQL_AGGREGATE_FUNCTIONS);
  fprintf(fd, "-define(SQL_AF_ALL, %ld).\n", (long) SQL_AF_ALL);
  fprintf(fd, "-define(SQL_AF_AVG, %ld).\n", (long) SQL_AF_AVG);
  fprintf(fd, "-define(SQL_AF_COUNT, %ld).\n", (long) SQL_AF_COUNT);
  fprintf(fd, "-define(SQL_AF_DISTINCT, %ld).\n", (long) SQL_AF_DISTINCT);
  fprintf(fd, "-define(SQL_AF_MAX, %ld).\n", (long) SQL_AF_MAX);
  fprintf(fd, "-define(SQL_AF_MIN, %ld).\n", (long) SQL_AF_MIN);
  fprintf(fd, "-define(SQL_AF_SUM, %ld).\n", (long) SQL_AF_SUM);

  fprintf(fd, "-define(SQL_ALTER_DOMAIN, %ld).\n", (long) SQL_ALTER_DOMAIN);
  fprintf(fd,
	  "-define(SQL_AD_ADD_DOMAIN_CONSTRAINT, %ld).\n",
	  (long) SQL_AD_ADD_DOMAIN_CONSTRAINT);
  fprintf(fd,
	  "-define(SQL_AD_ADD_DOMAIN_DEFAULT, %ld).\n",
	  (long) SQL_AD_ADD_DOMAIN_DEFAULT);
  fprintf(fd,
	  "-define(SQL_AD_CONSTRAINT_NAME_DEFINITION, %ld).\n",
	  (long) SQL_AD_CONSTRAINT_NAME_DEFINITION);
  fprintf(fd,
	  "-define(SQL_AD_DROP_DOMAIN_CONSTRAINT, %ld).\n",
	  (long) SQL_AD_DROP_DOMAIN_CONSTRAINT);
  fprintf(fd,
	  "-define(SQL_AD_DROP_DOMAIN_DEFAULT, %ld).\n",
	  (long) SQL_AD_DROP_DOMAIN_DEFAULT);
  fprintf(fd,
	  "-define(SQL_AD_ADD_CONSTRAINT_DEFERRABLE, %ld).\n",
	  (long) SQL_AD_ADD_CONSTRAINT_DEFERRABLE);
  fprintf(fd,
	  "-define(SQL_AD_ADD_CONSTRAINT_NON_DEFERRABLE, %ld).\n",
	  (long) SQL_AD_ADD_CONSTRAINT_NON_DEFERRABLE);
  fprintf(fd,
	  "-define(SQL_AD_ADD_CONSTRAINT_INITIALLY_DEFERRED, %ld).\n",
	  (long) SQL_AD_ADD_CONSTRAINT_INITIALLY_DEFERRED);
  fprintf(fd,
	  "-define(SQL_AD_ADD_CONSTRAINT_INITIALLY_IMMEDIATE, %ld).\n",
	  (long) SQL_AD_ADD_CONSTRAINT_INITIALLY_IMMEDIATE);

  fprintf(fd, "-define(SQL_ALTER_TABLE, %ld).\n", (long) SQL_ALTER_TABLE);
  fprintf(fd,
	  "-define(SQL_AT_ADD_COLUMN_COLLATION, %ld).\n",
	  (long) SQL_AT_ADD_COLUMN_COLLATION);
  fprintf(fd,
	  "-define(SQL_AT_ADD_COLUMN_DEFAULT, %ld).\n",
	  (long) SQL_AT_ADD_COLUMN_DEFAULT);
  fprintf(fd,
	  "-define(SQL_AT_ADD_COLUMN_SINGLE, %ld).\n",
	  (long) SQL_AT_ADD_COLUMN_SINGLE);
  fprintf(fd,
	  "-define(SQL_AT_ADD_CONSTRAINT, %ld).\n",
	  (long) SQL_AT_ADD_CONSTRAINT);
  fprintf(fd,
	  "-define(SQL_AT_ADD_TABLE_CONSTRAINT, %ld).\n",
	  (long) SQL_AT_ADD_TABLE_CONSTRAINT);
  fprintf(fd,
	  "-define(SQL_AT_CONSTRAINT_NAME_DEFINITION, %ld).\n",
	  (long) SQL_AT_CONSTRAINT_NAME_DEFINITION);
  fprintf(fd,
	  "-define(SQL_AT_DROP_COLUMN_CASCADE, %ld).\n",
	  (long) SQL_AT_DROP_COLUMN_CASCADE);
  fprintf(fd,
	  "-define(SQL_AT_DROP_COLUMN_DEFAULT, %ld).\n",
	  (long) SQL_AT_DROP_COLUMN_DEFAULT);
  fprintf(fd,
	  "-define(SQL_AT_DROP_COLUMN_RESTRICT, %ld).\n",
	  (long) SQL_AT_DROP_COLUMN_RESTRICT);
  fprintf(fd,
	  "-define(SQL_AT_DROP_TABLE_CONSTRAINT_CASCADE, %ld).\n",
	  (long) SQL_AT_DROP_TABLE_CONSTRAINT_CASCADE);
  fprintf(fd,
	  "-define(SQL_AT_DROP_TABLE_CONSTRAINT_RESTRICT, %ld).\n",
	  (long) SQL_AT_DROP_TABLE_CONSTRAINT_RESTRICT);
  fprintf(fd,
	  "-define(SQL_AT_SET_COLUMN_DEFAULT, %ld).\n",
	  (long) SQL_AT_SET_COLUMN_DEFAULT);
  fprintf(fd,
	  "-define(SQL_AT_CONSTRAINT_INITIALLY_DEFERRED, %ld).\n",
	  (long) SQL_AT_CONSTRAINT_INITIALLY_DEFERRED);
  fprintf(fd,
	  "-define(SQL_AT_CONSTRAINT_INITIALLY_IMMEDIATE, %ld).\n",
	  (long) SQL_AT_CONSTRAINT_INITIALLY_IMMEDIATE);
  fprintf(fd,
	  "-define(SQL_AT_CONSTRAINT_DEFERRABLE, %ld).\n",
	  (long) SQL_AT_CONSTRAINT_DEFERRABLE);
  fprintf(fd,
	  "-define(SQL_AT_CONSTRAINT_NON_DEFERRABLE, %ld).\n",
	  (long) SQL_AT_CONSTRAINT_NON_DEFERRABLE);

  fprintf(fd, "-define(SQL_ASYNC_MODE, %ld).\n", (long) SQL_ASYNC_MODE);
  fprintf(fd, "-define(SQL_AM_CONNECTION, %ld).\n", (long) SQL_AM_CONNECTION);
  fprintf(fd, "-define(SQL_AM_STATEMENT, %ld).\n", (long) SQL_AM_STATEMENT);
  fprintf(fd, "-define(SQL_AM_NONE, %ld).\n", (long) SQL_AM_STATEMENT);

  fprintf(fd, "-define(SQL_BATCH_ROW_COUNT, %ld).\n", (long) SQL_BATCH_ROW_COUNT);
  fprintf(fd, "-define(SQL_BRC_ROLLED_UP, %ld).\n", (long) SQL_BRC_ROLLED_UP);
  fprintf(fd, "-define(SQL_BRC_PROCEDURES, %ld).\n", (long) SQL_BRC_PROCEDURES);
  fprintf(fd, "-define(SQL_BRC_EXPLICIT, %ld).\n", (long) SQL_BRC_EXPLICIT);

  fprintf(fd, "-define(SQL_BATCH_SUPPORT, %ld).\n", (long) SQL_BATCH_SUPPORT);
  fprintf(fd,
	  "-define(SQL_BS_SELECT_EXPLICIT, %ld).\n",
	  (long) SQL_BS_SELECT_EXPLICIT);
  fprintf(fd,
	  "-define(SQL_BS_ROW_COUNT_EXPLICIT, %ld).\n",
	  (long) SQL_BS_ROW_COUNT_EXPLICIT);
  fprintf(fd,
	  "-define(SQL_BS_SELECT_PROC, %ld).\n",
	  (long) SQL_BS_SELECT_PROC);
  fprintf(fd,
	  "-define(SQL_BS_ROW_COUNT_PROC, %ld).\n",
	  (long) SQL_BS_ROW_COUNT_PROC);

  fprintf(fd,
	  "-define(SQL_BOOKMARK_PERSISTENCE, %ld).\n",
	  (long) SQL_BOOKMARK_PERSISTENCE);
  fprintf(fd, "-define(SQL_BP_CLOSE, %ld).\n", (long) SQL_BP_CLOSE);
  fprintf(fd, "-define(SQL_BP_DELETE, %ld).\n", (long) SQL_BP_DELETE);
  fprintf(fd, "-define(SQL_BP_DROP, %ld).\n", (long) SQL_BP_DROP);
  fprintf(fd, "-define(SQL_BP_TRANSACTION, %ld).\n", (long) SQL_BP_TRANSACTION);
  fprintf(fd, "-define(SQL_BP_UPDATE, %ld).\n", (long) SQL_BP_UPDATE);
  fprintf(fd, "-define(SQL_BP_OTHER_HSTMT, %ld).\n", (long) SQL_BP_OTHER_HSTMT);

  fprintf(fd, "-define(SQL_CATALOG_LOCATION, %ld).\n", (long) SQL_CATALOG_LOCATION);
  fprintf(fd, "-define(SQL_CL_START, %ld).\n", (long) SQL_CL_START);
  fprintf(fd, "-define(SQL_CL_END, %ld).\n", (long) SQL_CL_END);

  fprintf(fd, "-define(SQL_CATALOG_NAME, %ld).\n", (long) SQL_CATALOG_NAME);

  fprintf(fd,
	  "-define(SQL_CATALOG_NAME_SEPARATOR, %ld).\n",
	  (long) SQL_CATALOG_NAME_SEPARATOR);

  fprintf(fd, "-define(SQL_CATALOG_TERM, %ld).\n", (long) SQL_CATALOG_TERM);

  fprintf(fd, "-define(SQL_CATALOG_USAGE, %ld).\n", (long) SQL_CATALOG_USAGE);
  fprintf(fd,
	  "-define(SQL_CU_DML_STATEMENTS, %ld).\n",
	  (long) SQL_CU_DML_STATEMENTS);
  fprintf(fd,
	  "-define(SQL_CU_PROCEDURE_INVOCATION, %ld).\n",
	  (long) SQL_CU_PROCEDURE_INVOCATION);
  fprintf(fd,
	  "-define(SQL_CU_TABLE_DEFINITION, %ld).\n",
	  (long) SQL_CU_TABLE_DEFINITION);
  fprintf(fd,
	  "-define(SQL_CU_INDEX_DEFINITION, %ld).\n",
	  (long) SQL_CU_INDEX_DEFINITION);
  fprintf(fd,
	  "-define(SQL_CU_PRIVILEGE_DEFINITION, %ld).\n",
	  (long) SQL_CU_PRIVILEGE_DEFINITION);

  fprintf(fd, "-define(SQL_COLLATION_SEQ, %ld).\n", (long) SQL_COLLATION_SEQ);

  fprintf(fd, "-define(SQL_COLUMN_ALIAS, %ld).\n", (long) SQL_COLUMN_ALIAS);

  fprintf(fd,
	  "-define(SQL_CONCAT_NULL_BEHAVIOR, %ld).\n",
	  (long) SQL_CONCAT_NULL_BEHAVIOR);
  fprintf(fd, "-define(SQL_CB_NULL, %ld).\n", (long) SQL_CB_NULL);
  fprintf(fd, "-define(SQL_CB_NON_NULL, %ld).\n", (long) SQL_CB_NON_NULL);

  fprintf(fd, "-define(SQL_CONVERT_BIGINT, %ld).\n", (long) SQL_CONVERT_BIGINT);
  fprintf(fd, "-define(SQL_CONVERT_BINARY, %ld).\n", (long) SQL_CONVERT_BINARY);
  fprintf(fd, "-define(SQL_CONVERT_BIT, %ld).\n", (long) SQL_CONVERT_BIT);
  fprintf(fd, "-define(SQL_CONVERT_CHAR, %ld).\n", (long) SQL_CONVERT_CHAR);
  fprintf(fd, "-define(SQL_CONVERT_DATE, %ld).\n", (long) SQL_CONVERT_DATE);
  fprintf(fd, "-define(SQL_CONVERT_DECIMAL, %ld).\n", (long) SQL_CONVERT_DECIMAL);
  fprintf(fd, "-define(SQL_CONVERT_DOUBLE, %ld).\n", (long) SQL_CONVERT_DOUBLE);
  fprintf(fd, "-define(SQL_CONVERT_FLOAT, %ld).\n", (long) SQL_CONVERT_FLOAT);
  fprintf(fd, "-define(SQL_CONVERT_INTEGER, %ld).\n", (long) SQL_CONVERT_INTEGER);
  fprintf(fd,
	  "-define(SQL_CONVERT_INTERVAL_YEAR_MONTH, %ld).\n",
	  (long) SQL_CONVERT_INTERVAL_YEAR_MONTH);
  fprintf(fd,
	  "-define(SQL_CONVERT_INTERVAL_DAY_TIME, %ld).\n",
	  (long) SQL_CONVERT_INTERVAL_DAY_TIME);
  fprintf(fd,
	  "-define(SQL_CONVERT_LONGVARBINARY, %ld).\n",
	  (long) SQL_CONVERT_LONGVARBINARY);
  fprintf(fd,
	  "-define(SQL_CONVERT_LONGVARCHAR, %ld).\n",
	  (long) SQL_CONVERT_LONGVARCHAR);
  fprintf(fd, "-define(SQL_CONVERT_NUMERIC, %ld).\n", (long) SQL_CONVERT_NUMERIC);
  fprintf(fd, "-define(SQL_CONVERT_REAL, %ld).\n", (long) SQL_CONVERT_REAL);
  fprintf(fd, "-define(SQL_CONVERT_SMALLINT, %ld).\n", (long) SQL_CONVERT_SMALLINT);
  fprintf(fd, "-define(SQL_CONVERT_TIME, %ld).\n", (long) SQL_CONVERT_TIME);
  fprintf(fd,
	  "-define(SQL_CONVERT_TIMESTAMP, %ld).\n",
	  (long) SQL_CONVERT_TIMESTAMP);
  fprintf(fd, "-define(SQL_CONVERT_TINYINT, %ld).\n", (long) SQL_CONVERT_TINYINT);
  fprintf(fd,
	  "-define(SQL_CONVERT_VARBINARY, %ld).\n",
	  (long) SQL_CONVERT_VARBINARY);
  fprintf(fd, "-define(SQL_CONVERT_VARCHAR, %ld).\n", (long) SQL_CONVERT_VARCHAR);
  fprintf(fd, "-define(SQL_CVT_BIGINT, %ld).\n", (long) SQL_CVT_BIGINT);
  fprintf(fd, "-define(SQL_CVT_BINARY, %ld).\n", (long) SQL_CVT_BINARY);
  fprintf(fd, "-define(SQL_CVT_BIT, %ld).\n", (long) SQL_CVT_BIT);
  fprintf(fd, "-define(SQL_CVT_CHAR, %ld).\n", (long) SQL_CVT_CHAR);
  fprintf(fd, "-define(SQL_CVT_DATE, %ld).\n", (long) SQL_CVT_DATE);
  fprintf(fd, "-define(SQL_CVT_DECIMAL, %ld).\n", (long) SQL_CVT_DECIMAL);
  fprintf(fd, "-define(SQL_CVT_DOUBLE, %ld).\n", (long) SQL_CVT_DOUBLE);
  fprintf(fd, "-define(SQL_CVT_FLOAT, %ld).\n", (long) SQL_CVT_FLOAT);
  fprintf(fd, "-define(SQL_CVT_INTEGER, %ld).\n", (long) SQL_CVT_INTEGER);
  fprintf(fd,
	  "-define(SQL_CVT_INTERVAL_YEAR_MONTH, %ld).\n",
	  (long) SQL_CVT_INTERVAL_YEAR_MONTH);
  fprintf(fd,
	  "-define(SQL_CVT_INTERVAL_DAY_TIME, %ld).\n",
	  (long) SQL_CVT_INTERVAL_DAY_TIME);
  fprintf(fd,
	  "-define(SQL_CVT_LONGVARBINARY, %ld).\n",
	  (long) SQL_CVT_LONGVARBINARY);
  fprintf(fd, "-define(SQL_CVT_LONGVARCHAR, %ld).\n", (long) SQL_CVT_LONGVARCHAR);
  fprintf(fd, "-define(SQL_CVT_NUMERIC, %ld).\n", (long) SQL_CVT_NUMERIC);
  fprintf(fd, "-define(SQL_CVT_REAL, %ld).\n", (long) SQL_CVT_REAL);
  fprintf(fd, "-define(SQL_CVT_SMALLINT, %ld).\n", (long) SQL_CVT_SMALLINT);
  fprintf(fd, "-define(SQL_CVT_TIME, %ld).\n", (long) SQL_CVT_TIME);
  fprintf(fd, "-define(SQL_CVT_TIMESTAMP, %ld).\n", (long) SQL_CVT_TIMESTAMP);
  fprintf(fd, "-define(SQL_CVT_TINYINT, %ld).\n", (long) SQL_CVT_TINYINT);
  fprintf(fd, "-define(SQL_CVT_VARBINARY, %ld).\n", (long) SQL_CVT_VARBINARY);
  fprintf(fd, "-define(SQL_CVT_VARCHAR, %ld).\n", (long) SQL_CVT_VARCHAR);

  fprintf(fd,
	  "-define(SQL_CONVERT_FUNCTIONS, %ld).\n",
	  (long) SQL_CONVERT_FUNCTIONS);
  fprintf(fd, "-define(SQL_FN_CVT_CAST, %ld).\n", (long) SQL_FN_CVT_CAST);
  fprintf(fd, "-define(SQL_FN_CVT_CONVERT, %ld).\n", (long) SQL_FN_CVT_CONVERT);

  fprintf(fd, "-define(SQL_CORRELATION_NAME, %ld).\n", (long) SQL_CORRELATION_NAME);
  fprintf(fd, "-define(SQL_CN_NONE, %ld).\n", (long) SQL_CN_NONE);
  fprintf(fd, "-define(SQL_CN_DIFFERENT, %ld).\n", (long) SQL_CN_DIFFERENT);
  fprintf(fd, "-define(SQL_CN_ANY, %ld).\n", (long) SQL_CN_ANY);

  fprintf(fd, "-define(SQL_CREATE_ASSERTION, %ld).\n", (long) SQL_CREATE_ASSERTION);
  fprintf(fd,
	  "-define(SQL_CA_CREATE_ASSERTION, %ld).\n",
	  (long) SQL_CA_CREATE_ASSERTION);
  fprintf(fd,
	  "-define(SQL_CA_CONSTRAINT_INITIALLY_DEFERRED, %ld).\n",
	  (long) SQL_CA_CONSTRAINT_INITIALLY_DEFERRED);
  fprintf(fd,
	  "-define(SQL_CA_CONSTRAINT_INITIALLY_IMMEDIATE, %ld).\n",
	  (long) SQL_CA_CONSTRAINT_INITIALLY_IMMEDIATE);
  fprintf(fd,
	  "-define(SQL_CA_CONSTRAINT_DEFERRABLE, %ld).\n",
	  (long) SQL_CA_CONSTRAINT_DEFERRABLE);
  fprintf(fd,
	  "-define(SQL_CA_CONSTRAINT_NON_DEFERRABLE, %ld).\n",
	  (long) SQL_CA_CONSTRAINT_NON_DEFERRABLE);

  fprintf(fd,
	  "-define(SQL_CREATE_CHARACTER_SET, %ld).\n",
	  (long) SQL_CREATE_CHARACTER_SET);
  fprintf(fd,
	  "-define(SQL_CCS_CREATE_CHARACTER_SET, %ld).\n",
	  (long) SQL_CCS_CREATE_CHARACTER_SET);
  fprintf(fd,
	  "-define(SQL_CCS_COLLATE_CLAUSE, %ld).\n",
	  (long) SQL_CCS_COLLATE_CLAUSE);
  fprintf(fd,
	  "-define(SQL_CCS_LIMITED_COLLATION, %ld).\n",
	  (long) SQL_CCS_LIMITED_COLLATION);

  fprintf(fd, "-define(SQL_CREATE_COLLATION, %ld).\n", (long) SQL_CREATE_COLLATION);
  fprintf(fd,
	  "-define(SQL_CCOL_CREATE_COLLATION, %ld).\n",
	  (long) SQL_CCOL_CREATE_COLLATION);

  fprintf(fd, "-define(SQL_CREATE_DOMAIN, %ld).\n", (long) SQL_CREATE_DOMAIN);
  fprintf(fd,
	  "-define(SQL_CDO_CREATE_DOMAIN, %ld).\n",
	  (long) SQL_CDO_CREATE_DOMAIN);
  fprintf(fd,
	  "-define(SQL_CDO_CONSTRAINT_NAME_DEFINITION, %ld).\n",
	  (long) SQL_CDO_CONSTRAINT_NAME_DEFINITION);
  fprintf(fd, "-define(SQL_CDO_DEFAULT, %ld).\n", (long) SQL_CDO_DEFAULT);
  fprintf(fd, "-define(SQL_CDO_CONSTRAINT, %ld).\n", (long) SQL_CDO_CONSTRAINT);
  fprintf(fd, "-define(SQL_CDO_COLLATION, %ld).\n", (long) SQL_CDO_COLLATION);
  fprintf(fd,
	  "-define(SQL_CDO_CONSTRAINT_INITIALLY_DEFERRED, %ld).\n",
	  (long) SQL_CDO_CONSTRAINT_INITIALLY_DEFERRED);
  fprintf(fd,
	  "-define(SQL_CDO_CONSTRAINT_INITIALLY_IMMEDIATE, %ld).\n",
	  (long) SQL_CDO_CONSTRAINT_INITIALLY_IMMEDIATE);
  fprintf(fd,
	  "-define(SQL_CDO_CONSTRAINT_DEFERRABLE, %ld).\n",
	  (long) SQL_CDO_CONSTRAINT_DEFERRABLE);
  fprintf(fd,
	  "-define(SQL_CDO_CONSTRAINT_NON_DEFERRABLE, %ld).\n",
	  (long) SQL_CDO_CONSTRAINT_NON_DEFERRABLE);

  fprintf(fd, "-define(SQL_CREATE_SCHEMA, %ld).\n", (long) SQL_CREATE_SCHEMA);
  fprintf(fd, "-define(SQL_CS_CREATE_SCHEMA, %ld).\n", (long) SQL_CS_CREATE_SCHEMA);
  fprintf(fd, "-define(SQL_CS_AUTHORIZATION, %ld).\n", (long) SQL_CS_AUTHORIZATION);
  fprintf(fd,
	  "-define(SQL_CS_DEFAULT_CHARACTER_SET, %ld).\n",
	  (long) SQL_CS_DEFAULT_CHARACTER_SET);

  fprintf(fd, "-define(SQL_CREATE_TABLE, %ld).\n", (long) SQL_CREATE_TABLE);
  fprintf(fd, "-define(SQL_CT_CREATE_TABLE, %ld).\n", (long) SQL_CT_CREATE_TABLE);
  fprintf(fd,
	  "-define(SQL_CT_TABLE_CONSTRAINT, %ld).\n",
	  (long) SQL_CT_TABLE_CONSTRAINT);
  fprintf(fd,
	  "-define(SQL_CT_CONSTRAINT_NAME_DEFINITION, %ld).\n",
	  (long) SQL_CT_CONSTRAINT_NAME_DEFINITION);
  fprintf(fd,
	  "-define(SQL_CT_COMMIT_PRESERVE, %ld).\n",
	  (long) SQL_CT_COMMIT_PRESERVE);
  fprintf(fd, "-define(SQL_CT_COMMIT_DELETE, %ld).\n", (long) SQL_CT_COMMIT_DELETE);
  fprintf(fd,
	  "-define(SQL_CT_GLOBAL_TEMPORARY, %ld).\n",
	  (long) SQL_CT_GLOBAL_TEMPORARY);
  fprintf(fd,
	  "-define(SQL_CT_LOCAL_TEMPORARY, %ld).\n",
	  (long) SQL_CT_LOCAL_TEMPORARY);
  fprintf(fd,
	  "-define(SQL_CT_COLUMN_CONSTRAINT, %ld).\n",
	  (long) SQL_CT_COLUMN_CONSTRAINT);
  fprintf(fd,
	  "-define(SQL_CT_COLUMN_DEFAULT, %ld).\n",
	  (long) SQL_CT_COLUMN_DEFAULT);
  fprintf(fd,
	  "-define(SQL_CT_COLUMN_COLLATION, %ld).\n",
	  (long) SQL_CT_COLUMN_COLLATION);
  fprintf(fd,
	  "-define(SQL_CT_CONSTRAINT_INITIALLY_DEFERRED, %ld).\n",
	  (long) SQL_CT_CONSTRAINT_INITIALLY_DEFERRED);
  fprintf(fd,
	  "-define(SQL_CT_CONSTRAINT_INITIALLY_IMMEDIATE, %ld).\n",
	  (long) SQL_CT_CONSTRAINT_INITIALLY_IMMEDIATE);
  fprintf(fd,
	  "-define(SQL_CT_CONSTRAINT_DEFERRABLE, %ld).\n",
	  (long) SQL_CT_CONSTRAINT_DEFERRABLE);
  fprintf(fd,
	  "-define(SQL_CT_CONSTRAINT_NON_DEFERRABLE, %ld).\n",
	  (long) SQL_CT_CONSTRAINT_NON_DEFERRABLE);

  fprintf(fd,
	  "-define(SQL_CREATE_TRANSLATION, %ld).\n",
	  (long) SQL_CREATE_TRANSLATION);
  fprintf(fd,
	  "-define(SQL_CTR_CREATE_TRANSLATION, %ld).\n",
	  (long) SQL_CTR_CREATE_TRANSLATION);

  fprintf(fd, "-define(SQL_CREATE_VIEW, %ld).\n", (long) SQL_CREATE_VIEW);
  fprintf(fd, "-define(SQL_CV_CREATE_VIEW, %ld).\n", (long) SQL_CV_CREATE_VIEW);
  fprintf(fd, "-define(SQL_CV_CHECK_OPTION, %ld).\n", (long) SQL_CV_CHECK_OPTION);
  fprintf(fd, "-define(SQL_CV_CASCADED, %ld).\n", (long) SQL_CV_CASCADED);
  fprintf(fd, "-define(SQL_CV_LOCAL, %ld).\n", (long) SQL_CV_LOCAL);

  fprintf(fd,
	  "-define(SQL_CURSOR_COMMIT_BEHAVIOR, %ld).\n",
	  (long) SQL_CURSOR_COMMIT_BEHAVIOR);
  fprintf(fd,
	  "-define(SQL_CURSOR_ROLLBACK_BEHAVIOR, %ld).\n",
	  (long) SQL_CURSOR_ROLLBACK_BEHAVIOR);
  fprintf(fd, "-define(SQL_CB_DELETE, %ld).\n", (long) SQL_CB_DELETE);
  fprintf(fd, "-define(SQL_CB_CLOSE, %ld).\n", (long) SQL_CB_CLOSE);
  fprintf(fd, "-define(SQL_CB_PRESERVE, %ld).\n", (long) SQL_CB_PRESERVE);

  fprintf(fd,
	  "-define(SQL_CURSOR_SENSITIVITY, %ld).\n",
	  (long) SQL_CURSOR_SENSITIVITY);
  fprintf(fd, "-define(SQL_INSENSITIVE, %ld).\n", (long) SQL_INSENSITIVE);
  fprintf(fd, "-define(SQL_UNSPECIFIED, %ld).\n", (long) SQL_UNSPECIFIED);
  fprintf(fd, "-define(SQL_SENSITIVE, %ld).\n", (long) SQL_SENSITIVE);

  fprintf(fd, "-define(SQL_DATA_SOURCE_NAME, %ld).\n", (long) SQL_DATA_SOURCE_NAME);

  fprintf(fd,
	  "-define(SQL_DATA_SOURCE_READ_ONLY, %ld).\n",
	  (long) SQL_DATA_SOURCE_READ_ONLY);

  fprintf(fd, "-define(SQL_DATABASE_NAME, %ld).\n", (long) SQL_DATABASE_NAME);

  fprintf(fd,
	  "-define(SQL_DATETIME_LITERALS, %ld).\n",
	  (long) SQL_DATETIME_LITERALS);
  fprintf(fd,
	  "-define(SQL_DL_SQL92_TIME, %ld).\n",
	  (long) SQL_DL_SQL92_TIME);
  fprintf(fd,
	  "-define(SQL_DL_SQL92_TIMESTAMP, %ld).\n",
	  (long) SQL_DL_SQL92_TIMESTAMP);
  fprintf(fd,
	  "-define(SQL_DL_SQL92_INTERVAL_YEAR, %ld).\n",
	  (long) SQL_DL_SQL92_INTERVAL_YEAR);
  fprintf(fd,
	  "-define(SQL_DL_SQL92_INTERVAL_MONTH, %ld).\n",
	  (long) SQL_DL_SQL92_INTERVAL_MONTH);
  fprintf(fd,
	  "-define(SQL_DL_SQL92_INTERVAL_DAY, %ld).\n",
	  (long) SQL_DL_SQL92_INTERVAL_DAY);
  fprintf(fd,
	  "-define(SQL_DL_SQL92_INTERVAL_HOUR, %ld).\n",
	  (long) SQL_DL_SQL92_INTERVAL_HOUR);
  fprintf(fd,
	  "-define(SQL_DL_SQL92_INTERVAL_MINUTE, %ld).\n",
	  (long) SQL_DL_SQL92_INTERVAL_MINUTE);
  fprintf(fd,
	  "-define(SQL_DL_SQL92_INTERVAL_SECOND, %ld).\n",
	  (long) SQL_DL_SQL92_INTERVAL_SECOND);
  fprintf(fd,
	  "-define(SQL_DL_SQL92_INTERVAL_YEAR_TO_MONTH, %ld).\n",
	  (long) SQL_DL_SQL92_INTERVAL_YEAR_TO_MONTH);
  fprintf(fd,
	  "-define(SQL_DL_SQL92_INTERVAL_DAY_TO_HOUR, %ld).\n",
	  (long) SQL_DL_SQL92_INTERVAL_DAY_TO_HOUR);
  fprintf(fd,
	  "-define(SQL_DL_SQL92_INTERVAL_DAY_TO_MINUTE, %ld).\n",
	  (long) SQL_DL_SQL92_INTERVAL_DAY_TO_MINUTE);
  fprintf(fd,
	  "-define(SQL_DL_SQL92_INTERVAL_DAY_TO_SECOND, %ld).\n",
	  (long) SQL_DL_SQL92_INTERVAL_DAY_TO_SECOND);
  fprintf(fd,
	  "-define(SQL_DL_SQL92_INTERVAL_HOUR_TO_MINUTE, %ld).\n",
	  (long) SQL_DL_SQL92_INTERVAL_HOUR_TO_MINUTE);
  fprintf(fd,
	  "-define(SQL_DL_SQL92_INTERVAL_HOUR_TO_SECOND, %ld).\n",
	  (long) SQL_DL_SQL92_INTERVAL_HOUR_TO_SECOND);
  fprintf(fd,
	  "-define(SQL_DL_SQL92_INTERVAL_MINUTE_TO_SECOND, %ld).\n",
	  (long) SQL_DL_SQL92_INTERVAL_MINUTE_TO_SECOND);

  fprintf(fd, "-define(SQL_DBMS_NAME, %ld).\n", (long) SQL_DBMS_NAME);

  fprintf(fd, "-define(SQL_DBMS_VER, %ld).\n", (long) SQL_DBMS_VER);

  fprintf(fd, "-define(SQL_DDL_INDEX, %ld).\n", (long) SQL_DDL_INDEX);
  fprintf(fd, "-define(SQL_DI_CREATE_INDEX, %ld).\n", (long) SQL_DI_CREATE_INDEX);
  fprintf(fd, "-define(SQL_DI_DROP_INDEX, %ld).\n", (long) SQL_DI_DROP_INDEX);

  fprintf(fd,
	  "-define(SQL_DEFAULT_TXN_ISOLATION, %ld).\n",
	  (long) SQL_DEFAULT_TXN_ISOLATION);
  fprintf(fd,
	  "-define(SQL_TXN_ISOLATION_OPTION, %ld).\n",
	  (long) SQL_TXN_ISOLATION_OPTION);
  fprintf(fd,
	  "-define(SQL_TXN_READ_UNCOMMITTED, %ld).\n",
	  (long) SQL_TXN_READ_UNCOMMITTED);
  fprintf(fd,
	  "-define(SQL_TXN_READ_COMMITTED, %ld).\n",
	  (long) SQL_TXN_READ_COMMITTED);
  fprintf(fd,
	  "-define(SQL_TXN_REPEATABLE_READ, %ld).\n",
	  (long) SQL_TXN_REPEATABLE_READ);
  fprintf(fd, "-define(SQL_TXN_SERIALIZABLE, %ld).\n", (long) SQL_TXN_SERIALIZABLE);

  fprintf(fd,
	  "-define(SQL_DESCRIBE_PARAMETER, %ld).\n",
	  (long) SQL_DESCRIBE_PARAMETER);

  fprintf(fd, "-define(SQL_DM_VER, %ld).\n", (long) SQL_DM_VER);

  fprintf(fd, "-define(SQL_DRIVER_NAME, %ld).\n", (long) SQL_DRIVER_NAME);

  fprintf(fd, "-define(SQL_DRIVER_ODBC_VER, %ld).\n", (long) SQL_DRIVER_ODBC_VER);

  fprintf(fd, "-define(SQL_DRIVER_VER, %ld).\n", (long) SQL_DRIVER_VER);

  fprintf(fd, "-define(SQL_DROP_ASSERTION, %ld).\n", (long) SQL_DROP_ASSERTION);
  fprintf(fd,
	  "-define(SQL_DA_DROP_ASSERTION, %ld).\n",
	  (long) SQL_DA_DROP_ASSERTION);

  fprintf(fd,
	  "-define(SQL_DROP_CHARACTER_SET, %ld).\n",
	  (long) SQL_DROP_CHARACTER_SET);
  fprintf(fd,
	  "-define(SQL_DCS_DROP_CHARACTER_SET, %ld).\n",
	  (long) SQL_DCS_DROP_CHARACTER_SET);

  fprintf(fd, "-define(SQL_DROP_COLLATION, %ld).\n", (long) SQL_DROP_COLLATION);
  fprintf(fd,
	  "-define(SQL_DC_DROP_COLLATION, %ld).\n",
	  (long) SQL_DC_DROP_COLLATION);

  fprintf(fd, "-define(SQL_DROP_DOMAIN, %ld).\n", (long) SQL_DROP_DOMAIN);
  fprintf(fd, "-define(SQL_DD_DROP_DOMAIN, %ld).\n", (long) SQL_DD_DROP_DOMAIN);
  fprintf(fd, "-define(SQL_DD_CASCADE, %ld).\n", (long) SQL_DD_CASCADE);
  fprintf(fd, "-define(SQL_DD_RESTRICT, %ld).\n", (long) SQL_DD_RESTRICT);

  fprintf(fd, "-define(SQL_DROP_SCHEMA, %ld).\n", (long) SQL_DROP_SCHEMA);
  fprintf(fd, "-define(SQL_DS_DROP_SCHEMA, %ld).\n", (long) SQL_DS_DROP_SCHEMA);
  fprintf(fd, "-define(SQL_DS_CASCADE, %ld).\n", (long) SQL_DS_CASCADE);
  fprintf(fd, "-define(SQL_DS_RESTRICT, %ld).\n", (long) SQL_DS_RESTRICT);

  fprintf(fd, "-define(SQL_DROP_TABLE, %ld).\n", (long) SQL_DROP_TABLE);
  fprintf(fd, "-define(SQL_DT_DROP_TABLE, %ld).\n", (long) SQL_DT_DROP_TABLE);
  fprintf(fd, "-define(SQL_DT_CASCADE, %ld).\n", (long) SQL_DT_CASCADE);
  fprintf(fd, "-define(SQL_DT_RESTRICT, %ld).\n", (long) SQL_DT_RESTRICT);

  fprintf(fd, "-define(SQL_DROP_TRANSLATION, %ld).\n", (long) SQL_DROP_TRANSLATION);
  fprintf(fd,
	  "-define(SQL_DTR_DROP_TRANSLATION, %ld).\n",
	  (long) SQL_DTR_DROP_TRANSLATION);

  fprintf(fd, "-define(SQL_DROP_VIEW, %ld).\n", (long) SQL_DROP_VIEW);
  fprintf(fd, "-define(SQL_DV_DROP_VIEW, %ld).\n", (long) SQL_DV_DROP_VIEW);
  fprintf(fd, "-define(SQL_DV_CASCADE, %ld).\n", (long) SQL_DV_CASCADE);
  fprintf(fd, "-define(SQL_DV_RESTRICT, %ld).\n", (long) SQL_DV_RESTRICT);

  fprintf(fd,
	  "-define(SQL_DYNAMIC_CURSOR_ATTRIBUTES1, %ld).\n",
	  (long) SQL_DYNAMIC_CURSOR_ATTRIBUTES1);
  fprintf(fd,
	  "-define(SQL_FORWARD_ONLY_CURSOR_ATTRIBUTES1, %ld).\n",
	  (long) SQL_FORWARD_ONLY_CURSOR_ATTRIBUTES1);
  fprintf(fd,
	  "-define(SQL_KEYSET_CURSOR_ATTRIBUTES1, %ld).\n",
	  (long) SQL_KEYSET_CURSOR_ATTRIBUTES1);
  fprintf(fd,
	  "-define(SQL_STATIC_CURSOR_ATTRIBUTES1, %ld).\n",
	  (long) SQL_STATIC_CURSOR_ATTRIBUTES1);
  fprintf(fd, "-define(SQL_CA1_NEXT, %ld).\n", (long) SQL_CA1_NEXT);
  fprintf(fd, "-define(SQL_CA1_ABSOLUTE, %ld).\n", (long) SQL_CA1_ABSOLUTE);
  fprintf(fd, "-define(SQL_CA1_RELATIVE, %ld).\n", (long) SQL_CA1_RELATIVE);
  fprintf(fd, "-define(SQL_CA1_BOOKMARK, %ld).\n", (long) SQL_CA1_BOOKMARK);
  fprintf(fd,
	  "-define(SQL_CA1_LOCK_EXCLUSIVE, %ld).\n",
	  (long) SQL_CA1_LOCK_EXCLUSIVE);
  fprintf(fd,
	  "-define(SQL_CA1_LOCK_NO_CHANGE, %ld).\n",
	  (long) SQL_CA1_LOCK_NO_CHANGE);
  fprintf(fd, "-define(SQL_CA1_LOCK_UNLOCK, %ld).\n", (long) SQL_CA1_LOCK_UNLOCK);
  fprintf(fd, "-define(SQL_CA1_POS_POSITION, %ld).\n", (long) SQL_CA1_POS_POSITION);
  fprintf(fd, "-define(SQL_CA1_POS_UPDATE, %ld).\n", (long) SQL_CA1_POS_UPDATE);
  fprintf(fd, "-define(SQL_CA1_POS_DELETE, %ld).\n", (long) SQL_CA1_POS_DELETE);
  fprintf(fd, "-define(SQL_CA1_POS_REFRESH, %ld).\n", (long) SQL_CA1_POS_REFRESH);
  fprintf(fd,
	  "-define(SQL_CA1_POSITIONED_UPDATE, %ld).\n", 
	  (long) SQL_CA1_POSITIONED_UPDATE);
  fprintf(fd,
	  "-define(SQL_CA1_POSITIONED_DELETE, %ld).\n",
	  (long) SQL_CA1_POSITIONED_DELETE);
  fprintf(fd,
	  "-define(SQL_CA1_SELECT_FOR_UPDATE, %ld).\n",
	  (long) SQL_CA1_SELECT_FOR_UPDATE);
  fprintf(fd, "-define(SQL_CA1_BULK_ADD, %ld).\n", (long) SQL_CA1_BULK_ADD);
  fprintf(fd,
	  "-define(SQL_CA1_BULK_UPDATE_BY_BOOKMARK, %ld).\n",
	  (long) SQL_CA1_BULK_UPDATE_BY_BOOKMARK);
  fprintf(fd,
	  "-define(SQL_CA1_BULK_DELETE_BY_BOOKMARK, %ld).\n",
	  (long) SQL_CA1_BULK_DELETE_BY_BOOKMARK);
  fprintf(fd,
	  "-define(SQL_CA1_BULK_FETCH_BY_BOOKMARK, %ld).\n",
	  (long) SQL_CA1_BULK_FETCH_BY_BOOKMARK);

  fprintf(fd,
	  "-define(SQL_DYNAMIC_CURSOR_ATTRIBUTES2, %ld).\n",
	  (long) SQL_DYNAMIC_CURSOR_ATTRIBUTES2);
  fprintf(fd,
	  "-define(SQL_FORWARD_ONLY_CURSOR_ATTRIBUTES2, %ld).\n",
	  (long) SQL_FORWARD_ONLY_CURSOR_ATTRIBUTES2);
  fprintf(fd,
	  "-define(SQL_KEYSET_CURSOR_ATTRIBUTES2, %ld).\n",
	  (long) SQL_KEYSET_CURSOR_ATTRIBUTES2);
  fprintf(fd,
	  "-define(SQL_STATIC_CURSOR_ATTRIBUTES2, %ld).\n",
	  (long) SQL_STATIC_CURSOR_ATTRIBUTES2);
  fprintf(fd,
	  "-define(SQL_CA2_READ_ONLY_CONCURRENCY, %ld).\n",
	  (long) SQL_CA2_READ_ONLY_CONCURRENCY);
  fprintf(fd,
	  "-define(SQL_CA2_LOCK_CONCURRENCY, %ld).\n",
	  (long) SQL_CA2_LOCK_CONCURRENCY);
  fprintf(fd,
	  "-define(SQL_CA2_OPT_ROWVER_CONCURRENCY, %ld).\n",
	  (long) SQL_CA2_OPT_ROWVER_CONCURRENCY);
  fprintf(fd,
	  "-define(SQL_CA2_OPT_VALUES_CONCURRENCY, %ld).\n",
	  (long) SQL_CA2_OPT_VALUES_CONCURRENCY);
  fprintf(fd,
	  "-define(SQL_CA2_SENSITIVITY_ADDITIONS, %ld).\n",
	  (long) SQL_CA2_SENSITIVITY_ADDITIONS);
  fprintf(fd,
	  "-define(SQL_CA2_SENSITIVITY_DELETIONS, %ld).\n",
	  (long) SQL_CA2_SENSITIVITY_DELETIONS);
  fprintf(fd,
	  "-define(SQL_CA2_SENSITIVITY_UPDATES, %ld).\n",
	  (long) SQL_CA2_SENSITIVITY_UPDATES);
  fprintf(fd,
	  "-define(SQL_CA2_MAX_ROWS_SELECT, %ld).\n",
	  (long) SQL_CA2_MAX_ROWS_SELECT);
  fprintf(fd,
	  "-define(SQL_CA2_MAX_ROWS_INSERT, %ld).\n",
	  (long) SQL_CA2_MAX_ROWS_INSERT);
  fprintf(fd,
	  "-define(SQL_CA2_MAX_ROWS_DELETE, %ld).\n",
	  (long) SQL_CA2_MAX_ROWS_DELETE);
  fprintf(fd,
	  "-define(SQL_CA2_MAX_ROWS_UPDATE, %ld).\n",
	  (long) SQL_CA2_MAX_ROWS_UPDATE);
  fprintf(fd,
	  "-define(SQL_CA2_MAX_ROWS_CATALOG, %ld).\n",
	  (long) SQL_CA2_MAX_ROWS_CATALOG);
  fprintf(fd,
	  "-define(SQL_CA2_MAX_ROWS_AFFECTS_ALL, %ld).\n",
	  (long) SQL_CA2_MAX_ROWS_AFFECTS_ALL);
  fprintf(fd, "-define(SQL_CA2_CRC_EXACT, %ld).\n", (long) SQL_CA2_CRC_EXACT);
  fprintf(fd,
	  "-define(SQL_CA2_CRC_APPROXIMATE, %ld).\n",
	  (long) SQL_CA2_CRC_APPROXIMATE);
  fprintf(fd,
	  "-define(SQL_CA2_SIMULATE_NON_UNIQUE, %ld).\n",
	  (long) SQL_CA2_SIMULATE_NON_UNIQUE);
  fprintf(fd,
	  "-define(SQL_CA2_SIMULATE_TRY_UNIQUE, %ld).\n",
	  (long) SQL_CA2_SIMULATE_TRY_UNIQUE);
  fprintf(fd,
	  "-define(SQL_CA2_SIMULATE_UNIQUE, %ld).\n",
	  (long) SQL_CA2_SIMULATE_UNIQUE);

  fprintf(fd,
	  "-define(SQL_EXPRESSIONS_IN_ORDERBY, %ld).\n",
	  (long) SQL_EXPRESSIONS_IN_ORDERBY);

  fprintf(fd, "-define(SQL_FILE_USAGE, %ld).\n", (long) SQL_FILE_USAGE);
  fprintf(fd,
	  "-define(SQL_FILE_NOT_SUPPORTED, %ld).\n",
	  (long) SQL_FILE_NOT_SUPPORTED);
  fprintf(fd, "-define(SQL_FILE_TABLE, %ld).\n", (long) SQL_FILE_TABLE);
  fprintf(fd, "-define(SQL_FILE_CATALOG, %ld).\n", (long) SQL_FILE_CATALOG);

  fprintf(fd,
	  "-define(SQL_GETDATA_EXTENSIONS, %ld).\n",
	  (long) SQL_GETDATA_EXTENSIONS);
  fprintf(fd, "-define(SQL_GD_ANY_COLUMN, %ld).\n", (long) SQL_GD_ANY_COLUMN);
  fprintf(fd, "-define(SQL_GD_ANY_ORDER, %ld).\n", (long) SQL_GD_ANY_ORDER);
  fprintf(fd, "-define(SQL_GD_BLOCK, %ld).\n", (long) SQL_GD_BLOCK);
  fprintf(fd, "-define(SQL_GD_BOUND, %ld).\n", (long) SQL_GD_BOUND);

  fprintf(fd, "-define(SQL_GROUP_BY, %ld).\n", (long) SQL_GROUP_BY);
  fprintf(fd, "-define(SQL_GB_COLLATE, %ld).\n", (long) SQL_GB_COLLATE);
  fprintf(fd, "-define(SQL_GB_NOT_SUPPORTED, %ld).\n", (long) SQL_GB_NOT_SUPPORTED);
  fprintf(fd,
	  "-define(SQL_GB_GROUP_BY_EQUALS_SELECT, %ld).\n",
	  (long) SQL_GB_GROUP_BY_EQUALS_SELECT);
  fprintf(fd,
	  "-define(SQL_GB_GROUP_BY_CONTAINS_SELECT, %ld).\n",
	  (long) SQL_GB_GROUP_BY_CONTAINS_SELECT);
  fprintf(fd, "-define(SQL_GB_NO_RELATION, %ld).\n", (long) SQL_GB_NO_RELATION);

  fprintf(fd, "-define(SQL_IDENTIFIER_CASE, %ld).\n", (long) SQL_IDENTIFIER_CASE);
  fprintf(fd,
	  "-define(SQL_QUOTED_IDENTIFIER_CASE, %ld).\n",
	  (long) SQL_QUOTED_IDENTIFIER_CASE);
  fprintf(fd, "-define(SQL_IC_UPPER, %ld).\n", (long) SQL_IC_UPPER);
  fprintf(fd, "-define(SQL_IC_LOWER, %ld).\n", (long) SQL_IC_LOWER);
  fprintf(fd, "-define(SQL_IC_SENSITIVE, %ld).\n", (long) SQL_IC_SENSITIVE);
  fprintf(fd, "-define(SQL_IC_MIXED, %ld).\n", (long) SQL_IC_MIXED);

  fprintf(fd,
	  "-define(SQL_IDENTIFIER_QUOTE_CHAR, %ld).\n",
	  (long) SQL_IDENTIFIER_QUOTE_CHAR);

  fprintf(fd, "-define(SQL_INDEX_KEYWORDS, %ld).\n", (long) SQL_INDEX_KEYWORDS);
  fprintf(fd, "-define(SQL_IK_NONE, %ld).\n", (long) SQL_IK_NONE);
  fprintf(fd, "-define(SQL_IK_ASC, %ld).\n", (long) SQL_IK_ASC);
  fprintf(fd, "-define(SQL_IK_DESC, %ld).\n", (long) SQL_IK_DESC);
  fprintf(fd, "-define(SQL_IK_ALL, %ld).\n", (long) SQL_IK_ALL);

  fprintf(fd,
	  "-define(SQL_INFO_SCHEMA_VIEWS, %ld).\n",
	  (long) SQL_INFO_SCHEMA_VIEWS);
  fprintf(fd, "-define(SQL_ISV_ASSERTIONS, %ld).\n", (long) SQL_ISV_ASSERTIONS);
  fprintf(fd,
	  "-define(SQL_ISV_CHARACTER_SETS, %ld).\n",
	  (long) SQL_ISV_CHARACTER_SETS);
  fprintf(fd,
	  "-define(SQL_ISV_CHECK_CONSTRAINTS, %ld).\n",
	  (long) SQL_ISV_CHECK_CONSTRAINTS);
  fprintf(fd, "-define(SQL_ISV_COLLATIONS, %ld).\n", (long) SQL_ISV_COLLATIONS);
  fprintf(fd,
	  "-define(SQL_ISV_COLUMN_DOMAIN_USAGE, %ld).\n",
	  (long) SQL_ISV_COLUMN_DOMAIN_USAGE);
  fprintf(fd,
	  "-define(SQL_ISV_COLUMN_PRIVILEGES, %ld).\n",
	  (long) SQL_ISV_COLUMN_PRIVILEGES);
  fprintf(fd, "-define(SQL_ISV_COLUMNS, %ld).\n", (long) SQL_ISV_COLUMNS);
  fprintf(fd,
	  "-define(SQL_ISV_CONSTRAINT_COLUMN_USAGE, %ld).\n",
	  (long) SQL_ISV_CONSTRAINT_COLUMN_USAGE);
  fprintf(fd,
	  "-define(SQL_ISV_CONSTRAINT_TABLE_USAGE, %ld).\n",
	  (long) SQL_ISV_CONSTRAINT_TABLE_USAGE);
  fprintf(fd,
	  "-define(SQL_ISV_DOMAIN_CONSTRAINTS, %ld).\n",
	  (long) SQL_ISV_DOMAIN_CONSTRAINTS);
  fprintf(fd, "-define(SQL_ISV_DOMAINS, %ld).\n", (long) SQL_ISV_DOMAINS);
  fprintf(fd,
	  "-define(SQL_ISV_KEY_COLUMN_USAGE, %ld).\n",
	  (long) SQL_ISV_KEY_COLUMN_USAGE);
  fprintf(fd,
	  "-define(SQL_ISV_REFERENTIAL_CONSTRAINTS, %ld).\n",
	  (long) SQL_ISV_REFERENTIAL_CONSTRAINTS);
  fprintf(fd, "-define(SQL_ISV_SCHEMATA, %ld).\n", (long) SQL_ISV_SCHEMATA);
  fprintf(fd,
	  "-define(SQL_ISV_SQL_LANGUAGES, %ld).\n",
	  (long) SQL_ISV_SQL_LANGUAGES);
  fprintf(fd,
	  "-define(SQL_ISV_TABLE_CONSTRAINTS, %ld).\n",
	  (long) SQL_ISV_TABLE_CONSTRAINTS);
  fprintf(fd,
	  "-define(SQL_ISV_TABLE_PRIVILEGES, %ld).\n",
	  (long) SQL_ISV_TABLE_PRIVILEGES);
  fprintf(fd, "-define(SQL_ISV_TABLES, %ld).\n", (long) SQL_ISV_TABLES);
  fprintf(fd, "-define(SQL_ISV_TRANSLATIONS, %ld).\n", (long) SQL_ISV_TRANSLATIONS);
  fprintf(fd,
	  "-define(SQL_ISV_USAGE_PRIVILEGES, %ld).\n",
	  (long) SQL_ISV_USAGE_PRIVILEGES);
  fprintf(fd,
	  "-define(SQL_ISV_VIEW_COLUMN_USAGE, %ld).\n",
	  (long) SQL_ISV_VIEW_COLUMN_USAGE);
  fprintf(fd,
	  "-define(SQL_ISV_VIEW_TABLE_USAGE, %ld).\n",
	  (long) SQL_ISV_VIEW_TABLE_USAGE);
  fprintf(fd, "-define(SQL_ISV_VIEWS, %ld).\n", (long) SQL_ISV_VIEWS);

  fprintf(fd, "-define(SQL_INSERT_STATEMENT, %ld).\n", (long) SQL_INSERT_STATEMENT);
  fprintf(fd,
	  "-define(SQL_IS_INSERT_LITERALS, %ld).\n",
	  (long) SQL_IS_INSERT_LITERALS);
  fprintf(fd,
	  "-define(SQL_IS_INSERT_SEARCHED, %ld).\n",
	  (long) SQL_IS_INSERT_SEARCHED);
  fprintf(fd, "-define(SQL_IS_SELECT_INTO, %ld).\n", (long) SQL_IS_SELECT_INTO);

  fprintf(fd, "-define(SQL_INTEGRITY, %ld).\n", (long) SQL_INTEGRITY);

  fprintf(fd, "-define(SQL_KEYWORDS, %ld).\n", (long) SQL_KEYWORDS);

  fprintf(fd,
	  "-define(SQL_LIKE_ESCAPE_CLAUSE, %ld).\n",
	  (long) SQL_LIKE_ESCAPE_CLAUSE);

  fprintf(fd,
	  "-define(SQL_MAX_ASYNC_CONCURRENT_STATEMENTS, %ld).\n",
	  (long) SQL_MAX_ASYNC_CONCURRENT_STATEMENTS);

  fprintf(fd,
	  "-define(SQL_MAX_BINARY_LITERAL_LEN, %ld).\n",
	  (long) SQL_MAX_BINARY_LITERAL_LEN);

  fprintf(fd,
	  "-define(SQL_MAX_CATALOG_NAME_LEN, %ld).\n",
	  (long) SQL_MAX_CATALOG_NAME_LEN);

  fprintf(fd,
	  "-define(SQL_MAX_CHAR_LITERAL_LEN, %ld).\n",
	  (long) SQL_MAX_CHAR_LITERAL_LEN);

  fprintf(fd,
	  "-define(SQL_MAX_COLUMN_NAME_LEN, %ld).\n",
	  (long) SQL_MAX_COLUMN_NAME_LEN);

  fprintf(fd,
	  "-define(SQL_MAX_COLUMNS_IN_GROUP_BY, %ld).\n",
	  (long) SQL_MAX_COLUMNS_IN_GROUP_BY);

  fprintf(fd,
	  "-define(SQL_MAX_COLUMNS_IN_INDEX, %ld).\n",
	  (long) SQL_MAX_COLUMNS_IN_INDEX);

  fprintf(fd,
	  "-define(SQL_MAX_COLUMNS_IN_ORDER_BY, %ld).\n",
	  (long) SQL_MAX_COLUMNS_IN_ORDER_BY);

  fprintf(fd,
	  "-define(SQL_MAX_COLUMNS_IN_SELECT, %ld).\n",
	  (long) SQL_MAX_COLUMNS_IN_SELECT);

  fprintf(fd,
	  "-define(SQL_MAX_COLUMNS_IN_TABLE, %ld).\n",
	  (long) SQL_MAX_COLUMNS_IN_TABLE);

  fprintf(fd,
	  "-define(SQL_MAX_CONCURRENT_ACTIVITIES, %ld).\n",
	  (long) SQL_MAX_CONCURRENT_ACTIVITIES);

  fprintf(fd,
	  "-define(SQL_MAX_CURSOR_NAME_LEN, %ld).\n",
	  (long) SQL_MAX_CURSOR_NAME_LEN);

  fprintf(fd,
	  "-define(SQL_MAX_DRIVER_CONNECTIONS, %ld).\n",
	  (long) SQL_MAX_DRIVER_CONNECTIONS);

  fprintf(fd,
	  "-define(SQL_MAX_IDENTIFIER_LEN, %ld).\n",
	  (long) SQL_MAX_IDENTIFIER_LEN);

  fprintf(fd, "-define(SQL_MAX_INDEX_SIZE, %ld).\n", (long) SQL_MAX_INDEX_SIZE);
  fprintf(fd,
	  "-define(SQL_MAX_PROCEDURE_NAME_LEN, %ld).\n",
	  (long) SQL_MAX_PROCEDURE_NAME_LEN);

  fprintf(fd, "-define(SQL_MAX_ROW_SIZE, %ld).\n", (long) SQL_MAX_ROW_SIZE);
  fprintf(fd,
	  "-define(SQL_MAX_ROW_SIZE_INCLUDES_LONG, %ld).\n",
	  (long) SQL_MAX_ROW_SIZE_INCLUDES_LONG);

  fprintf(fd,
	  "-define(SQL_MAX_SCHEMA_NAME_LEN, %ld).\n",
	  (long) SQL_MAX_SCHEMA_NAME_LEN);

  fprintf(fd,
	  "-define(SQL_MAX_STATEMENT_LEN, %ld).\n",
	  (long) SQL_MAX_STATEMENT_LEN);

  fprintf(fd,
	  "-define(SQL_MAX_TABLE_NAME_LEN, %ld).\n",
	  (long) SQL_MAX_TABLE_NAME_LEN);

  fprintf(fd,
	  "-define(SQL_MAX_TABLES_IN_SELECT, %ld).\n",
	  (long) SQL_MAX_TABLES_IN_SELECT);

  fprintf(fd,
	  "-define(SQL_MAX_USER_NAME_LEN, %ld).\n",
	  (long) SQL_MAX_USER_NAME_LEN);

  fprintf(fd, "-define(SQL_MULT_RESULT_SETS, %ld).\n", (long) SQL_MULT_RESULT_SETS);

  fprintf(fd,
	  "-define(SQL_MULTIPLE_ACTIVE_TXN, %ld).\n",
	  (long) SQL_MULTIPLE_ACTIVE_TXN);

  fprintf(fd,
	  "-define(SQL_NEED_LONG_DATA_LEN, %ld).\n",
	  (long) SQL_NEED_LONG_DATA_LEN);

  fprintf(fd,
	  "-define(SQL_NON_NULLABLE_COLUMNS, %ld).\n",
	  (long) SQL_NON_NULLABLE_COLUMNS);
  fprintf(fd, "-define(SQL_NNC_NULL, %ld).\n", (long) SQL_NNC_NULL);
  fprintf(fd, "-define(SQL_NNC_NON_NULL, %ld).\n", (long) SQL_NNC_NON_NULL);

  fprintf(fd, "-define(SQL_NULL_COLLATION, %ld).\n", (long) SQL_NULL_COLLATION);
  fprintf(fd, "-define(SQL_NC_END, %ld).\n", (long) SQL_NC_END);
  fprintf(fd, "-define(SQL_NC_HIGH, %ld).\n", (long) SQL_NC_HIGH);
  fprintf(fd, "-define(SQL_NC_LOW, %ld).\n", (long) SQL_NC_LOW);
  fprintf(fd, "-define(SQL_NC_START, %ld).\n", (long) SQL_NC_START);

  fprintf(fd,
	  "-define(SQL_NUMERIC_FUNCTIONS, %ld).\n",
	  (long) SQL_NUMERIC_FUNCTIONS);
  fprintf(fd, "-define(SQL_FN_NUM_ABS, %ld).\n", (long) SQL_FN_NUM_ABS);
  fprintf(fd, "-define(SQL_FN_NUM_ACOS, %ld).\n", (long) SQL_FN_NUM_ACOS);
  fprintf(fd, "-define(SQL_FN_NUM_ASIN, %ld).\n", (long) SQL_FN_NUM_ASIN);
  fprintf(fd, "-define(SQL_FN_NUM_ATAN, %ld).\n", (long) SQL_FN_NUM_ATAN);
  fprintf(fd, "-define(SQL_FN_NUM_ATAN2, %ld).\n", (long) SQL_FN_NUM_ATAN2);
  fprintf(fd, "-define(SQL_FN_NUM_CEILING, %ld).\n", (long) SQL_FN_NUM_CEILING);
  fprintf(fd, "-define(SQL_FN_NUM_COS, %ld).\n", (long) SQL_FN_NUM_COS);
  fprintf(fd, "-define(SQL_FN_NUM_COT, %ld).\n", (long) SQL_FN_NUM_COT);
  fprintf(fd, "-define(SQL_FN_NUM_DEGREES, %ld).\n", (long) SQL_FN_NUM_DEGREES);
  fprintf(fd, "-define(SQL_FN_NUM_EXP, %ld).\n", (long) SQL_FN_NUM_EXP);
  fprintf(fd, "-define(SQL_FN_NUM_FLOOR, %ld).\n", (long) SQL_FN_NUM_FLOOR);
  fprintf(fd, "-define(SQL_FN_NUM_LOG, %ld).\n", (long) SQL_FN_NUM_LOG);
  fprintf(fd, "-define(SQL_FN_NUM_LOG10, %ld).\n", (long) SQL_FN_NUM_LOG10);
  fprintf(fd, "-define(SQL_FN_NUM_MOD, %ld).\n", (long) SQL_FN_NUM_MOD);
  fprintf(fd, "-define(SQL_FN_NUM_PI, %ld).\n", (long) SQL_FN_NUM_PI);
  fprintf(fd, "-define(SQL_FN_NUM_POWER, %ld).\n", (long) SQL_FN_NUM_POWER);
  fprintf(fd, "-define(SQL_FN_NUM_RADIANS, %ld).\n", (long) SQL_FN_NUM_RADIANS);
  fprintf(fd, "-define(SQL_FN_NUM_RAND, %ld).\n", (long) SQL_FN_NUM_RAND);
  fprintf(fd, "-define(SQL_FN_NUM_ROUND, %ld).\n", (long) SQL_FN_NUM_ROUND);
  fprintf(fd, "-define(SQL_FN_NUM_SIGN, %ld).\n", (long) SQL_FN_NUM_SIGN);
  fprintf(fd, "-define(SQL_FN_NUM_SIN, %ld).\n", (long) SQL_FN_NUM_SIN);
  fprintf(fd, "-define(SQL_FN_NUM_SQRT, %ld).\n", (long) SQL_FN_NUM_SQRT);
  fprintf(fd, "-define(SQL_FN_NUM_TAN, %ld).\n", (long) SQL_FN_NUM_TAN);
  fprintf(fd, "-define(SQL_FN_NUM_TRUNCATE, %ld).\n", (long) SQL_FN_NUM_TRUNCATE);

  fprintf(fd,
	  "-define(SQL_ODBC_INTERFACE_CONFORMANCE, %ld).\n",
	  (long) SQL_ODBC_INTERFACE_CONFORMANCE);
  fprintf(fd, "-define(SQL_OIC_CORE, %ld).\n", (long) SQL_OIC_CORE);
  fprintf(fd, "-define(SQL_OIC_LEVEL1, %ld).\n", (long) SQL_OIC_LEVEL1);
  fprintf(fd, "-define(SQL_OIC_LEVEL2, %ld).\n", (long) SQL_OIC_LEVEL2);

  fprintf(fd, "-define(SQL_ODBC_VER, %ld).\n", (long) SQL_ODBC_VER);

  fprintf(fd, "-define(SQL_OJ_CAPABILITIES, %ld).\n", (long) SQL_OJ_CAPABILITIES);
  fprintf(fd, "-define(SQL_OJ_LEFT, %ld).\n", (long) SQL_OJ_LEFT);
  fprintf(fd, "-define(SQL_OJ_RIGHT, %ld).\n", (long) SQL_OJ_RIGHT);
  fprintf(fd, "-define(SQL_OJ_FULL, %ld).\n", (long) SQL_OJ_FULL);
  fprintf(fd, "-define(SQL_OJ_NESTED, %ld).\n", (long) SQL_OJ_NESTED);
  fprintf(fd, "-define(SQL_OJ_NOT_ORDERED, %ld).\n", (long) SQL_OJ_NOT_ORDERED);
  fprintf(fd, "-define(SQL_OJ_INNER, %ld).\n", (long) SQL_OJ_INNER);
  fprintf(fd,
	  "-define(SQL_OJ_ALL_COMPARISON_OPS, %ld).\n",
	  (long) SQL_OJ_ALL_COMPARISON_OPS);

  fprintf(fd,
	  "-define(SQL_ORDER_BY_COLUMNS_IN_SELECT, %ld).\n",
	  (long) SQL_ORDER_BY_COLUMNS_IN_SELECT);

  fprintf(fd, "-define(SQL_OUTER_JOINS, %ld).\n", (long) SQL_OUTER_JOINS);

  fprintf(fd,
	  "-define(SQL_PARAM_ARRAY_ROW_COUNTS, %ld).\n",
	  (long) SQL_PARAM_ARRAY_ROW_COUNTS);
  fprintf(fd, "-define(SQL_PARC_BATCH, %ld).\n", (long) SQL_PARC_BATCH);
  fprintf(fd, "-define(SQL_PARC_NO_BATCH, %ld).\n", (long) SQL_PARC_NO_BATCH);

  fprintf(fd,
	  "-define(SQL_PARAM_ARRAY_SELECTS, %ld).\n",
	  (long) SQL_PARAM_ARRAY_SELECTS);
  fprintf(fd, "-define(SQL_PAS_BATCH, %ld).\n", (long) SQL_PAS_BATCH);
  fprintf(fd, "-define(SQL_PAS_NO_BATCH, %ld).\n", (long) SQL_PAS_NO_BATCH);
  fprintf(fd, "-define(SQL_PAS_NO_SELECT, %ld).\n", (long) SQL_PAS_NO_SELECT);

  fprintf(fd, "-define(SQL_PROCEDURE_TERM, %ld).\n", (long) SQL_PROCEDURE_TERM);

  fprintf(fd, "-define(SQL_PROCEDURES, %ld).\n", (long) SQL_PROCEDURES);

  fprintf(fd, "-define(SQL_ROW_UPDATES, %ld).\n", (long) SQL_ROW_UPDATES);

  fprintf(fd, "-define(SQL_SCHEMA_TERM, %ld).\n", (long) SQL_SCHEMA_TERM);

  fprintf(fd, "-define(SQL_SCHEMA_USAGE, %ld).\n", (long) SQL_SCHEMA_USAGE);
  fprintf(fd,
	  "-define(SQL_SU_DML_STATEMENTS, %ld).\n",
	  (long) SQL_SU_DML_STATEMENTS);
  fprintf(fd,
	  "-define(SQL_SU_PROCEDURE_INVOCATION, %ld).\n",
	  (long) SQL_SU_PROCEDURE_INVOCATION);
  fprintf(fd,
	  "-define(SQL_SU_TABLE_DEFINITION, %ld).\n",
	  (long) SQL_SU_TABLE_DEFINITION);
  fprintf(fd,
	  "-define(SQL_SU_INDEX_DEFINITION, %ld).\n",
	  (long) SQL_SU_INDEX_DEFINITION);
  fprintf(fd,
	  "-define(SQL_SU_PRIVILEGE_DEFINITION, %ld).\n",
	  (long) SQL_SU_PRIVILEGE_DEFINITION);

  fprintf(fd, "-define(SQL_SCROLL_OPTIONS, %ld).\n", (long) SQL_SCROLL_OPTIONS);
  fprintf(fd, "-define(SQL_SO_FORWARD_ONLY, %ld).\n", (long) SQL_SO_FORWARD_ONLY);
  fprintf(fd, "-define(SQL_SO_STATIC, %ld).\n", (long) SQL_SO_STATIC);
  fprintf(fd, "-define(SQL_SO_KEYSET_DRIVEN, %ld).\n", (long) SQL_SO_KEYSET_DRIVEN);
  fprintf(fd, "-define(SQL_SO_DYNAMIC, %ld).\n", (long) SQL_SO_DYNAMIC);
  fprintf(fd, "-define(SQL_SO_MIXED, %ld).\n", (long) SQL_SO_MIXED);

  fprintf(fd,
	  "-define(SQL_SEARCH_PATTERN_ESCAPE, %ld).\n",
	  (long) SQL_SEARCH_PATTERN_ESCAPE);

  fprintf(fd, "-define(SQL_SERVER_NAME, %ld).\n", (long) SQL_SERVER_NAME);

  fprintf(fd,
	  "-define(SQL_SPECIAL_CHARACTERS, %ld).\n",
	  (long) SQL_SPECIAL_CHARACTERS);

  fprintf(fd, "-define(SQL_SQL_CONFORMANCE, %ld).\n", (long) SQL_SQL_CONFORMANCE);
  fprintf(fd, "-define(SQL_SC_SQL92_ENTRY, %ld).\n", (long) SQL_SC_SQL92_ENTRY);
  fprintf(fd,
	  "-define(SQL_SC_FIPS127_2_TRANSITIONAL, %ld).\n",
	  (long) SQL_SC_FIPS127_2_TRANSITIONAL);
  fprintf(fd, "-define(SQL_SC_SQL92_FULL, %ld).\n", (long) SQL_SC_SQL92_FULL);
  fprintf(fd,
	  "-define(SQL_SC_SQL92_INTERMEDIATE, %ld).\n",
	  (long) SQL_SC_SQL92_INTERMEDIATE);

  fprintf(fd,
	  "-define(SQL_SQL92_DATETIME_FUNCTIONS, %ld).\n",
	  (long) SQL_SQL92_DATETIME_FUNCTIONS);
  fprintf(fd, "-define(SQL_SDF_CURRENT_DATE, %ld).\n", (long) SQL_SDF_CURRENT_DATE);
  fprintf(fd, "-define(SQL_SDF_CURRENT_TIME, %ld).\n", (long) SQL_SDF_CURRENT_TIME);
  fprintf(fd,
	  "-define(SQL_SDF_CURRENT_TIMESTAMP, %ld).\n",
	  (long) SQL_SDF_CURRENT_TIMESTAMP);

  fprintf(fd,
	  "-define(SQL_SQL92_FOREIGN_KEY_DELETE_RULE, %ld).\n",
	  (long) SQL_SQL92_FOREIGN_KEY_DELETE_RULE);
  fprintf(fd, "-define(SQL_SFKD_CASCADE, %ld).\n", (long) SQL_SFKD_CASCADE);
  fprintf(fd, "-define(SQL_SFKD_NO_ACTION, %ld).\n", (long) SQL_SFKD_NO_ACTION);
  fprintf(fd, "-define(SQL_SFKD_SET_DEFAULT, %ld).\n", (long) SQL_SFKD_SET_DEFAULT);
  fprintf(fd, "-define(SQL_SFKD_SET_NULL, %ld).\n", (long) SQL_SFKD_SET_NULL);

  fprintf(fd,
	  "-define(SQL_SQL92_FOREIGN_KEY_UPDATE_RULE, %ld).\n",
	  (long) SQL_SQL92_FOREIGN_KEY_UPDATE_RULE);
  fprintf(fd, "-define(SQL_SFKU_CASCADE, %ld).\n", (long) SQL_SFKU_CASCADE);
  fprintf(fd, "-define(SQL_SFKU_NO_ACTION, %ld).\n", (long) SQL_SFKU_NO_ACTION);
  fprintf(fd, "-define(SQL_SFKU_SET_DEFAULT, %ld).\n", (long) SQL_SFKU_SET_DEFAULT);
  fprintf(fd, "-define(SQL_SFKU_SET_NULL, %ld).\n", (long) SQL_SFKU_SET_NULL);

  fprintf(fd, "-define(SQL_SQL92_GRANT, %ld).\n", (long) SQL_SQL92_GRANT);
  fprintf(fd, "-define(SQL_SG_DELETE_TABLE, %ld).\n", (long) SQL_SG_DELETE_TABLE);
  fprintf(fd, "-define(SQL_SG_INSERT_COLUMN, %ld).\n", (long) SQL_SG_INSERT_COLUMN);
  fprintf(fd, "-define(SQL_SG_INSERT_TABLE, %ld).\n", (long) SQL_SG_INSERT_TABLE);
  fprintf(fd,
	  "-define(SQL_SG_REFERENCES_TABLE, %ld).\n",
	  (long) SQL_SG_REFERENCES_TABLE);
  fprintf(fd,
	  "-define(SQL_SG_REFERENCES_COLUMN, %ld).\n",
	  (long) SQL_SG_REFERENCES_COLUMN);
  fprintf(fd, "-define(SQL_SG_SELECT_TABLE, %ld).\n", (long) SQL_SG_SELECT_TABLE);
  fprintf(fd, "-define(SQL_SG_UPDATE_COLUMN, %ld).\n", (long) SQL_SG_UPDATE_COLUMN);
  fprintf(fd, "-define(SQL_SG_UPDATE_TABLE, %ld).\n", (long) SQL_SG_UPDATE_TABLE);
  fprintf(fd,
	  "-define(SQL_SG_USAGE_ON_DOMAIN, %ld).\n",
	  (long) SQL_SG_USAGE_ON_DOMAIN);
  fprintf(fd,
	  "-define(SQL_SG_USAGE_ON_CHARACTER_SET, %ld).\n",
	  (long) SQL_SG_USAGE_ON_CHARACTER_SET);
  fprintf(fd,
	  "-define(SQL_SG_USAGE_ON_COLLATION, %ld).\n",
	  (long) SQL_SG_USAGE_ON_COLLATION);
  fprintf(fd,
	  "-define(SQL_SG_USAGE_ON_TRANSLATION, %ld).\n",
	  (long) SQL_SG_USAGE_ON_TRANSLATION);
  fprintf(fd,
	  "-define(SQL_SG_WITH_GRANT_OPTION, %ld).\n",
	  (long) SQL_SG_WITH_GRANT_OPTION);

  fprintf(fd,
	  "-define(SQL_SQL92_NUMERIC_VALUE_FUNCTIONS, %ld).\n",
	  (long) SQL_SQL92_NUMERIC_VALUE_FUNCTIONS);
  fprintf(fd, "-define(SQL_SNVF_BIT_LENGTH, %ld).\n", (long) SQL_SNVF_BIT_LENGTH);
  fprintf(fd, "-define(SQL_SNVF_CHAR_LENGTH, %ld).\n", (long) SQL_SNVF_CHAR_LENGTH);
  fprintf(fd,
	  "-define(SQL_SNVF_CHARACTER_LENGTH, %ld).\n",
	  (long) SQL_SNVF_CHARACTER_LENGTH);
  fprintf(fd, "-define(SQL_SNVF_EXTRACT, %ld).\n", (long) SQL_SNVF_EXTRACT);
  fprintf(fd,
	  "-define(SQL_SNVF_OCTET_LENGTH, %ld).\n",
	  (long) SQL_SNVF_OCTET_LENGTH);
  fprintf(fd, "-define(SQL_SNVF_POSITION, %ld).\n", (long) SQL_SNVF_POSITION);

  fprintf(fd, "-define(SQL_SQL92_PREDICATES, %ld).\n", (long) SQL_SQL92_PREDICATES);
  fprintf(fd, "-define(SQL_SP_BETWEEN, %ld).\n", (long) SQL_SP_BETWEEN);
  fprintf(fd, "-define(SQL_SP_COMPARISON, %ld).\n", (long) SQL_SP_COMPARISON);
  fprintf(fd, "-define(SQL_SP_EXISTS, %ld).\n", (long) SQL_SP_EXISTS);
  fprintf(fd, "-define(SQL_SP_IN, %ld).\n", (long) SQL_SP_IN);
  fprintf(fd, "-define(SQL_SP_ISNOTNULL, %ld).\n", (long) SQL_SP_ISNOTNULL);
  fprintf(fd, "-define(SQL_SP_ISNULL, %ld).\n", (long) SQL_SP_ISNULL);
  fprintf(fd, "-define(SQL_SP_LIKE, %ld).\n", (long) SQL_SP_LIKE);
  fprintf(fd, "-define(SQL_SP_MATCH_FULL, %ld).\n", (long) SQL_SP_MATCH_FULL);
  fprintf(fd, "-define(SQL_SP_MATCH_PARTIAL, %ld).\n", (long) SQL_SP_MATCH_PARTIAL);
  fprintf(fd,
	  "-define(SQL_SP_MATCH_UNIQUE_FULL, %ld).\n",
	  (long) SQL_SP_MATCH_UNIQUE_FULL);
  fprintf(fd,
	  "-define(SQL_SP_MATCH_UNIQUE_PARTIAL, %ld).\n",
	  (long) SQL_SP_MATCH_UNIQUE_PARTIAL);
  fprintf(fd, "-define(SQL_SP_OVERLAPS, %ld).\n", (long) SQL_SP_OVERLAPS);
  fprintf(fd,
	  "-define(SQL_SP_QUANTIFIED_COMPARISON, %ld).\n",
	  (long) SQL_SP_QUANTIFIED_COMPARISON);
  fprintf(fd, "-define(SQL_SP_UNIQUE, %ld).\n", (long) SQL_SP_UNIQUE);

  fprintf(fd,
	  "-define(SQL_SQL92_RELATIONAL_JOIN_OPERATORS, %ld).\n",
	  (long) SQL_SQL92_RELATIONAL_JOIN_OPERATORS);
  fprintf(fd,
	  "-define(SQL_SRJO_CORRESPONDING_CLAUSE, %ld).\n",
	  (long) SQL_SRJO_CORRESPONDING_CLAUSE);
  fprintf(fd, "-define(SQL_SRJO_CROSS_JOIN, %ld).\n", (long) SQL_SRJO_CROSS_JOIN);
  fprintf(fd, "-define(SQL_SRJO_EXCEPT_JOIN, %ld).\n", (long) SQL_SRJO_EXCEPT_JOIN);
  fprintf(fd,
	  "-define(SQL_SRJO_FULL_OUTER_JOIN, %ld).\n",
	  (long) SQL_SRJO_FULL_OUTER_JOIN);
  fprintf(fd, "-define(SQL_SRJO_INNER_JOIN, %ld).\n", (long) SQL_SRJO_INNER_JOIN);
  fprintf(fd,
	  "-define(SQL_SRJO_INTERSECT_JOIN, %ld).\n",
	  (long) SQL_SRJO_INTERSECT_JOIN);
  fprintf(fd,
	  "-define(SQL_SRJO_LEFT_OUTER_JOIN, %ld).\n",
	  (long) SQL_SRJO_LEFT_OUTER_JOIN);
  fprintf(fd,
	  "-define(SQL_SRJO_NATURAL_JOIN, %ld).\n",
	  (long) SQL_SRJO_NATURAL_JOIN);
  fprintf(fd,
	  "-define(SQL_SRJO_RIGHT_OUTER_JOIN, %ld).\n",
	  (long) SQL_SRJO_RIGHT_OUTER_JOIN);
  fprintf(fd, "-define(SQL_SRJO_UNION_JOIN, %ld).\n", (long) SQL_SRJO_UNION_JOIN);

  fprintf(fd, "-define(SQL_SQL92_REVOKE, %ld).\n", (long) SQL_SQL92_REVOKE);
  fprintf(fd, "-define(SQL_SR_CASCADE, %ld).\n", (long) SQL_SR_CASCADE);
  fprintf(fd, "-define(SQL_SR_DELETE_TABLE, %ld).\n", (long) SQL_SR_DELETE_TABLE);
  fprintf(fd,
	  "-define(SQL_SR_GRANT_OPTION_FOR, %ld).\n",
	  (long) SQL_SR_GRANT_OPTION_FOR);
  fprintf(fd, "-define(SQL_SR_INSERT_COLUMN, %ld).\n", (long) SQL_SR_INSERT_COLUMN);
  fprintf(fd, "-define(SQL_SR_INSERT_TABLE, %ld).\n", (long) SQL_SR_INSERT_TABLE);
  fprintf(fd,
	  "-define(SQL_SR_REFERENCES_COLUMN, %ld).\n",
	  (long) SQL_SR_REFERENCES_COLUMN);
  fprintf(fd,
	  "-define(SQL_SR_REFERENCES_TABLE, %ld).\n",
	  (long) SQL_SR_REFERENCES_TABLE);
  fprintf(fd, "-define(SQL_SR_RESTRICT, %ld).\n", (long) SQL_SR_RESTRICT);
  fprintf(fd, "-define(SQL_SR_SELECT_TABLE, %ld).\n", (long) SQL_SR_SELECT_TABLE);
  fprintf(fd, "-define(SQL_SR_UPDATE_COLUMN, %ld).\n", (long) SQL_SR_UPDATE_COLUMN);
  fprintf(fd, "-define(SQL_SR_UPDATE_TABLE, %ld).\n", (long) SQL_SR_UPDATE_TABLE);
  fprintf(fd,
	  "-define(SQL_SR_USAGE_ON_DOMAIN, %ld).\n",
	  (long) SQL_SR_USAGE_ON_DOMAIN);
  fprintf(fd,
	  "-define(SQL_SR_USAGE_ON_CHARACTER_SET, %ld).\n",
	  (long) SQL_SR_USAGE_ON_CHARACTER_SET);
  fprintf(fd,
	  "-define(SQL_SR_USAGE_ON_COLLATION, %ld).\n",
	  (long) SQL_SR_USAGE_ON_COLLATION);
  fprintf(fd,
	  "-define(SQL_SR_USAGE_ON_TRANSLATION, %ld).\n",
	  (long) SQL_SR_USAGE_ON_TRANSLATION);

  fprintf(fd,
	  "-define(SQL_SQL92_ROW_VALUE_CONSTRUCTOR, %ld).\n",
	  (long) SQL_SQL92_ROW_VALUE_CONSTRUCTOR);
  fprintf(fd,
	  "-define(SQL_SRVC_VALUE_EXPRESSION, %ld).\n",
	  (long) SQL_SRVC_VALUE_EXPRESSION);
  fprintf(fd, "-define(SQL_SRVC_NULL, %ld).\n", (long) SQL_SRVC_NULL);
  fprintf(fd, "-define(SQL_SRVC_DEFAULT, %ld).\n", (long) SQL_SRVC_DEFAULT);
  fprintf(fd,
	  "-define(SQL_SRVC_ROW_SUBQUERY, %ld).\n",
	  (long) SQL_SRVC_ROW_SUBQUERY);

  fprintf(fd,
	  "-define(SQL_SQL92_STRING_FUNCTIONS, %ld).\n",
	  (long) SQL_SQL92_STRING_FUNCTIONS);
  fprintf(fd, "-define(SQL_SSF_CONVERT, %ld).\n", (long) SQL_SSF_CONVERT);
  fprintf(fd, "-define(SQL_SSF_LOWER, %ld).\n", (long) SQL_SSF_LOWER);
  fprintf(fd, "-define(SQL_SSF_UPPER, %ld).\n", (long) SQL_SSF_UPPER);
  fprintf(fd, "-define(SQL_SSF_SUBSTRING, %ld).\n", (long) SQL_SSF_SUBSTRING);
  fprintf(fd, "-define(SQL_SSF_TRANSLATE, %ld).\n", (long) SQL_SSF_TRANSLATE);
  fprintf(fd, "-define(SQL_SSF_TRIM_BOTH, %ld).\n", (long) SQL_SSF_TRIM_BOTH);
  fprintf(fd, "-define(SQL_SSF_TRIM_LEADING, %ld).\n", (long) SQL_SSF_TRIM_LEADING);
  fprintf(fd,
	  "-define(SQL_SSF_TRIM_TRAILING, %ld).\n",
	  (long) SQL_SSF_TRIM_TRAILING);

  fprintf(fd,
	  "-define(SQL_SQL92_VALUE_EXPRESSIONS, %ld).\n",
	  (long) SQL_SQL92_VALUE_EXPRESSIONS);
  fprintf(fd, "-define(SQL_SVE_CASE, %ld).\n", (long) SQL_SVE_CASE);
  fprintf(fd, "-define(SQL_SVE_CAST, %ld).\n", (long) SQL_SVE_CAST);
  fprintf(fd, "-define(SQL_SVE_COALESCE, %ld).\n", (long) SQL_SVE_COALESCE);
  fprintf(fd, "-define(SQL_SVE_NULLIF, %ld).\n", (long) SQL_SVE_NULLIF);

  fprintf(fd,
	  "-define(SQL_STANDARD_CLI_CONFORMANCE, %ld).\n",
	  (long) SQL_STANDARD_CLI_CONFORMANCE);
  fprintf(fd,
	  "-define(SQL_SCC_XOPEN_CLI_VERSION1, %ld).\n",
	  (long) SQL_SCC_XOPEN_CLI_VERSION1);
  fprintf(fd,
	  "-define(SQL_SCC_ISO92_CLI, %ld).\n",
	  (long) SQL_SCC_ISO92_CLI);

  fprintf(fd, "-define(SQL_STRING_FUNCTIONS, %ld).\n", (long) SQL_STRING_FUNCTIONS);
  fprintf(fd, "-define(SQL_FN_STR_ASCII, %ld).\n", (long) SQL_FN_STR_ASCII);
  fprintf(fd,
	  "-define(SQL_FN_STR_BIT_LENGTH, %ld).\n",
	  (long) SQL_FN_STR_BIT_LENGTH);
  fprintf(fd, "-define(SQL_FN_STR_CHAR, %ld).\n", (long) SQL_FN_STR_CHAR);
  fprintf(fd,
	  "-define(SQL_FN_STR_CHAR_LENGTH, %ld).\n",
	  (long) SQL_FN_STR_CHAR_LENGTH);
  fprintf(fd,
	  "-define(SQL_FN_STR_CHARACTER_LENGTH, %ld).\n",
	  (long) SQL_FN_STR_CHARACTER_LENGTH);
  fprintf(fd, "-define(SQL_FN_STR_CONCAT, %ld).\n", (long) SQL_FN_STR_CONCAT);
  fprintf(fd,
	  "-define(SQL_FN_STR_DIFFERENCE, %ld).\n",
	  (long) SQL_FN_STR_DIFFERENCE);
  fprintf(fd, "-define(SQL_FN_STR_INSERT, %ld).\n", (long) SQL_FN_STR_INSERT);
  fprintf(fd, "-define(SQL_FN_STR_LCASE, %ld).\n", (long) SQL_FN_STR_LCASE);
  fprintf(fd, "-define(SQL_FN_STR_LEFT, %ld).\n", (long) SQL_FN_STR_LEFT);
  fprintf(fd, "-define(SQL_FN_STR_LENGTH, %ld).\n", (long) SQL_FN_STR_LENGTH);
  fprintf(fd, "-define(SQL_FN_STR_LOCATE, %ld).\n", (long) SQL_FN_STR_LOCATE);
  fprintf(fd, "-define(SQL_FN_STR_LTRIM, %ld).\n", (long) SQL_FN_STR_LTRIM);
  fprintf(fd,
	  "-define(SQL_FN_STR_OCTET_LENGTH, %ld).\n",
	  (long) SQL_FN_STR_OCTET_LENGTH);
  fprintf(fd, "-define(SQL_FN_STR_POSITION, %ld).\n", (long) SQL_FN_STR_POSITION);
  fprintf(fd, "-define(SQL_FN_STR_REPEAT, %ld).\n", (long) SQL_FN_STR_REPEAT);
  fprintf(fd, "-define(SQL_FN_STR_REPLACE, %ld).\n", (long) SQL_FN_STR_REPLACE);
  fprintf(fd, "-define(SQL_FN_STR_RIGHT, %ld).\n", (long) SQL_FN_STR_RIGHT);
  fprintf(fd, "-define(SQL_FN_STR_RTRIM, %ld).\n", (long) SQL_FN_STR_RTRIM);
  fprintf(fd, "-define(SQL_FN_STR_SOUNDEX, %ld).\n", (long) SQL_FN_STR_SOUNDEX);
  fprintf(fd, "-define(SQL_FN_STR_SPACE, %ld).\n", (long) SQL_FN_STR_SPACE);
  fprintf(fd, "-define(SQL_FN_STR_SUBSTRING, %ld).\n", (long) SQL_FN_STR_SUBSTRING);
  fprintf(fd, "-define(SQL_FN_STR_UCASE, %ld).\n", (long) SQL_FN_STR_UCASE);

  fprintf(fd, "-define(SQL_SUBQUERIES, %ld).\n", (long) SQL_SUBQUERIES);
  fprintf(fd,
	  "-define(SQL_SQ_CORRELATED_SUBQUERIES, %ld).\n",
	  (long) SQL_SQ_CORRELATED_SUBQUERIES);
  fprintf(fd, "-define(SQL_SQ_COMPARISON, %ld).\n", (long) SQL_SQ_COMPARISON);
  fprintf(fd, "-define(SQL_SQ_EXISTS, %ld).\n", (long) SQL_SQ_EXISTS);
  fprintf(fd, "-define(SQL_SQ_IN, %ld).\n", (long) SQL_SQ_IN);
  fprintf(fd, "-define(SQL_SQ_QUANTIFIED, %ld).\n", (long) SQL_SQ_QUANTIFIED);

  fprintf(fd, "-define(SQL_SYSTEM_FUNCTIONS, %ld).\n", (long) SQL_SYSTEM_FUNCTIONS);
  fprintf(fd, "-define(SQL_FN_SYS_DBNAME, %ld).\n", (long) SQL_FN_SYS_DBNAME);
  fprintf(fd, "-define(SQL_FN_SYS_IFNULL, %ld).\n", (long) SQL_FN_SYS_IFNULL);
  fprintf(fd, "-define(SQL_FN_SYS_USERNAME, %ld).\n", (long) SQL_FN_SYS_USERNAME);

  fprintf(fd, "-define(SQL_TABLE_TERM, %ld).\n", (long) SQL_TABLE_TERM);

  fprintf(fd,
	  "-define(SQL_TIMEDATE_ADD_INTERVALS, %ld).\n",
	  (long) SQL_TIMEDATE_ADD_INTERVALS);
  fprintf(fd,
	  "-define(SQL_TIMEDATE_DIFF_INTERVALS, %ld).\n",
	  (long) SQL_TIMEDATE_DIFF_INTERVALS);
  fprintf(fd, 
	  "-define(SQL_FN_TSI_FRAC_SECOND, %ld).\n",
	  (long) SQL_FN_TSI_FRAC_SECOND);
  fprintf(fd, "-define(SQL_FN_TSI_SECOND, %ld).\n", (long) SQL_FN_TSI_SECOND);
  fprintf(fd, "-define(SQL_FN_TSI_MINUTE, %ld).\n", (long) SQL_FN_TSI_MINUTE);
  fprintf(fd, "-define(SQL_FN_TSI_HOUR, %ld).\n", (long) SQL_FN_TSI_HOUR);
  fprintf(fd, "-define(SQL_FN_TSI_DAY, %ld).\n", (long) SQL_FN_TSI_DAY);
  fprintf(fd, "-define(SQL_FN_TSI_WEEK, %ld).\n", (long) SQL_FN_TSI_WEEK);
  fprintf(fd, "-define(SQL_FN_TSI_MONTH, %ld).\n", (long) SQL_FN_TSI_MONTH);
  fprintf(fd, "-define(SQL_FN_TSI_QUARTER, %ld).\n", (long) SQL_FN_TSI_QUARTER);
  fprintf(fd, "-define(SQL_FN_TSI_YEAR, %ld).\n", (long) SQL_FN_TSI_YEAR);

  fprintf(fd,
	  "-define(SQL_TIMEDATE_FUNCTIONS, %ld).\n",
	  (long) SQL_TIMEDATE_FUNCTIONS);
  fprintf(fd,
	  "-define(SQL_FN_TD_CURRENT_DATE, %ld).\n",
	  (long) SQL_FN_TD_CURRENT_DATE);
  fprintf(fd,
	  "-define(SQL_FN_TD_CURRENT_TIME, %ld).\n",
	  (long) SQL_FN_TD_CURRENT_TIME);
  fprintf(fd,
	  "-define(SQL_FN_TD_CURRENT_TIMESTAMP, %ld).\n",
	  (long) SQL_FN_TD_CURRENT_TIMESTAMP);
  fprintf(fd, "-define(SQL_FN_TD_CURDATE, %ld).\n", (long) SQL_FN_TD_CURDATE);
  fprintf(fd, "-define(SQL_FN_TD_CURTIME, %ld).\n", (long) SQL_FN_TD_CURTIME);
  fprintf(fd, "-define(SQL_FN_TD_DAYNAME, %ld).\n", (long) SQL_FN_TD_DAYNAME);
  fprintf(fd, "-define(SQL_FN_TD_DAYOFMONTH, %ld).\n", (long) SQL_FN_TD_DAYOFMONTH);
  fprintf(fd, "-define(SQL_FN_TD_DAYOFWEEK, %ld).\n", (long) SQL_FN_TD_DAYOFWEEK);
  fprintf(fd, "-define(SQL_FN_TD_DAYOFYEAR, %ld).\n", (long) SQL_FN_TD_DAYOFYEAR);
  fprintf(fd, "-define(SQL_FN_TD_EXTRACT, %ld).\n", (long) SQL_FN_TD_EXTRACT);
  fprintf(fd, "-define(SQL_FN_TD_HOUR, %ld).\n", (long) SQL_FN_TD_HOUR);
  fprintf(fd, "-define(SQL_FN_TD_MINUTE, %ld).\n", (long) SQL_FN_TD_MINUTE);
  fprintf(fd, "-define(SQL_FN_TD_MONTH, %ld).\n", (long) SQL_FN_TD_MONTH);
  fprintf(fd, "-define(SQL_FN_TD_MONTHNAME, %ld).\n", (long) SQL_FN_TD_MONTHNAME);
  fprintf(fd, "-define(SQL_FN_TD_NOW, %ld).\n", (long) SQL_FN_TD_NOW);
  fprintf(fd, "-define(SQL_FN_TD_QUARTER, %ld).\n", (long) SQL_FN_TD_QUARTER);
  fprintf(fd, "-define(SQL_FN_TD_SECOND, %ld).\n", (long) SQL_FN_TD_SECOND);
  fprintf(fd,
	  "-define(SQL_FN_TD_TIMESTAMPADD, %ld).\n",
	  (long) SQL_FN_TD_TIMESTAMPADD);
  fprintf(fd,
	  "-define(SQL_FN_TD_TIMESTAMPDIFF, %ld).\n",
	  (long) SQL_FN_TD_TIMESTAMPDIFF);
  fprintf(fd, "-define(SQL_FN_TD_WEEK, %ld).\n", (long) SQL_FN_TD_WEEK);
  fprintf(fd, "-define(SQL_FN_TD_YEAR, %ld).\n", (long) SQL_FN_TD_YEAR);

  fprintf(fd, "-define(SQL_TXN_CAPABLE, %ld).\n", (long) SQL_TXN_CAPABLE);
  fprintf(fd, "-define(SQL_TC_NONE, %ld).\n", (long) SQL_TC_NONE);
  fprintf(fd, "-define(SQL_TC_DML, %ld).\n", (long) SQL_TC_DML);
  fprintf(fd, "-define(SQL_TC_DDL_COMMIT, %ld).\n", (long) SQL_TC_DDL_COMMIT);
  fprintf(fd, "-define(SQL_TC_DDL_IGNORE, %ld).\n", (long) SQL_TC_DDL_IGNORE);
  fprintf(fd, "-define(SQL_TC_ALL, %ld).\n", (long) SQL_TC_ALL);

  fprintf(fd, "-define(SQL_UNION, %ld).\n", (long) SQL_UNION);
  fprintf(fd, "-define(SQL_U_UNION, %ld).\n", (long) SQL_U_UNION);
  fprintf(fd, "-define(SQL_U_UNION_ALL, %ld).\n", (long) SQL_U_UNION_ALL);

  fprintf(fd, "-define(SQL_USER_NAME, %ld).\n", (long) SQL_USER_NAME);

  fprintf(fd, "-define(SQL_XOPEN_CLI_YEAR, %ld).\n", (long) SQL_XOPEN_CLI_YEAR);

  fprintf(fd, "\n\n");

  /* Miscellaneous macros */
  fprintf(fd, "%%%% Miscellaneous macros\n");
  fprintf(fd, "%%%%_____________________\n\n");

  fprintf(fd, "-define(SL_FETCH_NEXT, %ld).\n", (long) SQL_FETCH_NEXT);
  fprintf(fd, "-define(SQL_FETCH_FIRST, %ld).\n", (long) SQL_FETCH_FIRST);
  fprintf(fd, "-define(SQL_FETCH_FIRST_USER, %ld).\n", (long) SQL_FETCH_FIRST_USER);
  fprintf(fd,
	  "-define(SQL_FETCH_FIRST_SYSTEM, %ld).\n",
	  (long) SQL_FETCH_FIRST_SYSTEM);
  fprintf(fd, "-define(SQL_DRIVER_NOPROMPT, %ld).\n", (long) SQL_DRIVER_NOPROMPT);
  fprintf(fd, "-define(SQL_CLOSE, %ld).\n", (long) SQL_CLOSE);
  fprintf(fd, "-define(SQL_UNBIND, %ld).\n", (long) SQL_UNBIND);
  fprintf(fd, "-define(SQL_RESET_PARAMS, %ld).\n", (long) SQL_RESET_PARAMS);
  fprintf(fd, "-define(SQL_BEST_ROWID, %ld).\n", (long) SQL_BEST_ROWID);
  fprintf(fd, "-define(SQL_SCOPE_CURROW, %ld).\n", (long) SQL_SCOPE_CURROW);
  fprintf(fd,
	  "-define(SQL_SCOPE_TRANSACTION, %ld).\n",
	  (long) SQL_SCOPE_TRANSACTION);
  fprintf(fd, "-define(SQL_SCOPE_SESSION, %ld).\n", (long) SQL_SCOPE_SESSION);
  fprintf(fd, "-define(SQL_INDEX_UNIQUE, %ld).\n", (long) SQL_INDEX_UNIQUE);
  fprintf(fd, "-define(SQL_INDEX_ALL, %ld).\n", (long) SQL_INDEX_ALL);
  fprintf(fd, "-define(SQL_ENSURE, %ld).\n", (long) SQL_ENSURE);
  fprintf(fd, "-define(SQL_QUICK, %ld).\n", (long) SQL_QUICK);
  fprintf(fd, "-define(SQL_ALL_CATALOGS, \"%s\").\n", SQL_ALL_CATALOGS);
  fprintf(fd, "-define(SQL_ALL_SCHEMAS, \"%s\").\n", SQL_ALL_SCHEMAS);
  fprintf(fd, "-define(SQL_ALL_TABLE_TYPES, \"%s\").\n", SQL_ALL_TABLE_TYPES);

  /* Internal return codes */
  fprintf(fd, "%%%% Internal return codes\n");
  fprintf(fd, "%%%% _______________\n\n");

  fprintf(fd, "-define(ERROR, %ld).\n", (long) ODBC_ERROR);
  fprintf(fd, "-define(OK, %ld).\n", (long) ODBC_OK);


  fclose(fd);
  return 0;
}
