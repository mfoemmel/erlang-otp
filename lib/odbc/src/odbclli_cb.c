/*----------------------------------------------------------------------
 * File    : odbclli_cb.c
 * Author  : Joakim Hirsch <joke@smeagol>
 * Purpose : Contains the IDL callbacks for ODBC. There are two types
 *           of callbacks:
 *           - Those which call an ODBC function from the dynamic ODBC
 *             library.
 *           - Those who manage allocation/deallocation/reading/writing
 *             of buffers.
 *           There are also restore functions which deallocate temporary
 *           memory resources allocated in some callbacks, or which it is
 *           the responsibility of the callback to deallocate (dynamically
 *           allocated data).
 * Created : 22 Oct 1998 by Joakim Hirsch <joke@smeagol>
 * ----------------------------------------------------------------------
 */


#ifdef __WIN32__
#include <windows.h>
#endif
#include "sqlext.h"
#include "odbclli__s.h"
#include "odbclli_cb.h"
#include "odbclli_cb_codes.h"
#include <string.h>



/* Comment on the interpretation of buffer and handle parameters:
 *
 * These parameters, typically named RefHandle or RefBuf, are
 * passed as CORBA_unsigned_long*. The value represents a pointer
 * to a handle (e.g. SQLHANDLE*) or a pointer to a buffer (e.g. char*).
 * Take for example the function:
 * odbclli_sql_bind_col__cb(odbclli oe_obj,
 *			    CORBA_short* RetCode,
 *			    CORBA_unsigned_long* RefStmtHandle,
 *			    CORBA_unsigned_short* ColNum,
 *			    CORBA_short* BufCType,
 *			    CORBA_unsigned_long* RefBuf,
 *			    CORBA_long* BufLen,
 *			    CORBA_unsigned_long* RefLenInd,
 *			    CORBA_Environment *oe_env)
 * The RefStmtHandle parameter should be thought of as
 * SQLHSTMT* phstmt = *RefStmtHandle, or
 * SQLHSTMT hstmt = *((SQLHSTMT*) *RefStmtHandle).
 * The RefBuf parameter should be thought of as
 * void* pbuf = (void*) *RefBuf.
 */


/*------------------------------------------------------------------
 * Callbacks
 *------------------------------------------------------------------
 */


odbclli_sql_alloc_handle__rs*
odbclli_sql_alloc_handle__cb(odbclli oe_obj,
			     CORBA_short* RetCode,
			     CORBA_short* HandleType,
			     CORBA_unsigned_long* RefInputHandle,
			     CORBA_unsigned_long* RefOutputHandle,
			     CORBA_Environment *oe_env)
{
  SQLHENV* phenv = NULL;
  SQLHDBC* phdbc = NULL;
  SQLHSTMT* phstmt = NULL;

  *RetCode = ODBC_ERROR;
  *RefOutputHandle = (CORBA_unsigned_long) NULL;

  /* It is necessary to distinguish which type of handle to allocate. */
  if (*HandleType == SQL_HANDLE_ENV)
  {
    if ((phenv = (SQLHENV*) malloc(sizeof(SQLHENV))) == NULL)
      exit(NOMEM);

    /* Note that the *RefInputHandle is not a pointer to a handle here,
     * it is the integer SQL_NULL_HANDLE */
    *RetCode = (CORBA_short) SQLAllocHandle((SQLSMALLINT) *HandleType,
					    (SQLHANDLE) *RefInputHandle,
					    (SQLHANDLE*) phenv);

    if (*RetCode == SQL_ERROR)
      *RefOutputHandle = (CORBA_unsigned_long) *phenv;
    else
      *RefOutputHandle = (CORBA_unsigned_long) phenv;
  }
  else if (*HandleType == SQL_HANDLE_DBC)
  {
    if ((phdbc = (SQLHDBC*) malloc(sizeof(SQLHDBC))) == NULL)
      exit(NOMEM);

    *RetCode = (CORBA_short) SQLAllocHandle((SQLSMALLINT) *HandleType,
					    *((SQLHANDLE*) *RefInputHandle),
					    (SQLHANDLE*) phdbc);

    if (*RetCode == SQL_ERROR)
      *RefOutputHandle = (CORBA_unsigned_long) *phdbc;
    else
      *RefOutputHandle = (CORBA_unsigned_long) phdbc;
  }
  else if (*HandleType == SQL_HANDLE_STMT)
  {
    if ((phstmt = (SQLHSTMT*) malloc(sizeof(SQLHSTMT))) == NULL)
      exit(NOMEM);

    *RetCode = (CORBA_short) SQLAllocHandle((SQLSMALLINT) *HandleType,
					    *((SQLHANDLE*) *RefInputHandle),
					    (SQLHANDLE*) phstmt);

    if (*RetCode == SQL_ERROR)
      *RefOutputHandle = (CORBA_unsigned_long) *phstmt;
    else
      *RefOutputHandle = (CORBA_unsigned_long) phstmt;
  }

  return NULL;
}


odbclli_sql_bind_col__rs*
odbclli_sql_bind_col__cb(odbclli oe_obj,
			 CORBA_short* RetCode,
			 CORBA_unsigned_long* RefStmtHandle,
			 CORBA_unsigned_short* ColNum,
			 CORBA_short* BufCType,
			 CORBA_unsigned_long* RefBuf,
			 CORBA_long* BufLen,
			 CORBA_unsigned_long* RefLenInd,
			 CORBA_Environment *oe_env)
{
  *RetCode = (CORBA_short) SQLBindCol(*((SQLHSTMT*) *RefStmtHandle),
				      (SQLUSMALLINT) *ColNum,
				      (SQLSMALLINT) *BufCType,
				      (SQLPOINTER) *RefBuf,
				      (SQLINTEGER) *BufLen,
				      (SQLINTEGER*) *RefLenInd);

  return NULL;
}



odbclli_sql_close_cursor__rs*
odbclli_sql_close_cursor__cb(odbclli oe_obj,
			     CORBA_short* RetCode,
			     CORBA_unsigned_long* RefStmtHandle,
			     CORBA_Environment *oe_env)
{
  *RetCode = (CORBA_short) SQLCloseCursor(*((SQLHSTMT*) *RefStmtHandle));

  return NULL;
}



odbclli_sql_connect__rs*
odbclli_sql_connect__cb(odbclli oe_obj,
			CORBA_short* RetCode,
			CORBA_unsigned_long* RefConnHandle,
			CORBA_char* Server,
			CORBA_char* UID,
			CORBA_char* Auth,
			CORBA_Environment *oe_env)
{
  *RetCode = (CORBA_short) SQLConnect(*((SQLHDBC*) *RefConnHandle),
				      (SQLCHAR*) Server,
				      (SQLSMALLINT) SQL_NTS,
				      (SQLCHAR*) UID,
				      (SQLSMALLINT) SQL_NTS,
				      (SQLCHAR*) Auth,
				      (SQLSMALLINT) SQL_NTS);

  /* Cleanup of Server, UID, and Auth needed. */
  return (odbclli_sql_connect__rs*) sql_connect_cleanup;
}



odbclli_sql_describe_col__rs*
odbclli_sql_describe_col__cb(odbclli oe_obj,
			     CORBA_short* RetCode,
			     CORBA_unsigned_long* RefStmtHandle,
			     CORBA_short* ColNum,
			     CORBA_short* BufLenColName,
			     CORBA_char** ColName,
			     CORBA_short* LenColName,
			     CORBA_short* SqlType,
			     CORBA_unsigned_long* ColSize,
			     CORBA_short* DecDigs,
			     CORBA_short* Nullable,
			     CORBA_Environment *oe_env)
{
  CORBA_char* ColNameBuf;

  *RetCode = ODBC_ERROR;
  *ColName = NULL;

  if ((ColNameBuf = CORBA_string_alloc(*BufLenColName - 1)) == NULL)
    exit(NOMEM);

  *ColNameBuf = '\0';
  *ColName = ColNameBuf;

  *RetCode = (CORBA_short) SQLDescribeCol(*((SQLHSTMT*) *RefStmtHandle),
					  (SQLSMALLINT) *ColNum,
					  (SQLCHAR*) ColNameBuf,
					  (SQLSMALLINT) *BufLenColName,
					  (SQLSMALLINT*) LenColName,
					  (SQLSMALLINT*) SqlType,
					  (SQLUINTEGER*) ColSize,
					  (SQLSMALLINT*) DecDigs,
					  (SQLSMALLINT*) Nullable);

  /* Cleanup of ColNameBuf needed. */
  return (odbclli_sql_describe_col__rs*) sql_describe_col_cleanup;
}



odbclli_sql_disconnect__rs*
odbclli_sql_disconnect__cb(odbclli oe_obj,
			   CORBA_short* RetCode,
			   CORBA_unsigned_long* RefConnHandle,
			   CORBA_Environment *oe_env)
{
  *RetCode = (CORBA_short) SQLDisconnect(*((SQLHDBC*) *RefConnHandle));

  return NULL;
}



odbclli_sql_driver_connect__rs*
odbclli_sql_driver_connect__cb(odbclli oe_obj,
			       CORBA_short* RetCode,
			       CORBA_unsigned_long* RefConnHandle,
			       CORBA_char* InConnStr,
			       CORBA_short* BufLenOutConnStr,
			       CORBA_unsigned_short* DrvCompletion,
			       CORBA_char** OutConnStr,
			       CORBA_short* LenOutConnStr,
			       CORBA_Environment *oe_env)
{
  CORBA_char* OutConnStrBuf;

  *RetCode = ODBC_ERROR;
  *OutConnStr = NULL;

  if ((OutConnStrBuf = CORBA_string_alloc(*BufLenOutConnStr - 1)) == NULL)
    exit(NOMEM);

  *OutConnStrBuf = '\0';
  *OutConnStr = OutConnStrBuf;

  *RetCode = (CORBA_short) SQLDriverConnect(*((SQLHDBC*) *RefConnHandle),
					    (SQLHWND) NULL,
					    (SQLCHAR*) InConnStr,
					    (SQLSMALLINT) SQL_NTS,
					    (SQLCHAR*) OutConnStrBuf,
					    (SQLSMALLINT) *BufLenOutConnStr,
					    (SQLSMALLINT*) LenOutConnStr,
					    (SQLUSMALLINT) *DrvCompletion);

  /* Cleanup of InconnStr and OutConnStrBuf needed. */
  return (odbclli_sql_driver_connect__rs*) sql_driver_connect_cleanup;
}



odbclli_sql_end_tran__rs*
odbclli_sql_end_tran__cb(odbclli oe_obj,
			 CORBA_short* RetCode,
			 CORBA_short* HandleType,
			 CORBA_unsigned_long* RefHandle,
			 CORBA_short* ComplType,
			 CORBA_Environment *oe_env)
{
  *RetCode = (CORBA_short) SQLEndTran((SQLSMALLINT) *HandleType,
				      *((SQLHANDLE*) *RefHandle),
				      (SQLSMALLINT) *ComplType);

  return NULL;
}



odbclli_sql_exec_direct__rs*
odbclli_sql_exec_direct__cb(odbclli oe_obj,
			    CORBA_short* RetCode,
			    CORBA_unsigned_long* RefStmtHandle,
			    CORBA_char* Stmt,
			    CORBA_Environment *oe_env)
{
  *RetCode = (CORBA_short) SQLExecDirect(*((SQLHSTMT*) *RefStmtHandle),
					 (SQLCHAR*) Stmt,
					 (SQLINTEGER) SQL_NTS);

  return (odbclli_sql_exec_direct__rs*) sql_exec_direct_cleanup;
}



odbclli_sql_fetch__rs*
odbclli_sql_fetch__cb(odbclli oe_obj,
		      CORBA_short* RetCode,
		      CORBA_unsigned_long* RefStmtHandle,
		      CORBA_Environment *oe_env)
{
  *RetCode = (CORBA_short) SQLFetch(*((SQLHSTMT*) *RefStmtHandle));

  return NULL;
}



odbclli_sql_free_handle__rs*
odbclli_sql_free_handle__cb(odbclli oe_obj,
			    CORBA_short* RetCode,
			    CORBA_short* HandleType,
			    CORBA_unsigned_long* RefHandle,
			    CORBA_Environment *oe_env)
{
  *RetCode = (CORBA_short) SQLFreeHandle((SQLSMALLINT) *HandleType,
					 *((SQLHANDLE*) *RefHandle));

  /* Free the handle object. */
  free((void*) *RefHandle);

  return NULL;
}



odbclli_sql_get_connect_attr__rs*
odbclli_sql_get_connect_attr__cb(odbclli oe_obj,
				 CORBA_short* RetCode,
				 CORBA_unsigned_long* RefConnHandle,
				 CORBA_long* Attr,
				 CORBA_long* BufLen,
				 CORBA_short* BufCType,
				 CORBA_char** CharValue,
				 CORBA_long* LenCharValue,
				 CORBA_unsigned_long* NumValue,
				 CORBA_Environment *oe_env)
{
  CORBA_char* CharValueBuf = NULL;

  *RetCode = ODBC_ERROR;

  /* A character buffer must be allocated. */
  if (*BufCType == SQL_C_CHAR)
  {
    if ((CharValueBuf = CORBA_string_alloc(*BufLen - 1)) == NULL)
      exit(NOMEM);

    *CharValueBuf = '\0';
    *CharValue = CharValueBuf;

    *RetCode = (CORBA_short) SQLGetConnectAttr(*((SQLHDBC*) *RefConnHandle),
					       (SQLINTEGER) *Attr,
					       (SQLPOINTER) CharValueBuf,
					       (SQLINTEGER) *BufLen,
					       (SQLINTEGER*) LenCharValue);
    *NumValue = 0;
  }
  /* It is necessary to allocate a dummy character buffer, so
   * that an empty string can be returned. */
  else
  {
    if ((CharValueBuf = CORBA_string_alloc(0)) == NULL)
      exit(NOMEM);

    *CharValueBuf = '\0';
    *CharValue = CharValueBuf;
    *LenCharValue = 0;

    *RetCode = (CORBA_short) SQLGetConnectAttr(*((SQLHDBC*) *RefConnHandle),
					       (SQLINTEGER) *Attr,
					       (SQLPOINTER) NumValue,
					       (SQLINTEGER) *BufLen,
					       (SQLINTEGER*) LenCharValue);
  }

  /* Cleanup of CharValueBuf needed. */
  return (odbclli_sql_get_connect_attr__rs*) sql_get_connect_attr_cleanup;
}



odbclli_sql_get_diag_rec__rs*
odbclli_sql_get_diag_rec__cb(odbclli oe_obj,
			     CORBA_short* RetCode,
			     CORBA_short* HandleType,
			     CORBA_unsigned_long* RefHandle,
			     CORBA_short* RecNum,
			     CORBA_short* BufLenErrMsg,
			     CORBA_char** SqlState,
			     CORBA_long* NativeErr,
			     CORBA_char** ErrMsg,
			     CORBA_short* LenErrMsg,
			     CORBA_Environment *oe_env)
{
  CORBA_char *SqlStateBuf, *ErrMsgBuf;

  *RetCode = ODBC_ERROR;


  if ((SqlStateBuf = CORBA_string_alloc(SQL_SQLSTATE_SIZE)) == NULL)
    exit(NOMEM);

  if ((ErrMsgBuf = CORBA_string_alloc(*BufLenErrMsg - 1)) == NULL)
    exit(NOMEM);

  *SqlStateBuf = '\0';
  *ErrMsgBuf = '\0';
  *SqlState = SqlStateBuf;
  *ErrMsg = ErrMsgBuf;

  *RetCode = (CORBA_short) SQLGetDiagRec((SQLSMALLINT) *HandleType,
					 *((SQLHANDLE*) *RefHandle),
					 (SQLSMALLINT) *RecNum,
					 (SQLCHAR*) SqlStateBuf,
					 (SQLINTEGER*) NativeErr,
					 (SQLCHAR*) ErrMsgBuf,
					 (SQLSMALLINT) *BufLenErrMsg,
					 (SQLSMALLINT*) LenErrMsg);

  /* Cleanup of SqlStateBuf and ErrMsgBuf needed. */
  return (odbclli_sql_get_diag_rec__rs*) sql_get_diag_rec_cleanup;
}



odbclli_sql_num_result_cols__rs*
odbclli_sql_num_result_cols__cb(odbclli oe_obj,
				CORBA_short* RetCode,
				CORBA_unsigned_long* RefStmtHandle,
				CORBA_short* ColCount,
				CORBA_Environment *oe_env)
{
  *ColCount = 0;
  *RetCode = (CORBA_short) SQLNumResultCols(*((SQLHSTMT*) *RefStmtHandle),
					    (SQLSMALLINT*) ColCount);

  return NULL;
}



odbclli_sql_row_count__rs*
odbclli_sql_row_count__cb(odbclli oe_obj,
			  CORBA_short* RetCode,
			  CORBA_unsigned_long* RefStmtHandle,
			  CORBA_long* RowCount,
			  CORBA_Environment *oe_env)
{
  *RowCount = 0;
  *RetCode = (CORBA_short) SQLRowCount(*((SQLHSTMT*) *RefStmtHandle),
				       (SQLINTEGER*) RowCount);

  return NULL;
}



odbclli_sql_set_connect_attr__rs*
odbclli_sql_set_connect_attr__cb(odbclli oe_obj,
				 CORBA_short* RetCode,
				 CORBA_unsigned_long* RefConnHandle,
				 CORBA_long* Attr,
				 CORBA_char* CharValue,
				 CORBA_long* LenCharValue,
				 CORBA_unsigned_long* NumValue,
				 CORBA_short* BufCType,
				 CORBA_Environment *oe_env)
{
  /* It is necessary to decide which of the passed values CharValue and
   * NumValue actually contain the value of Attr. */
  if (*BufCType == SQL_C_CHAR)
  {
    *RetCode = (CORBA_short) SQLSetConnectAttr(*((SQLHDBC*) *RefConnHandle),
					       (SQLINTEGER) *Attr,
					       (SQLPOINTER) CharValue,
					       (SQLINTEGER) *LenCharValue);
  }
  else
  {
    *RetCode = (CORBA_short) SQLSetConnectAttr(*((SQLHDBC*) *RefConnHandle),
					       (SQLINTEGER) *Attr,
					       (SQLPOINTER) *NumValue,
					       (SQLINTEGER) *LenCharValue);
  }

  /* Cleanup of CharValue needed. */
  return (odbclli_sql_set_connect_attr__rs*) sql_set_connect_attr_cleanup;
}



odbclli_sql_set_env_attr__rs*
odbclli_sql_set_env_attr__cb(odbclli oe_obj,
			     CORBA_short* RetCode,
			     CORBA_unsigned_long* RefEnvHandle,
			     CORBA_long* Attr,
			     CORBA_char* CharValue,
			     CORBA_long* LenCharValue,
			     CORBA_unsigned_long* NumValue,
			     CORBA_short* BufCType,
			     CORBA_Environment *oe_env)
{
  /* It is necessary to decide which of the passed values CharValue and
   * NumValue actually contain the value of Attr. */
  if (*BufCType == SQL_C_CHAR)
  {
    *RetCode = (CORBA_short) SQLSetEnvAttr(*((SQLHENV*) *RefEnvHandle),
					   (SQLINTEGER) *Attr,
					   (SQLPOINTER) CharValue,
					   (SQLINTEGER) *LenCharValue);
  }
  else
  {
    *RetCode = (CORBA_short) SQLSetEnvAttr(*((SQLHENV*) *RefEnvHandle),
					   (SQLINTEGER) *Attr,
					   (SQLPOINTER) *NumValue,
					   (SQLINTEGER) *LenCharValue);
  }

  /* Cleanup of CharValue needed. */
  return (odbclli_sql_set_env_attr__rs*) sql_set_env_attr_cleanup;
}



odbclli_alloc_buffer__rs*
odbclli_alloc_buffer__cb(odbclli oe_obj,
			 CORBA_short* RetCode,
			 CORBA_short* BufCType,
			 CORBA_long* Size,
			 CORBA_unsigned_long* RefBuf,
			 CORBA_Environment *oe_env)
{
  CORBA_char* pchar;
  CORBA_long* plong;

  *RetCode = ODBC_ERROR;
  *RefBuf = (CORBA_unsigned_long) NULL;

  /* Which type of buffer to allocate? */
  switch (*BufCType)
  {
  case SQL_C_CHAR:
  case SQL_C_BINARY:

    if ((pchar = (CORBA_char*) calloc(*Size, sizeof(CORBA_char))) == NULL)
      exit(NOMEM);
    else
      *RefBuf = (CORBA_unsigned_long) pchar;
    break;


  case SQL_C_SLONG:

    if ((plong = (CORBA_long*) malloc(sizeof(CORBA_long))) == NULL)
      exit(NOMEM);
    else
      *RefBuf = (CORBA_unsigned_long) plong;
    break;

  }

  *RetCode = ODBC_OK;

  return NULL;
}



odbclli_dealloc_buffer__rs*
odbclli_dealloc_buffer__cb(odbclli oe_obj,
			   CORBA_short* RetCode,
			   CORBA_unsigned_long* RefBuf,
			   CORBA_Environment *oe_env)
{
  free((void*) *RefBuf);
  *RetCode = ODBC_OK;

  return NULL;
}



odbclli_read_buffer__rs*
odbclli_read_buffer__cb(odbclli oe_obj,
			CORBA_short* RetCode,
			CORBA_unsigned_long* RefBuf,
			CORBA_short* BufCType,
			CORBA_long* Length,
			CORBA_char** CharValue,
			SQLCHAR_G_SEQ** BinValue,
			CORBA_long* NumValue,
			CORBA_Environment *oe_env)
{
  SQLCHAR_G_SEQ* BinVal;

  *RetCode = ODBC_ERROR;
  *BinValue = NULL;

  if ((BinVal = (SQLCHAR_G_SEQ*) malloc(sizeof(SQLCHAR_G_SEQ))) == NULL)
    exit(NOMEM);

  /* Which type of value to read? */
  switch (*BufCType)
  {

  case SQL_C_CHAR:
    /* Value to return. */
    *CharValue = (CORBA_char*) *RefBuf;

    /* Fill with dummy values. */
    BinVal->_maximum = 0;
    BinVal->_length = 0;
    BinVal->_buffer = "";
    *BinValue = BinVal;

    /* Dummy value. */
    *NumValue = 0;
    break;


  case SQL_C_BINARY:

    /* Dummy value. */
    *CharValue = "";

    /* Value to return. */
    BinVal->_maximum = (CORBA_unsigned_long) *Length;
    BinVal->_buffer = (CORBA_char*) *RefBuf;
    BinVal->_length = (CORBA_unsigned_long) *Length;
    *BinValue = BinVal;

    /* Dummy value. */
    *NumValue = 0;
    break;


  case SQL_C_SLONG:

    /* Dummy value. */
    *CharValue = "";

    /* Fill with dummy values. */
    BinVal->_maximum = 0;
    BinVal->_length = 0;
    BinVal->_buffer = "";
    *BinValue = BinVal;

    /* Value to return. */
    *NumValue = *((CORBA_long*) *RefBuf);
     break;
  }


  *RetCode = ODBC_OK;

  /* Cleanup of the BinValue structure is needed. */
  return (odbclli_read_buffer__rs*) read_buffer_cleanup;
}






/*------------------------------------------------------------------
 * Cleanup functions
 *------------------------------------------------------------------
 */


void
sql_connect_cleanup(odbclli oe_obj,
		    CORBA_short* RetCode,
		    CORBA_unsigned_long* RefConnHandle,
		    CORBA_char* Server,
		    CORBA_char* UID,
		    CORBA_char* Auth,
		    CORBA_Environment *oe_env)
{
  CORBA_free(Server);
  CORBA_free(UID);
  CORBA_free(Auth);
}

void
sql_describe_col_cleanup(odbclli oe_obj,
			 CORBA_short* RetCode,
			 CORBA_unsigned_long* RefStmtHandle,
			 CORBA_short* ColNum,
			 CORBA_short* BufLenColName,
			 CORBA_char** ColName,
			 CORBA_short* LenColName,
			 CORBA_short* SqlType,
			 CORBA_unsigned_long* ColSize,
			 CORBA_short* DecDigs,
			 CORBA_short* Nullable,
			 CORBA_Environment *oe_env)
{
  CORBA_free(*ColName);
}



void
sql_driver_connect_cleanup(odbclli oe_obj,
			   CORBA_short* RetCode,
			   CORBA_unsigned_long* RefConnHandle,
			   CORBA_char* InConnStr,
			   CORBA_short* BufLenOutConnStr,
			   CORBA_unsigned_short* DrvCompletion,
			   CORBA_char** OutConnStr,
			   CORBA_short* LenOutConnStr,
			   CORBA_Environment *oe_env)
{
  CORBA_free(InConnStr);
  CORBA_free(*OutConnStr);
}



void
sql_exec_direct_cleanup(odbclli oe_obj,
			CORBA_short* RetCode,
			CORBA_unsigned_long* RefStmtHandle,
			CORBA_char* Stmt,
			CORBA_Environment *oe_env)
{
  CORBA_free(Stmt);
}



void
sql_get_connect_attr_cleanup(odbclli oe_obj,
			     CORBA_short* RetCode,
			     CORBA_unsigned_long* RefConnHandle,
			     CORBA_long* Attr,
			     CORBA_long* BufLen,
			     CORBA_short* BufCType,
			     CORBA_char** CharValue,
			     CORBA_long* LenCharValue,
			     CORBA_unsigned_long* NumValue,
			     CORBA_Environment *oe_env)
{
    /* Memory allocated only if BufCType is SQL_C_CHAR. */
/*   if (*BufCType == SQL_C_CHAR) */
    CORBA_free(*CharValue);
}




void
sql_get_diag_rec_cleanup(odbclli oe_obj,
			 CORBA_short* RetCode,
			 CORBA_short* HandleType,
			 CORBA_unsigned_long* RefHandle,
			 CORBA_short* RecNum,
			 CORBA_short* BufLenErrMsg,
			 CORBA_char** SqlState,
			 CORBA_long* NativeErr,
			 CORBA_char** ErrMsg,
			 CORBA_short* LenErrMsg,
			 CORBA_Environment *oe_env)
{
  CORBA_free(*SqlState);
  CORBA_free(*ErrMsg);
}




void
sql_set_connect_attr_cleanup(odbclli oe_obj,
			     CORBA_short* RetCode,
			     CORBA_unsigned_long* RefConnHandle,
			     CORBA_long* Attr,
			     CORBA_char* CharValue,
			     CORBA_long* LenCharValue,
			     CORBA_unsigned_long* NumValue,
			     CORBA_short* BufCType,
			     CORBA_Environment *oe_env)
{
  CORBA_free(CharValue);
}




void
sql_set_env_attr_cleanup(odbclli oe_obj,
			 CORBA_short* RetCode,
			 CORBA_unsigned_long* RefEnvHandle,
			 CORBA_long* Attr,
			 CORBA_char* CharValue,
			 CORBA_long* LenCharValue,
			 CORBA_unsigned_long* NumValue,
			 CORBA_short* BufCType,
			 CORBA_Environment *oe_env)
{
  CORBA_free(CharValue);
}




/* NOTE:
 * CharValue shall not be deallocated (just like the character buffer
 * member of BinValue). It is only the sequence structure BinValue that is
 * deallocated.
 */
void
read_buffer_cleanup(odbclli oe_obj,
		    CORBA_short* RetCode,
		    CORBA_unsigned_long* RefBuf,
		    CORBA_short* BufCType,
		    CORBA_long* Length,
		    CORBA_char** CharValue,
		    SQLCHAR_G_SEQ** BinValue,
		    CORBA_long* NumValue,
		    CORBA_Environment *oe_env)
{
  free(*BinValue);
}
