/*----------------------------------------------------------------------
 * File    : odbclli_cb.h
 * Author  : Joakim Hirsch <joke@smeagol>
 * Purpose : Contains the extern declarations of the cleanup
 *           functions called after callbacks in odbclli_cb.c.
 * Created : 22 Oct 1998 by Joakim Hirsch <joke@smeagol>
 * ----------------------------------------------------------------------
 */


#define NOMEM 17


extern void sql_connect_cleanup(odbclli oe_obj,
				CORBA_short* RetCode,
				CORBA_unsigned_long* RefConnHandle,
				CORBA_char* Server,
				CORBA_char* UID,
				CORBA_char* Auth,
				CORBA_Environment *oe_env);


extern void sql_describe_col_cleanup(odbclli oe_obj,
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
				     CORBA_Environment *oe_env);


extern void sql_driver_connect_cleanup(odbclli oe_obj,
				       CORBA_short* RetCode,
				       CORBA_unsigned_long* RefConnHandle,
				       CORBA_char* InConnStr,
				       CORBA_short* BufLenOutConnStr,
				       CORBA_unsigned_short* DrvCompletion,
				       CORBA_char** OutConnStr,
				       CORBA_short* LenOutConnStr,
				       CORBA_Environment *oe_env);


extern void sql_exec_direct_cleanup(odbclli oe_obj,
				    CORBA_short* RetCode,
				    CORBA_unsigned_long* RefStmtHandle,
				    CORBA_char* Stmt,
				    CORBA_Environment *oe_env);


extern void sql_get_connect_attr_cleanup(odbclli oe_obj,
					 CORBA_short* RetCode,
					 CORBA_unsigned_long* RefConnHandle,
					 CORBA_long* Attr,
					 CORBA_long* BufLen,
					 CORBA_short* BufCType,
					 CORBA_char** CharValue,
					 CORBA_long* LenCharValue,
					 CORBA_unsigned_long* NumValue,
					 CORBA_Environment *oe_env);


extern void sql_get_diag_rec_cleanup(odbclli oe_obj,
				     CORBA_short* RetCode,
				     CORBA_short* HandleType,
				     CORBA_unsigned_long* RefHandle,
				     CORBA_short* RecNum,
				     CORBA_short* BufLenErrMsg,
				     CORBA_char** SqlState,
				     CORBA_long* NativeErr,
				     CORBA_char** ErrMsg,
				     CORBA_short* LenErrMsg,
				     CORBA_Environment *oe_env);


extern void sql_set_connect_attr_cleanup(odbclli oe_obj,
					 CORBA_short* RetCode,
					 CORBA_unsigned_long* RefConnHandle,
					 CORBA_long* Attr,
					 CORBA_char* CharValue,
					 CORBA_long* LenCharValue,
					 CORBA_unsigned_long* NumValue,
					 CORBA_short* BufCType,
					 CORBA_Environment *oe_env);


extern void sql_set_env_attr_cleanup(odbclli oe_obj,
				     CORBA_short* RetCode,
				     CORBA_unsigned_long* RefEnvHandle,
				     CORBA_long* Attr,
				     CORBA_char* CharValue,
				     CORBA_long* LenCharValue,
				     CORBA_unsigned_long* NumValue,
				     CORBA_short* BufCType,
				     CORBA_Environment *oe_env);



extern void read_buffer_cleanup(odbclli oe_obj,
				CORBA_short* RetCode,
				CORBA_unsigned_long* RefBuf,
				CORBA_short* BufCType,
				CORBA_long* Length,
				CORBA_char** CharValue,
				SQLCHAR_G_SEQ** BinValue,
				CORBA_long* NumValue,
				CORBA_Environment *oe_env);
