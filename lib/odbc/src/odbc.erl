%% ``The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%% 
%%     $Id$
%%

-module(odbc).

-behaviour(gen_server).


%% External exports------------------------------------
%% Start/stop
-export([start_link/2, start_link/3]).
-export([stop/1, stop/2]).

%% Basic API
-export([sql_alloc_handle/3, sql_alloc_handle/4]).
-export([sql_bind_col/4, sql_bind_col/5]).
-export([sql_close_cursor/2, sql_close_cursor/3]).
-export([sql_connect/5, sql_connect/6]).
-export([sql_describe_col/4, sql_describe_col/5]).
-export([sql_disconnect/2, sql_disconnect/3]).
-export([sql_driver_connect/5, sql_driver_connect/6]).
-export([sql_end_tran/4, sql_end_tran/5]).
-export([sql_exec_direct/3, sql_exec_direct/4]).
-export([sql_fetch/2, sql_fetch/3]).
-export([sql_free_handle/3, sql_free_handle/4]).
-export([sql_get_connect_attr/4, sql_get_connect_attr/5]).
-export([sql_get_diag_rec/5, sql_get_diag_rec/6]).
-export([sql_num_result_cols/2, sql_num_result_cols/3]).
-export([sql_row_count/2, sql_row_count/3]).
-export([sql_set_connect_attr/5, sql_set_connect_attr/6]).
-export([sql_set_env_attr/5, sql_set_env_attr/6]).
-export([alloc_buffer/3, alloc_buffer/4]).
-export([dealloc_buffer/2, dealloc_buffer/3]).
-export([read_buffer/2, read_buffer/3]).
-export([write_buffer/3, write_buffer/4]).

%% Utility API
-export([init_env/1, init_env/2]).
-export([connect/3, connect/4, connect/5, connect/6]).
-export([execute_stmt/3, execute_stmt/4]).
-export([disconnect/2, disconnect/3]).
-export([terminate_env/2, terminate_env/3]).

%% Miscellaneous
%-export([sql_func_exists/2]).
-export([bitmask_is_set/2, display_size/2]).

%%------------------------------------------------------

%% Internal exports (for debug)
-export([get_state/1]).


%% Internal exports (for apply)
-export([init_env_impl/1]).
-export([connect_drv_impl/1, connect_impl/1]).
-export([execute_stmt_impl/1]).
-export([disconnect_impl/1]).
-export([terminate_env_impl/1]).


%% Internal exports (for supervisor).
-export([start_link_sup/2, start_link_sup/3]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).


-include("../include/odbc.hrl").



%% Internal state
-record(state, {cnode = undefined,           % The C node that serves requests
		port = undefined,            % The port which starts the c-node.
		max_len_data = undefined,    % Max length of table data when
		                             % using the Utility API
		                             % (see start_link).
		max_len_err_msg = undefined, % Max length of error messages when
		                             % using the Utility API
		                             % (see start_link).
		max_len_str = undefined,     % Max length of other strings when using
		                             % the Utility API (see start_link).
		handles = undefined,         % Active handles
		buffers = undefined,         % Active deferred buffers.
		conn_data = undefined}).     % Data about the current connection.


%% Deferred buffer data
-record(buf_data, {buffer = undefined,    % A handle to data structure on the C node.
		   ctype = undefined,     % The C type of the buffer.
		   size = undefined,      % The buffer size (0 for fixed size types).
		   len_ind = undefined}). % A handle to the associated
		                          % length/indicator buffer.

%% Data about current connection, when connected with SQLConnect
-record(connect_data, {dsn,               % Data source name
		       uid,               % User ID
		       pwd}).             % Password

%% Data about current connection, when connected with SQLDriverConnect
-record(drv_connect_data, {conn_str,      % Connection string.
			   len_conn_str,  % Length of conn_str.
			   drv_compl}).   % Driver completion.


%%---------------
%% Internal macros
%% Path to C server main
-define(ServerDir, filename:nativename(filename:join(code:priv_dir(odbc), "bin"))).

%% Name of C server main
-define(ServerProg, "odbcserver").

%% Default size of communication (to C node) buffer: 32*1024
%% The value is already converted to string.
-define(DefaultBufSize, "32768").
%% Minimum size of communication (to C node) buffer: 4*1024.
-define(MinBufSz, 4096).

%% Default length of ODBC strings.
-define(DefaultMaxLenData, 8192).  % 8*1024
-define(DefaultMaxLenErrMsg, 1024).
-define(DefaultMaxLenStr, 1024).

%% Default timeout.
-define(DefaultTimeout, 5000).

%% Buffer size of output connection string from sql_driver_connect
%% for the Utility API.
-define(MaxLenOutConnStr, 1024).

%%%----------------------------------------------


%%%================================================================================
%%% API
%%%================================================================================
%%%
%%% Exceptions for the entire API:
%%% {'EXIT', {badarg, M, F, A, ArgNo, Info}}   ; Arg. of wrong type or out of range.
%%%    M -> atom()                             ; Module.
%%%    F -> atom()                             ; Function.
%%%    A -> [term()]                           ; Arguments.
%%%    ArgNo -> integer()                      ; Argument no.
%%%    Info -> {Descr, ArgNo, ArgVal}
%%%      Descr -> string()                     ; Error description.
%%%      ArgVal -> term()                      ; Arg. value.
%%% {'EXIT', {internal_error, Info}}           ; Internal error.
%%%    Info -> {Fcn, Args, Retval}             ; Function, arguments, return value.
%%%      Fcn -> atom()                         ; Fcn in which error occurred.
%%%      Args -> {args, ArgList}               ; 
%%%        ArgList -> [term()]                 ; Actual arguments.
%%%      Retval -> {retval, Result}}           ;
%%%        Result -> term()                    ; Returned value (caught error).
%%% {'EXIT', GenServerSpecificInfo}            ; Error detected by gen_server.
%%%    GenServerSpecificInfo -> term()         ; gen_server specific.
%%% {'EXIT', {timeout, Info}}                  ; Timeout expired.
%%%    Info -> term()                          ; gen_server specific.
%%% {'EXIT', {stopped, Reason}}                ; ODBCE server (and C node) died.
%%%    Reason -> term()
%%%
%%%================================================================================

%%%----------------------------------------------------------
%%% Start and Stop
%%%----------------------------------------------------------


%%---------------------------------------------------
%% start_link/[2, 3]
%%
%% Starts the ODBCE server and the C node.
%% Links to the server to the calling process.
%% Registers the new server with the supervisor.
%%
%% start_link(ServerName, Args, Options, Client) ->
%% start_link(Args, Options, Client) ->
%%    Result
%%
%% Args -> [Arg]                        ; Initialisation arguments.
%% Arg -> {buffer_size, integer()} |    ; The size of the buffer for communication
%%                                        with the C node. Limits the amount of
%%                                        data that can pass in either direction of
%%                                        a function call. The default is 32 kb.
%%                                        The minimum is 4 kb.
%%        {max_len_data, integer()}       The maximum length, including
%%                                        null-termination, of table data returned
%%                                        from ODBC. This value must be chosen with
%%                                        the buffer size in mind. The default is
%%                                        8 kb. Used only by the Utility API.
%%        {max_len_err_msg, integer()}    The maximum length, including
%%                                        null-termination, of the message part
%%                                        of ODBC error messages. This value must
%%                                        be chosen with the buffer size in mind.
%%                                        The default is 1 kb. Used only by the
%%                                        Utility API.
%%        {max_len_str, integer()} |      The maximum length, including
%%                                        null-termination, of other strings passed
%%                                        from ODBC to the ODBCE server. This
%%                                        value must be chosen with the buffer size
%%                                        in mind. The default is 1 kb. Used only
%%                                        by the Utility API.
%% Options -> [Opt]                     ; List of options.
%% Opt -> {timeout, integer()} |        ; The time in ms allowed for initialisation,
%%                                        see gen_server.
%%        {debug, [Dbg]}                  Debug options, see gen_server.
%% Dbg -> trace |                       ; Debug options, see gen_server.
%%        log |
%%        statistics |
%%        {log_to_file, FileName} |
%%        {install, {Func, FuncState}}
%% ServerName -> {local, atom()} |      ; When supplied, causes the server to be
%%               {global, atom()}         registered, locally or globally. If the
%%                                        server is started without a name it can
%%                                        only be called using the returned Pid.
%% Result -> {ok, pid()} |              ; The pid of the erl. server.
%%           {error, Reason}              Error tuple.
%% Reason -> {already_started, pid()} | ; Server already started.
%%           timeout |                    Timeout expired.
%%           {no_c_node, Info}            Can't start C node. The program may not
%%                                        have been found or may not have been
%%                                        executable e.g.
%% Info -> string()                     ; More information.
%%


start_link(Args, Options) ->
    chk_args(Args, start_link, [Args, Options], 1),
    chk_options(Options, start_link, [Args, Options], 2),
    supervisor:start_child(odbc_sup, [[{client, self()} | Args], Options]).

start_link(ServerName, Args, Options) ->
    chk_server(ServerName, start_link, [ServerName, Args, Options], 1),
    chk_args(Args, start_link, [ServerName, Args, Options], 2),
    chk_options(Options, start_link, [ServerName, Args, Options], 3),
    supervisor:start_child(odbc_sup,
			   [ServerName, [{client, self()} | Args], Options]).



%% start_link_sup/[2, 3]
%%
%% Called by the supervisor to start a new server instance.
%% (start_link calls the supervisor which calls start_link_sup.)
%%
start_link_sup(Args, Options) ->
    gen_server:start_link(?MODULE, Args, Options).

start_link_sup(ServerName, Args, Options) ->
    gen_server:start_link(ServerName, ?MODULE, Args, Options).



%%-------------------------------------------------------
%% stop/[1, 2]
%%
%% Stops the ODBCE server and the C node.
%%
%% stop(Server) ->
%% stop(Server, Timeout) ->
%%    ok
%%
%% Server -> pid() |                 ; The pid of the server,
%%           Name |                    a registered name,
%%           {global, Name} |          a globally registered name, or
%%           {Name, Node}              a registered name on a remote node.
%% Timeout -> integer() |            ; Max time (ms) for serving the request.
%%            infinity
%%

stop(Server) ->
    stop(Server, ?DefaultTimeout).

stop(Server, Timeout) ->
    chk_server1(Server, stop, [Server, Timeout], 1),
    chk_timeout(Timeout, stop, [Server, Timeout], 2),
    gen_server:call(Server, stop, Timeout).




%%%--------------------------------------------------------------------------
%%% Basic API
%%%--------------------------------------------------------------------------


%%----------------------------------------------------------------
%% sql_alloc_handle/[3, 4]
%%
%% Allocates memory for an environment, connection, or
%% statement handle.
%%
%% sql_alloc_handle(Server, HandleType, RefInputHandle) ->
%% sql_alloc_handle(Server, HandleType, RefInputHandle, Timeout) ->
%%    {Result, RefOutputHandle}
%%
%% Server -> pid() |                      ; The pid of the server,
%%           Name |                         a registered name,
%%           {global, Name} |               a globally registered name, or
%%           {Name, Node}                   a registered name on a remote node.
%% HandleType -> ?SQL_HANDLE_ENV |        ; Macros which determine
%%               ?SQL_HANDLE_DBC |          which type of handle to allocate.
%%               ?SQL_HANDLE_STMT
%% RefInputHandle -> term() |             ; The context in which the new handle
%%                   ?SQL_NULL_HANDLE       is to be allocated. When allocating
%%                                          an environment handle, use
%%                                          ?SQL_NULL_HANDLE.
%% Timeout -> integer() |                 ; Max time (ms) for serving the request.
%%            infinity
%% Result -> ?SQL_SUCCESS |               ; Result macro.
%%           ?SQL_SUCCESS_WITH_INFO |
%%           ?SQL_INVALID_HANDLE |
%%           ?SQL_ERROR |
%% RefOutputHandle -> term()|             ; Reference to the allocated handle, or
%%                    ?SQL_NULL_HENV |      a value representing an error.
%%                    ?SQL_NULL_HDBC |
%%                    ?SQL_NULL_HSTMT
%%

sql_alloc_handle(Server, HandleType, RefInputHandle) ->
    sql_alloc_handle(Server, HandleType, RefInputHandle, ?DefaultTimeout).

% Difficult to find out how ODBC reports errors for these 2 cases,
% so it's better to treat it here.
sql_alloc_handle(Server, ?SQL_HANDLE_ENV, RefInputHandle, Timeout)
  when RefInputHandle =/= ?SQL_NULL_HANDLE ->
    exit({badarg,
	  ?MODULE,
	  sql_alloc_handle,
	  [Server, ?SQL_HANDLE_ENV, RefInputHandle, Timeout],
	  3,
	  {"Bad handle reference", RefInputHandle}});

sql_alloc_handle(Server, HandleType, ?SQL_NULL_HANDLE, Timeout)
  when HandleType =/= ?SQL_HANDLE_ENV ->
    case HandleType of
	?SQL_HANDLE_DBC ->
	    exit({badarg,
		  ?MODULE,
		  sql_alloc_handle,
		  [Server, HandleType, ?SQL_NULL_HANDLE, Timeout],
		  3,
		  {"Bad handle reference", ?SQL_NULL_HANDLE}});
	?SQL_HANDLE_STMT ->
	    exit({badarg,
		  ?MODULE,
		  sql_alloc_handle,
		  [Server, HandleType, ?SQL_NULL_HANDLE, Timeout],
		  3,
		  {"Bad handle reference", ?SQL_NULL_HANDLE}});
	_Other ->
	    exit({badarg,
		  ?MODULE,
		  sql_alloc_handle,
		  [Server, HandleType, ?SQL_NULL_HANDLE, Timeout],
		  2,
		  {"Bad handle type", HandleType}})
    end;

sql_alloc_handle(Server, HandleType, RefInputHandle, Timeout) ->
    chk_server1(Server,
		sql_alloc_handle,
		[Server, HandleType, RefInputHandle, Timeout],
		1),
    chk_handle_type(HandleType,
		    sql_alloc_handle,
		    [Server, HandleType, RefInputHandle, Timeout],
		    2),
    chk_timeout(Timeout,
		sql_alloc_handle,
		[Server, HandleType, RefInputHandle, Timeout],
		4),
    case gen_server:call(Server,
			 {basic, sql_alloc_handle, [HandleType, RefInputHandle]},
			 Timeout) of
	% Internal error
	{internal_error, Info} ->
	    exit({internal_error, Info});
	% gen_server specific error
	{error, Reason} ->
	    exit(Reason);
	% Bad input data
	{badref, _ArgNo} ->
	    exit({badarg,
		  ?MODULE,
		  sql_alloc_handle,
		  [Server, ?SQL_HANDLE_ENV, RefInputHandle, Timeout],
		  3,
		  {"Bad handle reference", RefInputHandle}});
	% Server going down
	{stopped, Reason} ->
	    exit({stopped, Reason});
	% Succeeded in making ODBC call (which may have returned an error)
	Other ->
	    Other
    end.




%%-------------------------------------------------------------------
%% sql_bind_col/[4, 5]
%%
%% Assigns storage and data type for a column in a result set
%% (binds a buffer to a  column).
%%
%% sql_bind_col(Server, RefStmtHandle, ColNum, RefBuf) ->
%% sql_bind_col(Server, RefStmtHandle, ColNum, RefBuf, Timeout) ->
%%   Result
%%
%% Server -> pid() |                      ; The pid of the server,
%%           Name |                         a registered name,
%%           {global, Name} |               a globally registered name, or
%%           {Name, Node}                   a registered name on a remote node.
%% RefStmtHandle -> term()                ; Reference to a stmt handle.
%% ColNum -> integer()                    ; Column number from left to right
%%                                          starting at 1.
%% RefBuf -> term() |                     ; Reference to the buffer where the
%%           ?NULL_REF                      column data is placed (and to the
%%                                          associated length/indicator buffer).
%%                                          ?NULL_REF removes the binding between
%%                                          a buffer and a column.
%% Timeout -> integer() |                 ; Max time (ms) for serving the request.
%%            infinity
%% Result -> ?SQL_SUCCESS |               ; Result macro.
%%           ?SQL_SUCCESS_WITH_INFO |
%%           ?SQL_INVALID_HANDLE |
%%           ?SQL_ERROR
%%
sql_bind_col(Server, RefStmtHandle, ColNum, RefBuf) ->
    sql_bind_col(Server, RefStmtHandle, ColNum, RefBuf, ?DefaultTimeout).

sql_bind_col(Server, RefStmtHandle, ColNum, RefBuf, Timeout) ->
    chk_server1(Server,
		sql_bind_col,
		[Server, RefStmtHandle, ColNum, RefBuf, Timeout],
		1),
    chk_Z_plus(ColNum,
	       sql_bind_col,
	       [Server, RefStmtHandle, ColNum, RefBuf, Timeout],
	       3),
    chk_timeout(Timeout,
		sql_bind_col,
		[Server, RefStmtHandle, ColNum, RefBuf, Timeout],
		5),
    case gen_server:call(Server,
			 {basic, sql_bind_col, [RefStmtHandle, ColNum, RefBuf]},
			 Timeout) of
	% Internal error
	{internal_error, Info} ->
	    exit({internal_error, Info});
	% gen_server specific error
	{error, Reason} ->
	    exit(Reason);
	% Bad input data
	{badref, 1} ->
	    exit({badarg,
		  ?MODULE,
		  sql_bind_col,
		  [Server, RefStmtHandle, ColNum, RefBuf, Timeout],
		  2,
		  {"Bad handle reference", RefStmtHandle}});
	{badref, 3} ->
	    exit({badarg,
		  ?MODULE,
		  sql_bind_col,
		  [Server, RefStmtHandle, ColNum, RefBuf, Timeout],
		  4,
		  {"Bad buffer reference", RefBuf}});
	% Server going down
	{stopped, Reason} ->
	    exit({stopped, Reason});
	% Succeeded in making ODBC call (which may have returned an error)
	Other ->
	    Other
    end.



%%-------------------------------------------------------------------
%% sql_close_cursor/[2, 3]
%%
%% Closes a cursor that has been opened on a statement and discards pending 
%% results.
%%
%% sql_close_cursor(Server, RefStmtHandle) ->
%% sql_close_cursor(Server, RefStmtHandle, Timeout) ->
%%   Result
%%
%% Server -> pid() |                      ; The pid of the server,
%%           Name |                         a registered name,
%%           {global, Name} |               a globally registered name, or
%%           {Name, Node}                   a registered name on a remote node.
%% RefStmtHandle -> term()                ; Reference to the statement handle.
%% Timeout -> integer() |                 ; Max time (ms) for serving the request.
%%            infinity
%% Result -> ?SQL_SUCCESS |               ; Result macro.
%%           ?SQL_SUCCESS_WITH_INFO |
%%           ?SQL_INVALID_HANDLE |
%%           ?SQL_ERROR

sql_close_cursor(Server, RefStmtHandle) ->
    sql_close_cursor(Server, RefStmtHandle, ?DefaultTimeout).

sql_close_cursor(Server, RefStmtHandle, Timeout) ->
    chk_server1(Server, sql_close_cursor, [Server, RefStmtHandle, Timeout], 1),
    chk_timeout(Timeout, sql_close_cursor, [Server, RefStmtHandle, Timeout], 3),
    case gen_server:call(Server,
			 {basic, sql_close_cursor, [RefStmtHandle]},
			 Timeout)    of
	% Internal error
	{internal_error, Info} ->
	    exit({internal_error, Info});
	% gen_server specific error
	{error, Reason} ->
	    exit(Reason);
	% Bad input data
	{badref, _ArgNo} ->
	    exit({badarg,
		  ?MODULE,
		  sql_close_cursor,
		  [Server, RefStmtHandle, Timeout],
		  2,
		  {"Bad handle reference", RefStmtHandle}});
	% Server going down
	{stopped, Reason} ->
	    exit({stopped, Reason});
	% Succeeded in making ODBC call (which may have returned an error)
	Other ->
	    Other
    end.




%%-------------------------------------------------------------------
%% sql_connect/[5, 6]
%%
%% Establishes a connection to a driver and a data source.
%%
%% sql_connect(Server, RefConnHandle, DSN, UID, Auth) ->
%% sql_connect(Server, RefConnHandle, DSN, UID, Auth, Timeout) ->
%%   Result
%%
%% Server -> pid() |                      ; The pid of the server,
%%           Name |                         a registered name,
%%           {global, Name} |               a globally registered name, or
%%           {Name, Node}                   a registered name on a remote node.
%% RefConnHandle -> term()                ; Reference to the connection handle.
%% DSN -> string()                        ; The name of the data source.
%% UID -> string()                        ; The user ID.
%% Auth -> string()                       ; The user's password for the data source.
%% Timeout -> integer() |                 ; Max time (ms) for serving the request.
%%            infinity
%% Result -> ?SQL_SUCCESS |               ; Result macro.
%%           ?SQL_SUCCESS_WITH_INFO |
%%           ?SQL_INVALID_HANDLE |
%%           ?SQL_ERROR

sql_connect(Server, RefConnHandle, DSN, UID, Auth) ->
    sql_connect(Server, RefConnHandle, DSN, UID, Auth, ?DefaultTimeout).

sql_connect(Server, RefConnHandle, DSN, UID, Auth, Timeout) ->
    chk_server1(Server,
		sql_connect,
		[Server, RefConnHandle, DSN, UID, Auth, Timeout],
		1),
    chk_str(DSN,
	    sql_connect,
	    [Server, RefConnHandle, DSN, UID, Auth, Timeout],
	    3),
    chk_str(UID,
	    sql_connect,
	    [Server, RefConnHandle, DSN, UID, Auth, Timeout],
	    4),
    chk_str(Auth,
	    sql_connect,
	    [Server, RefConnHandle, DSN, UID, Auth, Timeout],
	    5),
    chk_timeout(Timeout,
		sql_connect,
		[Server, RefConnHandle, DSN, UID, Auth, Timeout],
		6),
    case gen_server:call(Server,
			 {basic, sql_connect, [RefConnHandle, DSN, UID, Auth]},
			 Timeout)   of
	% Internal error
	{internal_error, Info} ->
	    exit({internal_error, Info});
	% gen_server specific error
	{error, Reason} ->
	    exit(Reason);
	% Bad input data
	{badref, _ArgNo} ->
	    exit({badarg,
		  ?MODULE,
		  sql_connect,
		  [Server, RefConnHandle, DSN, UID, Auth, Timeout],
		  2,
		  {"Bad handle reference", RefConnHandle}});
	% Server going down
	{stopped, Reason} ->
	    exit({stopped, Reason});
	% Succeeded in making ODBC call (which may have returned an error)
	Other ->
	    Other
    end.


%%-------------------------------------------------------------------
%% sql_describe_col/[4, 5]
%%
%% Returns the result descriptor -- column name, type, column size,
%% decimal digits, and nullability -- for one column in the result set.
%%
%% sql_describe_col(Server, RefStmtHandle, ColNum, BufLenColName) ->
%% sql_describe_col(Server, RefStmtHandle, ColNum, BufLenColName, Timeout) ->
%%   {Result, {ColName, LenColName}, SqlType, ColSize, DecDigs, Nullable}
%%
%% Server -> pid() |                      ; The pid of the server,
%%           Name |                         a registered name,
%%           {global, Name} |               a globally registered name, or
%%           {Name, Node}                   a registered name on a remote node.
%% RefStmtHandle -> term()                ; Reference to the statement handle.
%% ColNum -> integer()                    ; Column number from left to right
%%                                          starting at 1.
%% BufLenColName -> integer()             ; Maximum length of ColName buffer.
%% Timeout -> integer() |                 ; Max time (ms) for serving the request.
%%            infinity
%% Result -> ?SQL_SUCCESS |               ; Result macro.
%%           ?SQL_SUCCESS_WITH_INFO |
%%           ?SQL_INVALID_HANDLE |
%%           ?SQL_ERROR
%% ColName -> string()                    ; The column name.
%% LenColName -> integer()                ; Actual length of ColName.
%% SqlType -> integer()                   ; An ODBC SQL data type or a driver-
%%                                          specific type.
%% ColSize -> integer()                   ; The precision of the column. If the
%%                                          precision can not be determined, 0
%%                                          is returned.
%% DecDigs -> integer()                   ; The scale of the column. If the scale
%%                                          can not be determined, or is not
%%                                          applicable, 0 is returned.
%% Nullable -> ?SQL_NO_NULLS |            ; Indicates whether the olumn allows
%%             ?SQL_NULLABLE |              NULL values or not.
%%             ?SQL_NULLABLE_UNKNOWN
%%

sql_describe_col(Server, RefStmtHandle, ColNum, BufLenColName) ->
    sql_describe_col(Server, RefStmtHandle, ColNum, BufLenColName, ?DefaultTimeout).

sql_describe_col(Server, RefStmtHandle, ColNum, BufLenColName, Timeout) ->
    chk_server1(Server,
		sql_describe_col,
		[Server, RefStmtHandle, ColNum, BufLenColName, Timeout],
		1),
    chk_Z_plus(ColNum,
	       sql_describe_col,
	       [Server, RefStmtHandle, ColNum, BufLenColName, Timeout],
	       3),
    chk_Z_plus(BufLenColName,
	       sql_describe_col,
	       [Server, RefStmtHandle, ColNum, BufLenColName, Timeout],
	       4),
    chk_timeout(Timeout,
		sql_describe_col,
		[Server, RefStmtHandle, ColNum, BufLenColName, Timeout],
		5),
    case gen_server:call(Server,
			 {basic,
			  sql_describe_col,
			  [RefStmtHandle, ColNum, BufLenColName]},
			 Timeout)   of
	% Internal error
	{internal_error, Info} ->
	    exit({internal_error, Info});
	% gen_server specific error
	{error, Reason} ->
	    exit(Reason);
	% Bad input data
	{badref, _ArgNo} ->
	    exit({badarg,
		  ?MODULE,
		  sql_describe_col,
		  [Server, RefStmtHandle, ColNum, BufLenColName, Timeout],
		  2,
		  {"Bad handle reference", RefStmtHandle}});
	% Server going down
	{stopped, Reason} ->
	    exit({stopped, Reason});
	% Succeeded in making ODBC call (which may have returned an error)
	Other ->
	    Other
    end.




%%-------------------------------------------------------------------
%% sql_disconnect/[2, 3]
%%
%% Closes the connection associated with a specific connection handle.
%%
%% sql_disconnect(Server, RefConnHandle) ->
%% sql_disconnect(Server, RefConnHandle, Timeout) ->
%%   Result
%%
%% Server -> pid() |                      ; The pid of the server,
%%           Name |                         a registered name,
%%           {global, Name} |               a globally registered name, or
%%           {Name, Node}                   a registered name on a remote node.
%% RefConnHandle -> term()                ; Reference to the connection handle.
%% Timeout -> integer() |                 ; Max time (ms) for serving the request.
%%            infinity
%% Result -> ?SQL_SUCCESS |               ; Result macro.
%%           ?SQL_SUCCESS_WITH_INFO |
%%           ?SQL_INVALID_HANDLE |
%%           ?SQL_ERROR
%% 

sql_disconnect(Server, RefConnHandle) ->
    sql_disconnect(Server, RefConnHandle, ?DefaultTimeout).

sql_disconnect(Server, RefConnHandle, Timeout) ->
    chk_server1(Server, sql_disconnect, [Server, RefConnHandle, Timeout], 1),
    chk_timeout(Timeout, sql_disconnect, [Server, RefConnHandle, Timeout], 3),
    case gen_server:call(Server,
			 {basic, sql_disconnect, [RefConnHandle]},
			 Timeout)  of
	% Internal error
	{internal_error, Info} ->
	    exit({internal_error, Info});
	% gen_server specific error
	{error, Reason} ->
	    exit(Reason);
	% Bad input data
	{badref, _ArgNo} ->
	    exit({badarg,
		  ?MODULE,
		  sql_disconnect,
		  [Server, RefConnHandle, Timeout],
		  2,
		  {"Bad handle reference", RefConnHandle}});
	% Server going down
	{stopped, Reason} ->
	    exit({stopped, Reason});
	% Succeeded in making ODBC call (which may have returned an error)
	Other ->
	    Other
    end.



%%-------------------------------------------------------------------
%% sql_driver_connect/[5, 6]
%%
%% Establishes a connection to a driver and a data source which needs more
%% connection information than sql_connect offers.
%%
%% sql_driver_connect(Server, RefConnHandle, InConnStr,
%%                    BufLenOutConnStr, DrvCompletion) ->
%% sql_driver_connect(Server, RefConnHandle, InConnStr,
%%                    BufLenOutConnStr, DrvCompletion, Timeout) ->
%%   {Result, {OutConnStr, LenOutConnStr}}
%%
%% Server -> pid() |                      ; The pid of the server,
%%           Name |                         a registered name,
%%           {global, Name} |               a globally registered name, or
%%           {Name, Node}                   a registered name on a remote node.
%% RefConnHandle -> term()                ; Reference to the connection handle.
%% InConnStr -> string()                  ; A complete connection string.
%% BufLenOutConnStr -> integer()          ; Max length of OutConnStr buffer.
%% DrvCompletion -> ?SQL_DRIVER_NOPROMPT  ; No prompting with pop-ups.
%% Timeout -> integer() |                 ; Max time (ms) for serving the request.
%%            infinity
%% Result -> ?SQL_SUCCESS |               ; Result macro.
%%           ?SQL_SUCCESS_WITH_INFO |
%%           ?SQL_NO_DATA |
%%           ?SQL_INVALID_HANDLE |
%%           ?SQL_ERROR
%% OutConnStr -> string()                 ; A complete connection string.
%% LenOutConnStr -> integer()             ; The actual length of OutConnStr.
%%

sql_driver_connect(Server, RefConnHandle, InConnStr,
		   BufLenOutConnStr, DrvCompletion) ->
    sql_driver_connect(Server, RefConnHandle, InConnStr,
		       BufLenOutConnStr, DrvCompletion, ?DefaultTimeout).

sql_driver_connect(Server, RefConnHandle, InConnStr,
		   BufLenOutConnStr, DrvCompletion, Timeout) ->
    chk_server1(Server,
		sql_driver_connect,
		[Server,
		 RefConnHandle,
		 InConnStr,
		 BufLenOutConnStr,
		 DrvCompletion,
		 Timeout],
		1),
    chk_str(InConnStr,
	    sql_driver_connect,
	    [Server,
	     RefConnHandle,
	     InConnStr,
	     BufLenOutConnStr,
	     DrvCompletion,
	     Timeout],
	    3),
    chk_Z_plus(BufLenOutConnStr,
	       sql_driver_connect,
	       [Server,
		RefConnHandle,
		InConnStr,
		BufLenOutConnStr,
		DrvCompletion,
		Timeout],
	       4),
    chk_drv_compl(DrvCompletion,
		  sql_driver_connect,
		  [Server,
		   RefConnHandle,
		   InConnStr,
		   BufLenOutConnStr,
		   DrvCompletion,
		   Timeout],
		  5),
    chk_timeout(Timeout,
		sql_driver_connect,
		[Server,
		 RefConnHandle,
		 InConnStr,
		 BufLenOutConnStr,
		 DrvCompletion,
		 Timeout],
		6),
    case gen_server:call(Server,
			 {basic, sql_driver_connect, [RefConnHandle,
						      InConnStr,
						      BufLenOutConnStr,
						      DrvCompletion]},
			 Timeout)  of
	% Internal error
	{internal_error, Info} ->
	    exit({internal_error, Info});
	% gen_server specific error
	{error, Reason} ->
	    exit(Reason);
	% Bad input data
	{badref, _ArgNo} ->
	    exit({badarg,
		  ?MODULE,
		  sql_driver_connect,
		  [Server,
		   RefConnHandle,
		   InConnStr,
		   BufLenOutConnStr,
		   DrvCompletion,
		   Timeout],
		  2,
		  {"Bad handle reference", RefConnHandle}});
	% Server going down
	{stopped, Reason} ->
	    exit({stopped, Reason});
	% Succeeded in making ODBC call (which may have returned an error)
	Other ->
	    Other
    end.


%% sql_end_tran/[4, 5]
%%
%% Requests a commit or rollback operation for all active operations on all 
%% statement handles associated with a connection. It can also request that a 
%% commit or rollback operation be performed for all connections associated 
%% with the environment handle.
%%
%% sql_end_tran(Server, HandleType, RefHandle, ComplType) ->
%% sql_end_tran(Server, HandleType, RefHandle, ComplType, Timeout) ->
%%   Result
%%
%% Server -> pid() |                      ; The pid of the server,
%%           Name |                         a registered name,
%%           {global, Name} |               a globally registered name, or
%%           {Name, Node}                   a registered name on a remote node.
%% HandleType -> ?SQL_HANDLE_ENV |        ; The type of handle for which to perform
%%               ?SQL_HANDLE_DBC            the transaction.
%% RefHandle -> term()                    ; Reference to the handle.
%% ComplType -> ?SQL_COMMIT |             ; Commit operation.
%%              ?SQL_ROLLBACK               Rollback operation.
%% Timeout -> integer() |                 ; Max time (ms) for serving the request.
%%            infinity
%% Result -> ?SQL_SUCCESS |               ; Result macro.
%%           ?SQL_SUCCESS_WITH_INFO |
%%           ?SQL_INVALID_HANDLE |
%%           ?SQL_ERROR

sql_end_tran(Server, HandleType, RefHandle, ComplType) ->
    sql_end_tran(Server, HandleType, RefHandle, ComplType, ?DefaultTimeout).

sql_end_tran(Server, HandleType, RefHandle, ComplType, Timeout) ->
    chk_server1(Server,
		sql_end_tran,
		[Server, HandleType, RefHandle, ComplType, Timeout],
		1),
    chk_handle_type1(HandleType,
		     sql_end_tran,
		     [Server, HandleType, RefHandle, ComplType, Timeout],
		     2),
    chk_compl_type(ComplType,
		   sql_end_tran,
		   [Server, HandleType, RefHandle, ComplType, Timeout],
		   4),
    chk_timeout(Timeout,
		sql_end_tran,
		[Server, HandleType, RefHandle, ComplType, Timeout],
		5),
    case gen_server:call(Server,
			 {basic, sql_end_tran, [HandleType, RefHandle, ComplType]},
			 Timeout)  of
	% Internal error
	{internal_error, Info} ->
	    exit({internal_error, Info});
	% gen_server specific error
	{error, Reason} ->
	    exit(Reason);
	% Bad input data
	{badref, _ArgNo} ->
	    exit({badarg,
		  ?MODULE,
		  sql_end_tran,
		  [Server, HandleType, RefHandle, ComplType, Timeout],
		  3,
		  {"Bad handle reference", RefHandle}});
	% Server going down
	{stopped, Reason} ->
	    exit({stopped, Reason});
	% Succeeded in making ODBC call (which may have returned an error)
	Other ->
	    Other
    end.


%% sql_exec_direct/[3, 4]
%%
%% Executes a preparable statement using the current values of the parameter 
%% marker buffers (bound with sql_bind_parameter/[8, 9]) if any parameter 
%% markers exist in the statement.
%%
%% sql_exec_direct(Server, RefStmtHandle, Stmt) ->
%% sql_exec_direct(Server, RefStmtHandle, Stmt, Timeout) ->
%%   Result
%%
%% Server -> pid() |                      ; The pid of the server,
%%           Name |                         a registered name,
%%           {global, Name} |               a globally registered name, or
%%           {Name, Node}                   a registered name on a remote node.
%% RefStmtHandle -> term()                ; Reference to the statement handle.
%% Stmt -> string()                       ; An SQL statement.
%% Timeout -> integer() |                 ; Max time (ms) for serving the request.
%%            infinity
%% Result -> ?SQL_SUCCESS |               ; Result macro.
%%           ?SQL_SUCCESS_WITH_INFO |
%%           ?SQL_NEED_DATA |
%%           ?SQL_NO_DATA |
%%           ?SQL_INVALID_HANDLE |
%%           ?SQL_ERROR

sql_exec_direct(Server, RefStmtHandle, Stmt) ->
    sql_exec_direct(Server, RefStmtHandle, Stmt, ?DefaultTimeout).


sql_exec_direct(Server, RefStmtHandle, Stmt, Timeout) ->
    chk_server1(Server, sql_exec_direct, [Server, RefStmtHandle, Stmt, Timeout], 1),
    chk_str(Stmt, sql_exec_direct, [Server, RefStmtHandle, Stmt, Timeout], 3),
    chk_timeout(Timeout, sql_exec_direct, [Server, RefStmtHandle, Stmt, Timeout], 4),
    case gen_server:call(Server,
			 {basic, sql_exec_direct, [RefStmtHandle, Stmt]},
			 Timeout)  of
	% Internal error
	{internal_error, Info} ->
	    exit({internal_error, Info});
	% gen_server specific error
	{error, Reason} ->
	    exit(Reason);
	% Bad input data
	{badref, _ArgNo} ->
	    exit({badarg,
		  ?MODULE,
		  sql_exec_direct,
		  [Server, RefStmtHandle, Stmt, Timeout],
		  2,
		  {"Bad handle reference", RefStmtHandle}});
	% Server going down
	{stopped, Reason} ->
	    exit({stopped, Reason});
	% Succeeded in making ODBC call (which may have returned an error)
	Other ->
	    Other
    end.


%% sql_fetch/[2, 3]
%%
%% Fetches a row of data from a result set. The driver returns data for all columns 
%% that were bound to storage locations with sql_bind_col/[4, 5].
%%
%% sql_fetch(Server, RefStmtHandle) ->
%% sql_fetch(Server, RefStmtHandle, Timeout) ->
%%   Result
%%
%% Server -> pid() |                      ; The pid of the server,
%%           Name |                         a registered name,
%%           {global, Name} |               a globally registered name, or
%%           {Name, Node}                   a registered name on a remote node.
%% RefStmtHandle -> term()                ; Reference to the statement handle.
%% Timeout -> integer() |                 ; Max time (ms) for serving the request.
%%            infinity
%% Result -> ?SQL_SUCCESS |               ; Result macro.
%%           ?SQL_SUCCESS_WITH_INFO |
%%           ?SQL_NO_DATA |
%%           ?SQL_INVALID_HANDLE |
%%           ?SQL_ERROR
%% 

sql_fetch(Server, RefStmtHandle) ->
    sql_fetch(Server, RefStmtHandle, ?DefaultTimeout).

sql_fetch(Server, RefStmtHandle, Timeout) ->
    chk_server1(Server, sql_fetch, [Server, RefStmtHandle, Timeout], 1),
    chk_timeout(Timeout, sql_fetch, [Server, RefStmtHandle, Timeout], 3),
    case gen_server:call(Server,
			 {basic, sql_fetch, [RefStmtHandle]},
			 Timeout)  of
	% Internal error
	{internal_error, Info} ->
	    exit({internal_error, Info});
	% gen_server specific error
	{error, Reason} ->
	    exit(Reason);
	% Bad input data
	{badref, _ArgNo} ->
	    exit({badarg,
		  ?MODULE,
		  sql_fetch,
		  [Server, RefStmtHandle, Timeout],
		  2,
		  {"Bad handle reference", RefStmtHandle}});
	% Server going down
	{stopped, Reason} ->
	    exit({stopped, Reason});
	% Succeeded in making ODBC call (which may have returned an error)
	Other ->
	    Other
    end.



%%-------------------------------------------------------------------
%% sql_free_handle/[3, 4]
%%
%% Releases a handle and frees all resources associated with it.
%%
%% sql_free_handle(Server, HandleType, RefHandle) ->
%% sql_free_handle(Server, HandleType, RefHandle, Timeout) ->
%%   Result
%%
%% Server -> pid() |                      ; The pid of the server,
%%           Name |                         a registered name,
%%           {global, Name} |               a globally registered name, or
%%           {Name, Node}                   a registered name on a remote node.
%% HandleType -> ?SQL_HANDLE_ENV |        ; Macros which determine
%%               ?SQL_HANDLE_DBC |          which type of handle to free.
%%               ?SQL_HANDLE_STMT
%% RefHandle -> term()                    ; Reference to the handle.
%% Timeout -> integer() |                 ; Max time (ms) for serving the request.
%%            infinity
%% Result -> ?SQL_SUCCESS |               ; Result macro.
%%           ?SQL_INVALID_HANDLE |
%%           ?SQL_ERROR

sql_free_handle(Server, HandleType, RefHandle) ->
    sql_free_handle(Server, HandleType, RefHandle, ?DefaultTimeout).

sql_free_handle(Server, HandleType, RefHandle, Timeout) ->
    chk_server1(Server,
		sql_free_handle,
		[Server, HandleType, RefHandle, Timeout],
		1),
    chk_handle_type(HandleType,
		    sql_free_handle,
		    [Server, HandleType, RefHandle, Timeout],
		    2),
    chk_timeout(Timeout,
		sql_free_handle,
		[Server, HandleType, RefHandle, Timeout],
		4),
    case gen_server:call(Server,
			 {basic, sql_free_handle, [HandleType, RefHandle]},
			 Timeout) of
	% Internal error
	{internal_error, Info} ->
	    exit({internal_error, Info});
	% gen_server specific error
	{error, Reason} ->
	    exit(Reason);
	% Bad input data
	{badref, _ArgNo} ->
	    exit({badarg,
		  ?MODULE,
		  sql_free_handle,
		  [Server, HandleType, RefHandle, Timeout],
		  3,
		  {"Bad handle reference", RefHandle}});
	% Server going down
	{stopped, Reason} ->
	    exit({stopped, Reason});
	% Succeeded in making ODBC call (which may have returned an error)
	Other ->
	    Other
    end.
	



%% sql_get_connect_attr/[4, 5]
%%
%% Returns the current setting of a connection attribute. The following
%% attributes are supported (through macros):
%% - ?SQL_ATTR_ACCESS_MODE
%% - ?SQL_ATTR_AUTOCOMMIT
%% - ?SQL_ATTR_ODBC_CURSORS
%% - ?SQL_ATTR_TRACE
%% - ?SQL_ATTR_TRACEFILE
%% - ?SQL_ATTR_TRANSLATE_LIB
%% - ?SQL_ATTR_TRANSLATE_OPTION
%%
%% Driver-specific attributes are not supported (through macros), but can
%% be retrieved, if they are of character or signed/unsigned long integer types.
%%
%% sql_get_connect_attr(Server, RefConnHandle, Attr, BufType) ->
%% sql_get_connect_attr(Server, RefConnHandle, Attr, BufType, Timeout) ->
%%   {Result, Value}
%%
%% Server -> pid() |                      ; The pid of the server,
%%           Name |                         a registered name,
%%           {global, Name} |               a globally registered name, or
%%           {Name, Node}                   a registered name on a remote node.
%% RefConnHandle -> term()                ; Reference to the connection handle.
%% Attr -> integer()                      ; One of the attributes described above
%%                                          or a driver-specific attribute.
%% BufType -> {?SQL_C_CHAR, BufLen} |     ; The buffer type used for retrieving
%%            ?SQL_C_ULONG |                the data. For character type data
%%            {?SQL_C_ULONG, IntType}       also the buffer size. For integer
%%                                          type data that is driver-specific,
%%                                          also a subtype.
%%   BufLen -> integer()                  ; Buffer size for character type data.
%%   IntType -> ?SQL_IS_UINTEGER |        ; Used only for driver-specific attributes.
%%              ?SQL_IS_INTEGER
%% Timeout -> integer() |                 ; Max time (ms) for serving the request.
%%            infinity
%% Result -> ?SQL_SUCCESS |               ; Result macro.
%%           ?SQL_SUCCESS_WITH_INFO |
%%           ?SQL_NO_DATA |
%%           ?SQL_INVALID_HANDLE |
%%           ?SQL_ERROR
%% Value -> {CharValue, LenCharValue} |   ; Attribute data.
%%          NumValue
%%   CharValue -> string()                ; The value of the attribute when of
%%                                          character type.
%%   LenCharValue -> integer()            ; Actual length of CharValue.
%%   NumValue ->     integer()            ; The value of the attribute when of
%%                                          numeric type.
%%

sql_get_connect_attr(Server, RefConnHandle, Attr, BufType) ->
    sql_get_connect_attr(Server, RefConnHandle, Attr, BufType, ?DefaultTimeout).

sql_get_connect_attr(Server, RefConnHandle, Attr, BufType, Timeout) ->
    chk_server1(Server,
		sql_get_connect_attr,
		[Server, RefConnHandle, Attr, BufType, Timeout],
		1),
    chk_Z(Attr,
	  sql_get_connect_attr,
	  [Server, RefConnHandle, Attr, BufType, Timeout],
	  3),
    chk_buf_c_type1(BufType,
		    sql_get_connect_attr,
		    [Server, RefConnHandle, Attr, BufType, Timeout],
		    4),
    chk_timeout(Timeout,
		sql_get_connect_attr,
		[Server, RefConnHandle, Attr, BufType, Timeout],
		5),
    case gen_server:call(Server,
			 {basic, sql_get_connect_attr, [RefConnHandle,
							Attr,
							BufType]},
			 Timeout)  of
	% Internal error
	{internal_error, Info} ->
	    exit({internal_error, Info});
	% gen_server specific error
	{error, Reason} ->
	    exit(Reason);
	% Bad input data
	{badref, _ArgNo} ->
	    exit({badarg,
		  ?MODULE,
		  sql_get_connect_attr,
		  [Server, RefConnHandle, Attr, BufType, Timeout],
		  2,
		  {"Bad handle reference", RefConnHandle}});
	% Server going down
	{stopped, Reason} ->
	    exit({stopped, Reason});
	% Succeeded in making ODBC call (which may have returned an error)
	Other ->
	    Other
    end.



%% sql_get_diag_rec/[5, 6]
%%
%% Retrieves the current values of multiple fields of a diagnostic record that 
%% contains error, warning, and status information.
%%
%% sql_get_diag_rec(Server, HandleType, RefHandle, RecNum, BufLenErrMsg) ->
%% sql_get_diag_rec(Server, HandleType, RefHandle, RecNum, BufLenErrMsg, Timeout) ->
%%   {Result, SqlState, NativeErr, {ErrMsg, LenErrMsg}}
%%
%% Server -> pid() |                      ; The pid of the server,
%%           Name |                         a registered name,
%%           {global, Name} |               a globally registered name, or
%%           {Name, Node}                   a registered name on a remote node.
%% HandleType -> ?SQL_HANDLE_ENV |        ; Macros which determine which type of
%%               ?SQL_HANDLE_DBC |          handle to get error info about.
%%               ?SQL_HANDLE_STMT
%% RefHandle -> term()                    ; Reference to the handle.
%% RecNum -> integer()                    ; Indicates the status record from which
%%                                          to retrieve information (> 0).
%% BufLenErrMsg -> integer()              ; Max length of ErrMsg buffer.
%% Timeout -> integer() |                 ; Max time (ms) for serving the request.
%%            infinity
%% Result -> ?SQL_SUCCESS |               ; Result macro.
%%           ?SQL_SUCCESS_WITH_INFO |
%%           ?SQL_NO_DATA |
%%           ?SQL_INVALID_HANDLE |
%%           ?SQL_ERROR
%% SqlState -> string()                   ; The SQL state pertaining to the
%                                           diagnostic record
%% NativeErr -> integer()                 ; Data-source specific error code.
%% ErrMsg -> string()                     ; Error message.
%% LenErrMsg -> integer()                 ; Actual length of ErrMsg.
%%

sql_get_diag_rec(Server, HandleType, RefHandle, RecNum, BufLenErrMsg) ->
    sql_get_diag_rec(Server,
		     HandleType,
		     RefHandle,
		     RecNum,
		     BufLenErrMsg,
		     ?DefaultTimeout).

sql_get_diag_rec(Server, HandleType, RefHandle, RecNum, BufLenErrMsg, Timeout) ->
    chk_server1(Server,
		sql_get_diag_rec,
		[Server, HandleType, RefHandle, RecNum, BufLenErrMsg, Timeout],
		1),
    chk_handle_type(HandleType,
		    sql_get_diag_rec,
		    [Server, HandleType, RefHandle, RecNum, BufLenErrMsg, Timeout],
		    2),
    chk_Z_plus(RecNum,
	       sql_get_diag_rec,
	       [Server, HandleType, RefHandle, RecNum, BufLenErrMsg, Timeout],
	       4),
    chk_Z_plus(BufLenErrMsg,
	       sql_get_diag_rec,
	       [Server, HandleType, RefHandle, RecNum, BufLenErrMsg, Timeout],
	       5),
    chk_timeout(Timeout,
		sql_get_diag_rec,
		[Server, HandleType, RefHandle, RecNum, BufLenErrMsg, Timeout],
		6),
    case gen_server:call(Server,
			 {basic, sql_get_diag_rec, [HandleType,
						    RefHandle,
						    RecNum,
						    BufLenErrMsg]},
			 Timeout)  of
	% Internal error
	{internal_error, Info} ->
	    exit({internal_error, Info});
	% gen_server specific error
	{error, Reason} ->
	    exit(Reason);
	% Bad input data
	{badref, _ArgNo} ->
	    exit({badarg,
		  ?MODULE,
		  sql_get_diag_rec,
		  [Server, HandleType, RefHandle, RecNum, BufLenErrMsg, Timeout],
		  3,
		  {"Bad handle reference", RefHandle}});
	% Server going down
	{stopped, Reason} ->
	    exit({stopped, Reason});
	% Succeeded in making ODBC call (which may have returned an error)
	Other ->
	    Other
    end.


%% sql_num_result_cols/[2, 3]
%%
%% Returns the number of columns in a result set.
%%
%% sql_num_result_cols(Server, RefStmtHandle) ->
%% sql_num_result_cols(Server, RefStmtHandle, Timeout) ->
%%   {Result, ColCount}
%%
%% Server -> pid() |                      ; The pid of the server,
%%           Name |                         a registered name,
%%           {global, Name} |               a globally registered name, or
%%           {Name, Node}                   a registered name on a remote node.
%% RefStmtHandle -> term()                ; Reference to the statement handle.
%% Timeout -> integer() |                 ; Max time (ms) for serving the request.
%%            infinity
%% Result -> ?SQL_SUCCESS |               ; Result macro.
%%           ?SQL_SUCCESS_WITH_INFO |
%%           ?SQL_INVALID_HANDLE |
%%           ?SQL_ERROR
%% ColCount -> integer()                  ; The number of columns in the result set.
%%

sql_num_result_cols(Server, RefStmtHandle) ->
    sql_num_result_cols(Server, RefStmtHandle, ?DefaultTimeout).

sql_num_result_cols(Server, RefStmtHandle, Timeout) ->
    chk_server1(Server, sql_num_result_cols, [Server, RefStmtHandle, Timeout], 1),
    chk_timeout(Timeout, sql_num_result_cols, [Server, RefStmtHandle, Timeout], 3),
    case gen_server:call(Server,
			 {basic, sql_num_result_cols, [RefStmtHandle]},
			 Timeout)  of
	% Internal error
	{internal_error, Info} ->
	    exit({internal_error, Info});
	% gen_server specific error
	{error, Reason} ->
	    exit(Reason);
	% Bad input data
	{badref, _ArgNo} ->
	    exit({badarg,
		  ?MODULE,
		  sql_num_result_cols,
		  [Server, RefStmtHandle, Timeout],
		  2,
		  {"Bad handle reference", RefStmtHandle}});
	% Server going down
	{stopped, Reason} ->
	    exit({stopped, Reason});
	% Succeeded in making ODBC call (which may have returned an error)
	Other ->
	    Other
    end.



%% sql_row_count/[2, 3]
%%
%% Returns the number of rows affected by an UPDATE, INSERT, or DELETE 
%% statement.
%%
%% sql_row_count(Server, RefStmtHandle) ->
%% sql_row_count(Server, RefStmtHandle, Timeout) ->
%%   {Result, RowCount}
%%
%% Server -> pid() |                      ; The pid of the server,
%%           Name |                         a registered name,
%%           {global, Name} |               a globally registered name, or
%%           {Name, Node}                   a registered name on a remote node.
%% RefStmtHandle -> term()                ; Reference to the statement handle.
%% Timeout -> integer() |                 ; Max time (ms) for serving the request.
%%            infinity
%% Result -> ?SQL_SUCCESS |               ; Result macro.
%%           ?SQL_SUCCESS_WITH_INFO |
%%           ?SQL_INVALID_HANDLE |
%%           ?SQL_ERROR
%% RowCount -> integer()                  ; The number of affected rows (or -1
%%                                          if not available).
%%

sql_row_count(Server, RefStmtHandle) ->
    sql_row_count(Server, RefStmtHandle, ?DefaultTimeout).

sql_row_count(Server, RefStmtHandle, Timeout) ->
    chk_server1(Server, sql_row_count, [Server, RefStmtHandle, Timeout], 1),
    chk_timeout(Timeout, sql_row_count, [Server, RefStmtHandle, Timeout], 3),
    case gen_server:call(Server,
			 {basic, sql_row_count, [RefStmtHandle]},
			 Timeout)  of
	% Internal error
	{internal_error, Info} ->
	    exit({internal_error, Info});
	% gen_server specific error
	{error, Reason} ->
	    exit(Reason);
	% Bad input data
	{badref, _ArgNo} ->
	    exit({badarg,
		  ?MODULE,
		  sql_row_count,
		  [Server, RefStmtHandle, Timeout],
		  2,
		  {"Bad handle reference", RefStmtHandle}});
	% Server going down
	{stopped, Reason} ->
	    exit({stopped, Reason});
	% Succeeded in making ODBC call (which may have returned an error)
	Other ->
	    Other
    end.



%% sql_set_connect_attr/[5, 6]
%%
%% Sets attributes that govern aspects of connections. The supported
%% attributes are listed under sql_get_connect_attr/[4, 5]. Driver-specific
%% attributes are not supported through macros, but can be set, if  they
%% are strings or signed/unsigned long integers.
%%
%% sql_set_connect_attr(Server, RefConnHandle, Attr, Value, BufType) ->
%% sql_set_connect_attr(Server, RefConnHandle, Attr, Value, BufType, Timeout) ->
%%   Result
%%
%% Server -> pid() |                      ; The pid of the server,
%%           Name |                         a registered name,
%%           {global, Name} |               a globally registered name, or
%%           {Name, Node}                   a registered name on a remote node.
%% RefConnHandle -> term()                ; Reference to the connection handle.
%% Attr -> integer()                      ; One of the attributes described under
%%                                          sql_get_connect_attr/[4, 5] or a
%%                                          driver-specific attribute. The
%%                                          attributes defined by ODBC are
%%                                          supplied through macros, but driver-
%%                                          specific attributes are not.
%% Value -> string() |                    ; The new attribute value.
%%          integer()
%% BufType -> ?SQL_C_CHAR |               ; The buffer type. Either a
%%            ?SQL_C_ULONG |                (null-terminated) tring, an ODBC defined
%%            {?SQL_C_ULONG, IntType}       attribute of integer type, or a driver-
%%                                          specific attribute of integer type
%%                                          (which also has a subtype).
%%   IntType -> ?SQL_IS_UINTEGER |        ; Subtype for driver-specific
%%              ?SQL_IS_INTEGER             integer attributes.
%% Timeout -> integer() |                 ; Max time (ms) for serving the request.
%%            infinity
%% Result -> ?SQL_SUCCESS |               ; Result macro.
%%           ?SQL_SUCCESS_WITH_INFO |
%%           ?SQL_INVALID_HANDLE |
%%           ?SQL_ERROR
%%

sql_set_connect_attr(Server, RefConnHandle, Attr, Value, BufType) ->
    sql_set_connect_attr(Server,
			 RefConnHandle,
			 Attr,
			 Value,
			 BufType,
			 ?DefaultTimeout).

sql_set_connect_attr(Server, RefConnHandle, Attr, Value, BufType, Timeout) ->
    chk_server1(Server,
		sql_set_connect_attr,
		[Server, RefConnHandle, Attr, Value, BufType, Timeout],
		1),
    chk_Z(Attr,
	  sql_set_connect_attr,
	  [Server, RefConnHandle, Attr, Value, BufType, Timeout],
	  3),
    chk_buf_c_type2(BufType,
		    sql_set_connect_attr,
		    [Server, RefConnHandle, Attr, Value, BufType, Timeout],
		    5),
    chk_value({Value, BufType},
	      sql_set_connect_attr,
	      [Server, RefConnHandle, Attr, Value, BufType, Timeout],
	      4),
    chk_timeout(Timeout,
		sql_set_connect_attr,
		[Server, RefConnHandle, Attr, Value, BufType, Timeout],
		6),
    case gen_server:call(Server,
			 {basic, sql_set_connect_attr, [RefConnHandle,
							Attr,
							Value,
							BufType]},
			 Timeout)  of
	% Internal error
	{internal_error, Info} ->
	    exit({internal_error, Info});
	% gen_server specific error
	{error, Reason} ->
	    exit(Reason);
	% Bad input data
	{badref, _ArgNo} ->
	    exit({badarg,
		  ?MODULE,
		  sql_set_connect_attr,
		  [Server, RefConnHandle, Attr, Value, BufType, Timeout],
		  2,
		  {"Bad handle reference", RefConnHandle}});
	% Server going down
	{stopped, Reason} ->
	    exit({stopped, Reason});
	% Succeeded in making ODBC call (which may have returned an error)
	Other ->
	    Other
    end.




%% sql_set_env_attr/[5, 6]
%%
%% Sets attributes that govern aspects of environments. The supported
%% attributes are listed under sql_get_env_attr/[4, 5].
%%
%% sql_set_env_attr(Server, RefEnvHandle, Attr, Value, BufType) ->
%% sql_set_env_attr(Server, RefEnvHandle, Attr, Value, BufType, Timeout) ->
%%   Result
%%
%% Server -> pid() |                      ; The pid of the server,
%%           Name |                         a registered name,
%%           {global, Name} |               a globally registered name, or
%%           {Name, Node}                   a registered name on a remote node.
%% RefEnvHandle -> term()                 ; Reference to the environment handle.
%% Attr -> integer()                      ; One of the attributes described under
%%                                          sql_get_env_attr/[4, 5].
%% Value -> string() |                    ; The new attribute value.
%%          integer()
%% BufType -> ?SQL_C_CHAR |               ; The buffer type. Either a
%%            ?SQL_C_ULONG                  (null-terminated) tring, an ODBC defined
%%                                          attribute of integer type, or a driver-
%%                                          specific attribute of integer type
%%                                          (which also has a subtype).
%%   IntType -> ?SQL_IS_UINTEGER |        ; Subtype for driver-specific
%%              ?SQL_IS_INTEGER             integer attributes.
%% Timeout -> integer() |                 ; Max time (ms) for serving the request.
%%            infinity
%% Result -> ?SQL_SUCCESS |               ; Result macro.
%%           ?SQL_SUCCESS_WITH_INFO |
%%           ?SQL_INVALID_HANDLE |
%%           ?SQL_ERROR
%%

sql_set_env_attr(Server, RefEnvHandle, Attr, Value, BufType) ->
    sql_set_env_attr(Server, RefEnvHandle, Attr, Value, BufType, ?DefaultTimeout).

sql_set_env_attr(Server, RefEnvHandle, Attr, Value, BufType, Timeout) ->
    chk_server1(Server,
		sql_set_env_attr,
		[Server, RefEnvHandle, Attr, Value, BufType, Timeout],
		1),
    chk_Z(Attr,
	  sql_set_env_attr,
	  [Server, RefEnvHandle, Attr, Value, BufType, Timeout],
	  3),
    chk_buf_c_type3(BufType,
		    sql_set_env_attr,
		    [Server, RefEnvHandle, Attr, Value, BufType, Timeout],
		    5),
    chk_value({Value, BufType},
	      sql_set_env_attr,
	      [Server, RefEnvHandle, Attr, Value, BufType, Timeout],
	      4),
    chk_timeout(Timeout,
		sql_set_env_attr,
		[Server, RefEnvHandle, Attr, Value, BufType, Timeout],
		6),
    case gen_server:call(Server,
			 {basic, sql_set_env_attr, [RefEnvHandle,
						    Attr,
						    Value,
						    BufType]},
			 Timeout)  of
	% Internal error
	{internal_error, Info} ->
	    exit({internal_error, Info});
	% gen_server specific error
	{error, Reason} ->
	    exit(Reason);
	% Bad input data
	{badref, _ArgNo} ->
	    exit({badarg,
		  ?MODULE,
		  sql_set_env_attr,
		  [Server, RefEnvHandle, Attr, Value, BufType, Timeout],
		  2,
		  {"Bad handle reference", RefEnvHandle}});
	% Server going down
	{stopped, Reason} ->
	    exit({stopped, Reason});
	% Succeeded in making ODBC call (which may have returned an error)
	Other ->
	    Other
    end.




%%----------------------------------------------------------------
%% alloc_buffer/[3, 4]
%%
%% Allocates a deferred data buffer and an associated length/indicator buffer.
%%
%% alloc_buffer(Server, BufCType, Size) ->
%% alloc_buffer(Server, BufCType, Size, Timeout) ->
%%   {ok, RefBuf}
%%
%% Server -> pid() |                      ; The pid of the server,
%%           Name |                         a registered name,
%%           {global, Name} |               a globally registered name, or
%%           {Name, Node}                   a registered name on a remote node.
%% BufCType -> ?SQL_C_CHAR |              ; The C data type of the buffer.
%%             ?SQL_C_BINARY
%% Size -> integer()                      ; The buffer size.
%% Timeout -> integer() |                 ; Max time (ms) for serving the request.
%%            infinity
%% RefBuf -> term()                       ; A handle to the buffer.
%%
alloc_buffer(Server, BufCType, Size) ->
    alloc_buffer(Server, BufCType, Size, ?DefaultTimeout).

alloc_buffer(Server, BufCType, Size, Timeout) ->
    chk_server1(Server, alloc_buffer, [Server, BufCType, Size, Timeout], 1),
    chk_buf_c_type(BufCType, alloc_buffer, [Server, BufCType, Size, Timeout], 2),
    chk_Z_plus(Size, alloc_buffer, [Server, BufCType, Size, Timeout], 3),
    chk_timeout(Timeout, alloc_buffer, [Server, BufCType, Size, Timeout], 4),
    case gen_server:call(Server, {alloc_buffer, BufCType, Size}, Timeout) of
	% Internal error
	{internal_error, Info} ->
	    exit({internal_error, Info});
	% Server going down
	{stopped, Reason} ->
	    exit({stopped, Reason});
	% Succeeded in making IDL call (which may have returned an error)
	Other ->
	    Other
    end.



%%----------------------------------------------------------------
%% dealloc_buffer/[2, 3]
%%
%% Deallocates a deferred data buffer and the associated length/indicator buffer.
%%
%% dealloc_buffer(Server, RefBuf) ->
%% dealloc_buffer(Server, RefBuf, Timeout) ->
%%   ok
%%
%% Server -> pid() |                      ; The pid of the server,
%%           Name |                         a registered name,
%%           {global, Name} |               a globally registered name, or
%%           {Name, Node}                   a registered name on a remote node.
%% RefBuf -> term()                       ; A handle to the buffer.
%% Timeout -> integer() |                 ; Max time (ms) for serving the request.
%%            infinity
%%

dealloc_buffer(Server, RefBuf) ->
    dealloc_buffer(Server, RefBuf, ?DefaultTimeout).

dealloc_buffer(Server, RefBuf, Timeout) ->
    chk_server1(Server, dealloc_buffer, [Server, RefBuf, Timeout], 1),
    chk_timeout(Timeout, dealloc_buffer, [Server, RefBuf, Timeout], 3),
    case gen_server:call(Server, {dealloc_buffer, RefBuf}, Timeout) of
	% Internal error
	{internal_error, Info} ->
	    exit({internal_error, Info});
	% gen_server specific error
	{error, Reason} ->
	    exit(Reason);
	% Bad input data
	{badref, _ArgNo} ->
	    exit({badarg,
		  ?MODULE,
		  dealloc_buffer,
		  [Server, RefBuf, Timeout],
		  2,
		  {"Bad buffer reference", RefBuf}});
	% Server going down
	{stopped, Reason} ->
	    exit({stopped, Reason});
	% Succeeded in making IDL call (which may have returned an error)
	Other ->
	    Other
    end.



%%----------------------------------------------------------------
%% read_buffer/[2, 3]
%%
%% Returns the contents of a deferred data buffer and its associated
%% length/indicator buffer.
%%
%% read_buffer(Server, RefBuf) ->
%% read_buffer(Server, RefBuf, Timeout) ->
%%   {ok, {Value, LenInd}}
%%
%% Server -> pid() |                      ; The pid of the server,
%%           Name |                         a registered name,
%%           {global, Name} |               a globally registered name, or
%%           {Name, Node}                   a registered name on a remote node.
%% RefBuf -> term()                       ; A handle to the buffer.
%% Timeout -> integer() |                 ; Max time (ms) for serving the request.
%%            infinity
%% Value -> string() |                    ; Contents of the buffer associated with
%%          binary()                        RefBuf. The type is determined by the
%%                                          type chosen for RefBuf when it was
%%                                          allocated.
%% LenInd -> integer() |                  ; Length/indicator value associated with
%%           ?SQL_NULL_DATA |               RefBuf.
%%           ?SQL_NO_TOTAL
%%

read_buffer(Server, RefBuf) ->
    read_buffer(Server, RefBuf, ?DefaultTimeout).

read_buffer(Server, RefBuf, Timeout) ->
    chk_server1(Server, read_buffer, [Server, RefBuf, Timeout], 1),
    chk_timeout(Timeout, read_buffer, [Server, RefBuf, Timeout], 3),
    case gen_server:call(Server, {read_buffer, RefBuf}, Timeout) of
	% Internal error
	{internal_error, Info} ->
	    exit({internal_error, Info});
	% gen_server specific error
	{error, Reason} ->
	    exit(Reason);
	% Bad input data
	{badref, _ArgNo} ->
	    exit({badarg,
		  ?MODULE,
		  read_buffer,
		  [Server, RefBuf, Timeout],
		  2,
		  {"Bad buffer reference", RefBuf}});
	% Server going down
	{stopped, Reason} ->
	    exit({stopped, Reason});
	% Succeeded in making IDL call (which may have returned an error)
	Other ->
	    Other
    end.



%%----------------------------------------------------------------
%% write_buffer/[3, 4]
%%
%% Returns the contents of a deferred data buffer and its associated
%% length/indicator buffer.
%%
%% NOTE: The callback checks that data is type consistent
%%       with the data buffer. However, it is not checked
%%       that character data contains ASCII characters only --
%%       just that it is a list.
%%
%% write_buffer(Server, RefBuf, Data) ->
%% write_buffer(Server, RefBuf, Data, Timeout) ->
%%   ok
%%
%% Server -> pid() |                      ; The pid of the server,
%%           Name |                         a registered name,
%%           {global, Name} |               a globally registered name, or
%%           {Name, Node}                   a registered name on a remote node.
%% RefBuf -> term()                       ; A handle to the buffer.
%% Data -> {Value, LenInd}                ; Data to write to buffers.
%%   Value -> string() |                  ; Value to write to the buffer
%%            binary()                      associated with RefBuf.
%%   LenInd -> integer() |
%%             ?SQL_NULL_DATA |
%%             ?SQL_NTS |
%%             ?SQL_DATA_AT_EXEC |
%%             {sql_len_data_at_exec, L} |
%%             no_len_ind
%% Timeout -> integer() |                 ; Max time (ms) for serving the request.
%%            infinity
%% Value -> string() |                    ; Contents of the buffer associated with
%%          binary()                        RefBuf. The type is determined by the
%%                                          type chosen for RefBuf when it was
%%                                          allocated.
%% LenInd -> integer() |                  ; Length/indicator value associated with
%%           ?SQL_NULL_DATA |               RefBuf.
%%           ?SQL_NO_TOTAL
%%

write_buffer(Server, RefBuf, Data) ->
    write_buffer(Server, RefBuf, Data, ?DefaultTimeout).

write_buffer(Server, RefBuf, Data, Timeout) ->
    chk_server1(Server, write_buffer, [Server, RefBuf, Data, Timeout], 1),
    chk_buf_data(Data, write_buffer, [Server, RefBuf, Data, Timeout], 3),
    chk_timeout(Timeout, write_buffer, [Server, RefBuf, Data, Timeout], 4),
    case gen_server:call(Server, {write_buffer, RefBuf, Data}, Timeout) of
	% Internal error
	{internal_error, Info} ->
	    exit({internal_error, Info});
	% gen_server specific error
	{error, Reason} ->
	    exit(Reason);
	% Bad input data
	{baddata, _ArgNo} ->
	    exit({badarg,
		  ?MODULE,
		  write_buffer,
		  [Server, RefBuf, Data, Timeout],
		  3,
		  {"Wrong type", Data}});
	% Bad input data
	{badref, _ArgNo} ->
	    exit({badarg,
		  ?MODULE,
		  write_buffer,
		  [Server, RefBuf, Data, Timeout],
		  2,
		  {"Bad buffer reference", RefBuf}});
	% Server going down
	{stopped, Reason} ->
	    exit({stopped, Reason});
	% Succeeded in making IDL call (which may have returned an error)
	Other ->
	    Other
    end.





%%%----------------------------------------------------------------------
%%% Utility API
%%%
%%% All functionality of the Utility API is also available through
%%% use of functions of the Basic API.
%%%
%%%----------------------------------------------------------------------


%%-----------------------------------------------------
%% init_env/[1, 2]
%%
%% Initialises the ODBC environment on the C node.
%% Equivalent to calling the following Basic API fcns:
%% - sql_alloc_handle
%% - sql_set_env_attr
%%
%% init_env(Server) ->
%% init_env(Server, Timeout) ->
%%    {ok, RefEnvHandle} |
%%    {error, {Fcn, [Reason]}}
%%
%% Server -> pid() |                        ; The pid of the server,
%%           Name |                           a registered name,
%%           {global, Name} |                 a globally registered name, or
%%           {Name, Node}                     a registered name on a remote node.
%% Timeout -> integer() |                   ; Max time (ms) for serving the request.
%%            infinity
%%
%% RefEnvHandle -> term()                   ; Reference to the initialised
%%                                            environment.
%% Fcn -> atom()                            ; The originating function.
%% Reason -> {SqlState, MoreInfo}	    ; An ODBC error tuple.
%%   SqlState -> string()                   ; The SQL state
%%   MoreInfo -> {NativeCode, Msg, LenMsg}  ; More error info.
%%     NativeCode -> string()               ; Data source specific error code.
%%     Msg -> string()                      ; Error message.
%%     LenMsg -> integer()                  ; Length of Msg.
%%
init_env(Server) ->
    init_env(Server, ?DefaultTimeout).

init_env(Server, Timeout) ->
    chk_server1(Server, init_env, [Server, Timeout], 1),
    chk_timeout(Timeout, init_env, [Server, Timeout], 2),
    case gen_server:call(Server, {utility, init_env_impl, []}, Timeout) of
	% Internal error
	{internal_error, Info} ->
	    exit({internal_error, Info});
	% Server going down
	{stopped, Reason} ->
	    exit({stopped, Reason});
	% Succeeded in making ODBC call (which may have returned an error)
	Other ->
	    Other
    end.
    



%% connect/[3, 4, 5, 6]
%%
%% Opens a connection to a data source. There can be only one open data source 
%% connection per server. connect/[3, 4] is used when the information that can be 
%% supplied through connect/[5, 6] does not suffice.
%% Equivalent to calling the following Basic API fcns:
%% - sql_alloc_connect
%% - sql_connect or sql_driver_connect
%% - sql_get_diag_rec
%%
%% connect(Server, RefEnvHandle, ConnectStr) ->
%% connect(Server, RefEnvHandle, ConnectStr, Timeout) ->
%% connect(Server, RefEnvHandle, DSN, UID, PWD) ->
%% connect(Server, RefEnvHandle, DSN, UID, PWD, Timeout) ->
%%   {ok, RefConn} |
%%   {error, {Fcn, [Reason]}}
%%
%% Server -> pid() |                        ; The pid of the server,
%%           Name |                           a registered name,
%%           {global, Name} |                 a globally registered name, or
%%           {Name, Node}                     a registered name on a remote node.
%% RefEnvHandle -> term()                   ; Reference to the initialised
%%                                            environment.
%% ConnectStr -> string()                   ; 
%% Timeout -> integer() |                   ; Max time (ms) for serving the request.
%%            infinity
%% DSN -> string()                          ; Name of the data source.
%% UID -> string()                          ; User ID.
%% PWD -> string()                          ; Password.
%% Fcn -> atom()                            ; The originating function.
%% Reason -> {Fcn, SqlState, MoreInfo}	    ; An ODBC error tuple.
%%   SqlState -> string()                   ; The SQL state
%%   MoreInfo -> {NativeCode, Msg, LenMsg}  ; More error info.
%%     NativeCode -> string()               ; Data source specific error code.
%%     Msg -> string()                      ; Error message.
%%     LenMsg -> integer()                  ; Length of Msg.
%%
connect(Server, RefEnvHandle, ConnectStr) ->
    connect(Server, RefEnvHandle, ConnectStr, ?DefaultTimeout).

connect(Server, RefEnvHandle, ConnectStr, Timeout) ->
    chk_server1(Server, connect, [Server, RefEnvHandle, ConnectStr, Timeout], 1),
    chk_str(ConnectStr, connect, [Server, RefEnvHandle, ConnectStr, Timeout], 3),
    chk_timeout(Timeout, connect, [Server, RefEnvHandle, ConnectStr, Timeout], 4),
    case gen_server:call(Server,
			 {utility, connect_drv_impl, [RefEnvHandle, ConnectStr]},
			 Timeout)   of
	% Bad input
	{error, {badref, _ArgNo}} ->
	    exit({badarg,
		  ?MODULE,
		  connect,
		  [Server, RefEnvHandle, ConnectStr, Timeout],
		  2,
		  {"Bad handle reference", RefEnvHandle}});
	% Internal error
	{internal_error, Info} ->
	    exit({internal_error, Info});
	% Server going down
	{stopped, Reason} ->
	    exit({stopped, Reason});
	% Succeeded in making ODBC call (which may have returned an error)
	Other ->
	    Other
    end.

connect(Server, RefEnvHandle, DSN, UID, PWD) ->
    connect(Server, RefEnvHandle, DSN, UID, PWD, ?DefaultTimeout).

connect(Server, RefEnvHandle, DSN, UID, PWD, Timeout) ->
    chk_server1(Server, connect, [Server, RefEnvHandle, DSN, UID, PWD, Timeout], 1),
    chk_str(DSN, connect, [Server, RefEnvHandle, DSN, UID, PWD, Timeout], 3),
    chk_str(UID, connect, [Server, RefEnvHandle, DSN, UID, PWD, Timeout], 4),
    chk_str(PWD, connect, [Server, RefEnvHandle, DSN, UID, PWD, Timeout], 5),
    chk_timeout(Timeout, connect, [Server, RefEnvHandle, DSN, UID, PWD, Timeout], 6),
    case gen_server:call(Server,
			 {utility, connect_impl, [RefEnvHandle, DSN, UID, PWD]},
			 Timeout)   of
	% Bad input
	{error, {badref, _ArgNo}} ->
	    exit({badarg,
		  ?MODULE,
		  connect,
		  [Server, RefEnvHandle, DSN, UID, PWD, Timeout],
		  2,
		  {"Bad handle reference", RefEnvHandle}});
	% Internal error
	{internal_error, Info} ->
	    exit({internal_error, Info});
	% Server going down
	{stopped, Reason} ->
	    exit({stopped, Reason});
	% Succeeded in making ODBC call (which may have returned an error)
	Other ->
	    Other
    end.




%% execute_stmt/[3, 4]
%%
%% Executes a single SQL statement. All changes to the data source are
%% automatically committed if successful.
%% Equivalent to calling the following Basic API fcns:
%% - sql_alloc_stmt
%% - sql_exec_direct
%% - sql_num_result_cols
%% - sql_row_count
%% - sql_end_tran
%% - sql_describe_col
%% - alloc_buffer
%% - sql_bind_col
%% - sql_fetch
%% - read_buffer
%% - sql_free_handle
%% - dealloc_buffer
%% - sql_get_diag_rec
%%
%% execute_stmt(Server, RefConnHandle, Stmt) ->
%% execute_stmt(Server, RefConnHandle, Stmt, Timeout) ->
%%   {updated, NRows} |
%%   {selected, [ColName], [Row]}
%%   {error, {Fcn, [Reason]}}
%%
%% Server -> pid() |                        ; The pid of the server,
%%           Name |                           a registered name,
%%           {global, Name} |                 a globally registered name, or
%%           {Name, Node}                     a registered name on a remote node.
%% RefConnHandle -> term()                  ; Reference to an open connection.
%% Stmt -> string()                         ; SQL statement to execute.
%% Timeout -> integer() |                   ; Max time (ms) for serving the request.
%%            infinity
%% NRows -> integer()                       ; The number of updated rows.
%% ColName -> string()                      ; The name of a column in the resulting
%%                                            table.
%% Row -> [Value]                           ; One row of the resulting table.
%%   Value -> string                        ; One value in a row.
%% Fcn -> atom()                            ; The originating function.
%% Reason -> {Fcn, SqlState, MoreInfo}	    ; An ODBC error tuple.
%%   SqlState -> string()                   ; The SQL state
%%   MoreInfo -> {NativeCode, Msg, LenMsg}  ; More error info.
%%     NativeCode -> string()               ; Data source specific error code.
%%     Msg -> string()                      ; Error message.
%%     LenMsg -> integer()                  ; Length of Msg.
%%

execute_stmt(Server, RefConnHandle, Stmt) ->
    execute_stmt(Server, RefConnHandle, Stmt, ?DefaultTimeout).

execute_stmt(Server, RefConnHandle, Stmt, Timeout) ->
    chk_server1(Server, execute_stmt, [Server, RefConnHandle, Stmt, Timeout], 1),
    chk_str(Stmt, execute_stmt, [Server, RefConnHandle, Stmt, Timeout], 3),
    chk_timeout(Timeout, execute_stmt, [Server, RefConnHandle, Stmt, Timeout], 4),
    case gen_server:call(Server,
			 {utility, execute_stmt_impl, [RefConnHandle, Stmt]},
			 Timeout)   of
	% Bad input
	{error, {badref, _ArgNo}} ->
	    exit({badarg,
		  ?MODULE,
		  execute_stmt,
		  [Server, RefConnHandle, Stmt, Timeout],
		  2,
		  {"Bad handle reference", RefConnHandle}});
	% Internal error
	{internal_error, Info} ->
	    exit({internal_error, Info});
	% Server going down
	{stopped, Reason} ->
	    exit({stopped, Reason});
	% Succeeded in making ODBC call (which may have returned an error)
	Other ->
	    Other
    end.



%% disconnect/[2, 3]
%%
%% Closes the connection to a data source.
%% Equivalent to calling the following Basic API fcns:
%% - sql_disconnect
%% - sql_free_handle
%% - sql_get_diag_rec
%%
%% disconnect(Server, RefConnHandle) ->
%% disconnect(Server, RefConnHandle, Timeout) ->
%%   ok |
%%   {error, {Fcn, [Reason]}}
%%
%% Server -> pid() |                        ; The pid of the server,
%%           Name |                           a registered name,
%%           {global, Name} |                 a globally registered name, or
%%           {Name, Node}                     a registered name on a remote node.
%% RefConnHandle -> term()                  ; Reference to an open connection.
%% Timeout -> integer() |                   ; Max time (ms) for serving the request.
%%            infinity
%% Fcn -> atom()                            ; The originating function.
%% Reason -> {Fcn, SqlState, MoreInfo}	    ; An ODBC error tuple.
%%   SqlState -> string()                   ; The SQL state
%%   MoreInfo -> {NativeCode, Msg, LenMsg}  ; More error info.
%%     NativeCode -> string()               ; Data source specific error code.
%%     Msg -> string()                      ; Error message.
%%     LenMsg -> integer()                  ; Length of Msg.
%%

disconnect(Server, RefConnHandle) ->
    disconnect(Server, RefConnHandle, ?DefaultTimeout).

disconnect(Server, RefConnHandle, Timeout) ->
    chk_server1(Server, disconnect, [Server, RefConnHandle, Timeout], 1),
    chk_timeout(Timeout, disconnect, [Server, RefConnHandle, Timeout], 3),
    case gen_server:call(Server,
			 {utility, disconnect_impl, [RefConnHandle]},
			 Timeout)   of
	% Bad input
	{error, {badref, _ArgNo}} ->
	    exit({badarg,
		  ?MODULE,
		  disconnect,
		  [Server, RefConnHandle, Timeout],
		  2,
		  {"Bad handle reference", RefConnHandle}});
	% Internal error
	{internal_error, Info} ->
	    exit({internal_error, Info});
	% Server going down
	{stopped, Reason} ->
	    exit({stopped, Reason});
	% Succeeded in making ODBC call (which may have returned an error)
	Other ->
	    Other
    end.




%% terminate_env/[2, 3]
%%
%% Cleans up the ODBC environment on the C node. This can also be done
%% through use of functions of the Basic API.
%% Equivalent to calling the following Basic API fcns:
%% - sql_free_handle
%% - sql_get_diag_rec
%%
%% terminate_env(Server, RefEnvHandle) ->
%% terminate_env(Server, RefEnvHandle, Timeout) ->
%%   ok |
%%   {error, {Fcn, [Reason]}}
%%
%% Server -> pid() |                        ; The pid of the server,
%%           Name |                           a registered name,
%%           {global, Name} |                 a globally registered name, or
%%           {Name, Node}                     a registered name on a remote node.
%% RefEnvHandle -> term()                   ; Reference to the initialised
%%                                            environment.
%% Timeout -> integer() |                   ; Max time (ms) for serving the request.
%%            infinity
%% Fcn -> atom()                            ; The originating function.
%% Reason -> {Fcn, SqlState, MoreInfo}	    ; An ODBC error tuple.
%%   SqlState -> string()                   ; The SQL state
%%   MoreInfo -> {NativeCode, Msg, LenMsg}  ; More error info.
%%     NativeCode -> string()               ; Data source specific error code.
%%     Msg -> string()                      ; Error message.
%%     LenMsg -> integer()                  ; Length of Msg.
%%

terminate_env(Server, RefEnvHandle) ->
    terminate_env(Server, RefEnvHandle, ?DefaultTimeout).

terminate_env(Server, RefEnvHandle, Timeout) ->
    chk_server1(Server, terminate_env, [Server, RefEnvHandle, Timeout], 1),
    chk_timeout(Timeout, terminate_env, [Server, RefEnvHandle, Timeout], 3),
    case gen_server:call(Server,
			 {utility, terminate_env_impl, [RefEnvHandle]},
			 Timeout)   of
	% Bad input
	{error, {badref, _ArgNo}} ->
	    exit({badarg,
		  ?MODULE,
		  terminate_env,
		  [Server, RefEnvHandle, Timeout],
		  2,
		  {"Bad handle reference", RefEnvHandle}});
	% Internal error
	{internal_error, Info} ->
	    exit({internal_error, Info});
	% Server going down
	{stopped, Reason} ->
	    exit({stopped, Reason});
	% Succeeded in making ODBC call (which may have returned an error)
	Other ->
	    Other
    end.




%%%----------------------------------------------------------------------
%%% Miscellaneous API fcns
%%%----------------------------------------------------------------------

%% sql_func_exists/2
%%
%% Tells if an ODBC fcn is supported or not.
%% Returns ?SQL_TRUE or ?SQL_FALSE.
%% Replaces the SQL_FUNC_EXISTS macro of ODBC.
%% The implementation is taken from the DataDirect Connect ODBC
%% macro definition.
%%
%% sql_func_exists(Exists, Func) ->
%%   boolean()
%%
%% Exists -> list()    ; List representing the existance of all ODBC fcns.
%% Func -> integer()   ; A reference to an ODBC fcn. (supported through macros).
%%
%sql_func_exists(Exists, Func) when list(Exists), integer(Func) ->
%    N = Func bsr 4,
%    case (lists:nth(N, Exists) band (1 bsl (Func band 16#F))) of
%	Result when Result >= ?SQL_TRUE ->
%	    ?SQL_TRUE;
%	_Other ->
%	    ?SQL_FALSE
%    end.




%% bitmask_is_set/2
%%
%% Tells if the bit corresponding to a Mask is set in a value or not.
%% (Mask selects the interesting bit from Value.)
%%
%% bitmask_is_set(Mask, Value) ->
%%   boolean()
%%
%% Mask -> integer()     ; Has a single 1 in the position of interest.
%% Value -> integer()    ; Holds values for all possible masks for an item.
%%
bitmask_is_set(Mask, Value) when integer(Mask), integer(Value) ->
    if
	Mask band Value >= 1 ->
	    ?SQL_TRUE;
	true ->
	    ?SQL_FALSE
    end.




%% display_size/2
%%
%% Returns the buffer size needed for storing table values,
%% in character or binary format, from a column with a certain
%% column size and SQL type. The fcn calculates the display size
%% (see ODBC manual, appendix D (Data Types, Display Size) and,
%% when applicable, adds 1 to allow room for null termination.
%% If the column size is not available (?SQL_NO_TOTAL) the returned
%% value is the default maximum string length.
%% The arguments to the function are values returned by sql_describe_col.
%%
%% display_size(SqlType, ColumnSize) ->
%%   integer()
%%
%% SqlType -> integer()     ; The ODBC SQL data type (returned by sql_describe_col).
%% ColumnSize -> integer()  ; The column size (returned by sql_describe_col).
%%
display_size(SqlType, ColumnSize) when integer(SqlType), integer(ColumnSize) ->
    display_size1(SqlType, ColumnSize).

display_size1(_SqlType, ?SQL_NO_TOTAL) ->
    ?DefaultMaxLenStr;
display_size1(?SQL_CHAR, ColSize) ->
    ColSize + 1;  % Add null termination.
display_size1(?SQL_VARCHAR, ColSize) ->
    ColSize + 1;  % Add null termination.
display_size1(?SQL_LONGVARCHAR, ColSize) ->
    ColSize + 1;  % Add null termination.
display_size1(?SQL_DECIMAL, ColSize) ->
    ColSize + 3;  % Add sign, decimal point, and null termination.
display_size1(?SQL_NUMERIC, ColSize) ->
    ColSize + 3;  % Add sign, decimal point, and null termination.
display_size1(?SQL_SMALLINT, _ColSize) ->
    6 + 1;  % Add null termination.
display_size1(?SQL_INTEGER, _ColSize) ->
    11 + 1;  % Add null termination.
display_size1(?SQL_REAL, _ColSize) ->
    13 + 1;   % Add null termination.
display_size1(?SQL_FLOAT, _ColSize) ->
    22 + 1;  % Add null termination.
display_size1(?SQL_DOUBLE, _ColSize) ->
    22 + 1;  % Add null termination.
display_size1(?SQL_BIT, _ColSize) ->
    1 + 1;  % Add null termination.
display_size1(?SQL_TINYINT, _ColSize) ->
    4 + 1;  % Add null termination.
display_size1(?SQL_BIGINT, _ColSize) ->
    20 + 1;  % Add null termination.
display_size1(?SQL_BINARY, ColSize) ->
    2*ColSize;
display_size1(?SQL_VARBINARY, ColSize) ->
    2*ColSize;
display_size1(?SQL_LONGVARBINARY, ColSize) ->
    2*ColSize;
display_size1(?SQL_TYPE_DATE, _ColSize) ->
    10 + 1;  % Add null termination.
display_size1(?SQL_TYPE_TIME, ColSize) ->
    ColSize + 1;  % Add null termination.
display_size1(?SQL_TYPE_TIMESTAMP, ColSize) ->
    ColSize + 1;  % Add null termination.
display_size1(?SQL_INTERVAL_MONTH, ColSize) ->
    ColSize + 1;  % Add null termination.
display_size1(?SQL_INTERVAL_YEAR, ColSize) ->
    ColSize + 1;  % Add null termination.
display_size1(?SQL_INTERVAL_YEAR_TO_MONTH, ColSize) ->
    ColSize + 1;  % Add null termination.
display_size1(?SQL_INTERVAL_DAY, ColSize) ->
    ColSize + 1;  % Add null termination.
display_size1(?SQL_INTERVAL_HOUR, ColSize) ->
    ColSize + 1;  % Add null termination.
display_size1(?SQL_INTERVAL_MINUTE, ColSize) ->
    ColSize + 1;  % Add null termination.
display_size1(?SQL_INTERVAL_SECOND, ColSize) ->
    ColSize + 1;  % Add null termination.
display_size1(?SQL_INTERVAL_DAY_TO_HOUR, ColSize) ->
    ColSize + 1;  % Add null termination.
display_size1(?SQL_INTERVAL_DAY_TO_MINUTE, ColSize) ->
    ColSize + 1;  % Add null termination.
display_size1(?SQL_INTERVAL_DAY_TO_SECOND, ColSize) ->
    ColSize + 1;  % Add null termination.
display_size1(?SQL_INTERVAL_HOUR_TO_MINUTE, ColSize) ->
    ColSize + 1;  % Add null termination.
display_size1(?SQL_INTERVAL_HOUR_TO_SECOND, ColSize) ->
    ColSize + 1;  % Add null termination.
display_size1(?SQL_INTERVAL_MINUTE_TO_SECOND, ColSize) ->
    ColSize + 1.  % Add null termination.




%%%----------------------------------------------------------------------
%%% Debug fcns
%%%----------------------------------------------------------------------

%% Returns the internal state in a tagged tuple list.
%%
get_state(Server) ->
    State = gen_server:call(Server, get_state),
    [{cnode, State#state.cnode},
     {port, State#state.port},
     {max_len_data, State#state.max_len_data},
     {max_len_err_msg, State#state.max_len_err_msg},
     {max_len_str, State#state.max_len_str},
     {handles, State#state.handles},
     {buffers, State#state.buffers},
     {conn_data, State#state.conn_data}].





%%%======================================================================
%%% Callback functions from gen_server
%%%======================================================================

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%----------------------------------------------------------------------
%%
%% Creates command line arguments needed to start the C node.
%% Then starts the C node and checks that it is running. The
%% C node is started through a port in line-mode, which makes
%% it possible to spawn the C node as a separate process and
%% still synchronise on getting a reply.
%% init also links to the client process.
%%
init(Args) ->
    process_flag(trap_exit, true),

    % Link to the client.
    {value, {client, Client}} = lists:keysearch(client, 1, Args),
    link(Client),

    % Create the buffer length argument for the C node
    % from data in Args or the default value.
    BufArg =
	case lists:keysearch(buffer_size, 1, Args) of
	    {value, {buffer_size, BufSz}} ->
		" -bufsz " ++ integer_to_list(BufSz);
	    false ->
		" -bufsz " ++ ?DefaultBufSize
	end,

    % Decide max length for returned table data (Utility API only).
    MaxLenData =
	case lists:keysearch(max_len_data, 1, Args) of
	    {value, {max_len_data, MaxLenD}} ->
		MaxLenD;
	    false ->
		?DefaultMaxLenData
	end,

    % Decide max length for error messages (Utility API only).
    MaxLenErrMsg =
	case lists:keysearch(max_len_err_msg, 1, Args) of
	    {value, {max_len_err_msg, MaxLenE}} ->
		MaxLenE;
	    false ->
		?DefaultMaxLenErrMsg
	end,

    % Decide max length for other strings from ODBC (e.g. column names).
    % (Utility API only).
    MaxLenStr =
	case lists:keysearch(max_len_str, 1, Args) of
	    {value, {max_len_str, MaxLenS}} ->
		MaxLenS;
	    false ->
		?DefaultMaxLenStr
	end,

    % Create the cookie argument for the C node
    % from the cookie of this node.
    CookieArg = " -cookie " ++ atom_to_list(erlang:get_cookie()),

    % Create the connected pid argument for the C node
    % from self() as a string of the characters in the
    % external erlang term representation.
    EPidStr = binary_to_list(term_to_binary(self())),
    EPidArg = lists:flatten(io_lib:format(" -erlpid ~w", [EPidStr])),

    % Create the name argument for the C node 
    % from node() and now(). It will be unique
    % in the sense that it can't be generated
    % again this way. This ensures that no
    % name collisions will occur as long as
    % no nodes are started with the same name
    % by coincidence (VERY unlikely!).
    [NodeStr1, HostStr] = string:tokens(atom_to_list(node()), "@"),
    NodeStr2 = string:tokens(lists:flatten(io_lib:format("~w", [now()])),
			     ",{}"),                 % Leave only digits
    CNodeStr = lists:flatten([NodeStr1, NodeStr2]),  % Concatenate and flatten.
    CNodeArg = " -node " ++ CNodeStr,
    HostArg = " -host " ++ HostStr,

    % Start the C node
    case os:find_executable(?ServerProg, ?ServerDir) of
	FileName when list(FileName) ->
	    NativeFileName = filename:nativename(FileName),
	    Command =  NativeFileName ++ CNodeArg ++ HostArg ++
			      BufArg ++ CookieArg ++ EPidArg,
	    Port = open_port({spawn,Command},
			     [{line, 256}, in, exit_status]),

    % Check if the C node is running
    % and create return value accordingly.
	    Return =
		case get_port_reply(Port) of
		    ok ->  % C node is running.
			% Create a reference to the connected C node.
			CServerRef =
			    {odbc, list_to_atom(CNodeStr ++ "@" ++ HostStr)},

			% Initial state.
			{ok, #state{cnode = CServerRef,
				    port = Port,
				    max_len_data = MaxLenData,
				    max_len_err_msg = MaxLenErrMsg,
				    max_len_str = MaxLenStr,
				    handles = [],
				    buffers = []}};

		    {error, Reason} ->  % C node failed to start
			{stop, {no_c_node, Reason}}
		end;

% This code is for debugging purposes only.
% It prints the command line arguments for the C node, so that it can
% be started from the shell or a debugger.
% Comment out the code from the comment 'Start the C node' above to this code,
% section and use this part instead.
%
%    % Start the C node
%    case os:find_executable(?ServerProg, ?ServerDir) of
%	FileName when list(FileName) ->
%	    ok = io:format("Cserver start: ~s -erlpid ~p~n", [FileName ++ CNodeArg ++ HostArg ++ BufArg ++ CookieArg, "[," ++ lists:delete($],lists:delete($[,lists:flatten(io_lib:format("~w",[EPidStr])))) ++ ",]"]),
%	    CServerRef =
%		{odbc, list_to_atom(CNodeStr ++ "@" ++ HostStr)},

%			% Initial state.
%	    {ok, #state{cnode = CServerRef,
%			max_len_data = MaxLenData,
%			max_len_err_msg = MaxLenErrMsg,
%			max_len_str = MaxLenStr,
%			handles = [],
%			buffers = []}};

	false ->
	    {stop, {no_c_node, "Can't find executable."}}
    end.
		    



%%----------------------------------------------------------------------
%% Func: handle_call/3
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------

%%---------------------------------------------------------------------
%% Start and Stop Call Backs
%%---------------------------------------------------------------------

% stop
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};




%%---------------------------------------------------------------------
%% Basic API Call Backs
%%---------------------------------------------------------------------

% Most of the Basic API calls are handled here, since
% they share a pattern:
% - Modify the arguments by replacing references to handles and buffers
%   with the actual handles/buffers, and add the c-node "adress".
% - Call the IDL fcn.
% - Create a reply and a new state.
%
% NOTE: The buffer handling fcns are handled in separate clauses,
%       since they don't share the pattern described above.
%
handle_call({basic, Operation, UserArgs}, _From, State) ->
    case prepare_args(Operation, UserArgs, State) of
	% Args OK, continue.
	{ok, Args} ->
	    case catch apply(odbclli, Operation, Args) of
		Result ->
		    handle_result(Operation, Args, Result, State)
	    end;
	% Error (non-terminal).
	{error, Reply} ->
	    {reply, Reply, State}
    end;




% alloc_buffer
handle_call({alloc_buffer, BufCType, Size}, _From, State) ->
    case catch do_alloc_buffer(BufCType, Size, State) of
	% Returned value, or thrown error (non-terminal).
	{reply, Reply, State1} ->
	    {reply, Reply, State1};
	% Thrown error (terminal).
	{stop, Reason, Reply, State1} ->
	    {stop, Reason, Reply, State1};
	% Real exception from gen_server or c-node.
	% The server will terminate later only if the exception
	% killed the c-node.
	Other ->
	    {reply, {internal_error, Other}, State}
    end;




% dealloc_buffer
handle_call({dealloc_buffer, RefBufData}, _From, State) ->
    case catch do_dealloc_buffer(RefBufData, State) of
	% Returned value, or thrown error (non-terminal).
	{reply, Reply, State1} ->
	    {reply, Reply, State1};
	% Thrown error (terminal).
	{stop, Reason, Reply, State1} ->
	    {stop, Reason, Reply, State1};
	% Real exception from gen_server or c-node.
	% The server will terminate later only if the exception
	% killed the c-node.
	Other ->
	    {reply, {internal_error, Other}, State}
    end;




% read_buffer
handle_call({read_buffer, RefBufData}, _From, State) ->
    case catch do_read_buffer(RefBufData, State) of
	% Returned value, or thrown error (non-terminal).
	{reply, Reply, State1} ->
	    {reply, Reply, State1};
	% Thrown error (terminal).
	{stop, Reason, Reply, State1} ->
	    {stop, Reason, Reply, State1};
	% Real exception from gen_server or c-node.
	% The server will terminate later only if the exception
	% killed the c-node.
	Other ->
	    {reply, {internal_error, Other}, State}
    end;




% write_buffer
handle_call({write_buffer, RefBufData, Data}, _From, State) ->
    case catch do_write_buffer(RefBufData, Data, State) of   % Errors are thrown.
	% Returned value, or thrown error (non-terminal).
	{reply, Reply, State1} ->
	    {reply, Reply, State1};
	% Thrown error (terminal).
	{stop, Reason, Reply, State1} ->
	    {stop, Reason, Reply, State1};
	% Real exception from gen_server or c-node.
	% The server will terminate later only if the exception
	% killed the c-node.
	Other ->
	    {reply, {internal_error, Other}, State}
    end;




%%---------------------------------------------------------------------
%% Utility API Call Back
%%---------------------------------------------------------------------

% The Utility API is implemented through a single, general,
% handle_call-clause. This clause dispatches the call
% (through apply) to the specific implementation fcn.
% Should the call fail, the handle_call-clause tries
% to retrieve ODBC errors and recover. A second failure
% here causes the server to terminate.
%
% handle_call(Operation, ...) ->
%   case catch apply(Operation, ...) of
%     ok ->
%       {reply, Reply, State};
%     error ->
%       {reply, Error, State};
%     {error, {nohandle, {restore, Data}}} ->
%       if restore(Operation, Data, ...) of
%         ok ->
%           {reply, Error, State};
%         _ ->
%           {stop, Reason, Reply, State}
%       end;
%     {error, {HandleData, norestore}} ->
%       if get_odbc_errors(...) of
%         ok ->
%           {reply, Error, State};
%         _ ->
%           {stop, Reason, Reply, State}
%       end;
%       {reply, Error, State};
%     {error, {HandleData, {restore, Data}}} ->
%       if get_odbc_errors(...), restore(Operation, Data, ...) of
%         ok ->
%           {reply, Error, State};
%         _ ->
%           {stop, Reason, Reply, State}
%       end;
%       {reply, Error, State};
%     Other ->
%       {reply, {error, Other}, State}
%   end;
%
% Should an error occur during restore(...) or get_odbc_errors(...)
% the server returns {stop, ...} instead, since this means that the
% internal state is inconsistent.
%
% The reason for having the top handle_call-clause is that error
% handling (when to do it) is identical for all Utility API fcns.
% Restoration, which could be individual to API fcns, may be handled by
% separate clauses for each Utility fcn. Some restoration tasks that
% are common to several Utility fcns are handled by common clauses.
%
%
% The fcn called by apply (the name of which is constructed in analogy
% with init_env_impl for the interface fcn init_env) calls wrapper fcns
% for the IDL interface fcns. It returns errors using throw/1 to make it
% possible to have the fcn calls in a sequence instead of inside nested
% case-statements.
%
% init_env_impl(State, ...) ->
%   Val1 = case alloc_handle_wrap(...) of
%            {ok, V} ->
%              {...};
%            {error, Info} ->
%              throw({norestore, Info})
%          end,
%   Val2 = case set_env_attr_wrap(...) of
%            {ok, V} ->
%              {...};
%            {error, Info} ->
%              {..., Handle, ...} = Val1,
%              throw({{restore, [{handle, {HandleType, Handle}}]}, Info})
%          end,
%   NewState = ...,
%   RetVal = ...,
%   {ok, RetVal, NewState}.
%
%
% The wrapper fcns simply call the IDL Interface fcns and interpret the
% result (ok/error). Errors are returned (not thrown) from these fcns.
%
%-----------------------------------------------------------------------------

handle_call({utility, Operation, Args}, _From, State) ->
    % Perform Operation
    case catch apply(?MODULE, Operation, [[State | Args]]) of

	% Operation OK
	{ok, {Result, State1}} ->
	    {reply, Result, State1};

	% Operation failed.
	{error, {RecoverData, ReplyData1}} ->
	    case catch recover(Operation, RecoverData, State) of

		% Recover succeded. No ODBC error info, return ReplyData.
		ok ->
		    {reply, {error, ReplyData1}, State};

		% Recover Succeded. Return ODBC error data.
		{ok, Errors} ->
		    {_ErrType, {Fcn, _Args, _RetVal}} = ReplyData1,
		    {reply, {error, {Fcn, Errors}}, State};

		% Recover failed. Stop server.
		% Reply with original and second reason.
		{error, ReplyData2} ->
		    {stop,
		     corrupt_state,
		     {stopped, {ReplyData2, {original_error, ReplyData1}}},
		     State};

		% Exception (not just a thrown error). Terminate.
		Other ->
		    {stop,
		     Other,
		     {stopped, {Other, {original_error, ReplyData1}}},
		     State}
	    end;

	% Exception (not just a thrown error).
	% The server will terminate later only if the exception
	% killed the c-node.
	Other ->
	    {reply, {internal_error, Other}, State}
    end;





%%---------------------------------------------------------------------
%% Debug Call Back
%%---------------------------------------------------------------------

handle_call(get_state, _From, State) ->
    {reply, State, State};


%%-----------------------------------------------------------

% Unknown request
handle_call(Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%----------------------------------------------------------------------


%%----------------------------------------------------------------------
%% Func: handle_cast/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%----------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------

% Exit status received from c-node via port.
% Log status.
%
handle_info({Port, {exit_status, Status}}, State) when State#state.port == Port ->
    ok =
	error_logger:error_report({"ODBC: C-node:", {exit_status, Status}}),
    {noreply, State};

% Other exceptional message received from c-node via port.
% Log message.
%
handle_info({Port, {data, {eol, Info}}}, State) when State#state.port == Port ->
    ok = error_logger:error_report(Info),
    {noreply, State};

% EXIT from port.
% Log exit and terminate.
%
handle_info({'EXIT', Port, Reason}, State) when State#state.port == Port ->
    ok =
	error_logger:error_report({"ODBC: Port:", {'EXIT', Port, Reason}}),
    {stop, {stopped, {'EXIT', Port, Reason}}, State};

% EXIT from c-node.
% Log exit. (Wait for port to terminate before terminating.)
%
handle_info({'EXIT', CNode, Reason}, State) when State#state.cnode == CNode ->
    ok =
	error_logger:error_report({"ODBC: C-node:", {'EXIT', CNode, Reason}}),
    {noreply, State};

% EXIT from other linked process.
% Log exit and terminate.
%
handle_info({'EXIT', Pid, Reason}, State) when pid(Pid) ->
    ok =
	error_logger:error_report({"ODBC: Linked process:", {'EXIT', Pid, Reason}}),
    {stop, {stopped, {'EXIT', Pid, Reason}}, State};

% Other.
%
handle_info(_Info, State) ->
    {noreply, State}.

%%----------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any (ignored by gen_server)
%%----------------------------------------------------------------------
terminate(_Reason, State) ->
    Port = State#state.port,
    Port ! {self(), close}.





%%%======================================================================
%%% Internal functions
%%%======================================================================


%%%------------------------------------------------------------------
%%% Basic API, preparation for IDL call and result handling.
%%%------------------------------------------------------------------


%%--------------------------------------------------
%% prepare_args/3
%%
%% Prepares internal state arguments that are needed for the
%% call to an IDL fcn. All fcns need the C-node. Some may
%% need other data.
%%
%% prepare_args(Operation, UserArgs, State) ->
%%   {ok, Args} |
%%   {error, Reply} |
%%   {stop, Reason, Reply}
%%
%% Operation -> atom()                  ; The operation at hand.
%% UserArgs -> list()                   ; The args supplied by the user for the Op.
%% State -> record(state)               ; The internal state.
%% Args -> list()                       ; Modified list of args.
%% Reply -> term()                      ; Reply to user if an error occurs.
%% Reason -> term()                     ; Reason for termination if necessary.
%%

% sql_alloc_handle
% Allocation of handle other than env-handle (env-handle uses default clause).
% Replace RefHandle with Handle found in internal state.
% Add CNode.
prepare_args(sql_alloc_handle, [?SQL_HANDLE_ENV, RefInputHandle], State) ->
    {ok, [State#state.cnode, ?SQL_HANDLE_ENV, RefInputHandle]};
prepare_args(sql_alloc_handle, [HandleType, RefInputHandle], State)
  when HandleType =/= ?SQL_HANDLE_ENV ->
    Handles = State#state.handles,
    case lists:keysearch(RefInputHandle, 1, Handles) of
	% Valid input handle
	{value, {RefInputHandle, InputHandle}} ->
	    {ok, [State#state.cnode, HandleType, InputHandle]};
	% Invalid input handle
	false ->
	    {error, {badref, 2}}
    end;


% sql_bind_col
% Replace RefStmtHandle with StmtHandle found in internal state.
% Replace RefBuf with data from buf_data record found in internal state.
% Add CNode.
prepare_args(sql_bind_col, [RefStmtHandle, ColNum, ?NULL_REF], State) ->
    CNode = State#state.cnode,
    case lists:keysearch(RefStmtHandle, 1, State#state.handles) of

	% Valid handle reference
	{value, {RefStmtHandle, StmtHandle}} ->
	    {ok,
	     [State#state.cnode, StmtHandle, ColNum, 0, ?NULL_REF, 0, ?NULL_REF]};

	% Invalid handle reference
	false ->
	    {error, {badref, 1}}
    end;

prepare_args(sql_bind_col, [RefStmtHandle, ColNum, RefBuf], State) ->
    CNode = State#state.cnode,
    case lists:keysearch(RefStmtHandle, 1, State#state.handles) of

	% Valid handle reference
	{value, {RefStmtHandle, StmtHandle}} ->
	    case get_buf(RefBuf, State#state.buffers) of
		% Valid buffer reference
		{value, Buf} ->
		    {ok, [State#state.cnode,
			  StmtHandle,
			  ColNum,
			  Buf#buf_data.ctype,
			  Buf#buf_data.buffer,
			  Buf#buf_data.size,
			  Buf#buf_data.len_ind]};
		% Invalid buffer reference
		false ->
		    {error, {badref, 3}}
	    end;

	% Invalid handle reference
	false ->
	    {error, {badref, 1}}
    end;


% sql_close_cursor
% Replace RefStmtHandle with StmtHandle found in internal state.
% Add CNode.
prepare_args(sql_close_cursor, [RefStmtHandle], State) ->
    case lists:keysearch(RefStmtHandle, 1, State#state.handles) of
	% Valid handle reference.
	{value, {RefStmtHandle, StmtHandle}} ->
	    {ok, [State#state.cnode, StmtHandle]};
	% Invalid handle reference.
	false ->
	    {error, {badref, 1}}
    end;



% sql_connect
% Replace RefConnHandle with ConnHandle found in internal state.
% Add CNode.
prepare_args(sql_connect, [RefConnHandle, DSN, UID, Auth], State) ->
    case lists:keysearch(RefConnHandle, 1, State#state.handles) of
	% Valid handle reference.
	{value, {RefConnHandle, ConnHandle}} ->
	    {ok, [State#state.cnode, ConnHandle, DSN, UID, Auth]};
	% Invalid handlee reference.
	false ->
	    {error, {badref, 1}}
    end;



% sql_describe_col
% Replace RefStmtHandle with StmtHandle found in internal state.
% Add CNode.
prepare_args(sql_describe_col, [RefStmtHandle, ColNum, BufLenColName], State) ->
    case lists:keysearch(RefStmtHandle, 1, State#state.handles) of
	% Valid handle reference.
	{value, {RefStmtHandle, StmtHandle}} ->
	    {ok, [State#state.cnode, StmtHandle, ColNum, BufLenColName]};
	% Invalid handle reference.
	false ->
	    {error, {badref, 1}}
    end;



% sql_disconnect
% Replace RefConnHandle with ConnHandle found in internal state.
% Add CNode.
prepare_args(sql_disconnect, [RefConnHandle], State) ->
    case lists:keysearch(RefConnHandle, 1, State#state.handles) of
	% Valid handle reference.
	{value, {RefConnHandle, ConnHandle}} ->
	    {ok, [State#state.cnode, ConnHandle]};
	% Invalid handle reference.
	false ->
	    {error, {badref, 1}}
    end;



% sql_driver_connect
% Replace RefConnHandle with ConnHandle found in internal state.
% Add CNode.
prepare_args(sql_driver_connect,
	     [RefConnHandle, InConnStr, BufLenOutConnStr, DrvCompletion],
	     State) ->
    case lists:keysearch(RefConnHandle, 1, State#state.handles) of
	% Valid handle reference.
	{value, {RefConnHandle, ConnHandle}} ->
	    {ok, [State#state.cnode,
		  ConnHandle,
		  InConnStr,
		  BufLenOutConnStr,
		  DrvCompletion]};
	% Invalid handle reference.
	false ->
	    {error, {badref, 1}}
    end;



% sql_end_tran
% Replace RefHandle with Handle found in internal state.
% Add CNode.
prepare_args(sql_end_tran, [HandleType, RefHandle, ComplType], State) ->
    case lists:keysearch(RefHandle, 1, State#state.handles) of
	% Valid handle reference.
	{value, {RefHandle, Handle}} ->
	    {ok, [State#state.cnode, HandleType, Handle, ComplType]};
	% Invalid handle reference.
	false ->
	    {error, {badref, 2}}
    end;



% sql_exec_direct
% Replace RefStmtHandle with StmtHandle found in internal state.
% Add CNode.
prepare_args(sql_exec_direct, [RefStmtHandle, Stmt], State) ->
    case lists:keysearch(RefStmtHandle, 1, State#state.handles) of
	% Valid handle reference.
	{value, {RefStmtHandle, StmtHandle}} ->
	    {ok, [State#state.cnode, StmtHandle, Stmt]};
	% Invalid handle reference.
	false ->
	    {error, {badref, 1}}
    end;



% sql_fetch
% Replace RefStmtHandle with StmtHandle found in internal state.
% Add CNode.
prepare_args(sql_fetch, [RefStmtHandle], State) ->
    case lists:keysearch(RefStmtHandle, 1, State#state.handles) of
	% Valid handle reference.
	{value, {RefStmtHandle, StmtHandle}} ->
	    {ok, [State#state.cnode, StmtHandle]};
	% Invalid handle reference.
	false ->
	    {error, {badref, 1}}
    end;



% sql_free_handle
% Replace RefHandle with Handle found in internal state.
% Add CNode.
prepare_args(sql_free_handle, [HandleType, RefHandle], State) ->
    case lists:keysearch(RefHandle, 1, State#state.handles) of
	% Valid handle reference.
	{value, {RefHandle, Handle}} ->
	    {ok, [State#state.cnode, HandleType, Handle]};
	% Invalid handle reference.
	false ->
	    {error, {badref, 2}}
    end;



% sql_get_connect_attr
% Replace RefConnHandle with ConnHandle found in internal state.
% Decompose BufType when tuple.
% Add CNode.
prepare_args(sql_get_connect_attr,
	     [RefConnHandle, Attr, {?SQL_C_CHAR, BufLen}],
	     State) ->
    case lists:keysearch(RefConnHandle, 1, State#state.handles) of
	% Valid handle reference.
	{value, {RefConnHandle, ConnHandle}} ->
	    {ok, [State#state.cnode, ConnHandle, Attr, BufLen, ?SQL_C_CHAR]};
	% Invalid handle reference.
	false ->
	    {error, {badref, 1}}
    end;
prepare_args(sql_get_connect_attr,
	     [RefConnHandle, Attr, {?SQL_C_ULONG, IntType}],
	     State) ->
    case lists:keysearch(RefConnHandle, 1, State#state.handles) of
	% Valid handle reference.
	{value, {RefConnHandle, ConnHandle}} ->
	    {ok, [State#state.cnode, ConnHandle, Attr, IntType, ?SQL_C_ULONG]};
	% Invalid handle reference.
	false ->
	    {error, {badref, 1}}
    end;
prepare_args(sql_get_connect_attr, [RefConnHandle, Attr, ?SQL_C_ULONG], State) ->
    case lists:keysearch(RefConnHandle, 1, State#state.handles) of
	% Valid handle reference.
	{value, {RefConnHandle, ConnHandle}} ->
	    {ok, [State#state.cnode, ConnHandle, Attr, 0, ?SQL_C_ULONG]};
	% Invalid handle reference.
	false ->
	    {error, {badref, 1}}
    end;



% sql_get_diag_rec
% Replace RefHandle with Handle found in internal state.
% Add CNode.
prepare_args(sql_get_diag_rec,
	     [HandleType, RefHandle, RecNum, BufLenErrMsg],
	     State) ->
    case lists:keysearch(RefHandle, 1, State#state.handles) of
	% Valid handle reference.
	{value, {RefHandle, Handle}} ->
	    {ok, [State#state.cnode, HandleType, Handle, RecNum, BufLenErrMsg]};
	% Invalid handle reference.
	false ->
	    {error, {badref, 2}}
    end;



% sql_num_result_cols
% Replace RefStmtHandle with StmtHandle found in internal state.
% Add CNode.
prepare_args(sql_num_result_cols, [RefStmtHandle], State) ->
    case lists:keysearch(RefStmtHandle, 1, State#state.handles) of
	% Valid handle reference.
	{value, {RefStmtHandle, StmtHandle}} ->
	    {ok, [State#state.cnode, StmtHandle]};
	% Invalid handle reference.
	false ->
	    {error, {badref, 1}}
    end;



% sql_row_count
% Replace RefStmtHandle with StmtHandle found in internal state.
% Add CNode.
prepare_args(sql_row_count, [RefStmtHandle], State) ->
    case lists:keysearch(RefStmtHandle, 1, State#state.handles) of
	% Valid handle reference.
	{value, {RefStmtHandle, StmtHandle}} ->
	    {ok, [State#state.cnode, StmtHandle]};
	% Invalid handle reference.
	false ->
	    {error, {badref, 1}}
    end;



% sql_set_connect_attr
% Replace RefConnHandle with ConnHandle found in internal state.
% Add value for LenCharValue and dummy value for NumValue when
% Attr is of character type, or
% add dummy value for CharValue and LenCharValue when Attr is
% driver defined and of numeric type, or
% add dummy value for CharValue and set LenCharValue to IntType
% when Attr is driver defined and of numeric type.
% Add CNode.
prepare_args(sql_set_connect_attr,
	     [RefConnHandle, Attr, Value, ?SQL_C_CHAR],
	     State) ->
    case lists:keysearch(RefConnHandle, 1, State#state.handles) of
	% Valid handle reference.
	{value, {RefConnHandle, ConnHandle}} ->
	    {ok,
	     [State#state.cnode, ConnHandle, Attr, Value, ?SQL_NTS, 0, ?SQL_C_CHAR]};
	% Invalid handle reference.
	false ->
	    {error, {badref, 1}}
    end;
prepare_args(sql_set_connect_attr,
	     [RefConnHandle, Attr, Value, ?SQL_C_ULONG],
	     State) ->
    case lists:keysearch(RefConnHandle, 1, State#state.handles) of
	% Valid handle reference.
	{value, {RefConnHandle, ConnHandle}} ->
	    {ok, [State#state.cnode, ConnHandle, Attr, "", 0, Value, ?SQL_C_ULONG]};
	% Invalid handle reference.
	false ->
	    {error, {badref, 1}}
    end;
prepare_args(sql_set_connect_attr,
	     [RefConnHandle, Attr, Value, {?SQL_C_ULONG, IntType}],
	     State) ->
    case lists:keysearch(RefConnHandle, 1, State#state.handles) of
	% Valid handle reference.
	{value, {RefConnHandle, ConnHandle}} ->
	    CNode = State#state.cnode,
	    {ok,
	     [CNode, ConnHandle, Attr, "", IntType, Value, ?SQL_C_ULONG, IntType]};
	% Invalid handle reference.
	false ->
	    {error, {badref, 1}}
    end;



% sql_set_env_attr
% Replace RefEnvHandle with EnvHandle found in internal state.
% Add value for LenCharValue and dummy value for NumValue when
% Attr is of character type, or
% add dummy values for CharValue and LenCharValue when Attr is of
% numeric type.
% Add CNode.
prepare_args(sql_set_env_attr, [RefEnvHandle, Attr, Value, ?SQL_C_CHAR], State) ->
    case lists:keysearch(RefEnvHandle, 1, State#state.handles) of
	% Valid handle reference.
	{value, {RefEnvHandle, EnvHandle}} ->
	    {ok,
	     [State#state.cnode, EnvHandle, Attr, Value, ?SQL_NTS, 0, ?SQL_C_CHAR]};
	% Invalid handle reference.
	false ->
	    {error, {badref, 1}}
    end;
prepare_args(sql_set_env_attr, [RefEnvHandle, Attr, Value, ?SQL_C_ULONG], State) ->
    case lists:keysearch(RefEnvHandle, 1, State#state.handles) of
	% Valid handle reference.
	{value, {RefEnvHandle, EnvHandle}} ->
	    {ok, [State#state.cnode, EnvHandle, Attr, "", 0, Value, ?SQL_C_ULONG]};
	% Invalid handle reference.
	false ->
	    {error, {badref, 1}}
    end;



% Default.
% Add CNode.
prepare_args(_Operation, UserArgs, State) ->
    {ok, [State#state.cnode | UserArgs]}.




%%--------------------------------------------------
%% handle_result/4
%%
%% Returns a reply on the form wanted by gen_server.
%% The new internal state, and the reply, is calculated from
%% the operation, its arguments, result, and old state.
%% The "arguments" here (Args) is a list of the arguments
%% as returned by prepare_args.
%%
%% handle_result(Operation, Args, Result, State) ->
%%   {reply, Reply, State1} |
%%   {reply, {internal_error, {Operation, {args, Args}, {retval, Result}}}, State1}
%%
%% Operation -> atom()                  ; The operation at hand.
%% Args -> list()                       ; List of args for the IDL fcn call.
%% Result -> term()                     ; Return value from IDL fcn.
%% State -> record(state)               ; Internal state.
%% Reply -> term()                      ; Reply to user.
%% State1 -> record(state)              ; New internal state.
%% 

% sql_alloc_handle, SQL_SUCCESS, SQL_SUCCESS_WITH_INFO.
% New state: add the new handle.
% No modification of reply.
handle_result(sql_alloc_handle, _Args, {?SQL_SUCCESS, OutputHandle}, State) ->
    RefOutputHandle = {node(), now()},
    State1 = State#state{handles = [{RefOutputHandle, OutputHandle} |
				    State#state.handles]},
    {reply, {?SQL_SUCCESS, RefOutputHandle}, State1};

handle_result(sql_alloc_handle,
	      _Args,
	      {?SQL_SUCCESS_WITH_INFO, OutputHandle},
	      State) ->
    RefOutputHandle = {node(), now()},
    State1 = State#state{handles = [{RefOutputHandle, OutputHandle} |
				    State#state.handles]},
    {reply, {?SQL_SUCCESS, RefOutputHandle}, State1};


% sql_describe_col
% New state: same as the old state.
% Modify the reply.
handle_result(sql_describe_col, _Args, Result, State)
  when tuple(Result), element(1, Result) == ?SQL_SUCCESS ->
    {RetCode, ColName, LenColName, SqlType, ColSize, DecDigs, Nullable} = Result,
    {reply,
     {RetCode, {ColName, LenColName}, SqlType, ColSize, DecDigs, Nullable},
     State};

handle_result(sql_describe_col, _Args, Result, State)
  when tuple(Result), element(1, Result) == ?SQL_SUCCESS_WITH_INFO ->
    {RetCode, ColName, LenColName, SqlType, ColSize, DecDigs, Nullable} = Result,
    {reply,
     {RetCode, {ColName, LenColName}, SqlType, ColSize, DecDigs, Nullable},
     State};

handle_result(sql_describe_col, _Args, Result, State)
  when tuple(Result), element(1, Result) == ?SQL_ERROR ->
    {RetCode, ColName, LenColName, SqlType, ColSize, DecDigs, Nullable} = Result,
    {reply,
     {RetCode, {ColName, LenColName}, SqlType, ColSize, DecDigs, Nullable},
     State};

handle_result(sql_describe_col, _Args, Result, State)
  when tuple(Result), element(1, Result) == ?SQL_INVALID_HANDLE ->
    {RetCode, ColName, LenColName, SqlType, ColSize, DecDigs, Nullable} = Result,
    {reply,
     {RetCode, {ColName, LenColName}, SqlType, ColSize, DecDigs, Nullable},
     State};


% sql_driver_connect
% New state: same as the old state.
% Modify the reply.
handle_result(sql_driver_connect, _Args, Result, State)
  when tuple(Result), element(1, Result) == ?SQL_SUCCESS ->
    {RetCode, OutConnStr, LenOutConnStr} = Result,
    {reply,
     {RetCode, {OutConnStr, LenOutConnStr}},
     State};

handle_result(sql_driver_connect, _Args, Result, State)
  when tuple(Result), element(1, Result) == ?SQL_SUCCESS_WITH_INFO ->
    {RetCode, OutConnStr, LenOutConnStr} = Result,
    {reply,
     {RetCode, {OutConnStr, LenOutConnStr}},
     State};

handle_result(sql_driver_connect, _Args, Result, State)
  when tuple(Result), element(1, Result) == ?SQL_NO_DATA ->
    {RetCode, OutConnStr, LenOutConnStr} = Result,
    {reply,
     {RetCode, {OutConnStr, LenOutConnStr}},
     State};

handle_result(sql_driver_connect, _Args, Result, State)
  when tuple(Result), element(1, Result) == ?SQL_ERROR ->
    {RetCode, OutConnStr, LenOutConnStr} = Result,
    {reply,
     {RetCode, {OutConnStr, LenOutConnStr}},
     State};

handle_result(sql_driver_connect, _Args, Result, State)
  when tuple(Result), element(1, Result) == ?SQL_INVALID_HANDLE ->
    {RetCode, OutConnStr, LenOutConnStr} = Result,
    {reply,
     {RetCode, {OutConnStr, LenOutConnStr}},
     State};


% sql_free_handle, SQL_SUCCESS.
% New state: remove the freed handle.
% No modification of reply.
handle_result(sql_free_handle, Args, ?SQL_SUCCESS, State) ->
    [_CNode, _HandleType, Handle] = Args,
    State1 =
	State#state{handles = lists:keydelete(Handle, 2, State#state.handles)},
    {reply, ?SQL_SUCCESS, State1};


% sql_get_connect_attr
% New state: same as the old state.
% Modify the reply.
handle_result(sql_get_connect_attr, Args, Result, State)
  when tuple(Result), element(1, Result) == ?SQL_SUCCESS ->
    [_CNode, _ConnHandle, _Attr, _BufLen, BufCType] = Args,
    {RetCode, CharValue, LenCharValue, NumValue} = Result,
    case BufCType of
	?SQL_C_CHAR ->
	    {reply, {RetCode, {CharValue, LenCharValue}}, State};
	?SQL_C_ULONG ->
	    {reply, {RetCode, NumValue}, State}
    end;

handle_result(sql_get_connect_attr, Args, Result, State)
  when tuple(Result), element(1, Result) == ?SQL_SUCCESS_WITH_INFO ->
    [_CNode, _ConnHandle, _Attr, _BufLen, BufCType] = Args,
    {RetCode, CharValue, LenCharValue, NumValue} = Result,
    case BufCType of
	?SQL_C_CHAR ->
	    {reply, {RetCode, {CharValue, LenCharValue}}, State};
	?SQL_C_ULONG ->
	    {reply, {RetCode, NumValue}, State}
    end;

handle_result(sql_get_connect_attr, Args, Result, State)
  when tuple(Result), element(1, Result) == ?SQL_NO_DATA ->
    [_CNode, _ConnHandle, _Attr, _BufLen, BufCType] = Args,
    {RetCode, CharValue, LenCharValue, NumValue} = Result,
    case BufCType of
	?SQL_C_CHAR ->
	    {reply, {RetCode, {CharValue, LenCharValue}}, State};
	?SQL_C_ULONG ->
	    {reply, {RetCode, NumValue}, State}
    end;

handle_result(sql_get_connect_attr, Args, Result, State)
  when tuple(Result), element(1, Result) == ?SQL_ERROR ->
    [_CNode, _ConnHandle, _Attr, _BufLen, BufCType] = Args,
    {RetCode, CharValue, LenCharValue, NumValue} = Result,
    case BufCType of
	?SQL_C_CHAR ->
	    {reply, {RetCode, {CharValue, LenCharValue}}, State};
	?SQL_C_ULONG ->
	    {reply, {RetCode, NumValue}, State}
    end;

handle_result(sql_get_connect_attr, Args, Result, State)
  when tuple(Result), element(1, Result) == ?SQL_INVALID_HANDLE ->
    [_CNode, _ConnHandle, _Attr, _BufLen, BufCType] = Args,
    {RetCode, CharValue, LenCharValue, NumValue} = Result,
    case BufCType of
	?SQL_C_CHAR ->
	    {reply, {RetCode, {CharValue, LenCharValue}}, State};
	?SQL_C_ULONG ->
	    {reply, {RetCode, NumValue}, State}
    end;


% sql_get_diag_rec
% New state: same as the old state.
% Modify the reply.
handle_result(sql_get_diag_rec, _Args, Result, State)
  when tuple(Result), element(1, Result) == ?SQL_SUCCESS ->
    {RetCode, SqlState, NativeErr, ErrMsg, LenErrMsg} = Result,
    {reply, {RetCode, SqlState, NativeErr, {ErrMsg, LenErrMsg}}, State};

handle_result(sql_get_diag_rec, _Args, Result, State)
  when tuple(Result), element(1, Result) == ?SQL_SUCCESS_WITH_INFO ->
    {RetCode, SqlState, NativeErr, ErrMsg, LenErrMsg} = Result,
    {reply, {RetCode, SqlState, NativeErr, {ErrMsg, LenErrMsg}}, State};

handle_result(sql_get_diag_rec, _Args, Result, State)
  when tuple(Result), element(1, Result) == ?SQL_NO_DATA ->
    {RetCode, SqlState, NativeErr, ErrMsg, LenErrMsg} = Result,
    {reply, {RetCode, SqlState, NativeErr, {ErrMsg, LenErrMsg}}, State};

handle_result(sql_get_diag_rec, _Args, Result, State)
  when tuple(Result), element(1, Result) == ?SQL_ERROR ->
    {RetCode, SqlState, NativeErr, ErrMsg, LenErrMsg} = Result,
    {reply, {RetCode, SqlState, NativeErr, {ErrMsg, LenErrMsg}}, State};

handle_result(sql_get_diag_rec, _Args, Result, State)
  when tuple(Result), element(1, Result) == ?SQL_INVALID_HANDLE ->
    {RetCode, SqlState, NativeErr, ErrMsg, LenErrMsg} = Result,
    {reply, {RetCode, SqlState, NativeErr, {ErrMsg, LenErrMsg}}, State};


% Default.
% All other results of all other operations.
% No change of state.
% No modification of reply.
handle_result(_Operation, _Args, Result, State)
  when tuple(Result), element(1, Result) == ?SQL_SUCCESS ->
    {reply, Result, State};
handle_result(_Operation, _Args, ?SQL_SUCCESS, State) ->
    {reply, ?SQL_SUCCESS, State};

handle_result(_Operation, _Args, Result, State)
  when tuple(Result), element(1, Result) == ?SQL_SUCCESS_WITH_INFO ->
    {reply, Result, State};
handle_result(_Operation, _Args, ?SQL_SUCCESS_WITH_INFO, State) ->
    {reply, ?SQL_SUCCESS_WITH_INFO, State};

handle_result(_Operation, _Args, Result, State)
  when tuple(Result), element(1, Result) == ?SQL_NEED_DATA ->
    {reply, Result, State};
handle_result(_Operation, _Args, ?SQL_NEED_DATA, State) ->
    {reply, ?SQL_NEED_DATA, State};

handle_result(_Operation, _Args, Result, State)
  when tuple(Result), element(1, Result) == ?SQL_NO_DATA ->
    {reply, Result, State};
handle_result(_Operation, _Args, ?SQL_NO_DATA, State) ->
    {reply, ?SQL_NO_DATA, State};

handle_result(_Operation, _Args, Result, State)
  when tuple(Result), element(1, Result) == ?SQL_INVALID_HANDLE ->
    {reply, Result, State};
handle_result(_Operation, _Args, ?SQL_INVALID_HANDLE, State) ->
    {reply, ?SQL_INVALID_HANDLE, State};

handle_result(_Operation, _Args, Result, State)
  when tuple(Result), element(1, Result) == ?SQL_ERROR ->
    {reply, Result, State};
handle_result(_Operation, _Args, ?SQL_ERROR, State) ->
    {reply, ?SQL_ERROR, State};

handle_result(_Operation, _Args, Result, State)
  when tuple(Result), element(1, Result) == ?OK ->
    {reply, Result, State};
handle_result(_Operation, _Args, ?OK, State) ->
    {reply, ?OK, State};

handle_result(Operation, Args, Result, State)
  when tuple(Result), element(1, Result) == ?ERROR ->
    {reply, {internal_error, {Operation, {args, Args}, {retval, Result}}}, State};
handle_result(Operation, Args, ?ERROR, State) ->
    {reply, {internal_error, {Operation, {args, Args}, {retval, ?ERROR}}}, State};

% Unrecognised result codes.
% Error from "other" code, e.g. code generated by IC.
% The server will terminate later only if the exception
% killed the c-node.
handle_result(Operation, Args, Result, State) ->
    {reply, {internal_error, {Operation, {args, Args}, {retval, Result}}}, State}.




%%%------------------------------------------------------------------
%%% Basic API, buffer handling, impl.
%%%------------------------------------------------------------------
%%%
%%% These fcns implement the 4 buffer handling fcns:
%%% alloc_buffer, dealloc_buffer, read_buffer, and write_buffer.
%%% Return value:
%%% {reply, Reply, State}
%%%
%%% Errors are thrown:
%%% {reply, Reply, State}
%%% {stop, Reason, Reply, State}
%%%

do_alloc_buffer(BufCType, Size, State) ->
    CNode = State#state.cnode,

    % Allocate data buffer.
    {Result1, Buf} = odbclli:alloc_buffer(CNode, BufCType, Size),
    ok =
	case Result1 of
	    % Allocation of data buffer succeded.
	    ?OK ->
		ok;
	    % Allocation of data buffer failed.
	    ?ERROR ->
		Args1 = {args, [CNode, BufCType, Size]},
		RetVal1 = {retval, Result1},
		Reply1 = {internal_error, {alloc_buffer, Args1, RetVal1}},
		throw({reply, Reply1, State})
	end,

    % Allocate length/indicator buffer.
    {Result2, LenIndBuf} = odbclli:alloc_buffer(CNode, ?SQL_C_SLONG, 0),
    ok =
	case Result2 of
	    % Allocation of length/indicator buffer succeded.
	    ?OK ->
		ok;

	    % Allocation of length/indicator buffer failed.
	    % Try to deallocate data buffer and throw error.
	    ?ERROR ->
		Result3 = odbclli:dealloc_buffer(CNode, Buf),
		case Result3 of
		    % Deallocation of data buffer succeded.
		    % Return error.
		    ?OK ->
			Args2 = {args, [CNode, BufCType, Size]},
			RetVal2 = {retval, Result2},
			Reply2 = {internal_error, {alloc_buffer, Args2, RetVal2}},
			throw({reply, Reply2, State});

		    % Deallocation of data buffer failed.
		    % Inconsistent state. Terminate.
		    ?ERROR ->
			Args3 = {args, [CNode, ?SQL_C_SLONG, 0]},
			RetVal3 = {retval, Result2},
			Reply3 = {alloc_buffer, Args3, RetVal3},

			Args4 = {args, [CNode, Buf]},
			RetVal4 = {retval, Result3},
			Reply4 = {dealloc_buffer, Args4, RetVal4},

			Reply =
			    {stopped, {Reply4, {original_error, Reply3}}},

			throw({stop, corrupt_state, Reply, State})
		end
	end,

    BufData =
	#buf_data{buffer = Buf, ctype = BufCType, size = Size, len_ind = LenIndBuf},
    RefBufData = {node(), now()},
    State1 = State#state{buffers = [{RefBufData, BufData} | State#state.buffers]},
    {reply, {ok, RefBufData}, State1}.




do_dealloc_buffer(RefBufData, State) ->
    % Get buffer data record from state.
    Buffers = State#state.buffers,
    BufData =
	case lists:keysearch(RefBufData, 1, Buffers) of
	% Valid buffer reference.
	{value, {RefBufData, BufData1}} ->
		BufData1;
	% Invalid buffer reference.
	false ->
	    throw({reply, {badref, 1}, State})
	end,

    % Deallocate data buffer
    CNode = State#state.cnode,
    Buf = BufData#buf_data.buffer,
    ok =
	case odbclli:dealloc_buffer(CNode, Buf) of
	    % Deallocation of data buffer succeded.
	    ?OK ->
		ok;
	    % Deallocation of data buffer failed.
	    ?ERROR ->
		Args = {args, [CNode, Buf]},
		RetVal = {retval, ?ERROR},
		throw({reply, {internal_error, {dealloc_buffer, Args, RetVal}}})
	end,

    % Deallocate length/indicator buffer.
    LenIndBuf = BufData#buf_data.len_ind,
    ok =
	case odbclli:dealloc_buffer(CNode, LenIndBuf) of
	    %Deallocation of length/indicator buffer succeded.
	    ?OK ->
		ok;
		% Deallocation of length/indicator buffer failed.
		% Inconsistent state. Terminate.
	    ?ERROR ->
		Args1 = {args, [CNode, LenIndBuf]},
		RetVal1 = {retval, ?ERROR},
		Reply1 = {stopped, {dealloc_buffer, Args1, RetVal1}},
		throw({stop, corrupt_state, Reply1, State})
	end,

    Buffers1 = lists:keydelete(RefBufData, 1, Buffers),
    {reply, ok, State#state{buffers = Buffers1}}.




do_read_buffer(RefBufData, State) ->
    % Get buffer data record from state.
    BufData =
	case lists:keysearch(RefBufData, 1, State#state.buffers) of
	    % Valid buffer reference.
	    {value, {RefBufData, BufData1}} ->
		BufData1;
	    % Invalid buffer reference.
	    false ->
		throw({reply, {badref, 1}, State})
	end,

    % Read length/indicator buffer.
    CNode = State#state.cnode,
    {Result2, _CharVal, _BinVal, LenInd} =
	odbclli:read_buffer(CNode, BufData#buf_data.len_ind, ?SQL_C_SLONG, 0),
    ok =
	case Result2 of
	    % Read of length/indicator succeded.
	    ?OK ->
		ok;
	    % Read of length/indicator failed.
	    ?ERROR ->
		Args2 = {args, [CNode, BufData#buf_data.len_ind, ?SQL_C_SLONG]},
		RetVal2 = {retval, Result2},
		Reply2 = {internal_error, {read_buffer, Args2, RetVal2}},
		throw({reply, Reply2, State})
	end,

    % Read data buffer.
    BufCType = BufData#buf_data.ctype,
    {Result1, CharVal, BinVal, NumVal} =
	odbclli:read_buffer(CNode, BufData#buf_data.buffer, BufCType, LenInd),
    ok =
	case Result1 of
	    % Read of data buffer succeded.
	    ?OK ->
		ok;
	    % Read of data buffer failed.
	    ?ERROR ->
		Args1 = {args, [CNode, BufData#buf_data.buffer, BufCType]},
		RetVal1 = {retval, Result1},
		Reply1 = {internal_error, {read_buffer, Args1, RetVal1}},
		throw({reply, Reply1, State})
	end,

    % Get the returned data value (type dependent).
    Val =
	case BufCType of
	    % Character data.
	    ?SQL_C_CHAR ->
		CharVal;
	    % Binary data.
	    ?SQL_C_BINARY ->
		list_to_binary(BinVal)
	end,

    {reply, {ok, {Val, LenInd}}, State}.



% Checks that data is type consistent with the data buffer.
% It is not checked, however, that character data contains
% ASCII characters only -- just that it is a list.
do_write_buffer(RefBufData, Data, State) ->
    % Get buffer data record from state.
    BufData =
	case lists:keysearch(RefBufData, 1, State#state.buffers) of
	    % Valid buffer reference.
	    {value, {RefBufData, BufData1}} ->
		BufData1;
	    % Invalid buffer reference.
	    false ->
		throw({reply, {badref, 1}, State})
	end,

    % Check that Value of Data has correct type for data buffer.
    {Value, LenInd} = Data,
    BufCType = BufData#buf_data.ctype,
    {CharVal, BinVal, NumVal} =
	if
	    % Character data
	    BufCType == ?SQL_C_CHAR, list(Value) ->
		{Value, [], 0};
	    % Binary data
	    BufCType == ?SQL_C_BINARY, binary(Value) ->
		{"", binary_to_list(Value), 0};
	    % Type error
	    true ->
		throw({reply, {baddata, 2}, State})
	end,

    % Write to data buffer.
    CNode = State#state.cnode,
    Buf = BufData#buf_data.buffer,
    ok =
	case odbclli:write_buffer(CNode, Buf, CharVal, BinVal, NumVal, BufCType) of
	    % Write of data buffer succeded
	    ?OK ->
		ok;
	    % Write of data buffer failed.
	    ?ERROR ->
		Args1 = {args, [CNode, Buf, CharVal, BinVal, NumVal, BufCType]},
		RetVal1 = {retval, ?ERROR},
		Reply1 = {internal_error, {write_buffer, Args1, RetVal1}},
		throw({reply, Reply1, State})
	end,

    % Write to length/indicator buffer (if applicable).
    LenIndBuf = BufData#buf_data.len_ind,
    ok =
	case LenInd of
	    % No value for l/i buffer. Done.
	    no_len_ind ->
		ok;
	    % Calculate value for l/i buffer and write.
	    Other ->
		case odbclli:write_buffer(CNode,
					  LenIndBuf,
					  "",
					  [],
					  len_ind_val(LenInd),
					  ?SQL_C_SLONG) of
		    % Write of l/i buffer succeded.
		    ?OK ->
			ok;
		    % Write of l/i buffer failed.
		    ?ERROR ->
			Args2 =
			    {args, [CNode, LenIndBuf, "", [], LenInd, ?SQL_C_SLONG]},
			RetVal2 = {retval, ?ERROR},
			Reply2 = {internal_error, {write_buffer, Args2, RetVal2}},
			throw({reply, Reply2, State})
		end
	end,

    {reply, ok, State}.






%%%------------------------------------------------------------------
%%% Utility API impl.
%%%------------------------------------------------------------------
%%%
%%% These functions (*_impl), which are called from
%%% handle_call({utility, Op, Args}. ...) take
%%% the server state as their first argument, and
%%% then other arguments. (Example: init_env(Server)
%%% has no additional arguments for init_env_impl,
%%% since Args = []. So init_env_impl just takes the
%%% the state.
%%% The purpose of these fcns is to call a sequence
%%% of IDL-interface wrapper fcns (ODBC calls).
%%%
%%% These fcns all return
%%% {ok, {Reply, NewState}}.
%%% Reply -> term()    is returned to the client.
%%%
%%% Errors are thrown, which allows sequencing of wrapper
%%% calls instead of nested case-statements. Such exceptions
%%% are on the form
%%% {error, {RecoverData, ClientInfo}}
%%%   RecoverData -> {HandleData, RestoreData}
%%%     HandleData -> {HandleType, Handle} | nohandle
%%%       HandleType -> integer()
%%%       Handle -> integer()
%%%     RestoreData -> {restore, [RestoreItem]} | norestore
%%%       RestoreItem -> {handle, {HandleType, Handle}} |
%%%                      {buffer, record(buf_data)}
%%%   ClientInfo -> {ErrType, IDLInfo}
%%%     ErrType -> internal_error | odbc_error
%%%     IDLInfo -> {IDLFcn, {args, Args}, {retval, RetVal}}
%%%       IDLFcn -> atom()
%%%       Args -> [term()]
%%%       RetVal -> integer()
%%%
%%% RecoverData is intended for the recover-fcn:
%%% recover(Operation, RecoverData, State)
%%% (which uses HandleData to retrieve ODBC errors and
%%% RestoreData to discard resources and try to reinstate
%%% the internal state as it was before the operation began).
%%% ClientInfo is intended to be returned to the client
%%% if there is no ODBC error information. It contains:
%%% error type and info about the IDL fcn called.
%%%
%%% It is the responsibility of the *_impl-fcns to determine
%%% what needs to be restored, since this depends solely
%%% upon where in the sequence the error occurs.
%%% The information about what has gone wrong, and with which
%%% handle, is passed on from the wrapper fcns.
%%%
%%%------------------------------------------------------------------


%% init_env_impl/1
%%
%% Initialises the ODBC environment.
%%
%% init_env_impl([State]) -> {ok, {{ok, RefOutputHandle}, State1}}
%%
%% Errors thrown:
%% {error, {RecoverData, ClientInfo}}
%%   RecoverData -> {HandleData, RestoreData}
%%     HandleData -> {HandleType, Handle} | nohandle
%%       HandleType -> integer()
%%       Handle -> integer()
%%     RestoreData -> {restore, [RestoreItem]} |
%%                    norestore
%%       RestoreItem -> {handle, {HandleType, OutputHandle}} |
%%                      {buffer, record(buf_data)}
%%   ClientInfo -> term()
%%
%% Exceptions not handled:
%% {invalid_handle, {IDLFcn, Args, RetVal}}
%%
init_env_impl([State]) ->
    CNode = State#state.cnode,
    OutputHandle =
	case alloc_handle_wrap(CNode, ?SQL_HANDLE_ENV, ?SQL_NULL_HANDLE) of
	    % Continue
	    {ok, Value} ->
		Value;
	    % Error. Throw to top (handle_call).
	    % Nothing to restore yet.
	    {error, {HandleData1, ClientInfo1}} ->
		throw({error, {{HandleData1, norestore}, ClientInfo1}})
	end,

    ok =
	case set_env_attr_wrap(CNode, OutputHandle, ?SQL_ATTR_ODBC_VERSION,
			       "", 0, ?SQL_OV_ODBC3, ?SQL_C_ULONG) of
	    % Continue
	    ok ->
		ok;
	    % Error. Throw to top (handle_call).
	    % OutputHandle must be deallocated.
	    {error, {HandleData2, ClientInfo2}} ->
		throw({error,
		       {{HandleData2,
			 {restore, [{handle, {?SQL_HANDLE_ENV, OutputHandle}}]}},
			ClientInfo2}})
	end,

    RefOutputHandle = {node(), now()},
    State1 = State#state{handles = [{RefOutputHandle, OutputHandle} |
				    State#state.handles]},
    {ok, {{ok, RefOutputHandle}, State1}}.



%%------------------------------------------------------------------

%% connect_drv_impl/1
%%
%% Establishes a connection to a data source using SQLDriverConnect.
%%
%% connect_drv_impl([State, RefEnvHandle, ConnectStr]) ->
%%   {ok, {{ok, RefConnHandle}, State1}}
%%
%% Errors thrown:
%% {error, {RecoverData, ClientInfo}}
%%   RecoverData -> {HandleData, RestoreData}
%%     HandleData -> {HandleType, Handle} | nohandle
%%       HandleType -> integer()
%%       Handle -> integer()
%%     RestoreData -> {restore, [RestoreItem]} |
%%                    norestore
%%       RestoreItem -> {handle, {HandleType, OutputHandle}} |
%%                      {buffer, record(buf_data)}
%%   ClientInfo -> term()
%%
%% Exceptions not handled:
%% {invalid_handle, {IDLFcn, Args, RetVal}}
%%
connect_drv_impl([State, RefEnvHandle, ConnStr]) ->
    CNode = State#state.cnode,
    Handles = State#state.handles,

    EnvHandle =
	case lists:keysearch(RefEnvHandle, 1, Handles) of
	    % Valid handle reference.
	    {value, {RefEnvHandle, EnvHandle1}} ->
		EnvHandle1;
	    % Invalid handle reference.
	    false ->
		throw({error, {{nohandle, norestore}, {badref, 1}}})
	end,

    ConnHandle =
	case alloc_handle_wrap(CNode, ?SQL_HANDLE_DBC, EnvHandle) of
	    % Continue
	    {ok, Value1} ->
		Value1;
	    % Error. Throw to top (handle_call).
	    % Nothing to restore yet.
	    {error, {HandleData1, ClientInfo1}} ->
		throw({error, {{HandleData1, norestore}, ClientInfo1}})
	end,

    LenConnStr = length(ConnStr),
    {_OutConnStr, _LenOutConnStr} =
	case driver_connect_wrap(CNode,
				 ConnHandle,
				 ConnStr,
				 ?MaxLenOutConnStr,
				 ?SQL_DRIVER_NOPROMPT) of
	    % Continue
	    {ok, Value2} ->
		Value2;
	    % Error. Throw to top (handle_call).
	    % Restore ConnHandle.
	    {error, {HandleData2, ClientInfo2}} ->
		RestoreData2 = {restore, [{handle, {?SQL_HANDLE_DBC, ConnHandle}}]},
		RecoverData2 = {HandleData2, RestoreData2},
		throw({error, {RecoverData2, ClientInfo2}})
	end,

    RefConnHandle = {node(), now()},
    ConnData = #drv_connect_data{conn_str = ConnStr,
				 len_conn_str = LenConnStr + 1,
				 drv_compl = ?SQL_DRIVER_NOPROMPT},
    State1 = State#state{handles = [{RefConnHandle, ConnHandle} | Handles],
			 conn_data = ConnData},
    {ok, {{ok, RefConnHandle}, State1}}.


%%------------------------------------------------------------------

%% connect_impl/1
%%
%% Establishes a connection to a data source using SQLConnect.
%%
%% connect_impl([State, RefEnvHandle, DSN, UID, PWD]) ->
%%   {ok, {{ok, RefConnHandle}, State1}}
%%
%% Errors thrown:
%% {error, {RecoverData, ClientInfo}}
%%   RecoverData -> {HandleData, RestoreData}
%%     HandleData -> {HandleType, Handle} | nohandle
%%       HandleType -> integer()
%%       Handle -> integer()
%%     RestoreData -> {restore, [RestoreItem]} |
%%                    norestore
%%       RestoreItem -> {handle, {HandleType, OutputHandle}} |
%%                      {buffer, record(buf_data)}
%%   ClientInfo -> term()
%%
%% Exceptions not handled:
%% {invalid_handle, {IDLFcn, Args, RetVal}}
%%
connect_impl([State, RefEnvHandle, DSN, UID, PWD]) ->
    CNode = State#state.cnode,
    Handles = State#state.handles,

    EnvHandle =
	case lists:keysearch(RefEnvHandle, 1, Handles) of
	    % Valid handle reference.
	    {value, {RefEnvHandle, EnvHandle1}} ->
		EnvHandle1;
	    % Invalid handle reference.
	    false ->
		throw({error, {{nohandle, norestore}, {badref, 1}}})
	end,

    ConnHandle =
	case alloc_handle_wrap(CNode, ?SQL_HANDLE_DBC, EnvHandle) of
	    % Continue
	    {ok, Value} ->
		Value;
	    % Error. Throw to top (handle_call).
	    % Nothing to restore yet.
	    {error, {HandleData1, ClientInfo1}} ->
		throw({error, {{HandleData1, norestore}, ClientInfo1}})
	end,

    ok =
	case connect_wrap(CNode, ConnHandle, DSN, UID, PWD) of
	    % Continue
	    ok ->
		ok;
	    % Error. Throw to top (handle_call).
	    % Restore ConnHandle.
	    {error, {HandleData2, ClientInfo2}} ->
		RestoreData2 = {restore, [{handle, {?SQL_HANDLE_DBC, ConnHandle}}]},
		RecoverData2 = {HandleData2, RestoreData2},
		throw({error, {RecoverData2, ClientInfo2}})
	end,

    RefConnHandle = {node(), now()},
    ConnData = #connect_data{dsn = DSN, uid = UID, pwd = PWD},
    State1 = State#state{handles = [{RefConnHandle, ConnHandle} | Handles],
			 conn_data = ConnData},
    {ok, {{ok, RefConnHandle}, State1}}.


%%------------------------------------------------------------------

%% execute_stmt_impl/1
%%
%% Executes a single SQL statement. The fcn is split into
%% 2 branches after statement execution. One branch handles
%% collection of results from a SELECT statement, the other
%% one handles all other statement types.
%%
%% execute_stmt_impl([State, RefConnHandle, Stmt]) ->
%%   {ok, {{selected, Cols, Rows}, State1}} |
%%   {ok, {{updated, RowCount}, State1}}
%%
%% Errors thrown:
%% {error, {RecoverData, ClientInfo}}
%%   RecoverData -> {HandleData, RestoreData}
%%     HandleData -> {HandleType, Handle} | nohandle
%%       HandleType -> integer()
%%       Handle -> integer()
%%     RestoreData -> {restore, [RestoreItem]} |
%%                    norestore
%%       RestoreItem -> {handle, {HandleType, OutputHandle}} |
%%                      {buffer, record(buf_data)}
%%   ClientInfo -> term()
%%
%% Exceptions not handled:
%% {invalid_handle, {IDLFcn, Args, RetVal}}
%%
execute_stmt_impl([State, RefConnHandle, Stmt]) ->
    CNode = State#state.cnode,
    Handles = State#state.handles,

    ConnHandle =
	case lists:keysearch(RefConnHandle, 1, Handles) of
	    % Valid handle reference.
	    {value, {RefConnHandle, ConnHandle1}} ->
		ConnHandle1;
	    % Invalid handle reference.
	    false ->
		throw({error, {{nohandle, norestore}, {badref, 1}}})
	end,

    CommitMode =
	case get_connect_attr_wrap(CNode,
				   ConnHandle,
				   ?SQL_ATTR_AUTOCOMMIT,
				   0,
				   ?SQL_C_ULONG)   of
	    % Continue
	    {ok, CM} ->
		CM;
	    % Error. Throw to top (handle_call).
	    % Nothing to restore yet.
	    {error, {HandleData1, CliInfo1}} ->
		throw({error, {{HandleData1, norestore}, CliInfo1}})
	end,

    StmtHandle =
	case alloc_handle_wrap(CNode, ?SQL_HANDLE_STMT, ConnHandle) of
	    % Continue
	    {ok, Value} ->
		Value;
	    % Error. Throw to top (handle_call).
	    % Nothing to restore yet.
	    {error, {HandleData2, CliInfo2}} ->
		throw({error, {{HandleData2, norestore}, CliInfo2}})
	end,
    RestoreItems1 = [{handle, {?SQL_HANDLE_STMT, StmtHandle}}],

    ok =
	case exec_direct_wrap(CNode, StmtHandle, Stmt) of
	    % Continue
	    ok ->
		ok;
	    % Error. Throw to top (handle_call).
	    % Restore RestoreItems1.
	    {error, {HandleData3, CliInfo3}} ->
		throw({error, {{HandleData3, {restore, RestoreItems1}}, CliInfo3}})
	end,

    % In case we need to rollback.
    RestoreItems2 =
	[{rollback, {CommitMode, ?SQL_HANDLE_DBC, ConnHandle}} | RestoreItems1],

    Reply =
	case num_result_cols_wrap(CNode, StmtHandle) of
	    % SELECT statement
	    {ok, NumResultCols} when NumResultCols > 0 ->
		MaxLenData = State#state.max_len_data,
		MaxLenStr = State#state.max_len_str,
		handle_select(CNode,
			      StmtHandle,
			      NumResultCols,
			      MaxLenData,
			      MaxLenStr,
			      RestoreItems1);  % No rollback needed on error.

	    % UPDATE, INSERT, DELETE, or other type of statement
	    {ok, 0} ->
		handle_nonselect(CNode,
				 StmtHandle,
				 ConnHandle,
				 RestoreItems2);  % Rollback needed on error.

	    % Error. Throw to top (handle_call)
	    % Restore RestoreItems2, rollback needed.
	    {error, {HandleData4, CliInfo4}} ->
		throw({error, {{HandleData4, {restore, RestoreItems2}}, CliInfo4}})
	end,

    % Rollback is no longer possible on error. Clients need to know that
    % the statement was committed although the operation failed.
    RestoreItems3 = [{rollback, already_committed} | RestoreItems1],

    ok =
	case free_handle_wrap(CNode, ?SQL_HANDLE_STMT, StmtHandle) of
	    % Continue
	    ok ->
		ok;
	    % Error. Throw to top (handle_call).
	    % Restore RestoreItems.
	    {error, {HandleData5, CliInfo5}} ->
	    throw({error, {{HandleData5, {restore, RestoreItems3}}, CliInfo5}})
	end,

    {ok, {Reply, State}}.



%% handle_select/6
%%
%% Returns results from a SELECT statement.
%%
%% handle_select(CNode, StmtHandle, NumResultCols,
%%               MaxLenData, MaxLenStr, RestoreItems) ->
%%   {selected, Cols, Rows}
%%
%% Errors thrown:
%% {error, {RecoverData, ClientInfo}}
%%   RecoverData -> {HandleData, RestoreData}
%%     HandleData -> {HandleType, Handle} | nohandle
%%       HandleType -> integer()
%%       Handle -> integer()
%%     RestoreData -> {restore, [RestoreItem]} |
%%                    norestore
%%       RestoreItem -> {handle, {HandleType, OutputHandle}} |
%%                      {buffer, record(buf_data)}
%%   ClientInfo -> term()
%%
%% Exceptions not handled:
%% {invalid_handle, {IDLFcn, Args, RetVal}}
%%
handle_select(CNode,
	      StmtHandle,
	      NumResultCols,
	      MaxLenData,
	      MaxLenStr,
	      RestoreItems1) ->

    {Cols, Buffers, RestoreItems2} =
	bind_cols(CNode,
		  StmtHandle,
		  NumResultCols,
		  MaxLenData,
		  MaxLenStr,
		  [],
		  [],
		  RestoreItems1),

    Rows = fetch_rows(CNode, StmtHandle, Buffers, [], RestoreItems2),

    _RestoreItems3 = dealloc_buffer_recs(CNode, Buffers, RestoreItems2),
    {selected, Cols, Rows}.




%% handle_nonselect/4
%%
%% Returns results from all other than SELECT statements.
%%
%% handle_nonselect(CNode, StmtHandle, ConnHandle, RestoreItems) ->
%%   {updated, RowCount}
%%
%% Errors thrown:
%% {error, {RecoverData, ClientInfo}}
%%   RecoverData -> {HandleData, RestoreData}
%%     HandleData -> {HandleType, Handle} | nohandle
%%       HandleType -> integer()
%%       Handle -> integer()
%%     RestoreData -> {restore, [RestoreItem]} |
%%                    norestore
%%       RestoreItem -> {handle, {HandleType, OutputHandle}} |
%%                      {buffer, record(buf_data)}
%%   ClientInfo -> term()
%%
%% Exceptions not handled:
%% {invalid_handle, {IDLFcn, Args, RetVal}}
%%
handle_nonselect(CNode, StmtHandle, ConnHandle, RestoreItems) ->
    RowCount =
	case row_count_wrap(CNode, StmtHandle) of
	    % Continue
	    {ok, RC} ->
		RC;
	    % Error. Throw to top (handle_call).
	    % Restore RestoreData.
	    {error, {HandleData1, CliInfo1}} ->
		throw({error, {{HandleData1, {restore, RestoreItems}}, CliInfo1}})
	end,

    ok =
	case end_tran_wrap(CNode, ?SQL_HANDLE_DBC, ConnHandle, ?SQL_COMMIT) of
	    % Continue
	    ok ->
		ok;
	    % Error. Throw to top (handle_call).
	    % Restore RestoreData.
	    {error, {HandleData2, CliInfo2}} ->
		throw({error, {{HandleData2, {restore, RestoreItems}}, CliInfo2}})
	end,

    {updated, RowCount}.




%% bind_cols/8
%%
%% Allocates buffers and binds them to columns. Returns a list of
%% column names, a list of allocated buffers, and new restore data
%% (where the new buffers have been added).
%% Part of the retrieval of results from SELECT statements.
%%
%% bind_cols(CNode, StmtHandle, ColNum, MaxLenData, MaxLenStr,
%%           ColNames1, Buffers1, RestoreItems1) ->
%%   {ColNames2, Buffers2, RestoreItems2}
%%
%% Errors thrown:
%% {error, {RecoverData, ClientInfo}}
%%   RecoverData -> {HandleData, RestoreData}
%%     HandleData -> {HandleType, Handle} | nohandle
%%       HandleType -> integer()
%%       Handle -> integer()
%%     RestoreData -> {restore, [RestoreItem]} |
%%                    norestore
%%       RestoreItem -> {handle, {HandleType, OutputHandle}} |
%%                      {buffer, record(buf_data)}
%%   ClientInfo -> term()
%%
%% Exceptions not handled:
%% {invalid_handle, {IDLFcn, Args, RetVal}}
%%
bind_cols(_CNode,
	  _StmtHandle,
	  0,
	  _MaxLenData,
	  _MaxLenStr,
	  ColNames1,
	  Buffers1,
	  RestoreItems1) ->

    {ColNames1, Buffers1, RestoreItems1};

bind_cols(CNode,
	  StmtHandle,
	  ColNum,
	  MaxLenData,
	  MaxLenStr,
	  ColNames1,
	  Buffers1,
	  RestoreItems1) ->

    {ColName, BufSz} =
	case describe_col_wrap(CNode, StmtHandle, ColNum, MaxLenStr) of
	    % Size unknown (CSize==0). Use MaxLenData and continue.
	    {ok, {CName, _LenCName, SqlType, 0, _DecDigs, _Nullable}} ->
		{CName, MaxLenData};
	    % Size known. Calculate buffer size and continue.
	    {ok, {CName, _LenCName, SqlType, CSize, _DecDigs, _Nullable}} ->
		DSize = display_size(SqlType, CSize),
		if
		    % DSize too big. Use MaxLenData and continue.
		    DSize > MaxLenData ->
			{CName, MaxLenData};
		    % DSize ok. Continue.
		    true ->
			{CName, DSize}
		end;
	    % Error. Throw to top (handle_call).
	    % Restore RestoreData1.
	    {error, {HandleData1, CliInfo1}} ->
		throw({error, {{HandleData1, {restore, RestoreItems1}}, CliInfo1}})
	end,

    {BufRec, RestoreItems2} =
	alloc_buffer_rec(CNode, ?SQL_C_CHAR, BufSz, RestoreItems1),

    #buf_data{ctype = BufCType, buffer = Buffer, size = Size, len_ind = LenInd} =
        BufRec,
    ok =
	case bind_col_wrap(CNode,
			   StmtHandle,
			   ColNum,
			   BufCType,
			   Buffer,
			   Size,
			   LenInd) of
	    % Continue
	    ok ->
		ok;
	    % Error. Throw to top (handle_call).
	    % Restore RestoreData2.
	    {error, {HandleData3, CliInfo3}} ->
		throw({error, {{HandleData3, {restore, RestoreItems2}}, CliInfo3}})
	end,

    ColNames2 = [ColName | ColNames1],
    Buffers2 = [BufRec | Buffers1],
    bind_cols(CNode,
	      StmtHandle,
	      ColNum-1,
	      MaxLenData,
	      MaxLenStr,
	      ColNames2,
	      Buffers2,
	      RestoreItems2).



%% fetch_rows/5
%%
%% Returns all rows in the result set of a SELECT statement.
%% The order of the rows is undefined.
%% The fcn recurs as long as the last row has not been collected.
%% Each recursion collects a new row. The rows are accumulated
%% in the Rows argument.
%%
%% fetch_rows(CNode, StmtHandle, Buffers, Rows1, RestoreItems) ->
%%   Rows2
%%
%% Errors thrown:
%% {error, {RecoverData, ClientInfo}}
%%   RecoverData -> {HandleData, RestoreData}
%%     HandleData -> {HandleType, Handle} | nohandle
%%       HandleType -> integer()
%%       Handle -> integer()
%%     RestoreData -> {restore, [RestoreItem]} |
%%                    norestore
%%       RestoreItem -> {handle, {HandleType, OutputHandle}} |
%%                      {buffer, record(buf_data)}
%%   ClientInfo -> term()
%%
%% Exceptions not handled:
%% {invalid_handle, {IDLFcn, Args, RetVal}}
%%
fetch_rows(CNode, StmtHandle, Buffers, Rows1, RestoreItems) ->
    case fetch_wrap(CNode, StmtHandle) of
	% All rows fetched
	done ->
	    Rows1;
	% Fetched one row
	ok ->
	    Row = read_buffer_recs(CNode, Buffers, [], RestoreItems),
	    fetch_rows(CNode, StmtHandle, Buffers, [Row | Rows1], RestoreItems);
	% Error. Throw to top (handle_call).
	% Restore RestoreData.
	{error, {HandleData, ClientInfo}} ->
	    throw({error, {{HandleData, {restore, RestoreItems}}, ClientInfo}})
    end.




%% alloc_buffer_rec/4
%%
%% Allocates a data buffer and a length/indicator buffer and
%% returns a buf_data record and a new list of restore data
%% containing the new buffer record.
%%
%% alloc_buffer_rec(CNode, BufCType, Size, RestoreItems1) ->
%%   {record(buf_data), RestoreItems2}
%%
%% Errors thrown:
%% {error, {RecoverData, ClientInfo}}
%%   RecoverData -> {HandleData, RestoreData}
%%     HandleData -> {HandleType, Handle} | nohandle
%%       HandleType -> integer()
%%       Handle -> integer()
%%     RestoreData -> {restore, [RestoreItem]} |
%%                    norestore
%%       RestoreItem -> {handle, {HandleType, OutputHandle}} |
%%                      {buffer, record(buf_data)}
%%   ClientInfo -> term()
%%
%% Exceptions not handled:
%% {invalid_handle, {IDLFcn, Args, RetVal}}
%%
alloc_buffer_rec(CNode, BufCType, Size, RestoreItems1) ->
    DataBuf =
	case alloc_buffer_wrap(CNode, ?SQL_C_CHAR, Size) of
	    % Continue
	    {ok, DB} ->
		DB;
	    % Error. Throw to top (handle_call).
	    % Restore RestoreData1.
	    {error, {HandleData1, CliInfo1}} ->
		throw({error, {{HandleData1, {restore, RestoreItems1}}, CliInfo1}})
	end,
    BufRec1 = #buf_data{buffer = DataBuf, ctype = BufCType, size = Size},

    LenIndBuf =
	case alloc_buffer_wrap(CNode, ?SQL_C_SLONG, 0) of
	    % Continue
	    {ok, LIB} ->
		LIB;
	    % Error. Throw to top (handle_call).
	    % Restore RestoreData1.
	    {error, {HandleData2, ClientInfo2}} ->
		Restore = {restore, [{buffer, BufRec1} | RestoreItems1]},
		throw({error, {{HandleData2, Restore}, ClientInfo2}})
	end,
    BufRec2 = BufRec1#buf_data{len_ind = LenIndBuf},
    RestoreItems2 = [{buffer, BufRec2} | RestoreItems1],
    {BufRec2, RestoreItems2}.




%% dealloc_buffer_recs/3
%%
%% Deallocates buf_data records from a list and returns
%% modified restore data where the deallocated buffers
%% have been removed.
%%
%% dealloc_buffer_recs(CNode, [Buffer | Buffers], RestoreItems1) ->
%%   RestoreItems2
%%
%% Errors thrown:
%% {error, {RecoverData, ClientInfo}}
%%   RecoverData -> {HandleData, RestoreData}
%%     HandleData -> {HandleType, Handle} | nohandle
%%       HandleType -> integer()
%%       Handle -> integer()
%%     RestoreData -> {restore, [RestoreItem]} |
%%                    norestore
%%       RestoreItem -> {handle, {HandleType, OutputHandle}} |
%%                      {buffer, record(buf_data)}
%%   ClientInfo -> term()
%%
%% Exceptions not handled:
%% {invalid_handle, {IDLFcn, Args, RetVal}}
%%
dealloc_buffer_recs(_CNode, [], RestoreItems) ->
    RestoreItems;

dealloc_buffer_recs(CNode, [Buffer | Buffers], RestoreItems1) ->
    RestoreItems2 = dealloc_buffer_rec(CNode, Buffer, RestoreItems1),
    dealloc_buffer_recs(CNode, Buffers, RestoreItems2).


%% dealloc_buffer_rec/3
%%
%% Deallocates the buffers of a single buf_data record
%% and returns new restore data where the buffer is
%% deleted.
%%
%% dealloc_buffer_rec(CNode, Buffer, RestoreItems1) ->
%%   RestoreItems2
%%
%% Errors thrown:
%% {error, {RecoverData, ClientInfo}}
%%   RecoverData -> {HandleData, RestoreData}
%%     HandleData -> {HandleType, Handle} | nohandle
%%       HandleType -> integer()
%%       Handle -> integer()
%%     RestoreData -> {restore, [RestoreItem]} |
%%                    norestore
%%       RestoreItem -> {handle, {HandleType, OutputHandle}} |
%%                      {buffer, record(buf_data)}
%%   ClientInfo -> term()
%%
%% Exceptions not handled:
%% {invalid_handle, {IDLFcn, Args, RetVal}}
%%
dealloc_buffer_rec(CNode, Buffer1, RestoreItems1) ->
    ok =
	case dealloc_buffer_wrap(CNode, Buffer1#buf_data.buffer) of
	    % Continue
	    ok ->
		ok;
	    % Error. Throw to top (handle_call).
	    % Restore RestoreData1.
	    {error, {HandleData1, CliInfo1}} ->
		throw({error, {{HandleData1, {restore, RestoreItems1}}, CliInfo1}})
	end,
    RestoreItems2 = lists:delete({buffer, Buffer1}, RestoreItems1),

    ok =
	case dealloc_buffer_wrap(CNode, Buffer1#buf_data.len_ind) of
	    % Continue
	    ok ->
		ok;
	    % Error. Throw to top (handle_call).
	    % Restore RestoreData1 except deallocated data buffer.
	    {error, {HandleData2, CliInfo2}} ->
		Buffer2 = Buffer1#buf_data{buffer = undefined},
		RD = [{buffer, Buffer2} | RestoreItems2],
		throw({error, {{HandleData2, {restore, RD}}, CliInfo2}})
	end,

    RestoreItems2.



%% read_buffer_recs/4
%%
%% Returns a list of the values contained in a list of buffers.
%% The list of values is ordered as the list of buffers.
%% The buffers are buf_data records. The returned value for such
%% a buffer is either the contents of the length/indicator buffer
%% (null values are indicated here) or the contents of the data
%% buffer.
%% The fcn recurs over the buffer list and collects one buffer value
%% for each recursion. The values are accumulated in the Values
%% argument and they are
%% null |
%% string()
%%
%% read_buffer_recs(CNode, [Buffer | Buffers], Values, RestoreItems) ->
%%   Values1
%%
%% Errors thrown:
%% {error, {RecoverData, ClientInfo}}
%%   RecoverData -> {HandleData, RestoreData}
%%     HandleData -> {HandleType, Handle} | nohandle
%%       HandleType -> integer()
%%       Handle -> integer()
%%     RestoreData -> {restore, [RestoreItem]} |
%%                    norestore
%%       RestoreItem -> {handle, {HandleType, OutputHandle}} |
%%                      {buffer, record(buf_data)}
%%   ClientInfo -> term()
%%
%% Exceptions not handled:
%% {invalid_handle, {IDLFcn, Args, RetVal}}
%%
read_buffer_recs(_CNode, [], Values, _RestoreItems) ->
    lists:reverse(Values);

read_buffer_recs(CNode, [Buffer | Buffers], Values, RestoreItems) ->
    Value = read_buffer_rec(CNode, Buffer, RestoreItems),
    read_buffer_recs(CNode, Buffers, [Value | Values], RestoreItems).


%% read_buffer_rec/3
%%
%% Returns the value contained in a single buf_data record buffer.
%% The returned value for such a buffer is either the contents of
%% the length/indicator buffer (null values are indicated here) or
%% the contents of the data buffer.
%%
%% read_buffer_rec(CNode, BufRec, RestoreItems) ->
%%   null |
%%   string()
%%
%% Errors thrown:
%% {error, {RecoverData, ClientInfo}}
%%   RecoverData -> {HandleData, RestoreData}
%%     HandleData -> {HandleType, Handle} | nohandle
%%       HandleType -> integer()
%%       Handle -> integer()
%%     RestoreData -> {restore, [RestoreItem]} |
%%                    norestore
%%       RestoreItem -> {handle, {HandleType, OutputHandle}} |
%%                      {buffer, record(buf_data)}
%%   ClientInfo -> term()
%%
%% Exceptions not handled:
%% {invalid_handle, {IDLFcn, Args, RetVal}}
%%
read_buffer_rec(CNode, BufRec, RestoreItems) ->
    case read_buffer_wrap(CNode, BufRec#buf_data.len_ind, ?SQL_C_SLONG, 0) of

	% Continue.
	% Data is null, data buffer irrelevant.
	{ok, ?SQL_NULL_DATA} ->
	    null;

	% Continue.
	% Data is not null, so read the data buffer.
	{ok, Length} ->
	    case read_buffer_wrap(CNode,
				  BufRec#buf_data.buffer,
				  BufRec#buf_data.ctype,
				  Length)     of
		% Continue
		% Return Value.
		{ok, Value} ->
		    Value;
		% Error. Throw to top (handle_call).
		% Restore RestoreData.
		{error, {HandleData, CliInfo}} ->
		    throw({error, {{HandleData, {restore, RestoreItems}}, CliInfo}})
	    end;

	% Error. Throw to top (handle_call).
	% Restore RestoreData.
	{error, {HandleData, CliInfo}} ->
	    throw({error, {{HandleData, {restore, RestoreItems}}, CliInfo}})
    end.


%%------------------------------------------------------------------

%% disconnect_impl/1
%%
%% Closes a connection to a data source using.
%%
%% disconnect_impl([State, RefConnHandle]) ->
%%   {ok, {ok, State1}}
%%
%% Errors thrown:
%% {error, {RecoverData, ClientInfo}}
%%   RecoverData -> {HandleData, RestoreData}
%%     HandleData -> {HandleType, Handle} | nohandle
%%       HandleType -> integer()
%%       Handle -> integer()
%%     RestoreData -> {restore, [RestoreItem]} |
%%                    norestore
%%       RestoreItem -> {handle, {HandleType, OutputHandle}} |
%%                      {buffer, record(buf_data)}
%%   ClientInfo -> term()
%%
%% Exceptions not handled:
%% {invalid_handle, {IDLFcn, Args, RetVal}}
%%
disconnect_impl([State, RefConnHandle]) ->
    CNode = State#state.cnode,
    Handles = State#state.handles,

    ConnHandle =
	case lists:keysearch(RefConnHandle, 1, Handles) of
	    % Valid handle reference.
	    {value, {RefConnHandle, ConnHandle1}} ->
		ConnHandle1;
	    % Invalid handle reference.
	    false ->
		throw({error, {{nohandle, norestore}, {badref, 1}}})
	end,

    ok =
	case disconnect_wrap(CNode, ConnHandle) of
	    % Continue
	    ok ->
		ok;
	    % Error. Throw to top (handle_call).
	    % Nothing to restore yet.
	    {error, {HandleData1, CliInfo1}} ->
		throw({error, {{HandleData1, norestore}, CliInfo1}})
	end,
    RestoreData = {restore, [{connection, ConnHandle}]},

    ok =
	case free_handle_wrap(CNode, ?SQL_HANDLE_DBC, ConnHandle) of
	    % Continue
	    ok ->
		ok;
	    % Error. Throw to top (handle_call).
	    % Restore connection.
	    {error, {HandleData2, CliInfo2}} ->
	    throw({error, {{HandleData2, RestoreData}, CliInfo2}})
	end,

    State1 = State#state{handles = lists:keydelete(RefConnHandle, 1, Handles),
			 conn_data = undefined},
    {ok, {ok, State1}}.


%%------------------------------------------------------------------

%% terminate_env_impl/1
%%
%% Terminates the ODBC environment.
%%
%% terminate_env_impl([State, RefEnvHandle]) ->
%%   {ok, {ok, State1}}
%%
%% Errors thrown:
%% {error, {RecoverData, ClientInfo}}
%%   RecoverData -> {HandleData, RestoreData}
%%     HandleData -> {HandleType, Handle} | nohandle
%%       HandleType -> integer()
%%       Handle -> integer()
%%     RestoreData -> {restore, [RestoreItem]} |
%%                    norestore
%%       RestoreItem -> {handle, {HandleType, Handle}} |
%%                      {buffer, record(buf_data)}
%%   ClientInfo -> term()
%%
%% Exceptions not handled:
%% {invalid_handle, {IDLFcn, Args, RetVal}}
%%
terminate_env_impl([State, RefEnvHandle]) ->
    CNode = State#state.cnode,
    Handles = State#state.handles,

    EnvHandle =
	case lists:keysearch(RefEnvHandle, 1, Handles) of
	    % Valid handle reference.
	    {value, {RefEnvHandle, EnvHandle1}} ->
		EnvHandle1;
	    % Invalid handle reference.
	    false ->
		throw({error, {{nohandle, norestore}, {badref, 1}}})
	end,

    ok =
	case free_handle_wrap(CNode, ?SQL_HANDLE_ENV, EnvHandle) of
	    % Continue
	    ok ->
		ok;
	    % Error. Throw to top (handle_call).
	    % Nothing to restore yet.
	    {error, {HandleData, ClientInfo}} ->
	    throw({error, {{HandleData, norestore}, ClientInfo}})
	end,

    State1 = State#state{handles = lists:keydelete(RefEnvHandle, 1, Handles)},
    {ok, {ok, State1}}.





%%%--------------------------------------------------------------------------------
%%% IDL fcn wrappers (Utility API)
%%%--------------------------------------------------------------------------------
%%%
%%% Wrappers for the fcns of the odbclli module.
%%% They call the fcn and interpret the result.
%%% Used only by the Utility API.
%%%
%%% These fcns take the same arguments as do the wrapped fcns.
%%% They return:
%%% {ok, Value} |
%%% {error, {HandleData, ClientInfo}
%%%   HandleData -> {HandleType, Handle} | nohandle
%%%     HandleType -> integer()                  ; Used by recover-fcn to retrieve
%%%     Handle -> integer()                        ODBC error-info.
%%%   ClientInfo -> {ErrType, IDLInfo}           ; Intended to be returned to client.
%%%     ErrType -> internal_error | odbc_error
%%%     IDLInfo -> {IDLFcn, {args, Args}, {retval, RetVal}}
%%%       IDLFcn -> atom()       ; The name of the called IDL fcn.
%%%       Args -> [term()]       ; The argument list
%%%       RetVal -> integer()    ; The returned value from the C node.
%%%
%%% Exceptions:
%%% {invalid_handle, {IDLFcn, Args, RetVal}}    ; Thrown when ODBC returns
%%%   IDLFcn -> atom()                            ?SQL_INVALID_HANDLE, which always
%%%   Args -> {args, [term()]}                    indicates a programming error.
%%%   RetVal -> {retval, integer()}
%%%
%%%--------------------------------------------------------------------------------

alloc_handle_wrap(CNode, HandleType, InputHandle) ->
    {Result, OutputHandle} =
	odbclli:sql_alloc_handle(CNode, HandleType, InputHandle),

    case Result of
	?SQL_SUCCESS ->
	    {ok, OutputHandle};

	?SQL_SUCCESS_WITH_INFO ->
	    {ok, OutputHandle};

	?SQL_ERROR ->
	    HandleData =
		case HandleType of
		    ?SQL_HANDLE_ENV ->
			nohandle;
		    ?SQL_HANDLE_DBC ->
			{?SQL_HANDLE_ENV, InputHandle};
		    ?SQL_HANDLE_STMT ->
			{?SQL_HANDLE_DBC, InputHandle}
		end,
	    Args = {args, [CNode, HandleType, InputHandle]},
	    CliInfo = {odbc_error,
		       {sql_alloc_handle, Args, {retval, ?SQL_ERROR}}},
	    {error, {HandleData, CliInfo}};

	% Programming error.
	% Throw to top (handle_call).
	?SQL_INVALID_HANDLE ->
	    Args = {args, [CNode, HandleType, InputHandle]},
	    throw({invalid_handle,
		   {sql_alloc_handle, Args, {retval, ?SQL_INVALID_HANDLE}}});

	% Other internal error.
	?ERROR ->
	    HandleData = nohandle,
	    Args = {args, [CNode, HandleType, InputHandle]},
	    CliInfo = {internal_error,
		       {sql_alloc_handle, Args, {retval, ?ERROR}}},
	    {error, {HandleData, CliInfo}}
    end.


%%-----------------------------------------------------------------

bind_col_wrap(CNode, StmtHandle, ColNum, BufCType, Buf, Size, LenIndBuf) ->
    case odbclli:sql_bind_col(CNode,
			      StmtHandle,
			      ColNum,
			      BufCType,
			      Buf,
			      Size,
			      LenIndBuf) of
	?SQL_SUCCESS ->
	    ok;
	?SQL_SUCCESS_WITH_INFO ->
	    ok;
	?SQL_ERROR ->
	    HandleData = {?SQL_HANDLE_STMT, StmtHandle},
	    Args =
		{args, [CNode, StmtHandle, ColNum, BufCType, Buf, Size, LenIndBuf]},
	    CliInfo = {odbc_error, {sql_bind_col, Args, {retval, ?SQL_ERROR}}},
	    {error, {HandleData, CliInfo}};
	?SQL_INVALID_HANDLE ->
	    Args =
		{args, [CNode, StmtHandle, ColNum, BufCType, Buf, Size, LenIndBuf]},
	    throw({invalid_handle,
		   {sql_bind_col, Args, {retval, ?SQL_INVALID_HANDLE}}});
	?ERROR ->
	    HandleData = nohandle,
	    Args =
		{args, [CNode, StmtHandle, ColNum, BufCType, Buf, Size, LenIndBuf]},
	    CliInfo = {internal_error, {sql_bind_col, Args, {retval, ?ERROR}}},
	    {error, {HandleData, CliInfo}}
    end.


%%-----------------------------------------------------------------

connect_wrap(CNode, ConnHandle, DSN, UID, PWD) ->
    case odbclli:sql_connect(CNode, ConnHandle, DSN, UID, PWD) of
	?SQL_SUCCESS ->
	    ok;
	?SQL_SUCCESS_WITH_INFO ->
	    ok;
	?SQL_ERROR ->
	    HandleData = {?SQL_HANDLE_DBC, ConnHandle},
	    Args = {args, [CNode, ConnHandle, DSN, UID, PWD]},
	    CliInfo = {odbc_error, {sql_connect, Args, {retval, ?SQL_ERROR}}},
	    {error, {HandleData, CliInfo}};
	?SQL_INVALID_HANDLE ->
	    Args = {args, [CNode, ConnHandle, DSN, UID, PWD]},
	    throw({invalid_handle,
		   {sql_connect, Args, {retval, ?SQL_INVALID_HANDLE}}});
	?ERROR ->
	    HandleData = nohandle,
	    Args = {args, [CNode, ConnHandle, DSN, UID, PWD]},
	    CliInfo = {internal_error, {sql_connect, Args, {retval, ?ERROR}}},
	    {error, {HandleData, CliInfo}}
    end.


%%-----------------------------------------------------------------

describe_col_wrap(CNode, StmtHandle, ColNum, BufLenColName) ->
    {Result, ColName, LenColName, SqlType, ColSize, DecDigs, Nullable} =
	odbclli:sql_describe_col(CNode, StmtHandle, ColNum, BufLenColName),
    case Result of
	?SQL_SUCCESS ->
	    {ok, {ColName, LenColName, SqlType, ColSize, DecDigs, Nullable}};
	?SQL_SUCCESS_WITH_INFO ->
	    {ok, {ColName, LenColName, SqlType, ColSize, DecDigs, Nullable}};
	?SQL_ERROR ->
	    HandleData = {?SQL_HANDLE_STMT, StmtHandle},
	    Args = {args, [CNode, StmtHandle, ColNum, BufLenColName]},
	    CliInfo = {odbc_error, {sql_describe_col, Args, {retval, ?SQL_ERROR}}},
	    {error, {HandleData, CliInfo}};
	?SQL_INVALID_HANDLE ->
	    Args = {args, [CNode, StmtHandle, ColNum, BufLenColName]},
	    throw({invalid_handle,
		   {sql_describe_col, Args, {retval, ?SQL_INVALID_HANDLE}}});
	?ERROR ->
	    HandleData = nohandle,
	    Args = {args, [CNode, StmtHandle, ColNum, BufLenColName]},
	    CliInfo = {internal_error, {sql_describe_col, Args, {retval, ?ERROR}}},
	    {error, {HandleData, CliInfo}}
    end.


%%-----------------------------------------------------------------

disconnect_wrap(CNode, ConnHandle) ->
    case odbclli:sql_disconnect(CNode, ConnHandle) of
	?SQL_SUCCESS ->
	    ok;
	?SQL_SUCCESS_WITH_INFO ->
	    ok;
	?SQL_ERROR ->
	    HandleData = {?SQL_HANDLE_DBC, ConnHandle},
	    Args = {args, [CNode, ConnHandle]},
	    CliInfo = {odbc_error, {sql_disconnect, Args, {retval, ?SQL_ERROR}}},
	    {error, {HandleData, CliInfo}};
	?SQL_INVALID_HANDLE ->
	    Args = {args, [CNode, ConnHandle]},
	    throw({invalid_handle,
		   {sql_disconnect, Args, {retval, ?SQL_INVALID_HANDLE}}});
	?ERROR ->
	    HandleData = nohandle,
	    Args = {args, [CNode, ConnHandle]},
	    CliInfo = {internal_error, {sql_disconnect, Args, {retval, ?ERROR}}},
	    {error, {HandleData, CliInfo}}
    end.



%%-----------------------------------------------------------------

driver_connect_wrap(CNode, ConnHandle, ConnStr, BufLenStr, ComplType) ->
    {Result, OutConnStr, LenOutConnStr} =
	odbclli:sql_driver_connect(CNode, ConnHandle, ConnStr, BufLenStr, ComplType),
    case Result of
	?SQL_SUCCESS ->
	    {ok, {OutConnStr, LenOutConnStr}};
	?SQL_SUCCESS_WITH_INFO ->
	    {ok, {OutConnStr, LenOutConnStr}};
	?SQL_ERROR ->
	    HandleData = {?SQL_HANDLE_DBC, ConnHandle},
	    Args = {args, [CNode, ConnHandle, ConnStr, BufLenStr, ComplType]},
	    CliInfo = {odbc_error, {sql_driver_connect, Args, {retval, ?SQL_ERROR}}},
	    {error, {HandleData, CliInfo}};
	?SQL_INVALID_HANDLE ->
	    Args = {args, [CNode, ConnHandle, ConnStr, BufLenStr, ComplType]},
	    throw({invalid_handle,
		   {sql_driver_connect, Args, {retval, ?SQL_INVALID_HANDLE}}});
	?ERROR ->
	    HandleData = nohandle,
	    Args = {args, [CNode, ConnHandle, ConnStr, BufLenStr, ComplType]},
	    CliInfo = {internal_error, {sql_driver_connect, Args, {retval, ?ERROR}}},
	    {error, {HandleData, CliInfo}}
    end.


%%-----------------------------------------------------------------

end_tran_wrap(CNode, HandleType, Handle, ComplType) ->
    case odbclli:sql_end_tran(CNode, HandleType, Handle, ComplType) of
	?SQL_SUCCESS ->
	    ok;
	?SQL_SUCCESS_WITH_INFO ->
	    ok;
	?SQL_ERROR ->
	    HandleData = {HandleType, Handle},
	    Args = {args, [CNode, HandleType, Handle, ComplType]},
	    CliInfo = {odbc_error, {sql_end_tran, Args, {retval, ?SQL_ERROR}}},
	    {error, {HandleData, CliInfo}};
	?SQL_INVALID_HANDLE ->
	    Args = {args, [CNode, HandleType, Handle, ComplType]},
	    throw({invalid_handle,
		   {sql_end_tran, Args, {retval, ?SQL_INVALID_HANDLE}}});
	?ERROR ->
	    HandleData = nohandle,
	    Args = {args, [CNode, HandleType, Handle, ComplType]},
	    CliInfo = {internal_error, {sql_end_tran, Args, {retval, ?ERROR}}},
	    {error, {HandleData, CliInfo}}
    end.


%%-----------------------------------------------------------------

exec_direct_wrap(CNode, StmtHandle, Stmt) ->
    case odbclli:sql_exec_direct(CNode, StmtHandle, Stmt) of
	?SQL_SUCCESS ->
	    ok;
	?SQL_SUCCESS_WITH_INFO ->
	    ok;
	?SQL_NEED_DATA ->
	    HandleData = {?SQL_HANDLE_STMT, StmtHandle},
	    Args = {args, [CNode, StmtHandle, Stmt]},
	    CliInfo =
		{odbc_error, {sql_exec_direct, Args, {retval, ?SQL_NEED_DATA}}},
	    {error, {HandleData, CliInfo}};
	?SQL_NO_DATA ->
	    HandleData = {?SQL_HANDLE_STMT, StmtHandle},
	    Args = {args, [CNode, StmtHandle, Stmt]},
	    CliInfo =
		{odbc_error, {sql_exec_direct, Args, {retval, ?SQL_NO_DATA}}},
	    {error, {HandleData, CliInfo}};
	?SQL_ERROR ->
	    HandleData = {?SQL_HANDLE_STMT, StmtHandle},
	    Args = {args, [CNode, StmtHandle, Stmt]},
	    CliInfo =
		{odbc_error, {sql_exec_direct, Args, {retval, ?SQL_ERROR}}},
	    {error, {HandleData, CliInfo}};
	?SQL_INVALID_HANDLE ->
	    Args = {args, [CNode, StmtHandle, Stmt]},
	    throw({invalid_handle,
		   {sql_exec_direct, Args, {retval, ?SQL_INVALID_HANDLE}}});
	?ERROR ->
	    HandleData = nohandle,
	    Args = {args, [CNode, StmtHandle, Stmt]},
	    CliInfo = {internal_error, {sql_exec_direct, Args, {retval, ?ERROR}}},
	    {error, {HandleData, CliInfo}}
    end.


%%-----------------------------------------------------------------

fetch_wrap(CNode, StmtHandle) ->
    case odbclli:sql_fetch(CNode, StmtHandle) of
	?SQL_SUCCESS ->
	    ok;
	?SQL_SUCCESS_WITH_INFO ->
	    ok;
	?SQL_NO_DATA ->
	    done;
	?SQL_ERROR ->
	    HandleData = {?SQL_HANDLE_STMT, StmtHandle},
	    Args = {args, [CNode, StmtHandle]},
	    CliInfo = {odbc_error, {sql_fetch, Args, {retval, ?SQL_ERROR}}},
	    {error, {HandleData, CliInfo}};
	?SQL_INVALID_HANDLE ->
	    Args = {args, [CNode, StmtHandle]},
	    throw({invalid_handle,
		   {sql_fetch, Args, {retval, ?SQL_INVALID_HANDLE}}});
	?ERROR ->
	    HandleData = nohandle,
	    Args = {args, [CNode, StmtHandle]},
	    CliInfo = {internal_error, {sql_fetch, Args, {retval, ?ERROR}}},
	    {error, {HandleData, CliInfo}}
    end.


%%-----------------------------------------------------------------

free_handle_wrap(CNode, HandleType, Handle) ->
    case odbclli:sql_free_handle(CNode, HandleType, Handle) of
	?SQL_SUCCESS ->
	    ok;
	?SQL_ERROR ->
	    HandleData = {HandleType, Handle},
	    Args = {args, [CNode, HandleType, Handle]},
	    CliInfo = {odbc_error,
		       {sql_free_handle, Args, {retval, ?SQL_ERROR}}},
	    {error, {HandleData, CliInfo}};
	?SQL_INVALID_HANDLE ->
	    Args = {args, [CNode, HandleType, Handle]},
	    throw({invalid_handle,
		   {sql_free_handle, Args, {retval, ?SQL_INVALID_HANDLE}}});
	?ERROR ->
	    HandleData = nohandle,
	    Args = {args, [CNode, HandleType, Handle]},
	    CliInfo = {internal_error,
		       {sql_free_handle, Args, {retval, ?ERROR}}},
	    {error, {HandleData, CliInfo}}
    end.


%%-----------------------------------------------------------------

get_connect_attr_wrap(CNode, ConnHandle, Attr, BufLen, BufCType) ->
    {Result, CharValue, LenCharValue, NumValue} =
	odbclli:sql_get_connect_attr(CNode, ConnHandle, Attr, BufLen, BufCType),
    case Result of
	?SQL_SUCCESS ->
	    case BufCType of
		?SQL_C_CHAR ->
		    {ok, CharValue};
		?SQL_C_ULONG ->
		    {ok, NumValue}
	    end;
	?SQL_SUCCESS_WITH_INFO ->
	    case BufCType of
		?SQL_C_CHAR ->
		    {ok, CharValue};
		?SQL_C_ULONG ->
		    {ok, NumValue}
	    end;
	?SQL_NO_DATA ->
	    HandleData = {?SQL_HANDLE_DBC, ConnHandle},
	    Args = {args, [CNode, ConnHandle, Attr, BufLen, BufCType]},
	    CliInfo = {odbc_error,
		       {sql_get_connect_attr, Args, {retval, ?SQL_NO_DATA}}},
	    {error, {HandleData, CliInfo}};
	?SQL_ERROR ->
	    HandleData = {?SQL_HANDLE_DBC, ConnHandle},
	    Args = {args, [CNode, ConnHandle, Attr, BufLen, BufCType]},
	    CliInfo = {odbc_error,
		       {sql_get_connect_attr, Args, {retval, ?SQL_ERROR}}},
	    {error, {HandleData, CliInfo}};
	?SQL_INVALID_HANDLE ->
	    Args = {args, [CNode, ConnHandle, Attr, BufLen, BufCType]},
	    throw({invalid_handle,
		   {sql_get_connect_attr, Args, {retval, ?SQL_INVALID_HANDLE}}});
	?ERROR ->
	    HandleData = nohandle,
	    Args = {args, [CNode, ConnHandle, Attr, BufLen, BufCType]},
	    CliInfo = {internal_error,
		       {sql_get_connect_attr, Args, {retval, ?ERROR}}},
	    {error, {HandleData, CliInfo}}
    end.


%%-----------------------------------------------------------------

get_diag_rec_wrap(CNode, HandleType, Handle, RecNumber, BufLenMsg) ->
    case odbclli:sql_get_diag_rec(CNode, HandleType, Handle, RecNumber, BufLenMsg) of
	{?SQL_NO_DATA, _SQLState, _NativeErr, _Msg, _LenMsg} ->
	    done;
	{?SQL_SUCCESS, SQLState, NativeErr, Msg, LenMsg} ->
	    {ok, {SQLState, {NativeErr, Msg, LenMsg}}};
	{?SQL_SUCCESS_WITH_INFO, SQLState, NativeErr, Msg, LenMsg} ->
	    {ok, {SQLState, {NativeErr, Msg, LenMsg}}};
	{?SQL_ERROR, _SQLState, _NativeErr, _Msg, _LenMsg} ->
	    HandleData = nohandle,
	    Args = {args, [CNode, HandleType, Handle, RecNumber, BufLenMsg]},
	    CliInfo = {odbc_error,
		       {sql_get_diag_rec, Args, {retval, ?SQL_ERROR}}},
	    {error, {HandleData, CliInfo}};
	{?SQL_INVALID_HANDLE, _SQLState, _NativeErr, _Msg, _LenMsg} ->
	    Args = {args, [CNode, HandleType, Handle, RecNumber, BufLenMsg]},
	    throw({invalid_handle,
		   {sql_get_diag_rec, Args, {retval, ?SQL_INVALID_HANDLE}}});
	{?ERROR, _SQLState, _NativeErr, _Msg, _LenMsg} ->
	    HandleData = nohandle,
	    Args = {args, [CNode, HandleType, Handle, RecNumber, BufLenMsg]},
	    CliInfo = {internal_error,
		       {sql_get_diag_rec, Args, {retval,?ERROR}}},
	    {error, {HandleData, CliInfo}}
    end.


%%-----------------------------------------------------------------

num_result_cols_wrap(CNode, StmtHandle) ->
    {Result, ColCount} =
	odbclli:sql_num_result_cols(CNode, StmtHandle),
    case Result of
	?SQL_SUCCESS ->
	    {ok, ColCount};
	?SQL_SUCCESS_WITH_INFO ->
	    {ok, ColCount};
	?SQL_ERROR ->
	    HandleData = {?SQL_HANDLE_STMT, StmtHandle},
	    Args = {args, [CNode, StmtHandle]},
	    CliInfo = {odbc_error,
		       {sql_num_result_cols, Args, {retval, ?SQL_ERROR}}},
	    {error, {HandleData, CliInfo}};
	?SQL_INVALID_HANDLE ->
	    Args = {args, [CNode, StmtHandle]},
	    throw({invalid_handle,
		   {sql_num_result_cols, Args, {retval, ?SQL_INVALID_HANDLE}}});
	?ERROR ->
	    HandleData = nohandle,
	    Args = {args, [CNode, StmtHandle]},
	    CliInfo = {internal_error,
		       {sql_num_result_cols, Args, {retval,?ERROR}}},
	    {error, {HandleData, CliInfo}}
    end.


%%-----------------------------------------------------------------

row_count_wrap(CNode, StmtHandle) ->
    {Result, RowCount} =
	odbclli:sql_row_count(CNode, StmtHandle),
    case Result of
	?SQL_SUCCESS ->
	    {ok, RowCount};
	?SQL_SUCCESS_WITH_INFO ->
	    {ok, RowCount};
	?SQL_ERROR ->
	    HandleData = {?SQL_HANDLE_STMT, StmtHandle},
	    Args = {args, [CNode, StmtHandle]},
	    CliInfo = {odbc_error,
		       {sql_row_count, Args, {retval, ?SQL_ERROR}}},
	    {error, {HandleData, CliInfo}};
	?SQL_INVALID_HANDLE ->
	    Args = {args, [CNode, StmtHandle]},
	    throw({invalid_handle,
		   {sql_row_count, Args, {retval, ?SQL_INVALID_HANDLE}}});
	?ERROR ->
	    HandleData = nohandle,
	    Args = {args, [CNode, StmtHandle]},
	    CliInfo = {internal_error,
		       {sql_row_count, Args, {retval,?ERROR}}},
	    {error, {HandleData, CliInfo}}
    end.


%%-----------------------------------------------------------------

set_env_attr_wrap(CNode, EnvHandle, Attr, CharVal, LenCharVal, NumVal, BufType) ->
    case odbclli:sql_set_env_attr(CNode, EnvHandle, Attr,
				  CharVal, LenCharVal, NumVal, BufType) of

	?SQL_SUCCESS ->
	    ok;
	?SQL_SUCCESS_WITH_INFO ->
	    ok;
	?SQL_ERROR ->
	    HandleData = {?SQL_HANDLE_ENV, EnvHandle},
	    Args = {args,
		    [CNode, EnvHandle, Attr, CharVal, LenCharVal, NumVal, BufType]},
	    CliInfo = {odbc_error, {sql_set_env_attr, Args, {retval, ?SQL_ERROR}}},
	    {error, {HandleData, CliInfo}};
	?SQL_INVALID_HANDLE ->
	    Args = {args,
		    [CNode, EnvHandle, Attr, CharVal, LenCharVal, NumVal, BufType]},
	    throw({invalid_handle,
		   {sql_set_env_attr, Args, {retval, ?SQL_INVALID_HANDLE}}});
	?ERROR ->
	    HandleData = nohandle,
	    Args = {args,
		    [CNode, EnvHandle, Attr, CharVal, LenCharVal, NumVal, BufType]},
	    CliInfo = {internal_error,
		       {sql_set_env_attr, Args, {retval, ?ERROR}}},
	    {error, {HandleData, CliInfo}}
    end.


%%-----------------------------------------------------------------

alloc_buffer_wrap(CNode, BufCType, Size) ->
    {Result, Buf} = odbclli:alloc_buffer(CNode, BufCType, Size),
    case Result of
	?OK ->
	    {ok, Buf};
	?ERROR ->
	    Args = {args, [CNode, BufCType, Size]},
	    RetVal = {retval, Result},
	    CliInfo = {internal_error, {alloc_buffer, Args, RetVal}},
	    {error, {nohandle, CliInfo}}
    end.


%%-----------------------------------------------------------------

dealloc_buffer_wrap(_CNode, undefined) ->
    ok;
dealloc_buffer_wrap(CNode, Buffer) ->
    case odbclli:dealloc_buffer(CNode, Buffer) of
	?OK ->
	    ok;
	?ERROR ->
	    Args = {args, [CNode, Buffer]},
	    RetVal = {retval, ?ERROR},
	    CliInfo = {internal_error, {dealloc_buffer, Args, RetVal}},
	    {error, {nohandle, CliInfo}}
    end.


%%-----------------------------------------------------------------

read_buffer_wrap(CNode, Buffer, BufCType, Length) ->
    {Result, CharVal, BinVal, NumVal} =
	odbclli:read_buffer(CNode, Buffer, BufCType, Length),
    case Result of
	?OK ->
	    % Choose the correct return value according to BufCType
	    case BufCType of
		?SQL_C_CHAR ->
		    {ok, CharVal};
		?SQL_C_BINARY ->
		    {ok, BinVal};
		?SQL_C_SLONG ->
		    {ok, NumVal}
	    end;
	?ERROR ->
	    Args = {args, [CNode, Buffer, BufCType]},
	    RetVal = {retval, Result},
	    CliInfo = {internal_error, {read_buffer, Args, RetVal}},
	    {error, {nohandle, CliInfo}}
    end.


%%%-----------------------------------------------------------------------------
%%% Error Handling for Utility API
%%%-----------------------------------------------------------------------------
%%%
%%% The Utility API error handling is done in 2 steps
%%% in the fcn recover/3:
%%% - First ODBC errors are retrieved (if there are any).
%%%   This is done by the fcn get_odbc_errors/4. These
%%%   errors are returned.
%%% - Then resources are restored (if there are any).
%%%   This is done by the fcn restore/3.
%%% Should another error occur, an exception is thrown
%%% to the top level (handle_call).
%%%
%%%-----------------------------------------------------------------------------

%%----------------------------------------------------------------------------
%% recover/3
%%
%% Retrieves ODBC errors, if there are any, and
%% restores resources, if applicable.
%%
%% recover(Operation, RecoverData, State) -> ok | {ok, Errors}
%%
%% Operation -> atom()                                ; Operation to recover from.
%% RecoverData -> {HandleData, RestoreData}
%%   HandleData -> {HandleType, Handle}               ; ODBC handle to get error
%%                 nohandle                             info on. Passed on to
%%                                                      get_odbc_errors.
%%   RestoreData -> {restore, [RestoreItem]} |        ; Data to restore. Passed on to
%%                  norestore                           restore.
%%     RestoreItem -> {handle, {HandleType, Handle} | ; One item to restore.
%%                    {buffer, record(buf_data)}
%% State -> record(state)                             ; The servers internal state
%%
%% Exceptions:
%% {error, ReplyData}                          ; Return of error.
%%   ReplyData -> {ErrType, IDLInfo}           ; Intended to be returned to client.
%%     ErrType -> internal_error | odbc_error
%%     IDLInfo -> {IDLFcn, {args, Args}, {retval, RetVal}}
%%       IDLFcn -> atom()       ; The name of the called IDL fcn.
%%       Args -> [term()]       ; The argument list
%%       RetVal -> integer()    ; The returned value from the C node.
%%
%% {invalid_handle, {IDLFcn, Args, RetVal}}    ; Thrown when ODBC returns
%%   IDLFcn -> atom()                            ?SQL_INVALID_HANDLE, which always
%%   Args -> {args, [term()]}                    indicates a programming error.
%%   RetVal -> {retval, integer()}
%%

% No ODBC error to retrieve. No data to restore.
recover(_Operation, {nohandle, norestore}, _State) ->
    ok;

% Retrieve ODBC error. No data to restore.
recover(_Operation, {HandleData, norestore}, State) ->
    get_odbc_errors(State, HandleData, 1, []);

% No ODBC error to retrieve. Restore data.
recover(Operation, {nohandle, RestoreData}, State) ->
    restore(Operation, RestoreData, State);

% Retrieve ODBC error. Restore data.
recover(Operation, {HandleData, RestoreData}, State) ->
    Result = get_odbc_errors(State, HandleData, 1, []),  % Must get errors first!
    restore(Operation, RestoreData, State),
    Result.




%%------------------------------------------------------------------------------
%% get_odbc_errors/4
%%
%% Returns all ODBC errors currently reported on
%% a specific handle. The order of errors is the
%% same as in ODBC.
%%
%% get_odbc_errors(State, HandleData, ErrNum, ErrorAck) -> {ok, Errors} |
%%                                                         {error, Info}
%%
%% State -> record(state)               ; The server state.
%% HandleData -> {HandleType, Handle}   ; The ODBC handle to get error info on.
%%   HandleType -> integer()            ; The handle type.
%%   Handle -> integer()                ; The handle on which to return ODBC errors.
%% RecNum -> integer()                  ; The record number to get next.
%% ErrorAck -> [Error]                  ; Errors retrieved so far (reverse order).
%% Errors -> [Error]                    ; List of errors returned from ODBC.
%%   Error -> {Fcn, SQLState, More}     ; ODBC error info.
%%   Fcn -> atom()                      ; ODBC fcn.
%%   SQLState -> string()               ; SQL state code.
%%   More -> {NativeCode, Msg, LenMsg}  ; More details.
%%     NativeCode -> string()           ; Data-source specific error code.
%%     Msg -> string()                  ; Error message.
%%     LenMsg -> integer()              ; The length of Msg.
%% Info -> {ErrType, ReplyData} |       ; Error info, returned to client
%%   ErrType -> odbc_error |            ; The type of error encountered.
%%              internal_error
%%   ReplyData -> {IDLFcn, {args, Args}, {retval, SQLReturn}}
%%
%% Exceptions:
%% {error, ReplyData}                          ; Return of error.
%%   ReplyData -> {ErrType, IDLInfo}           ; Intended to be returned to client.
%%     ErrType -> internal_error | odbc_error
%%     IDLInfo -> {IDLFcn, {args, Args}, {retval, RetVal}}
%%       IDLFcn -> atom()       ; The name of the called IDL fcn.
%%       Args -> [term()]       ; The argument list
%%       RetVal -> integer()    ; The returned value from the C node.
%%
%%% {invalid_handle, {IDLFcn, Args, RetVal}}    ; Thrown when ODBC returns
%%%   IDLFcn -> atom()                            ?SQL_INVALID_HANDLE, which always
%%%   Args -> {args, [term()]}                    indicates a programming error.
%%%   RetVal -> {retval, integer()}
%%
get_odbc_errors(State, HandleData, RecNum, ErrorAck) ->
    {HandleType, Handle} = HandleData,
    case get_diag_rec_wrap(State#state.cnode, HandleType,
			   Handle, RecNum, State#state.max_len_err_msg) of
	done ->
	    {ok, lists:reverse(ErrorAck)};
	{ok, Error} ->
	    get_odbc_errors(State, HandleData, RecNum+1, [Error | ErrorAck]);
	{error, {HandleData1, ClientInfo}} ->
	    throw({error, ClientInfo})
    end.


%%----------------------------------------------------------------------------
%% restore/3
%%
%% The restore fcn tries to make the Erlang-server/C-server
%% states consistent, by deallocating memory (buffers and
%% handles), and by reconnecting if necessary. Errors are
%% returned by throwing exceptions.
%%
%% restore(Operation, RestoreData, State) -> ok
%%
%% Operation -> atom()                                    ; The name of the failed
%%                                                          operation to restore.
%% RestoreData -> {restore, [RestoreItem]} |              ; Data to deallocate etc.
%%                norestore
%%   RestoreItem -> {handle, {HandleType, Handle}} |      ; One item to restore.
%%                  {buffer, record(buf_data)} |
%%                  {rollback, RollbackData}
%%     HandleType -> integer()                            ; Type of handle to
%%                                                          restore.
%%     Handle -> integer()                                ; Handle to restore.
%%     RollbackData -> {CommitMode, HandleType, Handle} | ; Info about if and how
%%                     already_committed                    rollback is needed.
%% State -> record(state)                                 ; The servers internal
%%                                                          state before the
%%                                                          operation.
%%
%% Exceptions:
%% {error, ReplyData}
%%   ReplyData -> {ErrType, IDLInfo}           ; Intended to be returned to client.
%%     ErrType -> internal_error | odbc_error
%%     IDLInfo -> {IDLFcn, {args, Args}, {retval, RetVal}}
%%       IDLFcn -> atom()       ; The name of the called IDL fcn.
%%       Args -> [term()]       ; The argument list
%%       RetVal -> integer()    ; The returned value from the C node.
%%
%% {invalid_handle, {IDLFcn, Args, RetVal}}    ; Thrown when ODBC returns
%%   IDLFcn -> atom()                            ?SQL_INVALID_HANDLE, which always
%%   Args -> {args, [term()]}                    indicates a programming error.
%%   RetVal -> {retval, integer()}
%%

restore(Operation, {restore, RestoreItems}, State) ->
    restore(Operation, RestoreItems, State);

restore(_Operation, [], _State) ->
    ok;

restore(Operation, [{handle, HandleData} | Tail], State) ->
    {HandleType, Handle} = HandleData,
    ok =
	case free_handle_wrap(State#state.cnode, HandleType, Handle) of
	    % Continue
	    ok ->
		ok;
	    % Error. Throw to top (handle_call).
	    {error, {_HandleData, ReplyData}} ->
		throw({error, ReplyData})
	end,
    restore(Operation, Tail, State);

restore(Operation, [{buffer, Buffer} | Tail], State) ->
    ok =
	case dealloc_buffer_wrap(State#state.cnode, Buffer#buf_data.buffer) of
	    % Continue
	    ok ->
		ok;
	    % Error. Throw to top (handle_call).
	    {error, {_HandleData1, ReplyData1}} ->
		throw({error, ReplyData1})
	end,

    ok =
	case dealloc_buffer_wrap(State#state.cnode, Buffer#buf_data.len_ind) of
	    % Continue
	    ok ->
		ok;
	    % Error. Throw to top (handle_call).
	    {error, {_HandleData2, ReplyData2}} ->
		throw({error, ReplyData2})
	end,

    restore(Operation, Tail, State);


restore(Operation, [{rollback, already_committed} | Tail], State) ->
    throw({error, {rollback_error, already_committed}});

restore(Operation, [{rollback, {CommitMode, HandleType, Handle}} | Tail], State)
  when CommitMode == ?SQL_AUTOCOMMIT_ON ->
    throw({error, {rollback_error, autocommit_mode}});

restore(Operation, [{rollback, {CommitMode, HandleType, Handle}} | Tail], State) ->
    ok =
	case end_tran_wrap(State#state.cnode, HandleType, Handle, ?SQL_ROLLBACK) of
	    % Continue
	    ok ->
		ok;
	    % Error. Throw to top (handle_call).
	    {error, {_HandleData, ReplyData}} ->
		throw({error, {rollback_error, ReplyData}})
	end,
    restore(Operation, Tail, State);


restore(Operation, [{connection, ConnHandle} | Tail], State) ->
    case State#state.conn_data of
	ConnData when record(ConnData, connect_data) ->
	    ok =
		case connect_wrap(State#state.cnode,
				  ConnHandle,
				  ConnData#connect_data.dsn,
				  ConnData#connect_data.uid,
				  ConnData#connect_data.pwd) of
		    % Connection restored. Continue.
		    ok ->
			ok;
		    % Error. Throw to top (handle_call).
		    {error, {_HandleData2, CliInfo}} ->
			throw({error, {reconnection_error, CliInfo}})
		end;

	ConnData when record(ConnData, drv_connect_data) ->
	    {_OutConnStr, _LenOutConnStr} =
		case driver_connect_wrap(State#state.cnode,
					 ConnHandle,
					 ConnData#drv_connect_data.conn_str,
					 ConnData#drv_connect_data.len_conn_str + 1,
					 ConnData#drv_connect_data.drv_compl)   of
		    % Continue
		    {ok, Value} ->
			Value;
		    % Error. Throw to top (handle_call).
		    {error, {_HandleData, CliInfo}} ->
			throw({error, {reconnection_error, CliInfo}})
		end
    end,
    restore(Operation, Tail, State).
   
	    






%%%---------------------------------------------------------------------
%%% Argument checking
%%%---------------------------------------------------------------------
%%% These fcns check input arguments to the interface
%%% fcns. They all return ok. If an argument is not
%%% valid, the following exception thrown:
%%% {badarg, M, F, A, ArgNo, Info}
%%%   M -> atom()             ; Module.
%%%   F -> atom()             ; Function.
%%%   A -> [term()]           ; Argument list.
%%%   ArgNo -> integer()      ; The ordinal number of the argument in the
%%%                             interface fcn.
%%%   Info -> term()          ; Error info.
%%%
%%%---------------------------------------------------------------------


%%--------------------------------------------------
%% chk_args/4
%%
%% Checks that Args is a list of supported args.
%%
%% chk_args(Args, CallingFunc, OrigArgs, ArgNo) -> ok
%%

% done checking
chk_args([], _CallingFunc, _OrigArgs, _ArgNo) ->
    ok;

% first arg is {buffer_size, Value}
chk_args([{buffer_size, BufSz} | Args], CallingFunc, OrigArgs, ArgNo)
  when integer(BufSz), BufSz > ?MinBufSz ->
    chk_args(Args, CallingFunc, OrigArgs, ArgNo);
chk_args([{buffer_size, BufSz} | _Args], CallingFunc, OrigArgs, ArgNo)
  when integer(BufSz) ->
    exit({badarg,
	  ?MODULE,
	  CallingFunc,
	  OrigArgs,
	  ArgNo,
	  {"Out of range", {buffer_size, BufSz}}});
chk_args([{buffer_size, BufSz} | _Args], CallingFunc, OrigArgs, ArgNo) ->
    exit({badarg,
	  ?MODULE,
	  CallingFunc,
	  OrigArgs,
	  ArgNo,
	  {"Wrong type", {buffer_size, BufSz}}});

% first arg is max_len_str
chk_args([{max_len_str, MaxLenStr} | Args], CallingFunc, OrigArgs, ArgNo)
  when integer(MaxLenStr), MaxLenStr >= 0 ->
    chk_args(Args, CallingFunc, OrigArgs, ArgNo);
chk_args([{max_len_str, MaxLenStr} | _Args], CallingFunc, OrigArgs, ArgNo) ->
    exit({badarg,
	  ?MODULE,
	  CallingFunc,
	  OrigArgs,
	  ArgNo,
	  {"Wrong type or out of range", {max_len_str, MaxLenStr}}});

% first arg is max_len_err_msg
chk_args([{max_len_err_msg, MaxLenErrMsg} | Args], CallingFunc, OrigArgs, ArgNo)
  when integer(MaxLenErrMsg), MaxLenErrMsg >= 0 ->
    chk_args(Args, CallingFunc, OrigArgs, ArgNo);
chk_args([{max_len_err_msg, MaxLenErrMsg} | _Args], CallingFunc, OrigArgs, ArgNo) ->
    exit({badarg,
	  ?MODULE,
	  CallingFunc,
	  OrigArgs,
	  ArgNo,
	  {"Wrong type or out of range", {max_len_err_msg, MaxLenErrMsg}}});

% first arg is max_len_data
chk_args([{max_len_data, MaxLenData} | Args], CallingFunc, OrigArgs, ArgNo)
  when integer(MaxLenData), MaxLenData >= 0 ->
    chk_args(Args, CallingFunc, OrigArgs, ArgNo);
chk_args([{max_len_err_msg, MaxLenErrMsg} | _Args], CallingFunc, OrigArgs, ArgNo) ->
    exit({badarg,
	  ?MODULE,
	  CallingFunc,
	  OrigArgs,
	  ArgNo,
	  {"Wrong type or out of range", {max_len_err_msg, MaxLenErrMsg}}});

% first arg is unsupported
chk_args([Arg | _Args], CallingFunc, OrigArgs, ArgNo) ->
    exit({badarg,
	  ?MODULE,
	  CallingFunc,
	  OrigArgs,
	  ArgNo,
	  {"Unknown tag", Arg}});

% Args not even a list
chk_args(Args, CallingFunc, OrigArgs, ArgNo) ->
    exit({badarg,
	  ?MODULE,
	  CallingFunc,
	  OrigArgs,
	  ArgNo,
	  {"Wrong type", Args}}).



%%--------------------------------------------------------
%% chk_options/4
%%
%% Checks that Options is a list of supported options.
%%
%% chk_options(Options, CallingFunc, OrigArgs, ArgNo) -> ok
%%

% done checking
chk_options([], _CallingFunc, _OrigArgs, _ArgNo) ->
    ok;

% first option is {timeout, Value}
chk_options([{timeout, T} | Opts], CallingFunc, OrigArgs, ArgNo) when integer(T) ->
    chk_options(Opts, CallingFunc, OrigArgs, ArgNo);
chk_options([{timeout, T} | Opts], CallingFunc, OrigArgs, ArgNo) ->
    exit({badarg,
	  ?MODULE,
	  CallingFunc,
	  OrigArgs,
	  ArgNo,
	  {"Wrong type", {timeout, T}}});

% first option is {debug, Dbg}
chk_options([{debug, Dbg} | Opts], CallingFunc, OrigArgs, ArgNo) when list(Dbg) ->
    chk_dbg(Dbg, CallingFunc, OrigArgs, ArgNo),
    chk_options(Opts, CallingFunc, OrigArgs, ArgNo);
chk_options([{debug, Dbg} | Opts], CallingFunc, OrigArgs, ArgNo) ->
    exit({badarg,
	  ?MODULE,
	  CallingFunc,
	  OrigArgs,
	  ArgNo,
	  {"Wrong type", {debug, Dbg}}});

% first option is unsupported
chk_options([Opt | _Opts], CallingFunc, OrigArgs, ArgNo) ->
    exit({badarg,
	  ?MODULE,
	  CallingFunc,
	  OrigArgs,
	  ArgNo,
	  {"Unknown option", Opt}});

% Opts not even a list
chk_options(Opts, CallingFunc, OrigArgs, ArgNo) ->
    exit({badarg,
	  ?MODULE,
	  CallingFunc,
	  OrigArgs,
	  ArgNo,
	  {"Wrong type", Opts}}).


% check debug options as far as reasonable
chk_dbg([], _CallingFunc, _OrigArgs, _ArgNo) ->
    ok;
chk_dbg([trace | Dbgs], CallingFunc, OrigArgs, ArgNo) ->
    chk_dbg(Dbgs, CallingFunc, OrigArgs, ArgNo);
chk_dbg([log | Dbgs], CallingFunc, OrigArgs, ArgNo) ->
    chk_dbg(Dbgs, CallingFunc, OrigArgs, ArgNo);
chk_dbg([statistics | Dbgs], CallingFunc, OrigArgs, ArgNo) ->
    chk_dbg(Dbgs, CallingFunc, OrigArgs, ArgNo);
chk_dbg([{log_to_file, FileName} | Dbgs], CallingFunc, OrigArgs, ArgNo)
  when list(FileName) ->
    chk_dbg(Dbgs, CallingFunc, OrigArgs, ArgNo);
chk_dbg([{log_to_file, FileName} | Dbgs], CallingFunc, OrigArgs, ArgNo) ->
    exit({badarg,
	  ?MODULE,
	  CallingFunc,
	  OrigArgs,
	  ArgNo,
	  {"Wrong type", {log_to_file, FileName}}});
chk_dbg([{install, {Func, _FuncState}} | Dbgs], CallingFunc, OrigArgs, ArgNo)
  when function(Func) ->
    chk_dbg(Dbgs, CallingFunc, OrigArgs, ArgNo);
chk_dbg([{install, {Func, FuncState}} | Dbgs], CallingFunc, OrigArgs, ArgNo) ->
    exit({badarg,
	  ?MODULE,
	  CallingFunc,
	  OrigArgs,
	  ArgNo,
	  {"Wrong type", {install, {Func, FuncState}}}});
chk_dbg([Dbg | _Dbgs], CallingFunc, OrigArgs, ArgNo) ->
    exit({badarg,
	  ?MODULE,
	  CallingFunc,
	  OrigArgs,
	  ArgNo,
	  {"Unknown debug option", Dbg}}).


%%------------------------------------------------------------
%% chk_server/4
%%
%% Checks that Server is a type correct server name
%% (according to gen_server spec.)
%%
%% chk_server(Server, CallingFunc, OrigArgs, ArgNo) -> ok
%%
chk_server({local, Name}, _CallingFunc, _OrigArgs, _ArgNo) when atom(Name) ->
    ok;
chk_server({global, Name}, _CallingFunc, _OrigArgs, _ArgNo) when atom(Name) ->
    ok;
chk_server(Server, CallingFunc, OrigArgs, ArgNo) ->
    exit({badarg,
	  ?MODULE,
	  CallingFunc,
	  OrigArgs,
	  ArgNo,
	  {"Wrong type", Server}}).


%%----------------------------------------------------------------
%% chk_server1/4
%%
%% Checks that Server is a valid server name.
%% Exits otherwise.
%% Does not check that Server is an ODBCE-server
%% or that pids are alive.
%%
%% chk_server1(Server, CallingFunc, OrigArgs, ArgNo) -> ok
%%

% Server is pid
chk_server1(Server, _CallingFunc, _OrigArgs, _ArgNo) when pid(Server) ->
    ok;

% Server is locally registered name
chk_server1(Server, CallingFunc, OrigArgs, ArgNo) when atom(Server) ->
    case whereis(Server) of
	Pid when pid(Pid) ->
	    ok;
	undefined ->
	    exit({badarg,
		  ?MODULE,
		  CallingFunc,
		  OrigArgs,
		  ArgNo,
		  {"Not registered", Server}})
    end;

% Server is globally registered name
chk_server1({global, Name}, CallingFunc, OrigArgs, ArgNo) when atom(Name) ->
    case global:whereis_name(Name) of
	Pid when pid(Pid) ->
	    ok;
	undefined ->
	    exit({badarg,
		  ?MODULE,
		  CallingFunc,
		  OrigArgs,
		  ArgNo,
		  {"Not registered", {global, Name}}})
    end;

% Server is locally registered name on remote node
chk_server1({Name, Node}, CallingFunc, OrigArgs, ArgNo)
  when atom(Name), atom(Node) ->
    case catch rpc:call(Node, erlang, whereis, [Name]) of
	Pid when pid(Pid) ->
	    ok;
	_Other ->
	    exit({badarg,
		  ?MODULE,
		  CallingFunc,
		  OrigArgs,
		  ArgNo,
		  {"Not registered", {Name, Node}}})
    end;

% Type error
chk_server1(Server, CallingFunc, OrigArgs, ArgNo) ->
    exit({badarg,
	  ?MODULE,
	  CallingFunc,
	  OrigArgs,
	  ArgNo,
	  {"Wrong type", Server}}).
    



%%------------------------------------------------------------------
%% chk_timeout/4
%%
%% Checks that Timeout is a valid value.
%%
%% chk_timeout(Timeout, CallingFunc, OrigArgs, ArgNo) -> ok
%%
chk_timeout(Timeout, _CallingFunc, _OrigArgs, _ArgNo) when integer(Timeout) ->
    ok;
chk_timeout(infinity, _CallingFunc, _OrigArgs, _ArgNo) ->
    ok;
chk_timeout(Timeout, CallingFunc, OrigArgs, ArgNo) when atom(Timeout) ->
    exit({badarg,
	  ?MODULE,
	  CallingFunc,
	  OrigArgs,
	  ArgNo,
	  {"Illegal value", Timeout}});
chk_timeout(Timeout, CallingFunc, OrigArgs, ArgNo) ->
    exit({badarg,
	  ?MODULE,
	  CallingFunc,
	  OrigArgs,
	  ArgNo,
	  {"Wrong type", Timeout}}).


%%------------------------------------------------------------------
%% chk_handle_type/4
%%
%% Checks that HandleType is a valid value (env, dbc, or stmt).
%% 
%% chk_handle_type(HandleType, CallingFunc, OrigArgs, ArgNo) -> ok
%%

chk_handle_type(?SQL_HANDLE_ENV, _CallingFunc, _OrigArgs, _ArgNo) ->
    ok;
chk_handle_type(?SQL_HANDLE_DBC, _CallingFunc, _OrigArgs, _ArgNo) ->
    ok;
chk_handle_type(?SQL_HANDLE_STMT, _CallingFunc, _OrigArgs, _ArgNo) ->
    ok;
chk_handle_type(HandleType, CallingFunc, OrigArgs, ArgNo) when integer(HandleType) ->
    exit({badarg,
	  ?MODULE,
	  CallingFunc,
	  OrigArgs,
	  ArgNo,
	  {"Out of range", HandleType}});
chk_handle_type(HandleType, CallingFunc, OrigArgs, ArgNo) ->
    exit({badarg,
	  ?MODULE,
	  CallingFunc,
	  OrigArgs,
	  ArgNo,
	  {"Wrong type", HandleType}}).



%%------------------------------------------------------------------
%% chk_handle_type1/4
%%
%% Checks that  is a valid value (env or dbc).
%% 
%% chk_handle_type1(HandleType, CallingFunc, OrigArgs, ArgNo) -> ok
%%

chk_handle_type1(?SQL_HANDLE_ENV, _CallingFunc, _OrigArgs, _ArgNo) ->
    ok;
chk_handle_type1(?SQL_HANDLE_DBC, _CallingFunc, _OrigArgs, _ArgNo) ->
    ok;
chk_handle_type1(HandleType, CallingFunc, OrigArgs, ArgNo)
  when integer(HandleType) ->
    exit({badarg,
	  ?MODULE,
	  CallingFunc,
	  OrigArgs,
	  ArgNo,
	  {"Out of range", HandleType}});
chk_handle_type1(HandleType, CallingFunc, OrigArgs, ArgNo) ->
    exit({badarg,
	  ?MODULE,
	  CallingFunc,
	  OrigArgs,
	  ArgNo,
	  {"Wrong type", HandleType}}).




%%------------------------------------------------------------------
%% chk_Z/4
%%
%% Checks that a value is integer().
%%
%% chk_Z(Arg, CallingFunc, OrigArgs, ArgNo) -> ok
%%

chk_Z(Arg, _CallingFunc, _OrigArgs, _ArgNo) when integer(Arg) ->
    ok;
chk_Z(Arg, CallingFunc, OrigArgs, ArgNo) ->
    exit({badarg,
	  ?MODULE,
	  CallingFunc,
	  OrigArgs,
	  ArgNo,
	  {"Wrong type", Arg}}).




%%------------------------------------------------------------------
%% chk_Z_plus/4
%%
%% Checks that a value is integer() > 0.
%%
%% chk_Z_plus(Arg, CallingFunc, OrigArgs, ArgNo) -> ok
%%

chk_Z_plus(Arg, _CallingFunc, _OrigArgs, _ArgNo) when integer(Arg), Arg > 0 ->
    ok;
chk_Z_plus(Arg, CallingFunc, OrigArgs, ArgNo) when integer(Arg) ->
    exit({badarg,
	  ?MODULE,
	  CallingFunc,
	  OrigArgs,
	  ArgNo,
	  {"Out of range", Arg}});
chk_Z_plus(Arg, CallingFunc, OrigArgs, ArgNo) ->
    exit({badarg,
	  ?MODULE,
	  CallingFunc,
	  OrigArgs,
	  ArgNo,
	  {"Wrong type", Arg}}).




%%------------------------------------------------------------------
%% chk_buf_c_type/4
%%
%% Checks that a buffer C type argument is valid.
%%
%% chk_buf_c_type(BufCType, CallingFunc, OrigArgs, ArgNo) -> ok
%%

chk_buf_c_type(?SQL_C_CHAR, _CallingFunc, _OrigArgs, _ArgNo) ->
    ok;
chk_buf_c_type(?SQL_C_BINARY, _CallingFunc, _OrigArgs, _ArgNo) ->
    ok;
chk_buf_c_type(BufCType, CallingFunc, OrigArgs, ArgNo) when integer(BufCType) ->
    exit({badarg,
	  ?MODULE,
	  CallingFunc,
	  OrigArgs,
	  ArgNo,
	  {"Out of range", BufCType}});
chk_buf_c_type(BufCType, CallingFunc, OrigArgs, ArgNo) ->
    exit({badarg,
	  ?MODULE,
	  CallingFunc,
	  OrigArgs,
	  ArgNo,
	  {"Wrong type", BufCType}}).



%%------------------------------------------------------------------
%% chk_buf_c_type1/4
%%
%% Checks that a buffer C type argument is valid (for retrieval of attributes).
%%
%% chk_buf_c_type1(BufCType, CallingFunc, OrigArgs, ArgNo) -> ok
%%

chk_buf_c_type1({?SQL_C_CHAR, BufLen}, _CallingFunc, _OrigArgs, _ArgNo)
  when integer(BufLen), BufLen > 0 ->
    ok;
chk_buf_c_type1({?SQL_C_CHAR, BufLen}, CallingFunc, OrigArgs, ArgNo)
  when integer(BufLen) ->
    exit({badarg,
	  ?MODULE,
	  CallingFunc,
	  OrigArgs,
	  ArgNo,
	  {"Out of range", BufLen}});
chk_buf_c_type1({?SQL_C_CHAR, BufLen}, CallingFunc, OrigArgs, ArgNo) ->
    exit({badarg,
	  ?MODULE,
	  CallingFunc,
	  OrigArgs,
	  ArgNo,
	  {"Wrong type", BufLen}});
chk_buf_c_type1(?SQL_C_ULONG, _CallingFunc, _OrigArgs, _ArgNo) ->
    ok;
chk_buf_c_type1({?SQL_C_ULONG, ?SQL_IS_UINTEGER}, _CallingFunc, _OrigArgs, _ArgNo) ->
    ok;
chk_buf_c_type1({?SQL_C_ULONG, ?SQL_IS_INTEGER}, _CallingFunc, _OrigArgs, _ArgNo) ->
    ok;
chk_buf_c_type1(BufCType, CallingFunc, OrigArgs, ArgNo) ->
    exit({badarg,
	  ?MODULE,
	  CallingFunc,
	  OrigArgs,
	  ArgNo,
	  {"Wrong type", BufCType}}).


%%------------------------------------------------------------------
%% chk_buf_c_type2/4
%%
%% Checks that a buffer C type argument is valid (for setting of
%% conn/stmt attributes).
%%
%% chk_buf_c_type2(BufCType, CallingFunc, OrigArgs, ArgNo) -> ok
%%

chk_buf_c_type2(?SQL_C_CHAR, _CallingFunc, _OrigArgs, _ArgNo) ->
    ok;
chk_buf_c_type2(?SQL_C_ULONG, _CallingFunc, _OrigArgs, _ArgNo) ->
    ok;
chk_buf_c_type2({?SQL_C_ULONG, ?SQL_IS_UINTEGER}, _CallingFunc, _OrigArgs, _ArgNo) ->
    ok;
chk_buf_c_type2({?SQL_C_ULONG, ?SQL_IS_INTEGER}, _CallingFunc, _OrigArgs, _ArgNo) ->
    ok;
chk_buf_c_type2(BufCType, CallingFunc, OrigArgs, ArgNo) ->
    exit({badarg,
	  ?MODULE,
	  CallingFunc,
	  OrigArgs,
	  ArgNo,
	  {"Wrong type", BufCType}}).



%%------------------------------------------------------------------
%% chk_buf_c_type3/4
%%
%% Checks that a buffer C type argument is valid (for setting of env attributes).
%%
%% chk_buf_c_type3(BufCType, CallingFunc, OrigArgs, ArgNo) -> ok
%%

chk_buf_c_type3(?SQL_C_CHAR, _CallingFunc, _OrigArgs, _ArgNo) ->
    ok;
chk_buf_c_type3(?SQL_C_ULONG, _CallingFunc, _OrigArgs, _ArgNo) ->
    ok;
chk_buf_c_type3(BufCType, CallingFunc, OrigArgs, ArgNo) ->
    exit({badarg,
	  ?MODULE,
	  CallingFunc,
	  OrigArgs,
	  ArgNo,
	  {"Wrong type", BufCType}}).



%%------------------------------------------------------------------
%% chk_buf_data/4
%%
%% Checks that data may be written to deferred data buffers.
%% Note that no check that data is of the same type as a
%% particular buffer is done here. Only the form of data
%% tuple(2) and the type of the second element is checked.
%%
%% chk_buf_data(Data, CallingFunc, OrigArgs, ArgNo) -> ok
%%

chk_buf_data({_Value, LenInd}, _CallingFunc, _OrigArgs, _ArgNo)
  when integer(LenInd), LenInd > 0 ->
    ok;
chk_buf_data({_Value, LenInd}, _CallingFunc, _OrigArgs, _ArgNo)
  when LenInd == ?SQL_NULL_DATA ->
    ok;
chk_buf_data({_Value, LenInd}, _CallingFunc, _OrigArgs, _ArgNo)
  when LenInd == ?SQL_NTS ->
    ok;
chk_buf_data({_Value, LenInd}, _CallingFunc, _OrigArgs, _ArgNo)
  when LenInd == ?SQL_DATA_AT_EXEC ->
    ok;
chk_buf_data({_Value, {sql_len_data_at_exec, Len}}, _CallingFunc, _OrigArgs, _ArgNo)
  when integer(Len), Len > 0 ->
    ok;
chk_buf_data({_Value, no_len_ind}, _CallingFunc, _OrigArgs, _ArgNo) ->
    ok;
chk_buf_data(Data, CallingFunc, OrigArgs, ArgNo) ->
    exit({badarg,
	  ?MODULE,
	  CallingFunc,
	  OrigArgs,
	  ArgNo,
	  {"Wrong type", Data}}).



%%------------------------------------------------------------------
%% chk_str/4
%%
%% Checks that a string is valid.
%% NOTE: Does not check that the string contains ASCII
%%       characters only or that it doesn't contain a
%%       null-termination character!
%%
%% chk_str(Str, CallingFunc, OrigArgs, ArgNo) -> ok
%%

chk_str(Str, _CallingFunc, _OrigArgs, _ArgNo) when list(Str) ->
    ok;
chk_str(Str, CallingFunc, OrigArgs, ArgNo) ->
    exit({badarg,
	  ?MODULE,
	  CallingFunc,
	  OrigArgs,
	  ArgNo,
	  {"Wrong type", Str}}).



%%------------------------------------------------------------------
%% chk_drv_compl/4
%%
%% Checks that DrvCompl is a valid driver completion (for connecting).
%%
%% chk_drv_compl(DrvCompl, CallingFunc, OrigArgs, ArgNo) -> ok
%%

chk_drv_compl(?SQL_DRIVER_NOPROMPT, _CallingFunc, _OrigArgs, _ArgNo) ->
    ok;
chk_drv_compl(DrvCompl, CallingFunc, OrigArgs, ArgNo) ->
    exit({badarg,
	  ?MODULE,
	  CallingFunc,
	  OrigArgs,
	  ArgNo,
	  {"Out of range", DrvCompl}}).



%%------------------------------------------------------------------
%% chk_compl_type/4
%%
%% Checks that ComplType is a valid value (commit/rollback).
%%
%% chk_compl_type(ComplType, CallingFunc, OrigArgs, ArgNo) -> ok
%%

chk_compl_type(?SQL_COMMIT, _CallingFunc, _OrigArgs, _ArgNo) ->
    ok;
chk_compl_type(?SQL_ROLLBACK, _CallingFunc, _OrigArgs, _ArgNo) ->
    ok;
chk_compl_type(ComplType, CallingFunc, OrigArgs, ArgNo) ->
    exit({badarg,
	  ?MODULE,
	  CallingFunc,
	  OrigArgs,
	  ArgNo,
	  {"Out of range", ComplType}}).



%% chk_value/4
%%
%% Checks that Value is string/integer type and that this matches BufType.
%%
%% chk_value({Value, BufType}, CallingFunc, OrigArgs, ArgNo) -> ok
%%

chk_value({Value, ?SQL_C_CHAR}, _CallingFunc, _OrigArgs, _ArgNo) when list(Value) ->
    ok;
chk_value({Value, ?SQL_C_ULONG}, _CallingFunc, _OrigArgs, _ArgNo)
  when integer(Value) ->
    ok;
chk_value({Value, {?SQL_C_ULONG, _}}, _CallingFunc, _OrigArgs, _ArgNo)
  when integer(Value) ->
    ok;
chk_value({Value, BufType}, CallingFunc, OrigArgs, ArgNo) ->
    exit({badarg,
	  ?MODULE,
	  CallingFunc,
	  OrigArgs,
	  ArgNo,
	  {"Wrong type", Value}}).



%%%----------------------------------------------------------------
%%% Other Internal Fcns
%%%----------------------------------------------------------------


%%----------------------------------------------------------------
%% get_port_reply/1
%%
%% get_port_reply(Port) -> ok | {error, Reason}
%%
%% Returns ok if the tuple {eol, "running"} is received from
%% Port. Returns {error, Reason} if something else is received
%% from Port. Hangs if nothing is received.
%%
get_port_reply(Port) ->
    receive
	{Port, {data, {eol, "running"}}} ->
	    ok;
	{Port, {data, {eol, Reason}}} ->
	    receive
		{Port, {exit_status, Status}} ->
		    {error, {Reason, {exit_status, Status}}}
	    after
		5000 ->
		    {error, Reason}
	    end;
	_Other ->
	    get_port_reply(Port)
    end.
    



%%----------------------------------------------------------------
%% len_ind_val/1
%%
%% Calculates a length indicator value to
%% set a length/indicator buffer to.
%%

% Data is passed at execution (sql_put_data), the length
% is transposed with the macro.
len_ind_val({sql_len_data_at_exec, L}) ->
    ?SQL_LEN_DATA_AT_EXEC(L);

% The length is already specified or will be at execution.
len_ind_val(L) ->
    L.



%%----------------------------------------------------------------
%% get_buf/2
%%
%% Returns a buffer associated with RefBuf in the Buffer list,
%% ?NULL_REF if RefBuf == ?NULL_REF, or false.
%%

get_buf(?NULL_REF, _Buffers) ->
    ?NULL_REF;
get_buf(RefBuf, Buffers) ->
    case lists:keysearch(RefBuf, 1, Buffers) of
	{value, {RefBuf, Buf}} ->
	    {value, Buf};
	false ->
	    false
    end.





