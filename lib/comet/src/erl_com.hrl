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

%% selectors used by the port for comet. These must correspond to
%% defines in ../c_src/erl_com_drv.h .

-define(ERLCOM_CreateObject , 0).
-define(ERLCOM_QueryInterface , 1).
-define(ERLCOM_Release , 2).
-define(ERLCOM_Invoke , 3).
-define(ERLCOM_PropertyPut , 4).
-define(ERLCOM_GetMethodID , 5).
-define(ERLCOM_PropertyGet , 6).
-define(ERLCOM_GetInterfaceInfo , 7).
-define(ERLCOM_Call , 8).
-define(ERLCOM_GetTypeLibInfo , 9).
-define(ERLCOM_NewThread , 10).
-define(ERLCOM_EndThread , 11).
-define(ERLCOM_CurrentThread , 12).
-define(ERLCOM_PropertyPutRef , 13).
-define(ERLCOM_GetObject , 14).
-define(ERLCOM_Next , 15).
-define(ERLCOM_NextIntf , 16).
-define(ERLCOM_Reset, 17).
-define(ERLCOM_Quit , 50).
-define(ERLCOM_Test , 100).

-define(ERLCOM_DispatchIntf , 1).
-define(ERLCOM_VirtualIntf , 2).

%% some COM defines
-define(CLSCTX_INPROC_SERVER, 1).
-define(CLSCTX_INPROC_HANDLER, 2).
-define(CLSCTX_LOCAL_SERVER, 4).
-define(CLSCTX_REMOTE_SERVER, 16).

-define(IID_IDispatch, "{00020400-0000-0000-C000-000000000046}").

-define(DISPID_VALUE, 0).
-define(DISPID_NEWENUM, -4).
-define(DISPID_EVALUATE, -5).
