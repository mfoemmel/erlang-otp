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
 *
 */

/*
 * Purpose: Erlang-COM driver
 */

#define com_CreateObject	0
#define com_QueryInterface	1
#define com_Release		2
#define com_Invoke		3
#define com_PropertyPut		4
#define com_GetMethodID		5
#define com_PropertyGet		6
#define com_GetInterfaceInfo	7
#define com_Call	  	8
#define com_GetTypeLibInfo	9
#define com_NewThread	       10
#define com_EndThread	       11
#define com_Quit	       50
#define com_Test	      100

#define com_DispatchIntf        1
#define com_VirtualIntf		2