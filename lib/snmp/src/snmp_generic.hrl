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
%%-----------------------------------------------------------------
%% TableInfo - stored in snmp_symbolic_store for use by the
%% generic table functions.
%%   nbr_of_cols      is an integer
%%   defvals          is a list of {Col, Defval}, ordered by column
%%                    number
%%   status_col       is an integer
%%   not_accessible   a sorted list of columns (> first_accessible) that are
%%                    'not-accessible'
%%   indextypes       is a list of #asn1_type for the index-columns,
%%                    ordered by column number
%%   first_accessible is an integer, the first non-accessible
%%                    column
%%   first_own_index  is an integer. 0 if there is no such index for this table.
%%                    This is not the same as the last integer in the oid!
%%                    Example: If a table has one own index (oid.1), one column
%%                    (oid.2) and one imported index then first_own_index
%%                    will be 2.
%%-----------------------------------------------------------------

-record(table_info, {nbr_of_cols, defvals = [], status_col, not_accessible,
                     index_types, first_accessible = 1, first_own_index}).

%%-----------------------------------------------------------------
%% TableInfo - stored in snmp_symbolic_store for use by the
%% generic variable functions.
%%   defval           is a default value for the variable
%%-----------------------------------------------------------------
-record(variable_info, {defval}).
