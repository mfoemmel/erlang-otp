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

-define(UNDEFINED, {}).       % We use the UNDEFINED macro instead of the atom 'undefined',
                              % to avoid confusion.


-define(active, 1).
-define(notInService, 2).
-define(notReady, 3).
-define(createAndGo, 4).      % Action: written, not read.
-define(createAndWait, 5).    % Action: written, not read.
-define(destroy, 6).          % Action: written, not read.



-define(unlocked, 1).
-define(shutting_down, 2).
-define(locked, 3).


-define(started, 1).
-define(stopped, 2).


-define(enabled, 1).
-define(disabled, 2).


-define(upper, 1).
-define(lower, 2).


%% We map each interesting field in the records to an integer.
%% This integer *DOES NOT* correspond to the placement of the
%% field in the tuple implementation of the record (unless by 
%% coincidence!). Instead we check against the number to get 
%% the correct field name when extracting values from the record!
%% Note: all numbers have to be in sequence, i.e., no "holes" are allowed!!!



%% ************************
%% For the typeTable table:
%% ************************

-define(TypeName, 1).        % Only used as index!
-define(TypeInfo, 2).
-define(TypeCallbackMod, 3).
-define(TypeAdminState, 4).
-define(TypeMeasArgs, 5).
-define(TypeMaxInst, 6).
-define(TypeStatusCol, 7).

-define(FirstTypeCol, 2).    % First column not used as index!
-define(LastTypeCol, 7).

%%     - * -





%% ****************************
%% For the typeInfoTable table:
%% ****************************

-define(TypeInfoName, 1).
-define(TypeInfoCurrInst, 2).

-define(FirstTypeInfoCol, 2).
-define(LastTypeInfoCol, 2).

%%     - * -





%% ************************
%% For the measTable table:
%% ************************

-define(MeasId, 1).
-define(MeasType, 2).
-define(MeasInfo, 3).
-define(MeasResId, 4).
-define(MeasAdminState, 5).
-define(MeasArgs, 6).
-define(MeasStatusCol, 7).

-define(FirstMeasCol, 2).
-define(LastMeasCol, 7).

%%     - * -






%% ****************************
%% For the measInfoTable table:
%% ****************************

-define(MeasInfoId, 1).
% -define(MeasInfoOperState, 2).   Currently not used...
-define(MeasInfoLastVal, 2).
-define(MeasInfoLastValTime, 3).
-define(MeasInfoLastValInfo, 4).
-define(MeasInfoMaxTideCurr, 5).
-define(MeasInfoMaxTidePrev, 6).
-define(MeasInfoMinTideCurr, 7).
-define(MeasInfoMinTidePrev, 8).
-define(MeasInfoLastReset, 9).

-define(FirstMeasInfoCol, 2).
-define(LastMeasInfoCol, 9).

%%     - * -




%% ****************************
%% For the threshTable table:
%% ****************************

-define(ThreshMeasId, 1).
-define(ThreshId, 2).
-define(ThreshType, 3).
-define(ThreshVal1, 4).
-define(ThreshVal2, 5).
-define(ThreshAdminState, 6).
-define(ThreshStatusCol, 7).

-define(FirstThreshCol, 3).
-define(LastThreshCol, 7).

%%     - * -
