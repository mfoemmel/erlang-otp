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
-module(company_o).
-compile(export_all).
-compile({parse_transform,mnemosyne_lc}).

-import(mnemosyne, [eval/1]).
-import(mnesia, [transaction/1]).

%0
-include("company_o.hrl").


sinit() ->
    mnesia:create_schema([node()]).

init() ->
    mnesia:create_table(employee,
                        [{attributes, record_info(fields, employee)}]),
    mnesia:create_table(dept,
                        [{attributes, record_info(fields, dept)}]),
    mnesia:create_table(project, 
                        [{attributes, record_info(fields, project)}]).

%0
    


%1

insert_emp(Emp, DeptId, ProjNames) ->
    Ename = Emp#employee.name,
    Fun = fun() ->
                  mnesia:write(Emp#employee{dept = DeptId, 
					    projects = ProjNames})
          end,
    mnesia:transaction(Fun).


%1
 
%2
females() ->
    F = fun() ->
                mnemosyne:eval(query [E.name || E <- table(employee),
                                                E.sex = female]
                               end)
        end,
    mnesia:transaction(F).
%2

%3
female_bosses() ->
    F = fun() -> mnemosyne:eval(query 
				     [{E.name, Boss.name} ||
                                      E <- table(employee),
				      BossNo = E.manager,
				      Boss <- table(employee),
				      Boss.emp_no = BossNo,
				      E.sex = female] 
				end)
        end,
    mnesia:transaction(F).

                    
%4
raise_females(Amount) ->
    F = fun() ->
                Fs = mnemosyne:eval(query [E || E <-table(employee),
                                                E.sex = female]
                                    end),
                over_write(Fs, Amount)
        end,
    mnesia:transaction(F).

over_write([E|Tail], Amount) ->
    Salary = E#employee.salary + Amount,
    New = E#employee{salary = Salary},
    mnesia:write(New),
    1 + over_write(Tail, Amount);
over_write([], _) ->
    0.
%4

%5
raise(Eno, Raise) ->
    F = fun() ->
                [E] = mnesia:read({employee, Eno}),
                Salary = E#employee.salary + Raise,
                New = E#employee{salary = Salary},
                mnesia:write(New)
        end,
    mnesia:transaction(F).
%5


%6
bad_raise(Eno, Raise) ->
    F = fun() ->
                [E] = mnesia:read({employee, Eno}),
                Salary = E#employee.salary + Raise,
                New = E#employee{salary = Salary},
                io:format("Trying to write ... ~n", []),
                mnesia:write(New)
        end,
    mnesia:transaction(F).
%6


%7
f1() ->
    transaction(fun() ->
                        eval(query [E || E <- table(employee), 
                                         E.sex = female]
                             end)
                end).
%7

%8
f2() ->
    Pat0 = mnesia:table_info(employee, wild_pattern),
    Pat = Pat0#employee{sex = female},
    mnesia:transaction(fun() ->
                               mnesia:match_object(Pat)
                       end).
%8

                       
%9
get_emps(Salary, Dep) ->
    F = fun() -> eval(query
                      [E || E <- table(employee),
                            E.dept = Dept,
                            E.salary > Salary]
                      end)
        end,
    transaction(F).
%9



%10
get_emps2(Salary, Dep) ->
    Epat0 = mnesia:table_info(employee, wild_pattern),
    Epat = Epat0#employee{dept = Dep},
    F = fun() ->
                All = mnesia:match_object(Epat),
		[E || E <-All, E#employee.salary > Salary ]
	end,
    mnesia:transaction(F).
                

%10

