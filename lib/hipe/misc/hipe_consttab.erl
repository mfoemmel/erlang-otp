%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% CONSTTAB - maps labels to constants
%
% Data in the table look like:
%
%  * {ctdata, Type, Alignment, Exported, Data}
%     - Type: 'term' or 
%             'block' or 
%             'sorted_block' or
%             'ref'.
%     - Alignment: a power of two.
%     - Exported: 'true' or 'false'.
%     - Data: An erlang term if Type == 'term'. 
%             A bytelist if Type == 'block.
%             A list of terms if Type == 'sorted_block'
%             Internal data if Type == 'ref'
%
% 'constant' is a misnomer throughout this code.
%
%% Todo change name of labels to keys
%% Handle references to keys...

-module(hipe_consttab).

-export([new/0,             % new() -> ConstTab
	 insert_term/2,     % insert_term(ConstTab, Term) -> {NewTab, Lbl}
	 insert_fun/2,      % insert_term(ConstTab, Fun) -> {NewTab, Lbl}
	 insert_word/2,     % insert_word(ConstTab, Value) -> {NewTab, Lbl}
	 insert_sorted_block/2,     % insert_word(ConstTab, ValueList) -> 
	                           % {NewTab, Lbl}
	 insert_sorted_block/5,     
	 insert_block/4,
	 insert_global_word/2,     
	 insert_global_block/4,
	 update_word/3,     % update_word(ConstTab, Value) -> {NewTab, Lbl}
	 update_block/5,
	 update_global_word/3,     
	 update_global_block/5,
	 lookup/2,          % lookup(Key, ConstTab) -> [Term|Block]
	 labels/1,          % labels(ConstTab) -> LabelList
	 referred_labels/1, % referred_labels(ConstTab) -> LabelList
	 update_referred_labels/2, 
	 decompose/1,
	 size_of/1,
	 const_type/1,
	 const_align/1, 
	 const_exported/1,
	 const_data/1,
	 const_size/1,
	 block_size/1,      % size of a block in bytes 
	 repeat/2
	]).

%%
%% A consttable is a tuple {Data, ReferedLabels, NextConstLabel}
%%
%
% Create a new constant table
%

new() ->
    {gb_trees:empty(), [], 0}.


%
% Returns {NewTable, Label}
%

insert_term(ConstTab, Term) ->
   insert_const(ConstTab, term, 4, false, Term).

insert_fun(ConstTab, Fun) ->
   insert_const(ConstTab, term, 4, false, Fun).


insert_sorted_block(CTab, TermList) ->
  {NewTa, Id} = insert_const(CTab,
			     sorted_block, 
			     4, false, TermList).


%
% Shorthand for inserting a word.
%

insert_word(ConstTab, InitVal) ->
   insert_block(ConstTab, 4, word, [InitVal]).

insert_global_word(ConstTab, InitVal) ->
   insert_global_block(ConstTab, 4, word, [InitVal]).


%
% Returns {NewTable, Label}
%

insert_block({ConstTab, RefToLabels, NextLabel}, 
	     Align, ElementType, InitList) ->
%%  io:format("Insert block ~w\n",[{{ConstTab, RefToLabels, NextLabel}, 
%%	     Align, ElementType, InitList}]),
  %%  ByteList = decompose(size_of(ElementType), InitList),
  ReferredLabels = get_labels(InitList, []),
  NewRefTo = ReferredLabels ++ RefToLabels,
  {NewTa, Id} = insert_const({ConstTab, NewRefTo, NextLabel}, 
	       block, Align, false, {ElementType,InitList}),
  {insert_backrefs(NewTa, Id, ReferredLabels), Id}.


%
% Returns {NewTable, Label}
%

insert_sorted_block({ConstTab, RefToLabels, NextLabel}, 
	     Align, ElementType, InitList, SortOrder) ->
%%  io:format("Insert block ~w\n",[{{ConstTab, RefToLabels, NextLabel}, 
%%	     Align, ElementType, InitList}]),
  %%  ByteList = decompose(size_of(ElementType), InitList),
  ReferredLabels = get_labels(InitList, []),
  NewRefTo = ReferredLabels ++ RefToLabels,
  {NewTa, Id} = insert_const({ConstTab, NewRefTo, NextLabel}, 
	       block, Align, false, {ElementType,InitList,SortOrder}),
  {insert_backrefs(NewTa, Id, ReferredLabels), Id}.



insert_backrefs(Tbl, From, ToLabels) ->
  lists:foldl(fun(To,Tab) ->
		  insert_ref(Tab, From, To)
	      end, Tbl, ToLabels).

insert_ref({Table, RefToLabels, NextLblNr}, From, To) ->
  case gb_trees:lookup({To,ref}, Table) of
    none ->
      {gb_trees:insert({To,ref},[From], Table), RefToLabels,
       NextLblNr};
    {value, RefList} ->
      {gb_trees:update({To,ref},[From|RefList], Table), RefToLabels,
       NextLblNr}
  end.

find_refs(To, {Table,_,_}) ->
 %% returns 'none' or {value, V}
  gb_trees:lookup({To,ref}, Table).


delete_ref(To, {ConstTab, RefToLabels, NextLabel}) ->
  {gb_trees:delete({To,ref}, ConstTab), RefToLabels, NextLabel}.
  


%% TODO: handle refs to labels.
insert_global_block(ConstTab, Align, ElementType, InitList) ->
   ByteList = decompose(size_of(ElementType), InitList),
   insert_const(ConstTab, block, Align, true, {byte,ByteList}).

get_labels([{label, L}| Rest], Acc) ->
  get_labels(Rest, [L|Acc]);
get_labels([I|Rest],Acc) when number(I) -> 
  get_labels(Rest, Acc);
get_labels([],Acc) ->
  Acc.
  

size_of(byte) -> 1;
size_of(word) -> 4;
size_of(dword) -> 8;
size_of({array,S,N}) when integer(N), N > 0 ->
    N * size_of(S).

decompose({ElementType,Data}) ->
  decompose(size_of(ElementType), Data).

decompose(Bytes, []) ->
   [];
decompose(Bytes, [X|Xs]) ->
   number_to_bytes(Bytes, X, decompose(Bytes, Xs)).


number_to_bytes(0, _, Bytes) ->
   Bytes;
number_to_bytes(N, X, Bytes) ->
   Byte = X band 255,
   number_to_bytes(N-1, X bsr 8, [Byte|Bytes]).



%
%
%

block_size({ElementType,Block}) ->
  length(Block)*size_of(ElementType);
block_size({ElementType,Block,SortOrder}) ->
  length(Block)*size_of(ElementType).


%
% Update a label
%


%% TODO: Remove RefsTOfrom overwitten labels...
update_word(ConstTab, Label, InitVal) ->
   update_block(ConstTab, Label, 4,word, [InitVal]).

update_global_word(ConstTab, Label, InitVal) ->
   update_global_block(ConstTab, Label, 4,word, [InitVal]).

%
% Update info for an existing label
%
% Returns NewTable
%

update_block(ConstTab, Label, Align, ElementType, InitList) ->
   ByteList = decompose(size_of(ElementType), InitList),
   update_const(ConstTab, Label, block, Align, false, {ElementType,ByteList}).

update_block_labels(ConstTab, DataLbl, OldLbl, NewLbl) ->
  Const = lookup(DataLbl, ConstTab),
  Old = {label, OldLbl},
  case const_data(Const) of
    {Type,Data} -> 
      NewData = update_data(Data, Old, NewLbl),
      update(ConstTab,DataLbl,update_const_data(Const, {Type,NewData}));		
    {Type,Data,Order} -> 
      NewData = update_data(Data, Old, NewLbl),
      update(ConstTab,DataLbl,update_const_data(Const,
						{Type,NewData,
						 Order}))	
    end.

update_data(Data, Old, New) ->
  NewData = 
    lists:map(
      fun(Lbl) when Lbl =:= Old ->
	  {label, New};
	 (Lbl) -> Lbl
	    end,
	    Data).


  
    

update_global_block(ConstTab, Label, Align, ElementType, InitList) ->
   ByteList = decompose(size_of(ElementType), InitList),
   update_const(ConstTab, Label, block, Align, true, ByteList).

%
% Insert a constant in the table, returns {NewTable, Label}.
%

insert_const({Table, RefToLabels, NextLblNr}, Type, Alignment, Exported, Data) ->
   Const = mk_const(Type, Alignment, Exported, Data),
   {{gb_trees:insert(NextLblNr, Const, Table), RefToLabels, NextLblNr+1}, 
    NextLblNr}.

% Update information for a label, returns NewTable.
% (Removes old info.)

update_const({Table, RefToLabels, NextLblNr}, Label, Type, Alignment, Exported, Data) ->
   Const = mk_const(Type, Alignment, Exported, Data),
   {gb_trees:update(Label, Const, Table), RefToLabels, NextLblNr}.

update({Table, RefToLabels, NextLblNr}, Label, NewConst) ->
  {gb_trees:update(Label, NewConst, Table), RefToLabels, NextLblNr}.

%
% lookup a label
%

lookup(Lbl, {Table, RefToLabels, NextLblNr}) ->
   gb_trees:get(Lbl, Table).


%
% Return the labels bound in a table
%

labels({Table, RefToLabels, NextLblNr}) ->
   lists:map(fun({X, Y}) -> X end, gb_trees:to_list(Table)).

%
% Return the reffered labels bound in a table
%

referred_labels({Table, RefToLabels, NextLblNr}) ->
  RefToLabels.


%
% Change label names in constan blocks (jump_tables).
%
update_referred_labels(Table, LabelMap) ->
  %%  io:format("LabelMap: ~w\nTb:~w\n",[LabelMap, Table]),
  {Tb, Refs, Next} =
    lists:foldl(
      fun({OldLbl,NewLbl},Tbl) ->
	  case find_refs(OldLbl,Tbl) of
	    none ->
	      Tbl;
	    {value, DataLbls} ->
	      lists:foldl(fun(DataLbl, AccTbl) ->
			     insert_ref(
			       delete_ref(OldLbl,
					  update_block_labels(AccTbl, DataLbl, OldLbl, NewLbl)),
			       DataLbl, NewLbl)
			 end,
			 Tbl,
			 DataLbls)
	  end
      end,
      Table,
      LabelMap),
  NewRefs = 
    lists:map(fun(Lbl) ->
		  case lists:keysearch(Lbl, 1, LabelMap) of
		    {value, {_, New}} -> New;
		    _ -> Lbl
		  end
	      end,
	      Refs),
  %% io:format("NewTb:~w\n",[{Tb, NewRefs, Next}]),

  {Tb, NewRefs, Next}.


%
% primitves for constants
%

mk_const(Type, Alignment, Exported, Data) ->
   {ctdata, Type, Alignment, Exported, Data}.
const_type({ctdata, Type, Alignment, Exported, Data}) -> Type.
const_align({ctdata, Type, Alignment, Exported, Data}) -> Alignment.
const_exported({ctdata, Type, Alignment, Exported, Data}) -> Exported.
const_data({ctdata, Type, Alignment, Exported, Data}) -> Data.
update_const_data({ctdata, Type, Alignment, Exported, _}, Data) -> 
  {ctdata, Type, Alignment, Exported, Data}.
const_size(Constant) ->
  case const_type(Constant) of
    term -> hipe_bifs:term_size(const_data(Constant))*4;
    block -> block_size(const_data(Constant));
    sorted_block -> length(const_data(Constant))*4
  end.

% When we want to initialize a long list of similar elements:
%  repeat(N,Elt)

repeat(N,E) -> repeat(N,E,[]).

repeat(0,E,Acc) -> Acc;
repeat(N,E,Acc) when N > 0 ->
    repeat(N-1,E,[E|Acc]).

