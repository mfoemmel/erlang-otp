%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc
%% CONSTTAB - maps labels to constants.
%% <p>
%% <strong> Note:</strong> 'constant' is a misnomer throughout this code.</p>
%% <p>
%%  There are two different types of constants that can be stored:
%%  <ul>
%%     <li>Erlang terms</li>
%%     <li>Blocks of binary data</li>
%%  </ul>
%% </p>
%% <p>
%% Erlang terms are just what you would expect, you can store any
%% Erlang term in the constant table. 
%% The term is assumed to be loaded to the place in memory denoted by the
%% label returned by the insertion function. 
%% </p>
%% <p>
%% Blocks of binary data comes in some different shapes, you can 
%% either insert a block of integers (of byte, word (4 bytes), or
%% word (8 bytes) size) or a list of references to code.
%% These references will then be threated as word sized addresses
%% and can be used for jumptables.
%% The list of references can have an optiona ordering, so that 
%% you can create a jumptable that will be sorted on the load-time
%% representation of e.g. atoms.
%% </p>
%% @type ctdata(Type::ct_type(), Alignment::alignment(), Exported::bool(), Data::data())
%% @type ct_type() = term | block | sorted_block | ref
%% @type data() = term() | [term()] | [byte()] | internal().
%%  This type is dependent on the ct_type
%% <ul>
%%   <li> If ct_type() = term -- data() = term()</li>
%%   <li> If ct_type() = block -- data() = [byte()] </li>
%%   <li> If ct_type() = sorted_block -- data() = [term()] </li>
%%   <li> If ct_type() = ref -- data() = internal() </li>
%% </ul>
%% @type alignment(A::integer()). 
%%     A = 2^X, X =integer().
%%    Alignment is a power of two.
%% @end
%% @type byte(B::integer). 255 >= B >= 0. 
%% @type const_tab().
%% An abstract datatype for storing data.
%% @end
%%  Internal note:
%%   A consttable is a tuple {Data, ReferedLabels, NextConstLabel}
%% @type lbl().
%%  An abstract datatype for reffering to data.
%% @type element_type() = byte | word | dword | array()
%% @type array(Type::element_type(),NoElements::integer()) = {array, Type, NoElements}
%% @type block() = [integer() | label_ref()]
%% @type label_ref(Label::code_label()) = {label, Label}
%% @type code_label() = hipe_sparc:label_name() | hipe_x86:label_name()
%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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


%% @spec new() -> const_tab()
%% @doc Create a new constant table.
new() -> {gb_trees:empty(), [], 0}.


%% @spec insert_term(ConstTab::const_tab(), Term::term()) -> {NewTab, Lbl}
%% NewTab = const_tab()
%% Lbl = lbl()
%% @doc Inserts an erlang term into the const table.
insert_term(ConstTab, Term) ->
   insert_const(ConstTab, term, 4, false, Term).

%% @spec insert_fun(ConstTab::const_tab(), Term::term()) -> {NewTab, Lbl}
%% NewTab = const_tab()
%% Lbl = lbl()
%% @doc Inserts a Fun into the const table.
%% Don't ask me what this is for...
insert_fun(ConstTab, Fun) ->
   insert_const(ConstTab, term, 4, false, Fun).


%% @spec (ConstTab::const_tab(), TermList::[term()]) -> {NewTab, Lbl}
%% NewTab = const_tab()
%% Lbl = lbl()
%% @doc Inserts a list of terms into the const table.
insert_sorted_block(CTab, TermList) ->
  insert_const(CTab, sorted_block, 
	       4, false, TermList).


%% @spec (ConstTab::const_tab(), InitVal::integer()) -> {NewTab, Lbl}
%% NewTab = const_tab()
%% Lbl = lbl()
%% @doc Inserts a word into the const table.
%% Shorthand for inserting a word.
insert_word(ConstTab, InitVal) ->
   insert_block(ConstTab, 4, word, [InitVal]).

%% @spec (ConstTab::const_tab(), InitVal::integer()) -> {NewTab, Lbl}
%% NewTab = const_tab()
%% Lbl = lbl()
%% @doc Inserts a word into the const table.
%% This constant should be exported from the function...
%% <strong>Note</strong> Global constants are 
%% not supported in current version of HiPE.
insert_global_word(ConstTab, InitVal) ->
   insert_global_block(ConstTab, 4, word, [InitVal]).


%% @spec (ConstTab::const_tab(), Align::alignment(), 
%%             ElementType::element_type(), InitList::block()) -> {const_tab(), lbl()}
%% @doc Inserts a block into the const table.
%% The block can consist of references to labels in the code.
%% This is used for jump tables. These references should be tracked 
%% and the corresponding BBs should not be considered dead.
insert_block({ConstTab, RefToLabels, NextLabel}, 
	     Align, ElementType, InitList) ->
  ReferredLabels = get_labels(InitList, []),
  NewRefTo = ReferredLabels ++ RefToLabels,
  {NewTa, Id} = insert_const({ConstTab, NewRefTo, NextLabel}, 
	       block, Align, false, {ElementType,InitList}),
  {insert_backrefs(NewTa, Id, ReferredLabels), Id}.


%% @spec (ConstTab::const_tab(), Align::alignment(), 
%%             ElementType::element_type(), InitList::block(), SortOrder) -> {const_tab(), lbl()}
%% @doc Inserts a block into the const table.
%% The block can consist of references to labels in the code.
%% This is used for jump tables. These references should be tracked 
%% and the corresponding BBs should not be considered dead.
%% At load-time the block will be sorted according to SortOrder.
%% This is used to make jump tables on atom indices.
insert_sorted_block({ConstTab, RefToLabels, NextLabel}, 
	     Align, ElementType, InitList, SortOrder) ->
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
get_labels([I|Rest],Acc) when is_number(I) -> 
  get_labels(Rest, Acc);
get_labels([],Acc) ->
  Acc.
  
%% @spec (element_type()) -> integer()
%% @doc Returns the size in bytes of an element_type.
size_of(byte) -> 1;
size_of(word) -> 4;
size_of(dword) -> 8;
size_of({array,S,N}) when is_integer(N), N > 0 ->
    N * size_of(S).

%% @spec ({element_type(), block()}) -> [byte()]
%% @doc Turns a block into a list of bytes.
%% <strong>Note:</strong> Be careful with the byte order here.
decompose({ElementType,Data}) ->
  decompose(size_of(ElementType), Data).

decompose(_Bytes, []) ->
   [];
decompose(Bytes, [X|Xs]) ->
   number_to_bytes(Bytes, X, decompose(Bytes, Xs)).


number_to_bytes(0, _, Bytes) ->
   Bytes;
number_to_bytes(N, X, Bytes) ->
   Byte = X band 255,
   number_to_bytes(N-1, X bsr 8, [Byte|Bytes]).



%% @spec ({element_type(), block()}) -> integer()
%% @doc Returns the size in bytes of a block.
block_size({ElementType,Block}) ->
  length(Block)*size_of(ElementType);
block_size({ElementType,Block,_SortOrder}) ->
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
      update(ConstTab,DataLbl,update_const_data(Const, {Type,NewData,Order}))
    end.

update_data(Data, Old, New) ->
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


%% @spec (lbl(), const_tab()) -> ctdata()
%% @doc Lookup a label.
lookup(Lbl, {Table,_RefToLabels,_NextLblNr}) ->
   gb_trees:get(Lbl, Table).


%% @spec (const_tab()) -> [lbl()]
%% @doc Return the labels bound in a table.
labels({Table,_RefToLabels,_NextLblNr}) ->
    gb_trees:keys(Table).


%% @spec (const_tab()) -> [label_ref()]
%% @doc Return the reffered labels bound in a table.
referred_labels({_Table, RefToLabels,_NextLblNr}) ->
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
	      %% A label may be reffered several times.
	      UniqueLbls = ordsets:from_list(DataLbls),
	      lists:foldl(fun(DataLbl, AccTbl) ->
			     insert_ref(
			       delete_ref(OldLbl,
					  update_block_labels(AccTbl, DataLbl, OldLbl, NewLbl)),
			       DataLbl, NewLbl)
			 end,
			 Tbl,
			 UniqueLbls)
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


%%
%% primitives for constants
%%
%% @spec (Type::ct_type(), Alignment::alignment(), Exported::bool(), Data::data())
%%  -> ctdata()
mk_const(Type, Alignment, Exported, Data) ->
   {ctdata, Type, Alignment, Exported, Data}.
%% @spec (ctdata()) -> ct_type()
const_type({ctdata, Type, _Alignment, _Exported, _Data}) -> Type.
%% @spec (ctdata()) -> alignment()
const_align({ctdata,_Type, Alignment, _Exported, _Data}) -> Alignment.
%% @spec (ctdata()) -> bool()
const_exported({ctdata,_Type,_Alignment, Exported,_Data}) -> Exported.
%% @spec (ctdata()) -> data()
const_data({ctdata,_Type,_Alignment,_Exported, Data}) -> Data.
%% @spec (ctdata(), data()) -> ctdata()
update_const_data({ctdata, Type, Alignment, Exported, _}, Data) -> 
  {ctdata, Type, Alignment, Exported, Data}.
%% @spec (ctdata()) -> integer()
%% @doc Returns the size in bytes.
const_size(Constant) ->
  case const_type(Constant) of
    term -> hipe_bifs:term_size(const_data(Constant))*4;
    block -> block_size(const_data(Constant));
    sorted_block -> length(const_data(Constant))*4
  end.

%% @spec (N::integer,E::term()) -> [E]
%% @doc Create a list of length N with the element E.
repeat(N,E) -> repeat(N,E,[]).

repeat(0,_E,Acc) -> Acc;
repeat(N,E,Acc) when N > 0 ->
    repeat(N-1,E,[E|Acc]).

