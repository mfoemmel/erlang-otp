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
-module(erl_tar).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Purpose: Unix tar (tape archive) utility.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export([create/2, create/3, extract/1, extract/2, table/1, table/2,
	 open/2, close/1, add/3, add/4,
	 t/1, tt/1, format_error/1]).

-include_lib("kernel/include/file.hrl").

-record(add_opts,
	{read_info,				% Fun to use for read file/link info.
	 verbose = false}).			% Verbose on/off.

%% Opens a tar archive.

open(Name, Mode) ->
    case open_mode(Mode) of
	{ok, Access, Opts} ->
	    open1(Name, Access, Opts);
	{error, Reason} ->
	    {error, {Name, Reason}}
    end.

open1({binary, Bin}, read, Opts) ->
    case ram_file:open(Bin, [binary, read|Opts]) of
	{ok, File} ->
	    {ok, {read, File}};
	Error ->
	    Error
    end;
open1({file, Fd}, read, Opts) ->
    {ok, {read, Fd}};
open1(Name, Access, Opts) ->
    case file:open(Name, [raw, binary, Access|Opts]) of
	{ok, File} ->
	    {ok, {Access, File}};
	{error, Reason} ->
	    {error, {Name, Reason}}
    end.

%% Closes a tar archive.

close({read, File}) ->
    file:close(File),
    ok;
close({write, File}) ->
    PadResult = pad_file(File),
    file:close(File),
    PadResult;
close(_) ->
    {error, einval}.

%% Adds a file to a tape archive.

add(File, Name, Options) ->
    add(File, Name, Name, Options).

add({write, File}, Name, NameInArchive, Options) ->
    Opts = #add_opts{read_info={file,read_link_info}},
    add1(File, Name, NameInArchive, add_opts(Options, Opts));
add({read, _File}, _, _, _) ->
    {error, eacces};
add(_, _, _, _) ->
    {error, einval}.

add_opts([dereference|T], Opts) ->
    add_opts(T, Opts#add_opts{read_info={file,read_file_info}});
add_opts([verbose|T], Opts) ->
    add_opts(T, Opts#add_opts{verbose=true});
add_opts([_|T], Opts) ->
    add_opts(T, Opts);
add_opts([], Opts) ->
    Opts.

%% Creates a tar file Name containing the given files.

create(Name, Filenames) ->
    create(Name, Filenames, []).

%% Creates a tar archive Name containing the given files.
%% Accepted options: verbose, compressed

create(Name, FileList, Options) ->
    Mode = case lists:member(compressed, Options) of
	       true ->  [compressed];
	       false -> []
	   end,
    case open(Name, [write|Mode]) of
	{ok, TarFile} ->
	    Add = fun(Nm) -> add(TarFile, Nm, Nm, Options) end,
	    Result = foreach_while_ok(Add, FileList),
	    case {Result, close(TarFile)} of
		{ok, Res} -> Res;
		{Res, _} -> Res
	    end;
	Reason ->
	    Reason
    end.

%% Extracts all files from the tar file Name.

extract(Name) ->
    extract(Name, []).

%% Extracts (all) files from the tar file Name.
%% Options accepted: keep_old_files, {files, ListOfFilesToExtract}, verbose,
%%		{cwd, AbsoluteDirectory}

extract(Name, Opts) ->
    case foldl_read(Name, fun extract1/4, [], extract_opts(Opts)) of
	[] ->
	    ok;
	Other ->
	    Other
    end.

%% Returns a list of names of the files in the tar file Name.
%% Options accepted: verbose

table(Name) ->
    table(Name, []).

%% Returns a list of names of the files in the tar file Name.
%% Options accepted: compressed, verbose.

table(Name, Opts) ->
    foldl_read(Name, fun table1/4, [], table_opts(Opts)).


%% Comments for printing the contents of a tape archive,
%% meant to be invoked from the shell.

t(Name) ->
    case table(Name) of
	{ok, List} ->
	    lists:foreach(fun(N) -> ok = io:format("~s\n", [N]) end, List);
	Error ->
	    Error
    end.

tt(Name) ->
    case table(Name, [verbose]) of
	{ok, List} ->
	    lists:foreach(fun print_header/1, List);
	Error ->
	    Error
    end.

print_header({Name, Type, Size, Mtime, Mode, Uid, Gid}) ->
    io:format("~s~s ~4w/~-4w ~7w ~s ~s\n",
	      [type_to_string(Type), mode_to_string(Mode),
	       Uid, Gid, Size, time_to_string(Mtime), Name]).

type_to_string(regular) -> "-";
type_to_string(directory) -> "d";
type_to_string(link) -> "l";
type_to_string(symlink) -> "s";
type_to_string(char) -> "c";
type_to_string(block) -> "b";
type_to_string(fifo) -> "f";
type_to_string(_) -> "?".

mode_to_string(Mode) ->
    mode_to_string(Mode, "xwrxwrxwr", []).

mode_to_string(Mode, [C|T], Acc) when Mode band 1 == 1 ->
    mode_to_string(Mode bsr 1, T, [C|Acc]);
mode_to_string(Mode, [_|T], Acc) ->
    mode_to_string(Mode bsr 1, T, [$-|Acc]);
mode_to_string(_, [], Acc) ->
    Acc.

time_to_string({{Y, Mon, Day}, {H, Min, _}}) ->
    io_lib:format("~s ~2w ~s:~s ~w", [month(Mon), Day, two_d(H), two_d(Min), Y]).

two_d(N) ->
    tl(integer_to_list(N + 100)).

month(1) -> "Jan";
month(2) -> "Feb";
month(3) -> "Mar";
month(4) -> "Apr";
month(5) -> "May";
month(6) -> "Jun";
month(7) -> "Jul";
month(8) -> "Aug";
month(9) -> "Sep";
month(10) -> "Oct";
month(11) -> "Nov";
month(12) -> "Dec".

%% Converts short error reason to a descriptive string.

format_error(bad_header) -> "Bad directory header";
format_error(eof) -> "Unexpected end of file";
format_error(unsupported_file_type) -> "File type not supported";
format_error({Name, Reason}) ->
    lists:flatten(io_lib:format("~s: ~s", [Name, format_error(Reason)]));
format_error(Atom) when atom(Atom) ->
    file:format_error(Atom);
format_error(Term) ->
    lists:flatten(io_lib:format("~p", [Term])).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%	Useful definitions (also start of implementation).
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Offset for fields in the tar header.
%% Note that these offsets are ZERO-based as in the POSIX standard
%% document, while binaries use ONE-base offset.  Caveat Programmer.

-define(th_name, 0).
-define(th_mode, 100).
-define(th_uid, 108).
-define(th_gid, 116).
-define(th_size, 124).
-define(th_mtime, 136).
-define(th_chksum, 148).
-define(th_typeflag, 156).
-define(th_linkname, 157).
-define(th_prefix, 345).

%% Length of these fields.

-define(th_name_len, 100).
-define(th_mode_len, 8).
-define(th_uid_len, 8).
-define(th_gid_len, 8).
-define(th_size_len, 12).
-define(th_mtime_len, 12).
-define(th_chksum_len, 8).
-define(th_linkname_len, 100).
-define(th_prefix_len, 167).

-record(tar_header,
	{name,					% Name of file.
	 mode,					% Mode bits.
	 uid,					% User id.
	 gid,					% Group id.
	 size,					% Size of file
	 mtime,					% Last modified (seconds since
						% Jan 1, 1970).
	 chksum,				% Checksum of header.
	 typeflag = [],				% Type of file.
	 linkname = [],				% Name of link.
	 filler = [],
	 prefix}).				% Filename prefix.

-define(record_size, 512).
-define(block_size, (512*20)).

fields() ->
    Fields = #tar_header {name={s, 100},
			  mode={o, 8},
			  uid={o, 8},
			  gid={o, 8},
			  size={o, ?th_size_len},
			  mtime = {o, ?th_mtime_len},
			  chksum = {s, ?th_chksum_len},
			  typeflag = {s, 1},
			  linkname = {s, ?th_linkname_len},
			  filler = {s, ?th_prefix-?th_linkname-?th_linkname_len},
			  prefix = {s, ?th_prefix_len}},
    tl(tuple_to_list(Fields)).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% 	Adding members to a tar archive.
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

add1(TarFile, Name, NameInArchive, Opts) ->
    case read_file_and_info(Name, Opts) of
	{ok, Bin, Info} when Info#file_info.type == regular ->
	    Header = create_header(NameInArchive, Info),
	    add1(TarFile, Name, Header, Bin, Opts);
	{ok, PointsTo, Info} when Info#file_info.type == symlink ->
	    Info2 = Info#file_info{size=0},
	    Header = create_header(NameInArchive, Info2, PointsTo),
	    add1(TarFile, Name, Header, list_to_binary([]), Opts);
	{ok, Bin, Info} when Info#file_info.type == directory ->
	    add_directory(TarFile, Name, NameInArchive, Info, Opts);
	{ok, Bin, #file_info{type=Type}} ->
	    {error, {bad_file_type, Name, Type}};
	{error, Reason} ->
	    {error, {Name, Reason}};
	Error ->				% XXX: Case maybe not needed
	    Error
    end.

add1(Tar, Name, Header, Bin, Options) ->
    add_verbose(Options, "a ~s~n", [Name]),
    file:write(Tar, [Header, Bin, padding(size(Bin), ?record_size)]).

add_directory(TarFile, DirName, NameInArchive, Info, Options) ->
    case file:list_dir(DirName) of
	{ok, []} ->
	    add_verbose(Options, "a ~s~n", [DirName]),
	    Header = create_header(NameInArchive, Info),
	    file:write(TarFile, Header);
	{ok, Files} ->
	    Add = fun (File) ->
			  add1(TarFile,
			       filename:join(DirName, File),
			       filename:join(NameInArchive, File),
			       Options) end,
	    foreach_while_ok(Add, Files);
	{error, Reason} ->
	    {error, {DirName, Reason}}
    end.
    
%% Creates a header for file in a tar file.

create_header(Name, Info) ->
    create_header(Name, Info, []).
create_header(Name, #file_info {mode=Mode, uid=Uid, gid=Gid,
				size=Size, mtime=Mtime0, type=Type}, Linkname) ->
    Mtime = posix_time(erlang:localtime_to_universaltime(Mtime0)),
    {Prefix, Suffix} = split_filename(Name),
    create_header(#tar_header {name = Suffix,
			       mode = Mode,
			       uid = Uid,
			       gid = Gid,
			       size = Size,
			       mtime = Mtime,
			       typeflag = file_type(Type),
			       linkname = Linkname,
			       prefix = Prefix}).

file_type(regular) -> "0";
file_type(symlink) -> "2";
file_type(directory) -> "5".
    
%% Creates header from record.

create_header(Record0) ->
    %% Very important: There must 8 blanks in the chksum field below.
    Record = Record0#tar_header {chksum = "        "},
    create_header(tl(tuple_to_list(Record)), fields(), 0, []).

create_header([Value|Records], [{Type, Size}|Types], TotalSize, Result) ->
    create_header(Records, Types, TotalSize+Size,
		  [create_field(Type, Size, Value)|Result]);
create_header([], [], TotalSize, Result) ->
    512 = TotalSize,
    H = list_to_binary(lists:reverse(Result)),

    %% Calculate the checksum and insert it into the header.

    Checksum = flatsum(Result, 0),
    ChksumString = to_octal(Checksum, 6, [0, $\s]),
    {Before, _} = split_binary(H, ?th_chksum),
    {_, After} = split_binary(H, ?th_chksum+?th_chksum_len),
    [Before, ChksumString, After].

create_field(o, Size, Int) when integer(Int) ->
    to_octal(Int, Size);
create_field(s, Size, String) when list(String) ->
    to_string(String, Size, []);
create_field(b, Size, Int) when integer(Int) ->
    Int.

to_octal(Int, Count) when Count > 1 ->
    to_octal(Int, Count-1, [0]).

to_octal(Int, 0, Result) ->
    Result;
to_octal(Int, Count, Result) ->
    to_octal(Int div 8, Count-1, [Int rem 8 + $0|Result]).

to_string(String, 0, Result) ->
    lists:reverse(Result);
to_string([C|Rest], Count, Result) ->
    to_string(Rest, Count-1, [C|Result]);
to_string([], Count, Result) ->
    to_string([], Count-1, [0|Result]).

%% Pads out end of file.

pad_file(File) ->
    {ok, Position} = file:position(File, {cur, 0}),
    %% There must be at least one empty record at the end of the file.
    Zeros = zeros(?block_size - (Position rem ?block_size)),
    file:write(File, Zeros).

split_filename(Name) when length(Name) =< ?th_name_len ->
    {"", Name};
split_filename(Name0) ->
    split_filename(lists:reverse(filename:split(Name0)), [], [], 0).

split_filename([Comp|Rest], Prefix, Suffix, Len) 
when Len+length(Comp) < ?th_name_len ->
    split_filename(Rest, Prefix, [Comp|Suffix], Len+length(Comp)+1);
split_filename([Comp|Rest], Prefix, Suffix, Len) ->
    split_filename(Rest, [Comp|Prefix], Suffix, Len+length(Comp)+1);
split_filename([], Prefix, Suffix, Len) ->
    {filename:join(Prefix), filename:join(Suffix)}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% 	Retrieving files from a tape archive.
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Options used when reading a tar archive.

-record(read_opts,
	{cwd,					% Current working directory.
	 keep_old_files = false,		% Owerwrite or not.
	 files = all,				% Set of files to extract
						% (or all).
	 open_mode = [],			% Open mode options.
	 verbose = false}).			% Verbose on/off.

extract_opts(List) ->
    extract_opts(List, default_options()).

table_opts(List) ->
    read_opts(List, default_options()).

default_options() ->
    {ok, Cwd} = file:get_cwd(),
    #read_opts{cwd=Cwd}.

%% Parse options for extract.

extract_opts([keep_old_files|Rest], Opts) ->
    extract_opts(Rest, Opts#read_opts{keep_old_files=true});
extract_opts([{cwd, Cwd}|Rest], Opts) ->
    extract_opts(Rest, Opts#read_opts{cwd=Cwd});
extract_opts([{files, Files}|Rest], Opts) ->
    Set = ordsets:list_to_set(Files),
    extract_opts(Rest, Opts#read_opts{files=Set});
extract_opts([compressed|Rest], Opts) ->
    extract_opts(Rest, Opts#read_opts{open_mode=[compressed]});
extract_opts([verbose|Rest], Opts) ->
    extract_opts(Rest, Opts#read_opts{verbose=true});
extract_opts([Other|Rest], Opts) ->
    extract_opts(Rest, read_opts([Other], Opts));
extract_opts([], Opts) ->
    Opts.

%% Common options for all read operations.

read_opts([compressed|Rest], Opts) ->
    read_opts(Rest, Opts#read_opts{open_mode=[compressed]});
read_opts([verbose|Rest], Opts) ->
    read_opts(Rest, Opts#read_opts{verbose=true});
read_opts([_|Rest], Opts) ->
    read_opts(Rest, Opts);
read_opts([], Opts) ->
    Opts.

foldl_read(TarName, Fun, Accu, Opts) ->
    case open(TarName, [read|Opts#read_opts.open_mode]) of
	{ok, {read, File}} ->
	    Result = 
		case catch foldl_read1(Fun, Accu, File, Opts) of
		    {'EXIT', Reason} ->
			exit(Reason);
		    {error, {Reason, Format, Args}} ->
			read_verbose(Opts, Format, Args),
			{error, Reason};
		    {error, Reason} ->
			{error, Reason};
		    Ok ->
			Ok
		end,
	    file:close(File),
	    Result;
	Error ->
	    Error
    end.

foldl_read1(Fun, Accu0, File, Opts) ->
    case get_header(File) of
	eof ->
	    Fun(eof, File, Opts, Accu0);
	Header ->
	    {ok, NewAccu} = Fun(Header, File, Opts, Accu0),
	    foldl_read1(Fun, NewAccu, File, Opts)
    end.

table1(eof, File, Options, Result) ->
    {ok, lists:reverse(Result)};
table1(Header, File, #read_opts{verbose=true}, Result) when record(Header, tar_header) ->
    #tar_header{name=Name, size=Size, mtime=Mtime, typeflag=Type,
		mode=Mode, uid=Uid, gid=Gid} = Header,
    ok = skip(File, Size),
    {ok, [{Name, Type, Size, posix_to_erlang_time(Mtime), Mode, Uid, Gid}|Result]};
table1(Header, File, Opts, Result) when record(Header, tar_header) ->
    Name = Header#tar_header.name,
    ok = skip(File, Header#tar_header.size),
    {ok, [Name|Result]}.

extract1(eof, File, Opts, []) ->
    ok;
extract1(Header, File, Opts, Result) ->
    Name = Header#tar_header.name,
    ok = extract2(File, Header, Opts, check_extract(Name, Opts)),
    {ok, Result}.

extract2(File, Header, Opts, true) ->
    {ok, Bin} = get_element(File, Header),
    write_extracted_element(Header, Bin, Opts);
extract2(File, Header, Opts, false) ->
    skip(File, Header#tar_header.size).

%% Checks if the file Name should be extracted.

check_extract(Name, #read_opts{files=all}) ->
    true;
check_extract(Name, #read_opts{files=Files}) ->
    ordsets:is_element(Name, Files).

get_header(File) ->
    case file:read(File, ?record_size) of
	eof ->
	    throw({error, eof});
	{ok, Bin} when binary(Bin) ->
	    convert_header(Bin);
	{ok, List} ->
	    convert_header(list_to_binary(List));
	{error, Reason} ->
	    throw({error, Reason})
    end.

%% Converts the tar header to a record.

convert_header(Bin) when size(Bin) == ?record_size ->
    case verify_checksum(Bin) of
	ok ->
	    Hd = #tar_header{name=get_name(Bin),
			     mode=from_octal(Bin, ?th_mode, ?th_mode_len),
			     uid=from_octal(Bin, ?th_uid, ?th_uid_len),
			     gid=from_octal(Bin, ?th_gid, ?th_gid_len),
			     size=from_octal(Bin, ?th_size, ?th_size_len),
			     mtime=from_octal(Bin, ?th_mtime, ?th_mtime_len),
			     linkname=from_string(Bin,
						  ?th_linkname, ?th_linkname_len),
			     typeflag=typeflag(Bin)},
	    convert_header1(Hd);
	eof ->
	    eof
    end;
convert_header(Bin) when size(Bin) == 0 ->
    eof;
convert_header(_Bin) ->
    throw({error, eof}).

%% Basic sanity.  Better set the element size to zero here if the type
%% always is of zero length.

convert_header1(H) when H#tar_header.typeflag == symlink, H#tar_header.size =/= 0 ->
    convert_header1(H#tar_header{size=0});
convert_header1(H) when H#tar_header.typeflag == directory, H#tar_header.size =/= 0 ->
    convert_header1(H#tar_header{size=0});
convert_header1(Header) ->
    Header.

typeflag(Bin) ->
    [T] = binary_to_list(Bin, ?th_typeflag+1, ?th_typeflag+1),
    case T of
	0  -> regular;
	$0 -> regular;
	$1 -> link;
	$2 -> symlink;
	$3 -> char;
	$4 -> block;
	$5 -> directory;
	$6 -> fifo;
	$7 -> regular;
	_  -> unknown
    end.

%% Get the name of the file from the prefix and name fields of the
%% tar header.

get_name(Bin) ->
    Name = from_string(Bin, ?th_name, ?th_name_len),
    case binary_to_list(Bin, ?th_prefix+1, ?th_prefix+1) of
	[0] ->
	    Name;
	[_] ->
	    Prefix = binary_to_list(Bin, ?th_prefix+1, size(Bin)),
	    lists:reverse(remove_nulls(Prefix), [$/|Name])
    end.

from_string(Bin, Pos, Len) ->
    lists:reverse(remove_nulls(binary_to_list(Bin, Pos+1, Pos+Len))).
    
%% Returns all characters up to (but not including) the first null
%% character, in REVERSE order.

remove_nulls(List) ->
    remove_nulls(List, []).

remove_nulls([0|_], Result) ->
    remove_nulls([], Result);
remove_nulls([C|Rest], Result) ->
    remove_nulls(Rest, [C|Result]);
remove_nulls([], Result) ->
    Result.

from_octal(Bin, Pos, Len) ->
    from_octal(binary_to_list(Bin, Pos+1, Pos+Len)).

from_octal([$\s|Rest]) ->
    from_octal(Rest);
from_octal([Digit|Rest]) when $0 =< Digit, Digit =< $7 ->
    from_octal(Rest, Digit-$0);
from_octal(Other) ->
    throw({error, {bad_header, "Bad octal number: ~p", [Other]}}).

from_octal([Digit|Rest], Result) when $0 =< Digit, Digit =< $7 ->
    from_octal(Rest, Result*8+Digit-$0);
from_octal([$\s|_], Result) ->
    Result;
from_octal([0|_], Result) ->
    Result;
from_octal(Other, _) ->
    throw({error, {bad_header, "Bad contents in octal field: ~p", [Other]}}).

%% Retrieves the next element from the archive.
%% Returns {ok, Bin} | eof | {error, Reason}

get_element(File, #tar_header{size = 0}) ->
    skip_to_next(File),
    {ok, list_to_binary([])};
get_element(File, #tar_header{size = Size}) ->
    case file:read(File, Size) of
	{ok, Bin} when size(Bin) == Size ->
	    skip_to_next(File),
	    {ok, Bin};
	{ok, List} when length(List) == Size ->
	    skip_to_next(File),
	    {ok, list_to_binary(List)};
	{ok, _Bin} ->
	    throw({error, eof});
	eof ->
	    throw({error, eof});
	{error, Reason} ->
	    throw({error, Reason})
    end.

%% Verify the checksum in the header.  First try an unsigned addition
%% of all bytes in the header (as it should be according to Posix).

verify_checksum(Bin) ->
    AfterChecksum = ?th_chksum+?th_chksum_len+1,
    Data1 = binary_to_list(Bin, 1, ?th_chksum),
    Data2 = binary_to_list(Bin, AfterChecksum, size(Bin)),
    case lists:sum(Data1) + lists:sum(Data2) of
	0 ->
	    eof;
	Checksum0 ->
	    Csum = from_octal(Bin, ?th_chksum+1, ?th_chksum_len),
	    CsumInit = ?th_chksum_len * $\s,
	    case Checksum0 + CsumInit of 
		Csum ->
		    ok;
		Unsigned ->
		    verify_checksum(Data1, Data2, CsumInit, Csum, Unsigned)
	    end
    end.

%% The checksums didn't match.  Now try a signed addition.

verify_checksum(Data1, Data2, Csum, ShouldBe, Unsigned) ->
    case signed_sum(Data1, signed_sum(Data2, Csum)) of
	ShouldBe ->
	    ok;
	Signed ->
	    throw({error,
		   {bad_header, 
		    "Incorrect directory checksum ~w (~w), should be ~w",
		    [Signed, Unsigned, ShouldBe]}})
    end.

signed_sum([C|Rest], Sum) when C < 128 ->
    signed_sum(Rest, Sum+C);
signed_sum([C|Rest], Sum) ->
    signed_sum(Rest, Sum+C-256);
signed_sum([], Sum) ->
    Sum.

write_extracted_element(Header, Bin, Opts) ->
    Name = filename:absname(Header#tar_header.name, Opts#read_opts.cwd),
    Created = 
	case Header#tar_header.typeflag of
	    regular ->
		write_extracted_file(Name, Header, Bin, Opts);
	    directory ->
		create_extracted_dir(Name, Header, Opts);
	    symlink ->
		create_symlink(Name, Header, Opts);
	    Other ->				% Ignore.
		read_verbose(Opts, "x ~s - unsupported type ~p~n",
			     [Name, Other]),
		not_written
	end,
    case Created of
	ok  -> set_extracted_file_info(Name, Header);
	not_written -> ok
    end.

create_extracted_dir(Name, Header, Opts) ->
    case file:make_dir(Name) of
	ok -> ok;
	{error, enotsup} ->
	    read_verbose(Opts, "x ~s - symbolic links not supported~n", [Name]),
	    not_written;
	{error, eexist} ->
	    not_written;
	{error, enoent} ->
	    make_dirs(Name, dir);
	{error, Reason} ->
	    throw({error, Reason})
    end.

create_symlink(Name, Header, Opts) ->
    case file:make_symlink(Header#tar_header.linkname, Name) of
	ok -> ok;
	{error, eexist} ->
	    not_written;
	{error, enoent} ->
	    not_written;
	{error, Reason} ->
	    throw({error, Reason})
    end.

write_extracted_file(Name, Header, Bin, Opts) ->
    Write =
	case Opts#read_opts.keep_old_files of
	    true ->
		case file:read_file_info(Name) of
		    {ok, _} -> false;
		    _ -> true
		end;
	    false ->
		true
	end,
    case Write of
	true ->
	    read_verbose(Opts, "x ~s~n", [Name]),
	    write_file(Name, Bin);
	false ->
	    read_verbose(Opts, "x ~s - exists, not created~n", [Name]),
	    not_written
    end.

write_file(Name, Bin) ->
    case file:write_file(Name, Bin) of
	ok ->
	    ok;
	{error, enoent} ->
	    ok = make_dirs(Name, file),
	    write_file(Name, Bin);
	{error, Reason} ->
	    throw({error, Reason})
    end.

set_extracted_file_info(Name, #tar_header{typeflag = symlink}) ->
    ok;
set_extracted_file_info(Name, #tar_header{mode=Mode, mtime=Mtime}) ->
    Info = #file_info{mode=Mode, mtime=posix_to_erlang_time(Mtime)},
    file:write_file_info(Name, Info).

%% Makes all directories leading up to the file.

make_dirs(Name, Type) ->
    make_dirs1(filename:split(Name), Type).

make_dirs1([Dir, Next|Rest], Type) ->
    case file:read_file_info(Dir) of
	{ok, #file_info{type=directory}} ->
	    make_dirs1([filename:join(Dir, Next)|Rest], Type);
	{ok, #file_info{type=Other}} ->
	    throw({error, enotdir});
	{error, _} ->
	    case file:make_dir(Dir) of
		ok ->
		    make_dirs1([filename:join(Dir, Next)|Rest], Type);
		{error, Reason} ->
		    throw({error, Reason})
	    end
    end;
make_dirs1([File], file) ->
    ok;
make_dirs1([Dir], dir) ->
    file:make_dir(Dir);
make_dirs1([], Type) ->
    %% There must be something wrong here.  The list was not supposed
    %% to be empty.
    throw({error, enoent}).

%% Prints the message on if the verbose option is given (for reading).

read_verbose(#read_opts{verbose=true}, Format, Args) ->
    case io:format(Format, Args) of
	ok ->
	    io:nl();
	{error, _} ->
	    io:format("Format=~p, Args=~p~n", [Format, Args])
    end;
read_verbose(_, _, _) ->
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% 	Utility functions.
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Returns a list of zeros to pad out to the given block size.

padding(Size, BlockSize) ->
    zeros(pad_size(Size, BlockSize)).

pad_size(Size, BlockSize) ->
    case Size rem BlockSize of
	0 -> 0;
	Rem -> BlockSize-Rem
    end.

zeros(0) ->
    [];
zeros(1) ->
    [0];
zeros(2) ->
    [0, 0];
zeros(Number) ->
    Half = zeros(Number div 2),
    case Number rem 2 of
	0 -> [Half|Half];
	1 -> [Half|[0|Half]]
    end.

%% Skips the given number of bytes rounded up to an even record.

skip(File, Size) ->
    Amount = ((Size + ?record_size - 1) div ?record_size) * ?record_size,
    case file:position(File, {cur, Amount}) of
	{ok, _} -> ok;
	Other -> Other
    end.

%% Skips to the next record in the file.

skip_to_next(File) ->
    %% Note: There is no point in handling failure to get the current position
    %% in the file.  If it doesn't work, something serious is wrong.
    {ok, Position} = file:position(File, {cur, 0}),
    NewPosition = ((Position + ?record_size - 1) div ?record_size) * ?record_size,
    case file:position(File, NewPosition) of
	{ok, NewPosition} -> ok
%%	Other -> Other
    end.

%% Sums integers in a deep list.

flatsum([C|Rest], Sum) when integer(C) ->
    flatsum(Rest, Sum+C);
flatsum([List|Rest], Sum) when list(List) ->
    flatsum(Rest, flatsum(List, Sum));
flatsum([], Sum) ->
    Sum.

%% Prints the message on if the verbose option is given.

add_verbose(#add_opts{verbose=true}, Format, Args) ->
    io:format(Format, Args);
add_verbose(_, _, _) ->
    ok.

%% Converts a tuple containing the time to a Posix time (seconds
%% since Jan 1, 1970).

posix_time(Time) ->
    EpochStart = {{1970, 1, 1}, {0, 0, 0}},
    {Days, {Hour, Min, Sec}} = calendar:time_difference(EpochStart, Time),
    86400*Days + 3600*Hour + 60*Min + Sec.

posix_to_erlang_time(Sec) ->
    OneMillion = 1000000,
    Time = calendar:now_to_datetime({Sec div OneMillion, Sec rem OneMillion, 0}),
    erlang:universaltime_to_localtime(Time).

read_file_and_info(Name, Opts) ->
    ReadInfo = Opts#add_opts.read_info,
    case ReadInfo(Name) of
	{ok, Info} when Info#file_info.type == regular ->
	    case file:read_file(Name) of
		{ok, Bin} ->
		    {ok, Bin, Info};
		Error ->
		    Error
	    end;
	{ok, Info} when Info#file_info.type == symlink ->
	    case file:read_link(Name) of
		{ok, PointsTo} ->
		    {ok, PointsTo, Info};
		Error ->
		    Error
	    end;
	{ok, Info} ->
	    {ok, [], Info};
	Error ->
	    Error
    end.

foreach_while_ok(Fun, [First|Rest]) ->
    case Fun(First) of
	ok ->
	    foreach_while_ok(Fun, Rest);
	Other ->
	    Other
    end;
foreach_while_ok(_, []) ->
    ok.
    
open_mode(Mode) ->
    open_mode(Mode, false, []).

open_mode(read, _, _) ->
    {ok, read, []};
open_mode(write, _, _) ->
    {ok, write, []};
open_mode([read|Rest], false, Opts) ->
    open_mode(Rest, read, Opts);
open_mode([write|Rest], false, Opts) ->
    open_mode(Rest, write, Opts);
open_mode([compressed|Rest], Access, Opts) ->
    open_mode(Rest, Access, [compressed|Opts]);
open_mode([], Access, Opts) ->
    {ok, Access, Opts};
open_mode(_, _, _) ->
    {error, einval}.
