%%%-------------------------------------------------------------------
%%% File    : tftp_lib.erl
%%% Author  : Hakan Mattsson <hakan@erix.ericsson.se>
%%% Description : Option parsing, decode, encode etc.
%%%
%%% Created : 18 May 2004 by Hakan Mattsson <hakan@erix.ericsson.se>
%%%-------------------------------------------------------------------

-module(tftp_lib).

%%-------------------------------------------------------------------
%% Interface
%%-------------------------------------------------------------------

%% application internal functions
-export([
	 parse_config/1,
	 parse_config/2,
	 decode_msg/1,
	 encode_msg/1,
	 replace_val/3,
	 to_lower/1,
	 host_to_string/1
	]).

%%-------------------------------------------------------------------
%% Defines
%%-------------------------------------------------------------------

-include("tftp.hrl").

-define(LOWER(Char),
	if
	    Char >= $A, Char =< $Z ->
		Char - ($A - $a);
	    true ->
		Char
	end).

%%-------------------------------------------------------------------
%% Config
%%-------------------------------------------------------------------

parse_config(Options) ->
    parse_config(Options, #config{}).

parse_config(Options, Config) ->
    do_parse_config(Options, Config).

do_parse_config([{Key, Val} | Tail], Config) when record(Config, config) ->
    case Key of
	debug ->
	    case Val of
		none ->
		    do_parse_config(Tail, Config#config{debug_level = Val});
		brief ->
		    do_parse_config(Tail, Config#config{debug_level = Val});
		normal ->
		    do_parse_config(Tail, Config#config{debug_level = Val});
		verbose ->
		    do_parse_config(Tail, Config#config{debug_level = Val});
		all ->
		    do_parse_config(Tail, Config#config{debug_level = Val});
		_ ->
		    exit({badarg, {Key, Val}})
	    end;
	host ->
	    if
		list(Val) ->
		    do_parse_config(Tail, Config#config{udp_host = Val});
		tuple(Val), size(Val) == 4 ->
		    do_parse_config(Tail, Config#config{udp_host = Val});
		tuple(Val), size(Val) == 8 ->
		    do_parse_config(Tail, Config#config{udp_host = Val});
		true ->
		    exit({badarg, {Key, Val}})
	    end;
	port ->
	    if
		integer(Val), Val >= 0 ->
		    InitArg = list_to_atom("tftpd_" ++ integer_to_list(Val)),
		    UdpOptions =
			case init:get_argument(InitArg) of
			    {ok, [[FdStr]] = Badarg} when list(FdStr) ->
				case catch list_to_integer(FdStr) of
				    Fd when integer(Fd) ->
					[{fd, Fd} | Config#config.udp_options];
				    {'EXIT', _} ->
					exit({badarg, {prebound_fd, InitArg, Badarg}})
				end;
			    {ok, Badarg} ->
				exit({badarg, {prebound_fd, InitArg, Badarg}});
			    error ->
				Config#config.udp_options
			end,
		    do_parse_config(Tail, Config#config{udp_port = Val, udp_options = UdpOptions});
		true ->
		    exit({badarg, {Key, Val}})
	    end;
	port_policy ->
	    case Val of
		random ->
		    do_parse_config(Tail, Config#config{port_policy = Val});
		0 ->
		    do_parse_config(Tail, Config#config{port_policy = random});
		MinMax when integer(MinMax), MinMax > 0 ->
		    do_parse_config(Tail, Config#config{port_policy = {range, MinMax, MinMax}});
		{range, Min, Max} when Max >= Min, 
		integer(Min), Min > 0,
		integer(Max), Max > 0 ->
		    do_parse_config(Tail, Config#config{port_policy = Val});
		true ->
		    exit({badarg, {Key, Val}})
	    end;
	udp when list(Val) ->
	    Fun =  
		fun({K, V}, List) when K /= active -> 
			replace_val(K, V, List);
		   (V, List) when V /= list, V /= binary ->
			List ++ [V];
		   (V, _List) ->
			exit({badarg, {udp, [V]}})
		end,
	    UdpOptions = lists:foldl(Fun, Config#config.udp_options, Val),
	    do_parse_config(Tail, Config#config{udp_options = UdpOptions});
	use_tsize ->
	    case Val of
		true ->
		    do_parse_config(Tail, Config#config{use_tsize = Val});
		false ->
		    do_parse_config(Tail, Config#config{use_tsize = Val});
		_ ->
		    exit({badarg, {Key, Val}})
	    end;
	max_tsize ->
	    if
		Val == infinity ->
		    do_parse_config(Tail, Config#config{max_tsize = Val});
		integer(Val), Val >= 0 ->
		    do_parse_config(Tail, Config#config{max_tsize = Val});
		true ->
		    exit({badarg, {Key, Val}})
	    end;
	max_conn ->
	    if
		Val == infinity ->
		    do_parse_config(Tail, Config#config{max_conn = Val});
		integer(Val), Val > 0 ->
		    do_parse_config(Tail, Config#config{max_conn = Val});
		true ->
		    exit({badarg, {Key, Val}})
	    end;
	_ when list(Key), list(Val) ->
	    Key2 = to_lower(Key),
	    Val2 = to_lower(Val),
	    TftpOptions = replace_val(Key2, Val2, Config#config.user_options),
	    do_parse_config(Tail, Config#config{user_options = TftpOptions});
	reject ->
	    case Val of
		read ->
		    Rejected = [Val | Config#config.rejected],
		    do_parse_config(Tail, Config#config{rejected = Rejected});
		write ->
		    Rejected = [Val | Config#config.rejected],
		    do_parse_config(Tail, Config#config{rejected = Rejected});
		_ when list(Val) ->
		    Rejected = [Val | Config#config.rejected],
		    do_parse_config(Tail, Config#config{rejected = Rejected});
		_ ->
		    exit({badarg, {Key, Val}})
	    end;
	callback ->
	    case Val of
		{RegExp, Mod, State} when list(RegExp), atom(Mod) ->
		    case regexp:parse(RegExp) of
			{ok, Internal} ->
			    Callback = #callback{regexp   = RegExp,
						 internal = Internal,
						 module   = Mod,
						 state    = State},
			    Callbacks = Config#config.callbacks ++ [Callback],
			    do_parse_config(Tail, Config#config{callbacks = Callbacks});
			{error, Reason} ->
			    exit({badarg, {Key, Val}, Reason})
		    end;
		_ ->
		    exit({badarg, {Key, Val}})
	    end;
	_ ->
	    exit({badarg, {Key, Val}})
    end;
do_parse_config([], Config) when record(Config, config) ->
    UdpOptions = Config#config.udp_options,
    IsInet6 = lists:member(inet6, UdpOptions),
    IsInet  = lists:member(inet, UdpOptions),
    Host    = Config#config.udp_host,
    Host2 = 
	if
	    (IsInet and not IsInet6); (not IsInet and not IsInet6) -> 
		case inet:getaddr(Host, inet) of
		    {ok, Addr} ->
			Addr;
		    {error, Reason} ->
			exit({badarg, {host, Reason}})
		end;
	    (IsInet6 and not IsInet)  ->
		case inet:getaddr(Host, inet6) of
		    {ok, Addr} ->
			Addr;
		    {error, Reason} ->
			exit({badarg, {host, Reason}})
		end;
	    true ->
		%% Conflicting options
		exit({badarg, {udp, [inet]}})
	end,
    UdpOptions2 = lists:reverse(UdpOptions),
    TftpOptions = lists:reverse(Config#config.user_options),
    Config#config{udp_host = Host2, udp_options = UdpOptions2, user_options = TftpOptions};
do_parse_config(Options, Config) when record(Config, config) ->
    exit({badarg, Options}).

host_to_string(Host) ->
    case Host of
	String when list(String) ->
	    String;
	{A1, A2, A3, A4} -> % inet
	    lists:concat([A1, ".", A2, ".", A3, ".",A4]);
	{A1, A2, A3, A4, A5, A6, A7, A8} -> % inet6
	    lists:concat([
			  int16_to_hex(A1), "::",
			  int16_to_hex(A2), "::",
			  int16_to_hex(A3), "::",
			  int16_to_hex(A4), "::",
			  int16_to_hex(A5), "::",
			  int16_to_hex(A6), "::",
			  int16_to_hex(A7), "::",
			  int16_to_hex(A8)
			 ])
    end.

int16_to_hex(0) ->
    [$0];
int16_to_hex(I) ->
    N1 = ((I bsr 8) band 16#ff),
    N2 = (I band 16#ff),
    [code_character(N1 div 16), code_character(N1 rem 16),
     code_character(N2 div 16), code_character(N2 rem 16)].

code_character(N) when N < 10 ->
    $0 + N;
code_character(N) ->
    $A + (N - 10).

%%-------------------------------------------------------------------
%% Decode
%%-------------------------------------------------------------------

decode_msg(Bin) when binary(Bin) ->
    case Bin of
	<<?TFTP_OPCODE_RRQ:16/integer, Tail/binary>> ->
	    case decode_strings(Tail, [keep_case, lower_case]) of
		[Filename, Mode | Strings] ->
		    Options = decode_options(Strings),
		    #tftp_msg_req{access = read,
				  filename = Filename,
				  mode = to_lower(Mode),
				  options = Options};
		[_Filename | _Strings] ->
		    exit(#tftp_msg_error{code = undef,
					 text = "Missing mode"});
		_ ->
		    exit(#tftp_msg_error{code = undef,
					 text = "Missing filename"})
	    end;
	<<?TFTP_OPCODE_WRQ:16/integer, Tail/binary>> ->
	    case decode_strings(Tail, [keep_case, lower_case]) of
		[Filename, Mode | Strings] ->
		    Options = decode_options(Strings),
		    #tftp_msg_req{access = write,
				  filename = Filename,
				  mode = to_lower(Mode),
				  options = Options};
		[_Filename | _Strings] ->
		    exit(#tftp_msg_error{code = undef,
					 text = "Missing mode"});
		_ ->
		    exit(#tftp_msg_error{code = undef,
					 text = "Missing filename"})
	    end;
	<<?TFTP_OPCODE_DATA:16/integer, SeqNo:16/integer, Data/binary>> ->
	    #tftp_msg_data{block_no = SeqNo, data = Data};
	<<?TFTP_OPCODE_ACK:16/integer, SeqNo:16/integer>> ->
	    #tftp_msg_ack{block_no = SeqNo};
	<<?TFTP_OPCODE_ERROR:16/integer, ErrorCode:16/integer, Tail/binary>> ->
	    case decode_strings(Tail, [keep_case]) of
		[ErrorText] ->
		    ErrorCode2 = decode_error_code(ErrorCode),
		    #tftp_msg_error{code = ErrorCode2,
				    text = ErrorText};
		_ ->
		    exit(#tftp_msg_error{code = undef,
					 text = "Trailing garbage"})
	    end;
	<<?TFTP_OPCODE_OACK:16/integer, Tail/binary>> ->
	    Strings = decode_strings(Tail, [lower_case]),
	    Options = decode_options(Strings),
	    #tftp_msg_oack{options = Options};
	_ ->
	    exit(#tftp_msg_error{code = undef,
				 text = "Invalid syntax"})
    end.

decode_strings(Bin, Cases) when binary(Bin), list(Cases) ->
    do_decode_strings(Bin, Cases, []).

do_decode_strings(<<>>, _Cases, Strings) ->
    lists:reverse(Strings);
do_decode_strings(Bin, [Case | Cases], Strings) ->
    {String, Tail} = decode_string(Bin, Case, []),
    if
	Cases == [] ->
	    do_decode_strings(Tail, [Case], [String | Strings]);
	true ->
	    do_decode_strings(Tail, Cases,  [String | Strings])
    end.

decode_string(<<Char:8/integer, Tail/binary>>, Case, String) ->
    if
	Char == 0 ->
	    {lists:reverse(String), Tail};
	Case == keep_case ->
	    decode_string(Tail, Case, [Char | String]);
	Case == lower_case ->
	    Char2 = ?LOWER(Char),
	    decode_string(Tail, Case, [Char2 | String])
    end;
decode_string(<<>>, _Case, _String) ->
    exit(#tftp_msg_error{code = undef, text = "Trailing null missing"}).

decode_options([Key, Value | Strings]) ->
    [{to_lower(Key), Value} | decode_options(Strings)];
decode_options([]) ->
    [].

decode_error_code(Int) ->
    case Int of
	?TFTP_ERROR_UNDEF   -> undef;
	?TFTP_ERROR_ENOENT  -> enoent;
        ?TFTP_ERROR_EACCES  -> eacces;
        ?TFTP_ERROR_ENOSPC  -> enospc;
        ?TFTP_ERROR_BADOP   -> badop;
        ?TFTP_ERROR_BADBLK  -> badblk;
        ?TFTP_ERROR_EEXIST  -> eexist;
        ?TFTP_ERROR_BADUSER -> baduser;
        ?TFTP_ERROR_BADOPT  -> badopt;
        Int when integer(Int), Int >= 0, Int =< 65536 -> Int;
	_ -> exit(#tftp_msg_error{code = undef, text = "Error code outside range."})
    end.

%%-------------------------------------------------------------------
%% Encode
%%-------------------------------------------------------------------

encode_msg(Msg) when record(Msg, tftp_msg_req)  ->
    OpCode = case Msg#tftp_msg_req.access of
		 read  -> ?TFTP_OPCODE_RRQ;
		 write -> ?TFTP_OPCODE_WRQ
	     end,
    [
     <<OpCode:16/integer>>,
     Msg#tftp_msg_req.filename, 
     0, 
     Msg#tftp_msg_req.mode, 
     0,
     [[Key, 0, Val, 0] || {Key, Val} <- Msg#tftp_msg_req.options]
    ];
encode_msg(Msg) when record(Msg, tftp_msg_data) ->
    [
     <<?TFTP_OPCODE_DATA:16/integer, (Msg#tftp_msg_data.block_no):16/integer>>,
     Msg#tftp_msg_data.data
    ];
encode_msg(Msg) when record(Msg, tftp_msg_ack) ->
    <<?TFTP_OPCODE_ACK:16/integer, (Msg#tftp_msg_ack.block_no):16/integer>>;
encode_msg(Msg) when record(Msg, tftp_msg_error) ->
    Code = encode_error_code(Msg#tftp_msg_error.code),
    [
     <<?TFTP_OPCODE_ERROR:16/integer, Code:16/integer>>, 
     Msg#tftp_msg_error.text,
     0
    ];
encode_msg(Msg) when record(Msg, tftp_msg_error) ->
    [
     <<?TFTP_OPCODE_ERROR:16/integer, (Msg#tftp_msg_error.code):16/integer>>, 
     Msg#tftp_msg_error.text,
     0
    ];
encode_msg(Msg) when record(Msg, tftp_msg_oack) ->
    [
     <<?TFTP_OPCODE_OACK:16/integer>>,
     [[Key, 0, Val, 0] || {Key, Val} <- Msg#tftp_msg_oack.options]
    ].

encode_error_code(Code) ->
    case Code of
	undef   -> ?TFTP_ERROR_UNDEF;
	enoent  -> ?TFTP_ERROR_ENOENT;
        eacces  -> ?TFTP_ERROR_EACCES;
        enospc  -> ?TFTP_ERROR_ENOSPC;
        badop   -> ?TFTP_ERROR_BADOP;
        badblk  -> ?TFTP_ERROR_BADBLK;
        eexist  -> ?TFTP_ERROR_EEXIST;
        baduser -> ?TFTP_ERROR_BADUSER;
        badopt  -> ?TFTP_ERROR_BADOPT;
        Int when integer(Int), Int >= 0, Int =< 65536 -> Int
    end.

%%-------------------------------------------------------------------
%% Miscellaneous
%%-------------------------------------------------------------------

replace_val(Key, Val, List) ->
    case lists:keysearch(Key, 1, List) of
	false ->
	    List ++ [{Key, Val}];
	{value, {_, OldVal}} when OldVal == Val ->
	    List;
	{value, {_, _}} ->
	    lists:keyreplace(Key, 1, List, {Key, Val})
    end.

to_lower(Chars) ->
    [?LOWER(Char) || Char <- Chars].
