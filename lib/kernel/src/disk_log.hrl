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

%% HEADSZ is the size of the file header, 
%% HEADERSZ is the size of the item header ( = ?SIZESZ + ?MAGICSZ).
-define(HEADSZ, 8).
-define(SIZESZ, 4).
-define(MAGICSZ, 4).
-define(HEADERSZ, 8).
-define(MAGICHEAD, <<12,33,44,55>>).
-define(MAGICINT, 203500599).  %% ?MAGICHEAD = <<?MAGICINT:32>>

-define(MAX_FILES, 65000).
-define(MAX_CHUNK_SIZE, 8192).

%% Object defines
-define(LOGMAGIC, <<1,2,3,4>>). 
-define(OPENED, <<6,7,8,9>>).
-define(CLOSED, <<99,88,77,11>>).

%% record of args for open
-record(arg, {name = 0,
	      file = none,
	      repair = true,
	      size = infinity,
	      type = halt,
	      distributed = false,
	      format = internal,
	      linkto = self(),
	      head = none,
	      mode = read_write,
	      notify = false,
	      options}).

-record(log,
	{status = ok,         %%  ok | {blocked, QueueLogRecords}
	 name,                %%  the key leading to this structure
	 blocked_by = none,   %%  pid of blocker | none
	 users = 0,           %%  [pid], non-linked users
	 filename,            %%  real name of the file
	 owners = [],         %%  [{pid, notify}]
	 type = halt,         %%  halt | wrap
	 format = internal,   %%  internal | external
	 format_type,         %%  halt_int | wrap_int | halt_ext | wrap_ext
	 head = none,         %%  none | {head, H} | {M,F,A}
	                      %%  called when wraplog wraps
	 mode,                %%  read_write | read_only
	 size,                %%  value of open/1 option 'size' (never changed)
	 extra}               %%  record 'halt' for halt logs,
                              %%  record 'handle' for wrap logs.
	).

-record(handle,               %% For a wrap log.
	{filename,            %% Same as log.filename
	 maxB,                %% integer(). Max size of the files.
	 maxF,                %% MaxF = integer() | 
                              %% {NewMaxF, OldMaxF} = {integer(), integer()}.
                              %% MaxF. Maximum number of files.
                              %% {NewMaxF, OldMaxF}. This form is used when the
	                      %% number of wrap logs are decreased. The files 
                              %% are not removed when the size is changed but 
	                      %% next time the files are to be used, i.e next 
	                      %% time the wrap log has filled the 
                              %% Dir/Name.NewMaxF file.
	 curB,                %% integer(). Number of bytes on current file.
	 curF,                %% integer(). Current file number.
	 cur_fdc,             %% Current file descriptor. A cache record.
	 cur_name,            %% Current file name. For error reports.
	 cur_cnt,             %% integer(). Number of items on current file,
	                      %% header inclusive.
	 acc_cnt,             %% integer(). acc_cnt+cur_cnt is number of items
	                      %% written since the log was opened.
	 firstPos,            %% Start position for first item (after header).
	 noFull,              %% Number of overflows since last use of info/1
	                      %% on this log, or since log was opened if info/1
	                      %% has not yet been used on this log.
	 accFull}             %% noFull+accFull is number of overflows since
	                      %% the log was opened.
       ).

-record(halt,                 %% For a halt log.
	{fdc,                 %% A cache record.
	 curB,                %% integer(). Number of bytes on the file.
	 size}
	).

-record(continuation,         %% Chunk continuation.
	{pid = self(),
	 pos,
	 b}
	).

-record(cache,                %% Cache for logged terms (per file descriptor).
        {fd,                  %% File descriptor.
         sz = 0,              %% integer(). Number of bytes in the cache.
         c = []}              %% iolist(). The cache.
        ).
