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
%%     $Id $
%%

-define(DEFAULT_MIN_NO_SLOTS, 256).
-define(DEFAULT_MAX_NO_SLOTS, 2*1024*1024).
-define(DEFAULT_AUTOSAVE, 3). % minutes
-define(DEFAULT_CACHE, {3000, 12000}). % {delay, size} in {milliseconds, bytes}

%% Type.
-define(SET, 1).
-define(BAG, 2).
-define(DUPLICATE_BAG, 3).

-define(MAGIC, 16#0abcdef).   % dets cookie, won't ever change.
%% Status values.
-define(FREE, 16#3abcdef).
-define(ACTIVE, 16#12345678).

-define(FILE_FORMAT_VERSION_POS, 16).

-define(CHUNK_SIZE, 8192).

-define(SERVER_NAME, dets).

-define(POW(X), (1 bsl (X))).

%% Record holding the file header and more.
-record(head,  {
	  m,               % size
	  next,            % next position for growth (segm mgmt only)
	  fptr,            % the file descriptor
	  no_objects,      % number of objects in table,
	  no_keys,         % number of keys (version 9 only)
	  maxobjsize,      % size of the biggest object collection (unused)
	  n,               % split indicator
	  type,            % set | bag | duplicate_bag
	  keypos,          % default is 1 as for ets
	  freelists,       % tuple of free lists of buddies
	                   % if fixed =/= false, then a pair of freelists
	  freelists_p,     % cached FreelistsPointer
	  auto_save,       % Integer | infinity 
	  update_mode,     % saved | dirty | {error, Reason}
	  fixed = false,   % false | {now_time(), [{pid(),Counter}]}
                           % time of first fix, and number of fixes per process
	  hash_bif,        % hash bif used for this file (phash or hash)
	  min_no_slots,    % minimum number of slots (default or integer)
	  max_no_slots,    % maximum number of slots (default or integer)
	  cache,           % cache(). Write cache.

	  filename,             % name of the file being used
	  access = read_write,  % read | read_write
	  ram_file = false,     % true | false
	  name,                 % the name of the table

          %% Depending on the file format:
          version,
          mod,
          bump,
          base

	 }).

%% Info extracted from the file header.
-record(fileheader, {
	  freelist,
	  cookie,
	  closed_properly,
	  type,
	  version,
	  m,
	  next,
	  keypos,
	  no_objects,
	  no_keys,
	  min_no_slots,
	  max_no_slots,
	  maxobjsize,
	  trailer,
	  eof,
	  n,
	  mod
	}).

%% Write Cache.
-record(cache, {
	 cache,   % write cache, last item first
         csize,   % current size of the cached items
         inserts, % upper limit on inserted keys
	 per_key, % cached items split on key (a Sofs Family)
                  % (just to save work done by cache_lookup/3)
	 wrtime,  % last write or update time
	 tsize,   % threshold size of cache, in bytes
	 delay    % max time items are kept in RAM only, in milliseconds
	 }).

