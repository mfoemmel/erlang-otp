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

-record(file_info,
	{size,			% Size of file in bytes.
	 type,			% Atom: device, directory, regular,
				% or other.
	 access,		% Atom: read, write, read_write, or none.
	 atime,			% The local time the file was last read:
				% {{Year, Mon, Day}, {Hour, Min, Sec}}.
	 mtime,			% The local time the file was last written.
	 ctime,			% The interpreation of this time field
	                        % is dependent on operating system.
				% On Unix it is the last time the file or
				% or the inode was changed.  On Windows,
				% it is the creation time.
	 mode,			% Integer: File permissions.  On Windows,
	 			% the owner permissions will be duplicated
				% for group and user.
	 links,			% Number of links to the file (1 if the
				% filesystem doesn't support links).
	 major_device,		% Integer: Identifies the file system (Unix),
	 			% or the drive number (A: = 0, B: = 1) (Windows).

%% The following are Unix specific.  They are set to zero on other
%% operating systems.
	 minor_device,		% Only valid for devices.
	 inode,			% Inode number for file.
	 uid,			% User id for owner (integer).
	 gid}).		        % Group id for owner (integer).



-record(file_descriptor,
	{module,                % Module that handles this kind of file
	 data}).                % Module dependant data
