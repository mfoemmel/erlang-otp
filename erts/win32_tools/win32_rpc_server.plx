#!/usr/bin/env perl
# ``The contents of this file are subject to the Erlang Public License,
# Version 1.1, (the "License"); you may not use this file except in
# compliance with the License. You should have received a copy of the
# Erlang Public License along with this software. If not, it can be
# retrieved via the world wide web at http://www.erlang.org/.
# 
# Software distributed under the License is distributed on an "AS IS"
# basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
# the License for the specific language governing rights and limitations
# under the License.
# 
# The Initial Developer of the Original Code is Ericsson Utvecklings AB.
# Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
# AB. All Rights Reserved.''
# 
#     $Id$
#

use strict;
use Socket;
use FileHandle;

my $server_port = 6669;
my $use_nmake = 0;

if ($ARGV[0] eq '-nmake') {
    print "Using nmake...\n";
    $use_nmake = 1;
}
print "Listening on port $server_port\n";

socket(SERVER, PF_INET, SOCK_STREAM, getprotobyname('tcp'));

my $my_addr = sockaddr_in($server_port, INADDR_ANY);
bind(SERVER, $my_addr)
    or die "Couldn't bind to port $server_port: $!\n";

listen(SERVER, SOMAXCONN)
    or die "Couldn't listen on port $server_port: $!\n";

while (accept(CLIENT, SERVER)) {
    # 6 == IPPROTO_TCP, 1 == TCP_NODELAY on windoze
    setsockopt(CLIENT, 6, 1, 1) or
	die "setsockopt on client: $!\n";
    my $cmd = scalar(<CLIENT>);
    chomp($cmd);
    my @cmd = split(/\s/, $cmd);
    my $cm = shift @cmd;
    if ($cm eq "winmake" or $cm eq "winerl") {
	if ($cm eq "winmake") {
	    if ($use_nmake) {
		$cm = "nmake -f Makefile.win32";
	    } else {
		$cm = "omake -W -E -EN -f Makefile.win32";
	    }
	} elsif ($cm eq "winerl") {
	    $cm = "erl";
	}
	my $dir = shift @cmd;
	chdir $dir or print STDOUT "bad ";
	print STDOUT "dir $dir\n";
    }
    unshift @cmd, $cm;
    print STDOUT "@cmd\n";
    select CLIENT; $|=1;
    open(README, "@cmd 2>&1 |") or die "Couldn't fork: $!\n";
    my $line;
    while ($line = <README>) {
	print STDOUT $line; # local echo
	print $line;
    }
    close(CLIENT);
}

close(SERVER);
