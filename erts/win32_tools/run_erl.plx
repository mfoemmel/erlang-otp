#!/usr/bin/env perl -w

require 5.0;

use Win32API::Registry 0.13 qw( :ALL );

use strict;

@ARGV >= 2 or die "usage: <erl/werl> <base version of erlang>\n";
my $erlpgm = shift;
my $thisversion = shift;
my $debug = 1;

my $bindir = find_bindir($thisversion);

system "$bindir\\$erlpgm @ARGV";

#
# Finds the rootdir of an installed version of Erlang matching
# the string given as argument.
#
sub find_bindir() {
    my ($vertomatch) = @_;

    my $erl_key_name = "SOFTWARE\\Ericsson\\Erlang";
    my $ErlKey;
    RegOpenKeyEx(HKEY_LOCAL_MACHINE, $erl_key_name, 0, KEY_READ, $ErlKey) or
	error("No \\LOCAL_MACHINE\\$erl_key_name in registry");

    # Traverse all installed versions, saving all matching versions.
    
    &debug("Version to match: $vertomatch\n");
    my @version;
    for (my $i = 0; ; $i++) {
	# Need explicit sizing (why?)
	my $keysize = 100;
	my $keyname;
	my $result = RegEnumKeyEx($ErlKey, $i, $keyname, $keysize, [], [], [], []);
	debug("RegEnumKeyEx($ErlKey, $i) -> ");
	&debug(($result ? "$keyname" :
		("error: " . Win32::FormatMessage(Win32::GetLastError()))), "\n");
	last unless $result;
	push(@version, $keyname)
	    if $keyname =~ m/^$vertomatch/;
    }
    RegCloseKey($ErlKey);

    # Found the latest installed version. Reject development versions
    # (bindir at X:\\erts...).

    for my $ver (reverse sort @version) {
	my $key_name = "SOFTWARE\\Ericsson\\Erlang\\$ver";
	my $VerKey;

	RegOpenKeyEx(HKEY_LOCAL_MACHINE, $key_name, 0, KEY_READ, $VerKey) ||
	    error("could not open key $key_name: ", $!);
	my ($type, $bindir);
	RegQueryValueEx($VerKey, 'Bindir', [], $type, $bindir, []) ||
	    error("could not read value for $key_name\\Bindir: ", $!);
	RegCloseKey($VerKey);
	next if $bindir =~ m/^\w:\\erts/; # Skip development version.
	debug("Found version $ver; bindir is $bindir\n");
	return $bindir;
    }

    error("No installed erlang matches $vertomatch");
}

sub debug() {
    print "@_" if defined $debug;
}

sub error() {
    die "run_erl: ", @_, "\n";
}
