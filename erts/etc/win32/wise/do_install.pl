#
# Win32-Perl-Only file. Use ActivePerl 522 (Perl 5.0005) for win32
# 

require 5.0;
use strict;

use Win32API::Registry 0.13 qw( :ALL );

@ARGV == 3 or die "Usage: autorun.pl <erts version> <serial> <install executable>\n";
my $debug = 1;
my $thisversion = $ARGV[0]; 
my $installation = $ARGV[2];
my $realthisversion="$thisversion$ARGV[1]";
$| = 1;

uninstall_previous_versions($thisversion);

# Install the new version.
if (-f $installation) {
    print "Installing new Erlang package...\n";
    print $installation, "\n";
    system("$installation /S");
} else {
    error("could not find $installation");
}

&sanity_checks($realthisversion);

exit 0;

sub uninstall_previous_versions {
    my ($ver_base) = @_;
    my (%rootdir) = find_installed_versions($ver_base);

    foreach my $ver (keys %rootdir) {
	my $rootdir = $rootdir{$ver};
	print "Uninstalling $ver in $rootdir\n";
	debug("\"$rootdir\\unwise.exe\" /S /A $rootdir\\INSTALL.LOG");
	system("\"$rootdir\\unwise.exe\" /S /A $rootdir\\INSTALL.LOG");
    }
}

sub sanity_checks {
    my ($version) = @_;		# Exact version.
    my (%rootdir) = find_installed_versions($version);
    my $rootdir;

    error("Installed version not found after installation")
	unless defined ($rootdir = $rootdir{$version});

    print "Performing sanity check on installed system ($rootdir)... ";

    # Basic directory structure.
    die "Failed! (Root dir ($rootdir) not found) \n"
	unless -d $rootdir;
    die "Failed! (Lib dir not found) \n"
	unless -d "$rootdir\\lib";
    die "Failed! (Bin dir not found) \n"
	unless -d "$rootdir\\bin";
    die "Failed! (Usr dir not found) \n"
	unless -d "$rootdir\\usr";
    die "Failed! (Usr\\Include dir not found) \n"
	unless -d "$rootdir\\usr\\include";
    die "Failed! (Usr\\lib dir not found) \n"
	unless -d "$rootdir\\usr\\lib";
    die "Failed! (Releases dir not found) \n"
	unless -d "$rootdir\\releases";

    # Executable files.
    die "Failed! (erl.exe binary not found) \n"
	unless -f "$rootdir\\bin\\erl.exe";
    die "Failed! (werl.exe binary not found) \n"
	unless -f "$rootdir\\bin\\werl.exe";
    die "Failed! (erlc.exe binary not found) \n"
	unless -f "$rootdir\\bin\\erlc.exe";

    # Boot scripts.
    die "Failed! (Default boot script not found) \n"
	unless -f "$rootdir\\bin\\start.boot";
    die "Failed! (SASL boot script not found) \n"
	unless -f "$rootdir\\bin\\start_sasl.boot";
    die "Failed! (Clean boot script not found) \n"
	unless -f "$rootdir\\bin\\start_clean.boot";

    print "ok\n";
}

#
# Finds the rootdir of an installed version of Erlang matching
# the string given as argument.
#
sub find_installed_versions {
    my ($vertomatch) = @_;
    my %rootdir;

    my $erl_key_name = "SOFTWARE\\Ericsson\\Erlang";
    my $ErlKey;
    RegOpenKeyEx(HKEY_LOCAL_MACHINE, $erl_key_name, 0, KEY_READ, $ErlKey) or return;

    # Traverse all installed versions, saving all matching versions.
    
    &debug("Version to match: $vertomatch\n");
    for (my $i = 0; ; $i++) {
	# Need explicit sizing (why?)
	my $versize = 100;
	my $ver;
	my $result = RegEnumKeyEx($ErlKey, $i, $ver, $versize, [], [], [], []);
	debug("RegEnumKeyEx($ErlKey, $i) -> ");
	&debug(($result ? $ver :
		("error: " . Win32::FormatMessage(Win32::GetLastError()))), "\n");
	last unless $result;

	if ($ver =~ m/^$vertomatch/ || $ver =~ m/SNAPSHOT/) {
	    my $key_name = "SOFTWARE\\Ericsson\\Erlang\\$ver";
	    my $VerKey;
	    
	    RegOpenKeyEx(HKEY_LOCAL_MACHINE, $key_name, 0, KEY_READ, $VerKey) or next;
	    my ($type, $rootdir);
	    RegQueryValueEx($VerKey, 'Rootdir', [], $type, $rootdir, []) or next;
	    RegCloseKey($VerKey);

	    next if $rootdir =~ m/^\w:\\erts/; # Skip development version.
	    $rootdir{$ver} = $rootdir;
	}
    }
    RegCloseKey($ErlKey);
    %rootdir;
}

sub debug() {
    print "@_" if defined $debug;
}

sub error() {
    die "do_install.pl: ", @_, "\n";
}
