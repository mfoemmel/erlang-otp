#!/opt/local/bin/perl


#
# Script to autognerate WISE Installation files.
#

use File::Find;

# Trick to make find not ignore directories with only 2 files ( ., ..)
$File::Find::dont_use_nlink=1;

my $header = <<"HERE";
Document Type: WSE
item: Global
  Version=6.0
  Title=Erlang Installation
  Flags=10000100
  Languages=65 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
  Japanese Font Name=MS Gothic
  Japanese Font Size=10
  Progress Bar DLL=%_WISE_\\Progress\\WIZ%_EXE_OS_TYPE_%.DLL
  Start Gradient=0 0 255
  End Gradient=0 0 0
  Windows Flags=00100100000000010010010000011000
  Log Pathname=%MAINDIR%\\INSTALL.LOG
  Message Font=MS Sans Serif
  Font Size=8
  Disk Filename=SETUP
  Patch Flags=0000000000000001
  Patch Threshold=85
  Patch Memory=4000
  FTP Cluster Size=20
  Dialog Version=6
  Variable Name1=_SYS_
  Variable Default1=C:\\WINNT\\System32
  Variable Flags1=00001000
  Variable Name2=_WISE_
  Variable Default2=D:\\Program Files\\Wise
  Variable Flags2=00001000
end
HERE

my $blockend = <<"HERE";
item: End Block
end
HERE

my $srcdir = $ARGV[0];
my $out = $ARGV[1];
my $type = $ARGV[2];

my $numfiles=0;

die "No output dir given"
    unless defined $out;

die "No source directory specified"
    unless defined $srcdir;

die "No build type specified. Use `data', `doc' or `psdoc'."
    unless defined $type;

&find_all_files();

print STDOUT "Found $numfiles files.\n";

# Find type and set up variables...
if( $type eq "data" ) {
    $pattern = "\.*";
    $epattern = "^\.*(test/|man/|doc/)";
    $component = "A";
    $destination = "\%MAINDIR\%";
} elsif ( $type eq "doc" ) {
    $pattern = "^\.*\\.(html|gif)\$";
    $epattern = "^\.*(test/|man/|\\.jam|\\.erl|\\.beam|\\.yrl)";
    $component = "B";
    $destination = "\%MAINDIR\%";
} elsif ( $type eq "psdoc" ) {
    $pattern = "^\.*\\.(ps|pdf)\$";
    $epattern = "^\.*(test/|man/|\\.jam|\\.erl|\\.beam|\\.yrl)\$";
    $component = "C";
    $destination = "\%MAINDIR\%";
} elsif ( $type eq "test" ) {
    $pattern = "^\.*(test/)*";
    $epattern = "\.*krylzkvi";
    $component = "A";
    $destination = "\%TESTS_DIR\%";
} else {
    die "Unknown type specified.\n";
}
	 
&create_file();

exit 0;

sub create_file {
    $filename = "$out/$type-files.wse";
    print "Filename: $filename\n";
    open OUT, ">$filename";

    @keys=sort keys(%filelist);
    print OUT $header;

    print OUT "item: If/While Statement\n";
    print OUT "  Variable=COMPONENTS\n";
    print OUT "  Value=$component\n";
    print OUT "  Flags=00001010\n";
    print OUT "end\n";

    foreach $key ( @keys ) {
	chomp($key);
	if($key =~ m!$pattern!o ) {
	    if($key =~ m!$epattern!o ) {
		
	    } else {
		$destfile = $filelist{$key};
		$key =~ s!/!\\!g;                # Stupid WISE !
		$destfile =~ s!/!\\!g;           # Stupid WISE !
		print OUT "item: Install File\n";
		print OUT "  Source=", $key,"\n";
		print OUT "  Destination=", $destination, $destfile, "\n";
		print OUT "  Flags=0000000010000010\n";
		print OUT "end\n";
	    }
	}
    }

    print OUT $blockend;
    close OUT;
}


sub find_all_files {
    find(\&wanted, $srcdir);
}

sub wanted {
    return if -d $_;
    $dest = $File::Find::name;
    $dest =~ s/\Q$srcdir//;
    $src = $File::Find::name;
    $filelist{$src}=$dest;
    $numfiles = $numfiles + 1;
}
