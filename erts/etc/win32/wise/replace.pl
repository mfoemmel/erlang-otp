#! /usr/bin/env perl
# Search and replace in perl
# By Patrik Nyblom

use strict;

my @repl;
my $from;
my $to;
my @infiles;
my $outfile  = '-';
my $allinput = '';

while (defined ($_ = shift @ARGV)) {
    if (/^\-r$/) {
	push(@repl,shift @ARGV);
	push(@repl,shift @ARGV);
    } elsif (/^\-o$/) {
	$outfile = shift @ARGV;
    } elsif (/^\-/) {
	print STDERR "Unrecognized command option $_. Usage:\n".
	    "$0 -r <from> <to> [ -r <from> <to> ...] ".
		"[-o outfile] [infile ...]\n";
	exit(1);
    } else {
	push(@infiles,$_);
    }
}
push(@infiles,'-') unless @infiles > 0;
@ARGV = @infiles;
local($/);
$allinput = join('',<>);
while ($from = shift(@repl)) {
    $to = shift(@repl);
    $allinput =~ s/$from/$to/g;
}
open(OUT,"> $outfile");
print OUT $allinput;

exit(0);
