#! /usr/local/bin/perl -w

# $Id$

# a wrapper to deal with the unimplemented '--passphrase-fd 0' case in 
# gpg-0.3.5 . Call gpgwrap.pl instead of gpg: if it sees a --passphrase-fd 0
# on the arguments, it will:
#  read a single line from stdin (the passphrase)
#  create a pipe to make a new fd for the passphrase
#  write the passphrase to the pipe-write-fd
#  exec the real gpg with --passphrase-fd <pipe-read-fd>
# the idea is that stdin, stdout, and stderr are all normal. GPG gets the
# passphrase off a new fd and never knows the difference.

# reading the passphrase must be done with non-buffering calls to avoid
# consuming too much

my $gpg = "/usr/local/bin/gpg";

use IO::Handle;

my $fd_arg;

# examine arguments
for my $i (0 .. $#ARGV) {
    if ($ARGV[$i] eq '--passphrase-fd') {
	if ($ARGV[$i+1] eq '0') {
	    $fd_arg = $i+1;
	    last;
	}
    }
}

my($read,$write);
if (defined($fd_arg)) {
    ($read, $write) = (new IO::Handle, new IO::Handle);
    pipe($read, $write) or die;

    # read passphrase
    my($pw,$c);
    if (1) {
	$pwfd = new IO::Handle;
	$pwfd->fdopen($ARGV[$fd_arg], "r") or die "$!";
	while($pwfd->sysread($c, 1)) {
	    last if $c eq "\n";
	    $pw .= $c;
	}
	#$pwfd->close;
    } else {
	open(PW, "<&$ARGV[$fd_arg]") or die "$!";
	while(defined(sysread(PW, $c, 1)) and $c ne "\n") {
	    $pw .= $c;
	}
	close(PW);
    }

    # modify arguments
    $ARGV[$fd_arg] = $read->fileno();

    $write->write($pw, length($pw));
    $write->write("\n", 1);
    undef($write);
}

#print "exec($gpg,",join(',',@ARGV),")\n";
exec($gpg, @ARGV) or die "failed to exec: $!\n";
