#!/usr/bin/env perl

use strict;

my $prefix = '/usr/local/antiweb';

sub usage {
  print STDERR <<END;
Antiweb .deb packager script (C) Doug Hoyte
usage: $0 <lisp> <architecture> <operating system>

Valid lisp parameters:
  cmucl
  ccl

Valid architecture parameters:
  amd64
  i386

Valid operating system parameters:
  linux
  freebsd
END
  exit 1;
}


## SCRIPT PARAMETERS

my $lisp = shift || usage();
my $arch = shift || usage();
my $os = shift || usage();


## SOME BASIC SANITY CHECKS

die "This packager script must be run as root" if $<;
die "Unable to find DEBIAN/ directory. Not in packager directory" unless -d 'DEBIAN';
die "This antiweb repo has a ../local.lisp. Please back it up and remove it." if -e '../local.lisp';
die "This machine already seems to have an antiweb install in its prefix: $prefix" if -e $prefix;


## VERIFY SCRIPT PARAMETERS

my $bits = length pack("l!")==8 ? 64 : 32;

if ($lisp eq 'cmucl') {
  die "packaging cmucl for 64 bit platforms is not supported by this script (but is possible)" unless $arch eq 'i386';
} elsif ($lisp eq 'ccl') {
  die "clozurecl is only stable on 64 bit platforms" unless $bits == 64;
} else {
  print STDERR "Invalid lisp parameter: $lisp\n\n";
  usage();
}

if ($arch eq 'amd64') {
  die "this machine isn't an amd64" unless $bits == 64;
} elsif ($arch eq 'i386') {
  die "this machine isn't an i386" unless $bits == 32;
} else {
  print STDERR "Invalid architecture parameter: $arch\n\n";
  usage();
}

if ($os eq 'linux') {
  die "this machine isn't running linux" unless `uname -a` =~ /linux/i;
} elsif ($os eq 'freebsd') {
  die "this machine isn't running freebsd" unless `uname -a` =~ /freebsd/i;
} else {
  print STDERR "Invalid operating system parameter: $os\n\n";
  usage();
}




## COPY LISP SYSTEMS INTO PREFIX

if ($lisp eq 'cmucl') {
  die "cmucl unimplemented";
} elsif ($lisp eq 'ccl') {
  print "*** Please enter a path to a ClozureCL directory with a binary then press enter:\n";
  my $path = <>;
  chomp $path;

  die "path is not a directory: $path" unless -d $path;
  die "unable to find binary at $path/lx86cl64" unless -x "$path/lx86cl64";
  die "unable to find lisp image at $path/lx86cl64.image" unless -e "$path/lx86cl64.image";
  die "unable to find launcher script at $path/scripts/ccl64" unless -x "$path/scripts/ccl64";

  sys("mkdir -p $prefix/ccl/");
  sys("cp $path/lx86cl64 $prefix/ccl/");
  sys("cp $path/lx86cl64.image $prefix/ccl/");
  sys("cp $path/scripts/ccl64 $prefix/ccl/");
}


## COPY BERKELEYDB LIBRARY INTO PREFIX



## FIND ANTIWEB VERSION

my $aw_version = `git describe --tags --match antiweb-\*`;
print "Antiweb version: $aw_version\n";




## CONSTRUCT PACKAGE

sys("rm -rf build");
sys("mkdir build");

sys("cp -r DEBIAN/ build");

sys("mkdir -p build/usr/local/antiweb");




## UTILITIES

sub sys {
  my $cmd = shift;
  print "SYSTEM: $cmd\n";
  my $ret = system($cmd);
  die "** system() failed with non-zero exit code ($ret)" if $ret;
}


=pod
/usr/local/antiweb/
  lib

  bdb32
  bdb64

  ccl
=cut
