#!/usr/bin/env perl

use strict;

my $bits = length pack("l!")==8 ? 64 : 32;

my $tag = `git describe --tags --match antiweb-\*`;

print "QQQ: $tag\n";


die "Unable to find DEBIAN/ directory" unless -d 'DEBIAN';

sys("rm -rf build");
sys("cp -R DEBIAN build");

sys("mkdir -p build/usr/local/antiweb");

sys("mkdir -p build/usr/local/antiweb");



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
