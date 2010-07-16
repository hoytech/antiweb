#!/usr/bin/env perl

use strict;

my $prefix = '/usr/local/antiweb';
my $antiweb_bin_dir = '/usr/bin';
my $antiweb_lib_dir = '/usr/lib';

sub usage {
  print STDERR <<END;

Antiweb (C) Doug Hoyte

Debian package creation script

usage:
  $0 <lisp> <architecture> <operating system>

  $0 ccl amd64 linux
  $0 cmu i386 linux
  $0 cmu i386 freebsd ## not implemented

  $0 clean ## removes build/ and *.deb

END
  exit 1;
}


## SCRIPT PARAMETERS

my $lisp = shift || usage();

if ($lisp eq 'clean') {
  sys("rm -rf build/ *.deb");
  exit;
}

my $arch = shift || usage();
my $os = shift || usage();


## SOME BASIC SANITY CHECKS

my $bits = length pack("l!")==8 ? 64 : 32;

die "This packager script must be run as root" if $<;
die "Unable to find DEBIAN/ directory. Not in packager directory" unless -d 'DEBIAN';
die "This antiweb repo has a ../local.lisp. Please back it up and remove it." if -e '../local.lisp';
die "This machine already seems to have an antiweb launch script at $antiweb_bin_dir/antiweb" if -x "$antiweb_bin_dir/antiweb";
die "This machine already seems to have an antiweb library at $antiweb_lib_dir/libantiweb$bits.so" if -x "$antiweb_lib_dir/libantiweb$bits.so";
die "This machine already seems to have an antiweb install in its prefix: $prefix" if -e $prefix;


## VERIFY SCRIPT PARAMETERS

if ($lisp eq 'cmu') {
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



## FIND ANTIWEB VERSION

my $aw_version = `git describe --tags --match antiweb-\*`;
chomp $aw_version;

$aw_version =~ s/^antiweb-//;

if ($aw_version =~ /^(.*-)(g[a-f]+)$/) {
  # Work around dpkg lameness where it won't install a package unless the hashtag contains a digit
  $aw_version = $1 . '0' . $2;
}

print <<END;

OK, so far your setup looks good:
  Antiweb version: $aw_version
             Lisp: $lisp
     Architecture: $arch
 Operating System: $os

END



## COPY LISP SYSTEMS INTO PREFIX

if ($lisp eq 'cmu') {
  print "\n*** Please enter a path to a CMUCL directory then press enter:\n";
  my $path = <>;
  chomp $path;

  die "path is not a directory: $path" unless -d $path;
  die "unable to find binary at $path/bin/lisp" unless -x "$path/bin/lisp";
  die "unable to find lib directory at $path/lib/" unless -d "$path/lib/";

  sys("mkdir -p $prefix/cmucl/");
  sys("cp -r $path/bin $path/lib $prefix/cmucl/");
} elsif ($lisp eq 'ccl') {
  print "\n*** Please enter a path to a ClozureCL directory then press enter:\n";
  my $path = <>;
  chomp $path;

  die "path is not a directory: $path" unless -d $path;
  die "unable to find binary at $path/lx86cl64" unless -x "$path/lx86cl64";
  die "unable to find lisp image at $path/lx86cl64.image" unless -e "$path/lx86cl64.image";
  die "unable to find launcher script at $path/scripts/ccl64" unless -x "$path/scripts/ccl64";
  die "unable to find x86-headers64/ dir at $path/x86-headers64/" unless -d "$path/x86-headers64/";

  sys("mkdir -p $prefix/ccl/");
  sys("cp $path/lx86cl64 $prefix/ccl/");
  sys("cp $path/lx86cl64.image $prefix/ccl/");
  sys("cp $path/scripts/ccl64 $prefix/ccl/");
  sys("cp -r $path/x86-headers64/ $prefix/ccl/");

  # rewrite the ccl64 script to refer to our custom prefix
  sys(qq{ /usr/bin/env perl -pi -e 's|\\s*CCL_DEFAULT_DIRECTORY=.*|  CCL_DEFAULT_DIRECTORY=$prefix/ccl|' $prefix/ccl/ccl64 });
}


## COPY BERKELEYDB LIBRARY INTO PREFIX

{
  print "\n*** Please enter a path to a compiled BerkeleyDB installation:\n";
  my $path = <>;
  chomp $path;

  die "path is not a directory: $path" unless -d $path;
  die "unable to find db_recover utility at $path/bin/db_recover" unless -x "$path/bin/db_recover";
  die "unable to find lib/ directory at $path/lib/" unless -d "$path/lib";
  die "unable to find $path/lib/libdb.a" unless -e "$path/lib/libdb.a";
  die "unable to find include/ directory at $path/include/" unless -d "$path/include";

  sys("mkdir -p $prefix/bdb$bits/");

  sys("cp -r $path/bin/ $prefix/bdb$bits/");
  sys("cp -r $path/lib/ $prefix/bdb$bits/");
  sys("cp -r $path/include/ $prefix/bdb$bits/");
}



## CREATE ../local.lisp BUILD CONFIG AND BUILD ANTIWEB

if ($lisp eq 'cmu') {
  open(FH, "> ../local.lisp");
  print FH <<END;
#+cmu
(progn
  (setq aw-bin-dir "$antiweb_bin_dir")
  (setq aw-lib-dir "$antiweb_lib_dir")
  (setq aw-cmu-executable "$prefix/cmucl/bin/lisp")
  (setq aw-use-bdb t)
  (setq aw-extra-cflags "-L$prefix/bdb$bits/lib -I$prefix/bdb$bits/include/ -Wl,-rpath=$prefix/bdb$bits/lib")
)

#-cmu
(error "this package was configured for CMUCL")
END
  close(FH);

  sys("cd .. ; $prefix/cmucl/bin/lisp -quiet -load build.lisp");
} elsif ($lisp eq 'ccl') {
  open(FH, "> ../local.lisp");
  print FH <<END;
#+ccl
(progn
  (setq aw-bin-dir "$antiweb_bin_dir")
  (setq aw-lib-dir "$antiweb_lib_dir")
  (setq aw-ccl-executable "$prefix/ccl/ccl64")
  (setq aw-use-bdb t)
  (setq aw-extra-cflags "-L$prefix/bdb$bits/lib -I$prefix/bdb$bits/include/ -Wl,-rpath=$prefix/bdb$bits/lib")
)

#-ccl 
(error "this package was configured for ClozureCL")
END
  close(FH);

  sys("cd .. ; $prefix/ccl/ccl64 -Q -l build.lisp");
}


## VERIFY BUILD

die "unable to find ../bin/antiweb - build failure?" unless -x "../bin/antiweb";
die "unable to find ../bin/libantiweb$bits.so - build failure?" unless -e "../bin/libantiweb$bits.so";
die "unable to find ../bin/antiweb.$lisp.image - build failure?" unless -e "../bin/antiweb.$lisp.image";



## SETUP PACKAGE build/ DIRECTORY

sys("rm -rf build");
sys("mkdir build");

sys("cp -r DEBIAN/ build");

sys(qq{ /usr/bin/env perl -pi -e 's|{{VERSION}}|$aw_version|' build/DEBIAN/control });
sys(qq{ /usr/bin/env perl -pi -e 's|{{ARCH}}|$arch|' build/DEBIAN/control });

sys("mkdir -p build$prefix");
sys("cp -r $prefix/* build$prefix");

sys("mkdir -p build$antiweb_bin_dir");
sys("cp ../bin/antiweb build$antiweb_bin_dir");

sys("mkdir -p build$antiweb_lib_dir");
sys("cp ../bin/libantiweb$bits.so build$antiweb_lib_dir");
sys("cp ../bin/antiweb.$lisp.image build$antiweb_lib_dir");



## REMOVE VIRGIN LISP IMAGE - ONLY THE NEW ANTIWEB IMAGE IS REQUIRED

if ($lisp eq 'cmu') {
  sys("rm build$prefix/cmucl/lib/cmucl/lib/*.core");
} elsif ($lisp eq 'ccl') {
  sys("rm build$prefix/ccl/lx86cl64.image");
}



## REMOVE TEMPORARY FILES

sys("rm -rf $prefix");
sys("rm ../local.lisp");



## ACTUALLY BUILD PACKAGE

my $deb_package_filename = "antiweb" . "_" . $aw_version . "_" . "$lisp-$arch-$os" . ".deb";

sys("dpkg -b build/ $deb_package_filename");

print "\n\nCongratulations, your debian package was created:\n\n$deb_package_filename\n\n";

exit;



## UTILITIES

sub sys {
  my $cmd = shift;
  print "SYSTEM: $cmd\n";
  my $ret = system($cmd);
  die "** system() failed with non-zero exit code ($ret)" if $ret;
}
