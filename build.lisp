;; Antiweb (C) Doug Hoyte

;;;;;;;;;;;;;;; ANTIWEB BUILD OPTIONS ;;;;;;;;;;;;;;;;;

;; TIP: These options can be set in a file called local.lisp
;; For example: (setq aw-bin-dir "/usr/local/bin")

;; Directory to put antiweb perl launch script
(defvar aw-bin-dir "/usr/bin")

;; Directory to put libantiwebBITS.so and antiweb.SYS.image
(defvar aw-lib-dir "/usr/lib")

;; See lisp compiler messages during build of bundled libs?
(defvar aw-debugging nil)

;; Compile in BerkeleyDB support? Requires BerkeleyDB 4.6+
(defvar aw-use-bdb nil)

;; Extra flags for gcc
(defvar aw-extra-cflags "")

;; Lisp executables
(defvar aw-cmu-executable "lisp")
(defvar aw-clisp-executable "clisp")
(defvar aw-ccl-executable "ccl64")

;;;;;;;;;;;;;;; END OF ANTIWEB BUILD OPTIONS ;;;;;;;;;;;;;;;;;

(defvar aw-warning-cflags "-Wall -Wformat=2 -Wpointer-arith")

(format t "************* Antiweb Build Script *************~%")

(ignore-errors
  (load "local.lisp")
  (format t "BUILD: Loaded site-local configuration from local.lisp~%"))



(defmacro redirect-standard-output-to-dev-null (&rest body)
  `(let ((out *standard-output*))
     (ignore-errors
       (handler-bind ((error (lambda (c)
                                (format out "~%There was an ERROR building a bundled dependency:~2%")
                                (format out "~a~2%" c)
                                (format out "Trying to proceed anyways...~2%"))))
            ,(if aw-debugging
               `(progn ,@body)
               `(with-open-file (*standard-output* "/dev/null" :direction :output :if-exists :append)
                  (let ((*error-output* *standard-output*))
                    ,@body)))))))

#+clisp (setq *compile-verbose* nil)

(format t "BUILD: Compiling and loading CL-PPCRE (please be patient)~%")
(redirect-standard-output-to-dev-null (load "bundled/cl-ppcre/load.lisp"))
(setf cl-ppcre:*regex-char-code-limit* 256)
(setf cl-ppcre:*use-bmh-matchers* nil)

(format t "BUILD: Compiling and loading CFFI (please be patient)~%")
(redirect-standard-output-to-dev-null (load "bundled/cffi/load.lisp"))

(format t "BUILD: Compiling and loading Let Over Lambda, Antiweb production edition~%")
(redirect-standard-output-to-dev-null
  (load "bundled/lol.lisp") ; Need to load before compiling because it uses its own read macros
  (load (compile-file "bundled/lol.lisp")))

(format t "BUILD: Compiling and loading ISAAC random number generator~%")
(redirect-standard-output-to-dev-null
  (load (compile-file "bundled/isaac.lisp")))

(format t "BUILD: Compiling and loading jsmin.lisp~%")
(redirect-standard-output-to-dev-null
  (load (compile-file "bundled/jsmin.lisp")))

(format t "BUILD: Compiling and loading local-time~%")
(redirect-standard-output-to-dev-null
  (load (compile-file "bundled/local-time.lisp")))



(cffi:defcfun ("system" silent-system) :int (command :string))

(defun system (command)
  (format t "SYSTEM: ~a~%" command)
  (silent-system command))

(system "mkdir -p bin/")

(defvar aw-endian)
(defvar aw-bits)

(with-open-file (o "temp-type-size-finder.c" :direction :output :if-exists :supersede)
  (format o #"
#include <stdio.h>
#include <sys/types.h>
#include <arpa/inet.h>

int main() {
  printf("ilp %d %d %d\n", (int) sizeof(int), (int) sizeof(long), (int) sizeof(char *));
  printf("time_t %d\n", (int) sizeof(time_t));
  printf("off_t %d\n", (int) sizeof(off_t));
  printf("size_t %d\n", (int) sizeof(size_t));
  printf("endian %s\n", 1==htonl(1) ? "big" : "little");
  return 0;
}
"#))
(system (format nil "gcc ~a -Wall -D_FILE_OFFSET_BITS=64 temp-type-size-finder.c -o temp-type-size-finder" aw-extra-cflags))
(system "./temp-type-size-finder > temp-type-size-finder.output")

(format t "BUILD: Discovering architecture... ")

(funcall (compile nil (lambda ()
(with-open-file (h "temp-type-size-finder.output" :direction :input)
  (loop for l = (read-line h nil nil) while l do
    (if-match (#~m/^ilp (\d+) (\d+) (\d+)/ l)
      (setq aw-bits
            (cond ((and (equal $1 "4") (equal $2 "4") (equal $3 "4")) 32) ; ILP32
                  ((and (equal $1 "4") (equal $2 "8") (equal $3 "8")) 64) ; LP64
                  (t (error "unexpected architecture")))))
    (if-match (#~m/^time_t (\d+)/ l)
      (eval `(cffi:defctype :time_t ,(cond ((equal $1 "4") :int32)
                                           ((equal $1 "8") :int64)
                                           (t (error "unexpected time_t value"))))))
    (if-match (#~m/^off_t (\d+)/ l)
      (eval `(cffi:defctype :off_t ,(cond ((equal $1 "4") (error "off_t must be 64 bits"))
                                          ((equal $1 "8") :int64)
                                          (t (error "unexpected off_t value"))))))
    (if-match (#~m/^size_t (\d+)/ l)
      (eval `(cffi:defctype :size_t ,(cond ((equal $1 "4") :uint32)
                                           ((equal $1 "8") :uint64)
                                           (t (error "unexpected size_t value"))))))
    (if-match (#~m/^endian big$/ l)
      (setq aw-endian 'big))
    (if-match (#~m/^endian little$/ l)
      (setq aw-endian 'little)))))))

(format t #"Detected ~a, ~a-endian~%"#
  (ecase aw-bits
    ((32) "ILP32")
    ((64) "LP64"))
  (ecase aw-endian
    ((little) "little")
    ((big) "big")))

(system "rm temp-type-size-finder*")







(setf *print-pretty* nil) ; faster on all platforms and required for clisp

(defconstant crlf (coerce '(#\return #\linefeed) 'string))

(defvar aw-isaac-ctx)
(defvar conn-table (make-hash-table))
(defvar inet-conn-table (make-hash-table))
(defvar worker-conn-table (make-hash-table))
(defvar locked-worker-table (make-hash-table))
(defvar host-to-conn-dispatch-table (make-hash-table :test #'equalp))
(defvar aw-hub-conf)
(defvar aw-hub-dir) ; used by hub AND workers
(defvar aw-worker-conf)
(defvar aw-worker-cache)
(defvar aw-worker-chroot)
(defvar all-antiweb-modules nil)
(defvar aw-fast-files-table (make-hash-table :test #'equal))
(defvar aw-start-time)

(defvar hub-stats-total-conns 0)
(defvar hub-stats-dispatched-conns 0)

(defvar worker-stats-total-conns 0)
(defvar worker-stats-total-requests 0)


(defun load-libantiweb ()
  (cffi:load-foreign-library `(:default ,(format nil "libantiweb~a" aw-bits))))


(format t "BUILD: Compiling libantiweb~a.so~%" aw-bits)
(system (format nil "gcc ~a ~a -fPIC -s -O3 -D_FORTIFY_SOURCE=2 -D_FILE_OFFSET_BITS=64 -D~a_ENDIAN ~a-DUSE_~a src/libantiweb.c bundled/sha1.c -lz ~a-shared -o bin/libantiweb~a.so"
                    aw-extra-cflags
                    aw-warning-cflags
                    (if (eq aw-endian 'big) "BIG" "LITTLE")
                    (if aw-use-bdb "-DUSE_BDB " "")
                    #+(or linux :clc-os-debian) "EPOLL" #-(or linux :clc-os-debian) "KQUEUE"
                    (if aw-use-bdb "-ldb " "")
                    aw-bits))


(format t "BUILD: Linking in ./bin/libantiweb~a.so~%" aw-bits)
(let ((cffi:*foreign-library-directories* '("./bin/")))
  (load-libantiweb))

;; HACK for CMUCL: After loading a library from a specific directory, CMUCL will store the
;; full path of the library in system::*global-table*. Instead, we change the value of
;; system::*global-table* manually before the image is saved so that it's loaded from
;; the system directory when the image is restarted.
#+cmu
(loop for i in system::*global-table* do
  (if (cdr i)
    (if-match (#~m|/(libantiweb\d\d[.]so)$| (cdr i))
      (setf (cdr i) $1))))

;; HACK for CMUCL: On start-up, when CMUCL re-loads the antiweb foreign library it prints
;; the text  Reloaded library "libantiweb32.so"  which clutters the output of most antiweb
;; commands. This hack removes the function that prints this from ext:*after-save-initializations*
;; and puts in place a function that executes the original function but discards what was
;; written to standard output.
#+cmu
(when (ignore-errors (symbol-function 'system::reinitialize-global-table)) ; do nothing if this changes in CMUCL
  (setf ext:*after-save-initializations*
        (delete #'system::reinitialize-global-table ext:*after-save-initializations*))
  (push 
    (funcall (compile nil (lambda ()
      (let ((orig-global-table (symbol-function 'system::reinitialize-global-table)))
        (lambda ()
          (with-output-to-string (*standard-output*)
            (funcall orig-global-table)))))))
    ext:*after-save-initializations*))


(format t "BUILD: Converting libantiweb.h into libantiweb-h.lisp and loading it~%")
(funcall (compile nil (lambda ()
(with-open-file (h "src/libantiweb.h" :direction :input)
  (with-open-file (o "src/libantiweb-h.lisp" :direction :output :if-exists :supersede)
    (format o ";; DO NOT MODIFY THIS FILE~%;; It is autogenerated from libantiweb.h~%")
    (loop for l = (read-line h nil nil) while l do
      (setq l (#~s|//.*$|| l))
      (setq l (#~s|\s*$|| l))

      (labels ((proc-fun-args (s)
                 (let ((args (cl-ppcre:split "," s)))
                   (mapcar
                     (lambda (a)
                       (or (if-match (#~m/^\s*(\S+) ([^*]\S*)\s*$/ a)
                             (format nil " (~a :~a)" $2 $1))
                           (if-match (#~m/[*](\S*)\s*$/ a)
                             (format nil " (~a :pointer)" $1))))
                     args))))

        (format o "~a~%"
          (or
            (if-match (#~m/^#define (\S+) (\S+)$/ l)
              (format nil #"(defconstant ~a ~a)"# $1 $2))

            (if-match (#~m/^struct (\S+) {$/ l)
              (format nil #"(cffi:defcstruct ~a"# $1)) ; )

            (if-match (#~m/^};$/ l) ; (
              (format nil #")"#))

            (if-match (#~m/^  (\S+) ([^*]\S*);$/ l)
              (format nil #"  (~a :~a)"# $2 $1))

            (if-match (#~m/^  .*[*](\S*);$/ l)
              (format nil #"  (~a :pointer)"# $1))

            (if-match (#~m/^extern (\S+) ([^*]\S*);$/ l)
              (format nil #"(cffi:defcvar ("~a" ~a) :~a)"# $2 $2 $1))

            (if-match (#~m/^extern .*[*](\S*);$/ l)
              (format nil #"(cffi:defcvar ("~a" ~a) :pointer)"# $1 $1))

            (if-match (#~m/^.* [*](\S+)\((.*)\);$/ l)
              (format nil #"(cffi:defcfun ("~a" ~a) :pointer~% ~a)"#
                          $1 $1 (apply #'concatenate 'string (proc-fun-args $2))))

            (if-match (#~m/^(\S+) ([^*]\S+)\((.*)\);$/ l)
              (format nil #"(cffi:defcfun ("~a" ~a) :~a~% ~a)"#
                          $2 $2 $1 (apply #'concatenate 'string (proc-fun-args $3))))

            "")))))))))

(load (compile-file "src/libantiweb-h.lisp"))



(format t "BUILD: Compiling and loading the Antiweb system:~%")
(dolist (f '("conf" "awp" "mime-types" "antiweb" "modules" "glue"))
  (format t #"LISP: (load (compile-file "src/~a.lisp"))~%"# f)
  (load (compile-file (format nil "src/~a.lisp" f))))



(format t "BUILD: Creating Antiweb launch script~%")
(with-open-file (o "bin/antiweb" :direction :output :if-exists :supersede)
  (format o #"#!/usr/bin/perl~%"#)
  (format o #"use strict;~%"#)

  (format o #"my $cl_sys = "~a";~%"# #+cmu "cmu" #+clisp "clisp" #+ccl "ccl")

  (format o #"my $bin_dir = "~a";~%"# aw-bin-dir)
  (format o #"my $lib_dir = "~a";~%"# aw-lib-dir)
  (format o #"my $bits = ~a;~%"# aw-bits)

  (format o #"my $AW_VERSION = "~a";~%"# AW_VERSION)

  (format o #"my $cmu_exec = "~a";~%"# aw-cmu-executable)
  (format o #"my $clisp_exec = "~a";~%"# aw-clisp-executable)
  (format o #"my $ccl_exec = "~a";~%"# aw-ccl-executable)

  (princ #>END_OF_ANTIWEB_LAUNCH_SCRIPT

sub usage {
  print <<END;
Antiweb v$AW_VERSION Launch Script - (C) Doug Hoyte
  antiweb [optional flags] -command [parameters]

Installation Skeletons:
  antiweb -skel-hub-dir <hub directory to create>
  antiweb -skel-worker-basic
  antiweb -skel-worker-full
  antiweb -skel-worker-chrooted

Launching Antiweb:
  antiweb -hub <hub directory>
  antiweb -worker <worker conf file>
  antiweb -check-worker <worker conf file>

Maintenance/Development:
  antiweb -version [hub directory or worker conf file]
  antiweb -reload <worker conf file>
  antiweb -reopen-log-files <hub directory>
  antiweb -add-listener <hub directory> <ip> <port>
  antiweb -close-listener <hub directory> <ip> <port>
  antiweb -kill <hub directory or worker conf file>
  antiweb -stats <hub directory or worker conf file>
  antiweb -room <hub directory or worker conf file>
  antiweb -attach <hub directory or worker conf file>
  antiweb -repl
  antiweb -eval <expression>
  antiweb -awp <awp file> <base directory>

Optional Flags:
  -cmu -clisp -ccl -nodaemon -noreadline

END
  exit;
}

my $switch;
my $nodaemon;
my $noreadline;

while(1) {
  $switch = shift or usage();

  if ($switch eq "-cmu") {
    $cl_sys = "cmu";
  } elsif ($switch eq "-clisp") {
    $cl_sys = "clisp";
  } elsif ($switch eq "-ccl") {
    $cl_sys = "ccl";
  } elsif ($switch eq "-nodaemon") {
    $nodaemon = 1;
  } elsif ($switch eq "-noreadline") {
    $noreadline = 1;
  } else {
    last;
  }
}


sub exec_lisp {
  my $expr = shift;

  if ($cl_sys eq "cmu") {
    exec("$cmu_exec -quiet -core '$lib_dir/antiweb.cmu.image' -eval '$expr'");
    die "Couldn't exec CMUCL program '$cmu_exec'";
  } elsif ($cl_sys eq "clisp") {
    my $q="";
    $q = "cat | " if $noreadline;
    exec("$q $clisp_exec -q -repl -M '$lib_dir/antiweb.clisp.image' -x '$expr'");
    die "Couldn't exec CLISP program '$clisp_exec'";
  } elsif ($cl_sys eq "ccl") {
    exec("$ccl_exec -Q -I '$lib_dir/antiweb.ccl.image' -e '$expr'");
    die "Couldn't exec ClozureCL program '$ccl_exec'";
  }
  die "Unknown cl_sys: $cl_sys";
}

sub attempt_connection_to_unix_socket {
  use IO::Socket;
  my $path = shift;

  my $sock = IO::Socket::UNIX->new(Peer => $path, Type => SOCK_STREAM, Timeout  => 2);

  if (defined $sock) {
    close($sock);
    return 1;
  }

  return undef;
}

sub my_sleep {
  my $v = shift;
  select(undef,undef,undef,$v);
}

if ($switch eq "-hub") {
  my $arg = shift or usage();
  die "Not a hub directory: $arg" unless (-d $arg);
  die "Path to hub directory must be absolute" unless $arg =~ m|^/|;
  die "Couldn't find $arg/hub.conf" unless (-f "$arg/hub.conf");
  die "Couldn't find $arg/aw_log/" unless (-d "$arg/aw_log");
  die "Couldn't find $arg/empty/" unless (-d "$arg/empty");

  if (attempt_connection_to_unix_socket("$arg/hub.socket")) {
    print STDERR "Hub already running in hub directory '$arg'\n";
    print STDERR "run 'antiweb -kill $arg' to stop it\n";
    exit(-1);
  }

  my $rv = fork();
  die "couldn't fork: $!" unless defined $rv;

  if ($nodaemon) {
    # When nodaemon mode is on, the hub process runs in the foreground
    # but the logger process runs in the background. To do this, a child
    # process is started before the hub process which waits a short amount
    # of time and then launches the logger process

    if ($rv == 0) {
      my_sleep(0.5);

      my $rv = fork();
      die "couldn't fork: $!" unless defined $rv;

      if ($rv) {
        waitpid($rv, 0);
        print "WARNING: Unable to start logger process\n" if $?;
        exit;
      } else {
        exec_lisp("(run-logger \"$arg\")");
      }
    }

    exec_lisp("(run-hub \"$arg\" t)");
  } else {
    # In regular operation, a child process is forked which launches the hub
    # in the background. If it exits successfully, the logger process is also
    # launched in the background.

    if ($rv == 0) {
      exec_lisp("(run-hub \"$arg\")");
    }

    waitpid($rv, 0);
    exec_lisp("(run-logger \"$arg\")") unless $?;
  }

  print STDERR "*** Failed to start Antiweb ***\n";
  exit(-1);
} elsif ($switch eq "-worker") {
  my $arg = shift or usage();
  die "Not a worker conf: $arg" unless (-f $arg);
  die "Path to worker conf must be absolute" unless $arg =~ m|^/|;
  exec_lisp("(run-worker \"$arg\" :nodaemon t)") if $nodaemon;
  exec_lisp("(run-worker \"$arg\")");
} elsif ($switch eq "-check-worker") {
  my $arg = shift or usage();
  die "Not a worker conf: $arg" unless (-f $arg);
  die "Path to worker conf must be absolute" unless $arg =~ m|^/|;
  exec_lisp("(run-worker \"$arg\" :nodaemon t :checking t)") if $nodaemon;
  exec_lisp("(run-worker \"$arg\" :checking t)");
} elsif ($switch eq "-reload") {
  my $arg = shift or usage();
  die "Not a worker conf: $arg" unless (-f $arg);
  die "Path to worker conf must be absolute" unless $arg =~ m|^/|;
  exec_lisp("(run-reload-worker-conf \"$arg\")");
} elsif ($switch eq "-reopen-log-files") {
  my $arg = shift or usage();
  die "Not a hub directory: $arg" unless (-d $arg);
  die "Path to hubdir/worker conf must be absolute" unless $arg =~ m|^/|;
  exec_lisp("(run-supervise-hub \"$arg\" \"(aw-hub-reopen-log-files)\" t)");
} elsif ($switch eq "-add-listener") {
  my $dir = shift or usage();
  my $ip = shift or usage();
  my $port = shift or usage();
  die "Not a hub directory: $dir" unless (-d $dir);
  die "Path to hubdir must be absolute" unless $dir =~ m|^/|;
  exec_lisp("(run-add-listener \"$dir\" \"$ip\" $port)");
} elsif ($switch eq "-close-listener") {
  my $dir = shift or usage();
  my $ip = shift or usage();
  my $port = shift or usage();
  die "Not a hub directory: $dir" unless (-d $dir);
  die "Path to hubdir must be absolute" unless $dir =~ m|^/|;
  exec_lisp("(run-supervise-hub \"$dir\" #\"(hub-close-inet-listener \"$ip\" $port)\"# t)");
} elsif ($switch eq "-attach") {
  my $arg = shift or usage();
  die "Path to hubdir/worker conf must be absolute" unless $arg =~ m|^/|;
  if (-d $arg) {
    exec_lisp("(run-supervise-hub \"$arg\")");
  } elsif (-f $arg) {
    exec_lisp("(run-supervise-worker \"$arg\")");
  }
  die "not a hub directory or worker conf file: $arg";
} elsif ($switch eq "-kill") {
  my $arg = shift or usage();
  die "Path to hubdir/worker conf must be absolute" unless $arg =~ m|^/|;
  if (-d $arg) {
    exec_lisp("(run-supervise-hub \"$arg\" \"(quit)\")");
  } elsif (-f $arg) {
    exec_lisp("(run-supervise-worker \"$arg\" \"(quit)\")");
  }
  die "not a hub directory or worker conf file: $arg";
} elsif ($switch eq "-stats") {
  my $arg = shift or usage();
  die "Path to hubdir/worker conf must be absolute" unless $arg =~ m|^/|;
  if (-d $arg) {
    exec_lisp("(run-supervise-hub \"$arg\" \"(stats)\" t)");
  } elsif (-f $arg) {
    exec_lisp("(run-supervise-worker \"$arg\" \"(stats)\" t)");
  }
  die "not a hub directory or worker conf file: $arg";
} elsif ($switch eq "-room") {
  my $arg = shift or usage();
  die "Path to hubdir/worker conf must be absolute" unless $arg =~ m|^/|;
  if (-d $arg) {
    exec_lisp("(run-supervise-hub \"$arg\" \"(aw-room)\" t)");
  } elsif (-f $arg) {
    exec_lisp("(run-supervise-worker \"$arg\" \"(aw-room)\" t)");
  }
  die "not a hub directory or worker conf file: $arg";
} elsif ($switch eq "-version") {
  print "CURRENT INSTALLATION ON THIS SYSTEM:\n";
  print "    Antiweb version: $AW_VERSION\n";
  print "    Antiweb launch script: $bin_dir/antiweb\n";
  print "    Antiweb library: $lib_dir/libantiweb$bits.so\n";
  print "    Default lisp environment: $cl_sys\n";
  print "    Lisp binary: ";
  print $cmu_exec if $cl_sys eq "cmu";
  print $clisp_exec if $cl_sys eq "clisp";
  print $ccl_exec if $cl_sys eq "ccl";
  print "\n";
  print "\n";
  my $arg = shift;
  if (!$arg) {
    print "Provide either a hub directory or a worker conf file as an argument to\n";
    print "-version to see what version the specified hub or worker process is running.\n\n";
    exit;
  }
  die "Path to hubdir/worker conf must be absolute" unless $arg =~ m|^/|;
  if (-d $arg) {
    print "HUB $arg is running Antiweb version...\n";
    exec_lisp("(run-supervise-hub \"$arg\" \"AW_VERSION\" t)");
  } elsif (-f $arg) {
    print "WORKER $arg is running Antiweb version...\n";
    exec_lisp("(run-supervise-worker \"$arg\" \"AW_VERSION\" t)");
  }
  die "not a hub directory or worker conf file: $arg";
} elsif ($switch eq "-repl") {
  exec_lisp("(do-aw-init nil)");
} elsif ($switch eq "-eval") {
  my $expr = shift or usage();
  exec_lisp("(unwind-protect (progn $expr) (quit))");
} elsif ($switch eq "-awp") {
  my $awpfile = shift or usage();
  my $basedir = shift or usage();
  die "first argument to -awp must be absolute path to an .awp file"
    unless ($awpfile =~ m|^/| && $awpfile =~ m|/([^/]+[.]awp)$|i && (-f $awpfile));
  $awpfile =~ m|/([^/]+[.]awp)|;
  my $name = $1;
  die "second argument to -awp must be absolute path to a base directory" unless ($basedir =~ m|^/| && (-d $basedir));
  my $dirtomake = "$basedir/$name";
  die ".awp files can't be compiled to the same directory they are stored" if (-f $dirtomake);
  exec_lisp("(progn (do-aw-init nil) (unwind-protect (awp-compile \"$awpfile\" 0 \"$basedir/$name\") (quit)))");
} elsif ($switch eq "-skel-hub-dir") {
  my $dir = shift or die "need a directory to create for the hub";

  my $hub_user = 20000;
  my $logger_user = 20001;

  die "$dir already exists" if (-e $dir);
  mkdir($dir) or die "unable to mkdir: $dir";
  mkdir("$dir/aw_log") or die "unable to mkdir: $dir/aw_log";
  system("chown $logger_user:$logger_user $dir/aw_log");
  mkdir("$dir/empty") or die "unable to mkdir: $dir/empty";
  skel_to_file("$dir/hub.conf", <<END);
(hub-uid $hub_user)
(logger-uid $logger_user)
(max-fds 10000)
(listen "0.0.0.0" 80)
END
  print "Created Antiweb hub directory: $dir\n";
} elsif ($switch eq "-skel-worker-basic") {
print <<END;
(worker example)
(hub-dir "/var/aw")
(max-fds 32767)
(uid 20100)

(handler
  :hosts ("localhost" "127.0.0.1"
          "example.com" "www.example.com")
  :root "/var/www/example.com"
  :index ("index.html")
  :etags
  :cgi (pl)
)
END
} elsif ($switch eq "-skel-worker-full") {
print <<END;
(worker worker-full)
(hub-dir "/var/aw")
(max-fds 32767)
(uid "user")
(cache "/var/www/cache") ; must be owned by user
(keepalive 65 s)

(eval-every 6 h
  (gc))

(handler
  :hosts ("example.com" "www.example.com")
  :root "/var/www/example.com"

  :default-mime-type "text/plain"
  :mime-type (htm "text/html; charset=iso-8859-1")

  :index ("index.html" "index.pl")
  :dir-listings :etags :download-resuming

  :gzip (html txt css js)
  :gzip-size-range (256 1000000)
  :fast-1x1gif "/1x1.gif"
  :fast-files ("/favicon.ico" "/robots.txt")

  :cgi (pl)
  :awp
)

(handler
  :hosts ("static.example.com" "images.example.com"
          "example.ca" "www.example.ca")
  :simple-vhost-root "/var/www"
  ;; Maps to: /var/www/static.example.com/
  ;;   * use symlinks to share directories

  :index ("index.html")
  :dir-listings :etags :download-resuming
)
END
} elsif ($switch eq "-skel-worker-chrooted") {
print <<END;
(worker sandboxed-worker)
(hub-dir "/var/aw") ; relative to the ORIGINAL root
(max-fds 32767)
(uid "noprivuser")
(chroot "/var/sandbox")
(cache "/cache") ; relative to the NEW root. /var/sandbox/cache

(handler
  :hosts ("comehackme.example.com")
  :root "/" ; relative to the NEW root. /var/sandbox
  :awp ; no problem as long as you have a cache
)
END
} elsif ($switch =~ m/^--?h(elp|)$/i) {
  usage();
} else {
  die "Unknown switch: $switch";
}

sub skel_to_file {
  my $filename = shift;
  my $contents = shift;

  open(FH, ">$filename");
  print FH $contents;
  close(FH);
}
END_OF_ANTIWEB_LAUNCH_SCRIPT o))
(system "chmod a+x bin/antiweb")


(format t "BUILD: Creating Antiweb install script~%")
(with-open-file (o "bin/install.sh" :direction :output :if-exists :supersede)
  (format o #"#!/bin/sh

install -m 755 antiweb ~a/
install -m 644 libantiweb~a.so ~a/
install -m 644 antiweb.~a.image ~a/
"# aw-bin-dir
   aw-bits aw-lib-dir
   #+cmu "cmu" #+clisp "clisp" #+ccl "ccl" aw-lib-dir))
(system "chmod a+x bin/install.sh")


#+clisp
(progn
  (setq custom:*prompt-finish* "* ")
  (setq custom:*prompt-body* ""))

(setf *read-eval* nil) ; security. bind to t if needed

(format t "BUILD: Antiweb v~a build OK~%" AW_VERSION)

(let ((image-name (format nil "bin/antiweb.~a.image" #+cmu "cmu" #+clisp "clisp" #+ccl "ccl")))
  (format t "************* Saving lisp image to ~a *************~%" image-name)
  #+cmu (save-lisp image-name)
  #+clisp (ext:saveinitmem image-name)
  #+ccl (save-application image-name)
)
