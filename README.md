# Rashell, the resilient and replicant shell programming library

As discussed by Lowell Jay Arthur in *Unix shell programming*
(2nd ed.), the Unix shell is a brillant tool for quickly
designing prototypes.  Unluckily, error management is very
difficult in the shell.  Most interfaces to the Unix shell found in
programming languages share this fragile optimism, which makes
impossible to write maintainable and resilient programs.

**Rashell** defines primitives which combine ease of use with the
ability to write maintainable and resilient programs leveraging the
full power of Unix.  These primitives implements common patterns to
interact with Unix utilities as subprocesses. These patterns either
yield a string or a stream of lines, which will also adequately report
error conditions on subprocesses.

**Rashell** is based on the excellent [Lwt][lwt-home] library and its
[Lwt_process][lwt-process] module.

[![Build Status](https://travis-ci.org/michipili/rashell.svg?branch=master)](https://travis-ci.org/michipili/rashell?branch=master)


## Rashell commands

The module *Rashell_Command* defines the description of commands and
common patterns to run computation consuming a subprocess executing a
command.

```ocaml
(** The type of command descriptions.

    As for [Lwt_process.exec], if the [program] name is the empty string,
    then the first argument will be used. You should specify a name only
    if you do not want the executable to be searched in the PATH. On
    Windows the only way to enable automatic seach in PATH is to pass an
    empty name.

    If [env] supplied, this is the environment of the child
    process, otherwise the environment is inherited.

    If [workdir] supplied, the child process will be started in
    [workdir] instead of the current working directory. *)
type t = {
  program: string;
  argv: string array;
  env: string array option;
  workdir: string option;
}

(** The type of process status. *)
type process_status = Unix.process_status  =
  | WEXITED of int
  | WSIGNALED of int
  | WSTOPPED of int

exception Error of t * process_status * string
(** The exception thrown by failed subprocesses.  When the subprocess
consumed by a computation fails, then the thread yielding the result
of the computation or the thread reading from the stream yielding
results of the computation will fail with the given exception.

The string parameter of the exception is the aggregated standard error
output of the failed process. *)
```

The module implements four computations consuming a subprocess, which
are classfied as utility, test, query and filter.

A *utility* is a program which is called to perform a computation
which results are stored on the filesystem (like *make*, a compiler or
inplace edition with *sed*) or sent on *stdout*, like *uname*,
*uptime* or *hostname*.

```ocaml
(** Execute the given command and return its exit status, the content
    of stdout and of stderr. *)
val exec_utility : t -> string Lwt.t
```

A *test* is a program used as a predicate, whose exit status is
examined: if it is 0, then the predicate is satisfied, if it is 1 then
the predicate is not satisfied. Other values are interpreted as a
failure of the program.

```ocaml
(** Execute the given command and test its exit status. An exit status
    of [0] indicates success of the test, [1] indicates failure of the
    test and other values indicate a general error. *)
val exec_test : t -> bool Lwt.t
```

A *query* is a program retrieving information structured in records,
one on each line.  Such programs are *join*, *paste*, *find*, *ps*,
*sed* and *awk* for instance.

```ocaml
(** Execute the given command and return a stream reading the output
    of the command and its exit status with error output. *)
val exec_query : t -> string Lwt_stream.t
```

A *filter* is a program used to transform a stream of lines in a
stream of lines.  Program commonly used as filters are *tr*, *sed*,
*awk*.

```ocaml
(** Execute the given command and return a function mapping streams,
    and its exit status with error output. *)
val exec_filter : t -> string Lwt_stream.t -> string Lwt_stream.t
```


## Other modules

Other modules builds atop *Rashell_Command* to implement a convenient
interface to various systems.  At that time, only a few POSIX utilies
have been wrapped, but the number of available modules should
increase rapidly.


## Free software

Rashell is free software: copying it and redistributing it is
very much welcome under conditions of the [MIT][license-url]
license agreement, found in the [LICENSE][license-en]
file of the distribution.


## Setup guide

It is easy to install **Rashell** using **opam** and its *pinning*
feature.  In a shell visiting the repository, say

```console
% autoconf
% opam pin add rashell .
```

It is also possible to install **Rashell** manually.
The installation procedure is based on the portable build system
[BSD Owl Scripts][bsdowl-home] written for BSD Make.

1. Verify that prerequisites are installed:
   - BSD Make
   - [BSD OWl][bsdowl-install]
   - OCaml
   - GNU Autoconf

2. Get the source, either by cloning the repository or by exploding a
   [distribution tarball](releases).

3. Optionally run `autoconf` to produce a configuration script. This
   is only required if the script is not already present.

4. Run `./configure`, you can choose the installation prefix with
   `--prefix`.

5. Run `make build`.

6. Optionally run `make test` to test your build.

7. Finally run `make install`.

Depending on how **BSD Make** is called on your system, you may need to
replace `make` by `bsdmake` or `bmake` in steps 5, 6, and 7.
The **GNU Make** program usually give up the ghost, croaking
`*** missing separator. Stop.` when you mistakingly use it instead of
**BSD Make**.

Step 7 requires that you can `su -` if you are not already `root`.


Michael Grünewald in Bonn, on September 5, 2015


  [license-url]:        https://opensource.org/licenses/MIT
  [license-en]:         LICENSE
  [bsdowl-home]:        https://github.com/michipili/bsdowl
  [bsdowl-install]:     https://github.com/michipili/bsdowl/wiki/Install
  [lwt-home]:           http://ocsigen.org/lwt/
  [lwt-process]:        http://ocsigen.org/lwt/2.5.0/api/Lwt_process
