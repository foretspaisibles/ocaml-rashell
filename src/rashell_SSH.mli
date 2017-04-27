(* Rashell_SSH -- SSH support

   Rashell (https://github.com/michipili/rashell)
   This file is part of Rashell

   Copyright © 2015—2017 Michael Grünewald

   This file must be used under the terms of the MIT license.
   This source file is licensed as described in the file LICENSE, which
   you should have received as part of this distribution. The terms
   are also available at
   https://opensource.org/licenses/MIT *)

(** The type of SSH client options. *)
type options = {
  hostname:string;
  config_file: string option; (* -F *)
  stdin_null: bool; (* -n *)
  fork_after_authentication: bool; (* -f *)
  forward_x11: bool; (* -Y *)
  identity_file: string option; (* -i *)
  local_forward: string option; (* -L *)
  remote_forward: string option; (* -R *)
  dynamic_forward: string option; (* -D *)
  user: string option; (* -l *)
  no_shell: bool; (* -N *)
  request_tty: bool option; (* -t *)
  port: string option; (* -p *)
  options: string list; (* -o *)
  workdir: string option; (* client workdir *)
  env: string array option; (* client env *)
}


(** {6 Running computations consuming a subprocess} *)

val options : ?config_file:string -> ?stdin_null:bool -> ?fork_after_authentication:bool -> ?forward_x11:bool -> ?identity_file:string -> ?local_forward:string -> ?dynamic_forward:string -> ?remote_forward:string -> ?user:string -> ?no_shell:bool -> ?request_tty:bool -> ?port:string -> ?options:string list -> ?workdir:string -> ?env:string array -> string -> options
(** [options hostname] prepare a command description with the
    given [program] and argument vector [argv]. *)

val command : options -> Rashell_Command.t -> Rashell_Command.t
(** [command (program, argv)] prepare a command description with the
    given [program] and argument vector [argv]. *)
