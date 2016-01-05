open Lwt.Infix

module Git =
  Rashell_Git

let list () =
  Git.hook_list ()
  >>= Lwt_list.iter_s Lwt_io.printl

let co () =
  Git.branch_checkout ~start:"master" "releng/v4.0.0"

let clone () =
  Git.clone ~workdir:(Rashell_Command.expand_path "~/obj") "git@github.com:michipili/rashell.git"

let branches () =
  Git.branch_list ~workdir:(Rashell_Command.expand_path "~/obj/rashell") ~all:true ()
  >>= Lwt_list.iter_s Lwt_io.printl

let current () =
  Git.branch_current ~workdir:(Rashell_Command.expand_path "~/obj/rashell") ()
  >>= Lwt_io.printl

let () =
  Lwt_main.run(current())
