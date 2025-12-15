(** PLCOPEN-CP2: All code shall be used in the application *)
open IECCheckerCore
module S = Syntax
val do_check : Cfg.t list -> S.iec_library_element list -> Warn.t list
