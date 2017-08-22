open Html_types
open Eliom_content.Html.D
(* TODO parameterize into a functor and provide two modules D and F. *)

val flow5 :
  Omd.t -> flow5 elt list_wrap

val phrasing :
  Omd.t -> phrasing elt list_wrap

val flow5_without_interactive :
  Omd.t -> flow5_without_interactive elt list_wrap

val phrasing_without_interactive :
  Omd.t -> phrasing_without_interactive elt list_wrap

