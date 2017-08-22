open Eliom_content.Html.D

exception Not_allowed of string * string (* what, where *)

let rec phrasing_without_interactive t = List.map (function
  | Omd.H1 t -> raise (Not_allowed ("H1", "phrasing_without_interactive"))
  | Omd.H2 t -> raise (Not_allowed ("H2", "phrasing_without_interactive"))
  | Omd.H3 t -> raise (Not_allowed ("H3", "phrasing_without_interactive"))
  | Omd.H4 t -> raise (Not_allowed ("H4", "phrasing_without_interactive"))
  | Omd.H5 t -> raise (Not_allowed ("H5", "phrasing_without_interactive"))
  | Omd.H6 t -> raise (Not_allowed ("H6", "phrasing_without_interactive"))
  | Omd.Hr -> raise (Not_allowed ("HR", "phrasing_without_interactive"))
  | Omd.Html (tag, attrs, t)
  | Omd.Html_block (tag, attrs, t) ->
      let a = List.map (fun (aname, aval) ->
        Unsafe.string_attrib aname (match aval with None -> "" | Some s -> s)
      ) attrs in
      Unsafe.node tag ~a (phrasing_without_interactive t)
  | Omd.Html_comment string -> pcdata ""
  | Omd.Paragraph t -> raise (Not_allowed ("P", "phrasing_without_interactive"))
  | Omd.Text s -> pcdata s
  | Omd.Emph t -> span ~a:[a_style "font-style: emph;"] (phrasing_without_interactive t)
  | Omd.Bold t -> span ~a:[a_style "font-weight: bold;"] (phrasing_without_interactive t)
  | Omd.Br -> br ()
  | Omd.Blockquote t -> raise (Not_allowed ("BLOCKQUOTE", "phrasing_without_interactive"))
  | Omd.Ul t -> raise (Not_allowed ("UL", "phrasing_without_interactive"))
  | Omd.Ulp t -> raise (Not_allowed ("UL", "phrasing_without_interactive"))
  | Omd.Url (href, t, title) -> raise (Not_allowed ("A", "phrasing_without_interactive"))
  | Omd.Ol t -> raise (Not_allowed ("OL", "phrasing_without_interactive"))
  | Omd.Olp t -> raise (Not_allowed ("OL", "phrasing_without_interactive"))
  | Omd.Code (lang, code) -> span ~a:[a_style "font-family: monospace;"] [pcdata code]
  | Omd.Code_block (lang, code) -> raise (Not_allowed ("DIV", "phrasing_without_interactive"))
  | Omd.NL -> br ()
  (* TODO *)
  | Omd.Ref (_, _, _, _) -> pcdata ""
  | Omd.Raw s -> pcdata s
  | Omd.Raw_block s -> raise (Not_allowed ("DIV", "phrasing_without_interactive"))
  | Omd.Img (alt, src, title) -> img ~src:(Xml.uri_of_string src) ~alt ~a:[a_title title] ()
  (* TODO *)
  | Omd.Img_ref (_, _, _, _) -> pcdata ""
  (* TODO *)
  | Omd.X _ -> pcdata ""
) t

let rec flow5_without_interactive t = List.map (function
  | Omd.H1 t -> h1 (phrasing_without_interactive t)
  | Omd.H2 t -> h2 (phrasing_without_interactive t)
  | Omd.H3 t -> h3 (phrasing_without_interactive t)
  | Omd.H4 t -> h4 (phrasing_without_interactive t)
  | Omd.H5 t -> h5 (phrasing_without_interactive t)
  | Omd.H6 t -> h6 (phrasing_without_interactive t)
  | Omd.Hr -> hr ()
  | Omd.Html (tag, attrs, t)
  | Omd.Html_block (tag, attrs, t) ->
      let a = List.map (fun (aname, aval) ->
        Unsafe.string_attrib aname (match aval with None -> "" | Some s -> s)
      ) attrs in
      Unsafe.node tag ~a (flow5_without_interactive t)
  | Omd.Html_comment string -> pcdata ""
  | Omd.Paragraph t -> p (phrasing_without_interactive t)
  | Omd.Text s -> pcdata s
  | Omd.Emph t -> span ~a:[a_style "font-style: emph;"] (phrasing_without_interactive t)
  | Omd.Bold t -> span ~a:[a_style "font-weight: bold;"] (phrasing_without_interactive t)
  | Omd.Br -> br ()
  | Omd.Blockquote t -> blockquote (flow5_without_interactive t)
  | Omd.Ul t -> ul (List.map (fun i -> li (flow5_without_interactive i)) t)
  | Omd.Ulp t -> ul (List.map (fun i -> li (phrasing_without_interactive i)) t)
  | Omd.Url (href, t, title) -> raise (Not_allowed ("Url", "flow5_without_interactive"))
  | Omd.Ol t -> ol (List.map (fun i -> li (flow5_without_interactive i)) t)
  | Omd.Olp t -> ol (List.map (fun i -> li (phrasing_without_interactive i)) t)
  | Omd.Code (lang, code) -> span ~a:[a_style "font-family: monospace;"] [pcdata code]
  | Omd.Code_block (lang, code) -> div ~a:[a_style "font-family: monospace;"] [pcdata code]
  | Omd.NL -> br ()
  (* TODO *)
  | Omd.Ref (_, _, _, _) -> pcdata ""
  | Omd.Raw s -> pcdata s
  | Omd.Raw_block s -> div [pcdata s]
  | Omd.Img (alt, src, title) -> img ~src:(Xml.uri_of_string src) ~alt ~a:[a_title title] ()
  (* TODO *)
  | Omd.Img_ref (_, _, _, _) -> pcdata ""
  (* TODO *)
  | Omd.X _ -> pcdata ""
) t

let rec phrasing t = List.map (function
  | Omd.H1 t -> raise (Not_allowed ("H1", "phrasing"))
  | Omd.H2 t -> raise (Not_allowed ("H2", "phrasing"))
  | Omd.H3 t -> raise (Not_allowed ("H3", "phrasing"))
  | Omd.H4 t -> raise (Not_allowed ("H4", "phrasing"))
  | Omd.H5 t -> raise (Not_allowed ("H5", "phrasing"))
  | Omd.H6 t -> raise (Not_allowed ("H6", "phrasing"))
  | Omd.Hr -> raise (Not_allowed ("HR", "phrasing"))
  | Omd.Html (tag, attrs, t)
  | Omd.Html_block (tag, attrs, t) ->
      let a = List.map (fun (aname, aval) ->
        Unsafe.string_attrib aname (match aval with None -> "" | Some s -> s)
      ) attrs in
      Unsafe.node tag ~a (phrasing t)
  | Omd.Html_comment string -> pcdata ""
  | Omd.Paragraph t -> raise (Not_allowed ("P", "phrasing"))
  | Omd.Text s -> pcdata s
  | Omd.Emph t -> span ~a:[a_style "font-style: emph;"] (phrasing t)
  | Omd.Bold t -> span ~a:[a_style "font-weight: bold;"] (phrasing t)
  | Omd.Br -> br ()
  | Omd.Blockquote t -> raise (Not_allowed ("BLOCKQUOTE", "phrasing"))
  | Omd.Ul t -> raise (Not_allowed ("UL", "phrasing"))
  | Omd.Ulp t -> raise (Not_allowed ("UL", "phrasing"))
  | Omd.Url (href, t, title) -> Raw.a ~a:[a_href (Xml.uri_of_string href); a_title title] (phrasing_without_interactive t)
  | Omd.Ol t -> raise (Not_allowed ("OL", "phrasing"))
  | Omd.Olp t -> raise (Not_allowed ("OL", "phrasing"))
  | Omd.Code (lang, code) -> span ~a:[a_style "font-family: monospace;"] [pcdata code]
  | Omd.Code_block (lang, code) -> raise (Not_allowed ("DIV", "phrasing"))
  | Omd.NL -> br ()
  (* TODO *)
  | Omd.Ref (_, _, _, _) -> pcdata ""
  | Omd.Raw s -> pcdata s
  | Omd.Raw_block s -> raise (Not_allowed ("DIV", "phrasing"))
  | Omd.Img (alt, src, title) -> img ~src:(Xml.uri_of_string src) ~alt ~a:[a_title title] ()
  (* TODO *)
  | Omd.Img_ref (_, _, _, _) -> pcdata ""
  (* TODO *)
  | Omd.X _ -> pcdata ""
) t

let rec flow5 t = List.map (function
  | Omd.H1 t -> h1 (phrasing t)
  | Omd.H2 t -> h2 (phrasing t)
  | Omd.H3 t -> h3 (phrasing t)
  | Omd.H4 t -> h4 (phrasing t)
  | Omd.H5 t -> h5 (phrasing t)
  | Omd.H6 t -> h6 (phrasing t)
  | Omd.Hr -> hr ()
  | Omd.Html (tag, attrs, t)
  | Omd.Html_block (tag, attrs, t) ->
      let a = List.map (fun (aname, aval) ->
        Unsafe.string_attrib aname (match aval with None -> "" | Some s -> s)
      ) attrs in
      Unsafe.node tag ~a (flow5 t)
  | Omd.Html_comment string -> pcdata ""
  | Omd.Paragraph t -> p (phrasing t)
  | Omd.Text s -> pcdata s
  | Omd.Emph t -> span ~a:[a_style "font-style: emph;"] (phrasing t)
  | Omd.Bold t -> span ~a:[a_style "font-weight: bold;"] (phrasing t)
  | Omd.Br -> br ()
  | Omd.Blockquote t -> blockquote (flow5 t)
  | Omd.Ul t -> ul (List.map (fun i -> li (flow5 i)) t)
  | Omd.Ulp t -> ul (List.map (fun i -> li (phrasing i : Html_types.phrasing elt list_wrap :> Html_types.flow5 elt list_wrap)) t)
  | Omd.Url (href, t, title) -> Raw.a ~a:[a_href (Xml.uri_of_string href); a_title title] (flow5_without_interactive t)
  | Omd.Ol t -> ol (List.map (fun i -> li (flow5 i)) t)
  | Omd.Olp t -> ol (List.map (fun i -> li (phrasing i : Html_types.phrasing elt list_wrap :> Html_types.flow5 elt list_wrap)) t)
  | Omd.Code (lang, code) -> span ~a:[a_style "font-family: monospace;"] [pcdata code]
  | Omd.Code_block (lang, code) -> div ~a:[a_style "font-family: monospace;"] [pcdata code]
  | Omd.NL -> br ()
  (* TODO *)
  | Omd.Ref (_, _, _, _) -> pcdata ""
  | Omd.Raw s -> pcdata s
  | Omd.Raw_block s -> div [pcdata s]
  | Omd.Img (alt, src, title) -> img ~src:(Xml.uri_of_string src) ~alt ~a:[a_title title] ()
  (* TODO *)
  | Omd.Img_ref (_, _, _, _) -> pcdata ""
  (* TODO *)
  | Omd.X _ -> pcdata ""
) t

