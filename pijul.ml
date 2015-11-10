(*
  Copyright Florent Becker and Pierre-Etienne Meunier 2015.

  This file is part of Pijul.

  Pijul is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  Pijul is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with Pijul.  If not, see <http://www.gnu.org/licenses/>.
*)

type host =
  | Localhost
  | Ssh_host of string

type repository_ref = { host : host; path : string }

let url r =
  match r.host with
  | Localhost -> r.path
  | Ssh_host (user_host) -> user_host^":"^r.path

let parse_url u =
  try
    let n=String.index u ':' in
    let host=String.sub u 0 n in
    let path=String.sub u (n+1) (String.length u-n-1) in
    {host = Ssh_host host; path}
  with
    Not_found-> {host = Localhost; path = u}

let default_remote_file="default_remote"

let get_remote dir url =
  let d = Filename.concat dir default_remote_file in
  if url = "" then
    if Sys.file_exists d
    then
      let o = open_in d in
      let l = input_line o in
      close_in o;
      Some l
    else
      None
  else (
    if not (Sys.file_exists d) then (
      let o = open_out d in
      output_string o url;
      close_out o
    );
    Some url)
type repository={ current_branch:string; dbi_nodes:Mdb.dbi; dbi_contents: Mdb.dbi; dbi_patches:Mdb.dbi; dbi_branches: Mdb.dbi; dbi_revbranches: Mdb.dbi; dbi_tree: Mdb.dbi; dbi_revtree:Mdb.dbi; dbi_inodes: Mdb.dbi; dbi_revinodes:Mdb.dbi }
let current_branch alldb=alldb.current_branch
let default_branch="main"
let open_repository txn=
  let dbi_nodes=Mdb.dbi_open txn "nodes" (Mdb._CREATE lor Mdb._DUPSORT lor Mdb._DUPFIXED) in
  let dbi_contents=Mdb.dbi_open txn "contents" (Mdb._CREATE) in
  let dbi_patches=Mdb.dbi_open txn "patches" (Mdb._CREATE lor Mdb._DUPSORT) in
  let dbi_branches=Mdb.dbi_open txn "branches" (Mdb._CREATE lor Mdb._DUPSORT) in
  let dbi_revbranches=Mdb.dbi_open txn "revbranches" (Mdb._CREATE lor Mdb._DUPSORT) in
  let dbi_tree=Mdb.dbi_open txn "tree" (Mdb._CREATE lor Mdb._DUPSORT) in
  let dbi_revtree=Mdb.dbi_open txn "revtree" (Mdb._CREATE) in
  let dbi_inodes=Mdb.dbi_open txn "inodes" (Mdb._CREATE) in
  let dbi_revinodes=Mdb.dbi_open txn "revinodes" (Mdb._CREATE) in
  let current_branch=
    try Mdb.get txn dbi_branches "\000" with Mdb.Notfound->default_branch
  in
  { current_branch;dbi_nodes;dbi_contents;dbi_patches;dbi_branches;dbi_revbranches;dbi_tree;dbi_revtree;dbi_inodes;dbi_revinodes }

let fc=Filename.concat

let to_hex digest=
  let str=String.create (String.length digest*2) in
  let hexletter x=
    if x<10 then (char_of_int (int_of_char '0'+x)) else char_of_int (int_of_char 'a' + x-10)
  in
  for i=0 to String.length digest-1 do
    str.[i*2]<-hexletter (int_of_char digest.[i] lsr 4);
    str.[i*2+1]<-hexletter (int_of_char digest.[i] land 0xf);
  done;
  str

let from_hex hex_patch_id=
  let id=String.create (String.length hex_patch_id / 2) in
  let from_hex c=
    let a=int_of_char c-int_of_char '0' in
    if a>=0 && a<=9 then a else (int_of_char c-int_of_char 'a'+10)
  in
  for i=0 to String.length id-1 do
    id.[i]<-char_of_int ((from_hex hex_patch_id.[2*i] lsl 4) lor (from_hex hex_patch_id.[2*i+1]))
  done;
  id

let _pijul=".pijul"
let pijuldir x=Filename.concat x _pijul
let patchesdir x=Filename.concat x "patches"
let pristinedir x=Filename.concat x "pristine"
let changesfile branch=("changes."^(to_hex branch))


let with_env path f=
  let env=Mdb.env_create () in
  try
    Mdb.env_set_mapsize env (10485760 lsl 7);
    Mdb.env_set_maxdbs env 9;
    let _=Mdb.reader_check env in
    Mdb.env_open env path 0 0o750;
    let x=f env in
    Mdb.env_close env;
    x
  with e->(Mdb.env_close env;raise e)

let with_txn env parent f=
  let t=Mdb.txn_begin env parent 0 in
  try let x=f t in Mdb.txn_commit t; x
  with e->(Mdb.txn_abort t;raise e)

let with_pijul pijul_path f=
  with_env (pristinedir pijul_path) (fun env->with_txn env None (fun txn->f env txn))


let with_cursor txn dbi f=
  let curs=Mdb.cursor_open txn dbi in
  try let x=f curs in Mdb.cursor_close curs; x with e->(Mdb.cursor_close curs;raise e)

type patch=
    { edges:(string * string) list list;
      dependencies:string list;
      name:string;
      author:string;
      time:string;
    }

let empty_patch={edges=[];dependencies=[];name="";author="";time=""}
let hash_size=20
let line_size=4
let key_size=hash_size+line_size
let root_key=String.make key_size '\000'

let sha1 file=
  let buf=String.create 64 in
  let sha1_=Cryptokit.Hash.sha1 () in
  let o=open_in_bin file in
  try
    let rec inp ()=
      let len=input o buf 0 (String.length buf) in
      if len>0 then (
        sha1_#add_substring buf 0 len;
        inp ()
      )
    in
    inp ();
    close_in o;
    (sha1_#result)
  with
    e->(close_in o;raise e)

let base64_output o str=
  let enc="ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/" in
  let rec encode i=
    if i+1<String.length str then (
      if i+2<String.length str then (
        let n=(int_of_char str.[i] lsl 16) lor (int_of_char str.[i+1] lsl 8) lor (int_of_char str.[i+2]) in
        output_char o (enc.[(n lsr 18) land 0x3f]);
        output_char o (enc.[(n lsr 12) land 0x3f]);
        output_char o (enc.[(n lsr 6) land 0x3f]);
        output_char o (enc.[n land 0x3f]);
        if i+3<String.length str then encode (i+3)
      ) else (
        let n=(int_of_char str.[i] lsl 16) lor (int_of_char str.[i+1] lsl 8) in
        output_char o (enc.[(n lsr 18) land 0x3f]);
        output_char o (enc.[(n lsr 12) land 0x3f]);
        output_char o (enc.[(n lsr 6) land 0x3f]);
        output_char o '='
      )
    ) else (
      let n=(int_of_char str.[i] lsl 16) in
      output_char o (enc.[(n lsr 18) land 0x3f]);
      output_char o (enc.[(n lsr 12) land 0x3f]);
      output_string o "=="
    )
  in
  encode 0


(*
let base64_encode str=
  let l=String.length str in
  let b=String.create (4*((l+2)/3)) in
  let enc="ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+" in
  let rec encode i j=
    if i+2<String.length str then (
      let n=(int_of_char str.[i] lsl 16) lor (int_of_char str.[i+1] lsl 8) lor (int_of_char str.[i+2]) in
      b.[j]<-enc.[n lsr 18];
      b.[j+1]<-enc.[(n lsr 12) land 0x3f];
      b.[j+2]<-enc.[(n lsr 6) land 0x3f];
      b.[j+3]<-enc.[n land 0x3f];
      encode (i+3) (j+4)
    ) else i
  in
  let i=encode 0 0 in
  if i+2=String.length str then (
    let n=(int_of_char str.[i] lsl 16) lor (int_of_char str.[i+1] lsl 8) in
    let l=String.length b in
    b.[l-4]<-enc.[n lsr 18];
    b.[l-3]<-enc.[(n lsr 12) land 0x3f];
    b.[l-2]<-enc.[(n lsr 6) land 0x3f];
    b.[l-1]<-'='
  ) else if i+1=String.length str then (
    let n=(int_of_char str.[i] lsl 16) in
    let l=String.length b in
    b.[l-4]<-enc.[n lsr 18];
    b.[l-3]<-enc.[(n lsr 12) land 0x3f];
    b.[l-2]<-'=';
    b.[l-1]<-'='
  );
  b
*)
let base64_decode str=
  let l=String.length str in
  let dec_char c=
    if c>='A' && c<='Z' then int_of_char c-int_of_char 'A' else
      if c>='a' && c<='z' then 26+int_of_char c-int_of_char 'a' else
        if c>='0' && c<='9' then 52+int_of_char c-int_of_char '0' else
          if c='+' then 62 else
            if c='/' then 63 else 0
  in
  let s=
    String.create
      (if str.[l-2]='=' then (l/4)*3-2 else
          if str.[l-1]='=' then (l/4)*3-1 else ((l/4)*3))
  in
  let rec decode i j=
    let a=dec_char str.[i] in
    let b=dec_char str.[i+1] in
    let c=dec_char str.[i+2] in
    let d=dec_char str.[i+3] in
    let n=(a lsl 18) lor (b lsl 12) lor (c lsl 6) lor d in
    s.[j]<-char_of_int (n lsr 16);
    if j+1<String.length s then (
      s.[j+1]<-char_of_int ((n lsr 8) land 0xff);
      if j+2<String.length s then(
        s.[j+2]<-char_of_int (n land 0xff);
        if j+3 < String.length s then decode (i+4) (j+3)
      )
    )
  in
  decode 0 0;
  s


let json_output_patch o p=
  let json_string name x=
    output_string o "\"";
    output_string o name;
    output_string o "\": \"";
    for i=0 to String.length x-1 do
      match x.[i] with
        '"' | '\\' | '/' -> (
          output_char o '\\';
          output_char o x.[i];
        )
      | '\b'->output_string o "\\b"
      | '\012'->output_string o "\\f"
      | '\n'->output_string o "\\n"
      | '\r'->output_string o "\\r"
      | '\t'->output_string o "\\t"
      | _->output_char o x.[i]
    done;
    output_string o "\"";
  in
  output_string o "{";
  json_string "time" p.time;output_string o ",";
  json_string "name" p.name;output_string o ",";
  json_string "author" p.author;output_string o ",";
  (**)
  output_string o "\"dependencies\":[";
  let rec json_list f l=match l with
      []->()
    | [h]->f h
    | h::s->(f h;output_string o ",";json_list f s)
  in
  json_list (fun x->output_char o '"';base64_output o x;output_char o '"') p.dependencies;
  (**)
  output_string o "], \"edges\":[";
  json_list
    (fun l->
      output_string o "[";
      json_list (fun (a,b)->
        output_string o "[\"";
        base64_output o a;
        output_string o "\",\"";
        base64_output o b;
        output_string o "\"]";
      ) l;
      output_string o "]"
    )
    p.edges;
  (**)
  output_string o "]}\n"


let json_input_patch o=
  let open Yojson.Basic.Util in
  let json=Yojson.Basic.from_channel o in
  let time=to_string (member "time" json) in
  let name=to_string (member "name" json) in
  let author=to_string (member "author" json) in
  let dependencies=List.map (fun x->base64_decode (to_string x)) (to_list (member "dependencies" json)) in
  let edges=
    List.map (fun x->
      List.map (fun y->match to_list y with
        a::b::_-> (
          let ba=base64_decode (to_string a) and bb= base64_decode (to_string b) in
          (* Printf.eprintf "dec : %S %S\n" (to_hex ba) (to_hex bb); *)
          ba,bb
        )
      | _->failwith "json parse error"
      ) (to_list x))
      (to_list (member "edges" json))
  in
  {time;name;author;dependencies;edges}

let input_dependencies o=
  (json_input_patch o).dependencies


let rec streq a i b j l=
  if l=0 then 0 else
    let c=compare a.[i] b.[j] in
    if c=0 then
      streq a (i+1) b (j+1) (l-1)
    else
      c

let node_of_val x=String.sub x 1 key_size

(** Iterates function f over all (k,v) in the database, such that k=key and char is a prefix of v.
Raises Invalid_argument if one of these values of v is strictly shorter than char. *)
let iter_buf=" "
let neighbors_iter curs key f char=
  (* In this module, this function is used to (1) iterate over one specific type of edges and (2) iterate over all edges created by a specific patch. *)
  iter_buf.[0]<-char_of_int char;
  try
    let rec iterate k b=
      if b.[0]=iter_buf.[0] then f b else
        if b.[0]>iter_buf.[0] then raise Mdb.Notfound;
      let k,b=Mdb.cursor_get curs k b Mdb._NEXT_DUP in
      iterate k b
    in
    let k,b=Mdb.cursor_get curs key iter_buf Mdb._GET_BOTH_RANGE in
    iterate k b
  with
    Mdb.Notfound->()


external cursor_position:Mdb.cursor->string->string->int->int="caml_cursor_position"

module S=Set.Make(String)
module M=Map.Make(String)
let str c=String.make 1 (char_of_int c)
let c c=char_of_int c

let new_flag=16
let deleted_flag=8
let parent_flag=4
let folder_flag=2
let pseudo_flag=1

let is_new c=int_of_char c land new_flag <> 0
let is_deleted c=int_of_char c land deleted_flag <> 0
let is_pseudo c=int_of_char c land pseudo_flag <> 0
let is_folder c=int_of_char c land folder_flag <> 0
let is_parent c=int_of_char c land parent_flag <> 0
let change_direction c=char_of_int (int_of_char c lxor parent_flag)
let change_deleted c=char_of_int (int_of_char c lxor deleted_flag)
let not_new c=char_of_int ((int_of_char c lor new_flag) lxor new_flag)

let key_buffer=String.create key_size
let edge_contents txn repo a b=
  if is_new b.[0] then
    String.sub a line_size (String.length a-line_size)
  else (
    try
      if is_parent b.[0] then (
        Mdb.get txn repo.dbi_contents a
      ) else (
        String.blit b 1 key_buffer 0 key_size;
        Mdb.get txn repo.dbi_contents key_buffer
      )
    with
      Mdb.Notfound->""
  )

let folder_children_iter txn db key f=
  with_cursor txn db.dbi_nodes (fun curs->neighbors_iter curs key f ((folder_flag)))
let folder_parents_iter txn db key f=
  with_cursor txn db.dbi_nodes (fun curs->neighbors_iter curs key f ((parent_flag lor folder_flag)))

let real_and_pseudo_children_iter txn db key f=
  with_cursor
    txn db.dbi_nodes
    (fun curs->
     neighbors_iter curs key f (0);
     neighbors_iter curs key f (pseudo_flag)
    )
(** Iterate on all non-pseudo parents, folder or not. *)
let real_parents_iter txn db key f=
  with_cursor
    txn db.dbi_nodes
    (fun curs->
     neighbors_iter curs key f (parent_flag);
     neighbors_iter curs key f ((parent_flag lor folder_flag));
     neighbors_iter curs key f ((parent_flag lor deleted_flag));
     neighbors_iter curs key f ((parent_flag lor folder_flag lor deleted_flag))
    )

exception Found

let inode_size=10
let root_inode=String.make inode_size '\000'

(** create_new_inode must be called with a cursor on the dbi_revtree base. *)
let create_new_inode curs=
  let inode=String.create inode_size in
  try
    while true do
      for i=0 to inode_size-1 do
        inode.[i]<-char_of_int (Random.int 0x100)
      done;
      let k,_=Mdb.cursor_get curs inode "" Mdb._SET_RANGE in
      if streq k 0 inode 0 inode_size <> 0 then raise Mdb.Notfound
    done;
    ""
  with
    Mdb.Notfound->inode

let apply_hunk uu vv txn repo curs patch_id hunk=
  List.iter
    (fun (u,v)->
      (* Printf.eprintf "hunk: %S %S\n" (to_hex u) (to_hex v);flush stderr; *)
      let blit_u r off=
        if is_new v.[0] then (
          String.blit patch_id 0 r off hash_size;
          String.blit u 0 r (off+hash_size) line_size;
        ) else
          String.blit u 0 r off key_size
      in
      let blit_v r off=
        if String.length v=1+line_size then (
          String.blit patch_id 0 r off hash_size;
          String.blit v 1 r (off+hash_size) line_size;
        ) else
          String.blit v 1 r off key_size
      in

      if String.length v=1+key_size+hash_size then (
        blit_u uu 0;
        String.blit v 0 vv 0 (1+key_size+hash_size);
        vv.[0]<-change_deleted vv.[0];
        vv.[0]<-not_new vv.[0];

        let ok=cursor_position curs uu vv Mdb._GET_BOTH in
        if ok=0 then (
          Mdb.cursor_del curs 0;

          vv.[0]<-change_direction vv.[0];
          blit_u vv 1;
          blit_v uu 0;
          let ok=cursor_position curs uu vv Mdb._GET_BOTH in
          if ok=0 then Mdb.cursor_del curs 0;
        )
      );


      blit_u uu 0;
      if is_new v.[0] then
        Mdb.put txn repo.dbi_contents uu (String.sub u line_size (String.length u-line_size)) 0;
      blit_v vv 1;
      vv.[0]<-not_new v.[0];
      String.blit patch_id 0 vv (1+key_size) hash_size;

      Mdb.put txn repo.dbi_nodes uu vv 0;

      blit_u vv 1;
      blit_v uu 0;
      vv.[0]<-change_direction vv.[0];
      Mdb.put txn repo.dbi_nodes uu vv 0;


    ) hunk


external has_neighbors:Mdb.cursor->string->int->bool="caml_has_neighbors"
let is_dead curs node=
  has_neighbors curs node (parent_flag lor deleted_flag)

external delete_edges:Mdb.cursor->string->int->unit="caml_delete_edges"
(* external connect_plus:Mdb.txn->Mdb.dbi->Mdb.cursor->string->string->unit="caml_connect_plus" *)

let unsafe_apply txn repo patch patch_id=
  (* A node is "alive" when no deleted edge points to it, and dead if no non-deleted edge points to it.
It is a "zombie" else.
This function has three duties:
1. apply the hunks
2. if the patch adds a line after a dead line, reconnect the graph (because other functions are not supposed to follow dead edges). This is a potentially lengthy operation, depending on the size of history. However, it is still linear in the number of lines in conflict.
   This is done by connect_plus.
3. if the patch deletes a line b, we need to reconnect the alive parents of b (if any) to the alive descendants of b.
   This is done by connect_minus.
*)

  (* We start by applying the hunks. *)
  with_cursor
    txn repo.dbi_nodes
    (fun curs->

      let rv=String.create key_size in
      let ru=String.create (1+hash_size+key_size) in
      List.iter (apply_hunk rv ru txn repo curs patch_id) patch.edges;

     (* The rest of this function removes and adds extra "pseudo" edges. *)

     (* First remove incorrect pseudo-edges. *)
      (* let pseudo_parent=str (pseudo_flag lor parent_flag) in *)
      (* let bb=String.create key_size in *)
     List.iter
       (List.iter
          (fun (u,v)->
            if is_deleted v.[0] then
              begin
                let deleted_node=if is_parent v.[0] then u else (String.blit v 1 rv 0 key_size;rv) in
                (* Are there still other nodes pointing to deleted_node ? *)
                if is_dead curs deleted_node then (
(*
                  let rec kill_all ()=
                    let _,par=Mdb.cursor_get curs deleted_node pseudo_parent Mdb._GET_BOTH_RANGE in
                    if par.[0]=pseudo_parent.[0] then (
                      Mdb.cursor_del curs 0;
                      String.blit par 1 bb 0 key_size;
                      par.[0]<-change_direction par.[0];
                      String.blit deleted_node 0 par 1 key_size;
                      let ok=cursor_position curs bb par Mdb._GET_BOTH in
                      if ok=0 then Mdb.cursor_del curs 0;
                      kill_all ()
                    )
                  in
                  try kill_all () with Mdb.Notfound->()
*)
                  (* C implementation of the same thing. *)
                  delete_edges curs deleted_node (pseudo_flag lor parent_flag);
                )
              end
          )
       ) patch.edges;

      (* Then add pseudo-edges if we just disconnected the non-pseudo part of the graph. *)

     let extra=Buffer.create (key_size*10) in
     let connect_plus a0 b=
       (* connect_ancestors a b adds an edge between the alive ancestors of a and b (used if a is not alive). *)

       let stop=ref S.empty in
       let rec connect_ancestors first a=
         if not (S.mem a !stop) then
           begin
             stop:=S.add a !stop;
             let next=String.create key_size in
             neighbors_iter
               curs a
               (fun edge->
                 String.blit edge 1 next 0 key_size;
                 connect_ancestors false next) ((parent_flag lor deleted_flag));
             if not first then (
               (* is a alive or zombie ? *)
               if (has_neighbors curs a parent_flag)
                 || (has_neighbors curs a (parent_flag lor pseudo_flag))
                 || (has_neighbors curs a (parent_flag lor folder_flag))
                 || (has_neighbors curs a (parent_flag lor pseudo_flag lor pseudo_flag))
                 && a<>b
               then (
                 Buffer.add_string extra a;
                 Buffer.add_string extra b
               (* extra:=(a,String.copy b):: !extra; *)
               )
             )
           end
       in
       connect_ancestors true a0
     (* PEM: there is a C version of this function. I stopped
        debugging it because I'm not 100% convinced of its usefulness. *)
       (* connect_plus txn repo.dbi_nodes curs a0 b *)
     in
     let connect_minus a b=
       (* If a is alive, and this patch deletes b, then connect a to the alive descendants of b. *)
       if (has_neighbors curs a parent_flag)
         || (has_neighbors curs a (parent_flag lor pseudo_flag))
       then
         begin
           let f edge=
             Buffer.add_string extra a;
             Buffer.add_substring extra edge 1 key_size
             (* extra:=(String.copy a,String.sub edge 1 key_size):: !extra *)
           in
           neighbors_iter curs b f 0;
           neighbors_iter curs b f pseudo_flag;
         end
     in
     (* apply connect_plus and connect_minus to the patch. The only effect of this is to fill variable "extra".*)

      let ra=String.create key_size in
      let rb=String.create key_size in
     List.iter
       (List.iter
          (fun (a,b)->
            let _=
              if is_new b.[0] then (
                String.blit patch_id 0 ra 0 hash_size;
                String.blit a 0 ra hash_size line_size;
              ) else (
                String.blit a 0 ra 0 key_size
              )
            in
            let _=
              if String.length b=1+line_size then (
                String.blit patch_id 0 rb 0 hash_size;
                String.blit b 1 rb hash_size line_size;
              ) else (
                String.blit b 1 rb 0 key_size
              )
            in
            if is_deleted b.[0] then
              if is_parent b.[0] then
                connect_minus rb ra
              else
                connect_minus ra rb
            else
              if String.length b>=1+line_size then
                begin
                  if is_parent b.[0] then (
                    if String.length b>1+line_size then
                      connect_plus rb ra
                  ) else
                    if not (is_new b.[0]) then
                      connect_plus ra rb
                end
          )) patch.edges;
     (* Then, add all edges from !extra. *)
      String.blit patch_id 0 ru (1+key_size) hash_size;
      let rec add_extra i=
        if i<Buffer.length extra then (
          ru.[0]<-char_of_int pseudo_flag;
          Buffer.blit extra i rv 0 key_size;
          Buffer.blit extra (i+key_size) ru 1 key_size;
          Mdb.cursor_put curs rv ru 0;

          ru.[0]<-change_direction ru.[0];

          Buffer.blit extra (i+key_size) rv 0 key_size;
          Buffer.blit extra i ru 1 key_size;
          Mdb.cursor_put curs rv ru 0;
          add_extra (i+2*key_size)
        )
      in
      add_extra 0
    )



let branch_has_patch txn alldb branch patch=
  with_cursor
    txn alldb.dbi_branches
    (fun curs->
     try
       let _,p=Mdb.cursor_get curs branch ("\001"^patch) Mdb._GET_BOTH_RANGE in
       streq p 1 patch 0 hash_size = 0
     with
       Mdb.Notfound->false
    )

let branch_patches txn repo current_branch f=
  with_cursor
    txn repo.dbi_revbranches
    (fun cursor->
     let rec get_all_patches k a=
       f (String.sub a (1+hash_size) hash_size);
       let k',a'=Mdb.cursor_get cursor k a Mdb._NEXT_DUP in
       get_all_patches k' a'
     in
     try
       let k,a=Mdb.cursor_get cursor current_branch "" Mdb._SET in
       get_all_patches k a;
     with Mdb.Notfound->()
    )

let branch_revpatches txn repo current_branch f=
  with_cursor
    txn repo.dbi_revbranches
    (fun cursor->
     let rec get_all_patches k a=
       f (String.sub a (1+hash_size) hash_size);
       let k',a'=Mdb.cursor_get cursor k a Mdb._PREV_DUP in
       get_all_patches k' a'
     in
     try
       let _=Mdb.cursor_get cursor current_branch "" Mdb._SET in
       let k,a=Mdb.cursor_get cursor current_branch "" Mdb._LAST_DUP in
       get_all_patches k a;
     with Mdb.Notfound->()
    )

let branch_previous_patch txn repo current_branch a=
  with_cursor
    txn repo.dbi_revbranches
    (fun cursor->
     let _,a=Mdb.cursor_get cursor current_branch a Mdb._GET_BOTH_RANGE in
     let _,a=Mdb.cursor_get cursor current_branch a Mdb._PREV_DUP in
     a)

let branch_last_patch txn repo current_branch=
  with_cursor
    txn repo.dbi_revbranches
    (fun cursor->
     let _,a=Mdb.cursor_get cursor current_branch "" Mdb._SET in
     let _,a=Mdb.cursor_get cursor current_branch a Mdb._LAST_DUP in
     a)





exception Dependency_not_met of string*string
let apply txn alldb patch patch_id=
  with_cursor
    txn alldb.dbi_branches
    (fun curs->
     let applicable=
       try
         (* Is this patch new? *)
         let _,pid=Mdb.cursor_get curs alldb.current_branch ("\001"^patch_id) Mdb._GET_BOTH_RANGE in
         (streq pid 1 patch_id 0 (hash_size) <> 0)
       with Mdb.Notfound->true
     in
     if applicable then
       begin
         List.iter
           (fun dep->
            if not (branch_has_patch txn alldb alldb.current_branch dep) then
              raise (Dependency_not_met (to_hex patch_id,to_hex dep))
            else
              Mdb.put txn alldb.dbi_patches dep patch_id 0;
           ) patch.dependencies;
         with_cursor
           txn alldb.dbi_revbranches
           (fun revcurs->
            let number=
              try
                let _,k=Mdb.cursor_get revcurs alldb.current_branch "\001" Mdb._LAST in
                let k=String.sub k 1 hash_size in
                let rec increment_string i=
                  if i<0 then ()
                  else
                    if int_of_char k.[i]<0xff then
                      k.[i]<-char_of_int (int_of_char k.[i]+1)
                    else (
                      k.[i]<-'\000';
                      increment_string (i-1)
                    )
                in
                increment_string (hash_size-1);
                k
              with
                Mdb.Notfound->String.make hash_size '\000'
            in
            Mdb.put txn alldb.dbi_branches alldb.current_branch ("\001"^patch_id^number) 0;
            Mdb.put txn alldb.dbi_revbranches alldb.current_branch ("\001"^number^patch_id) 0;
           );
         unsafe_apply txn alldb patch patch_id
       end
    )



let dependencies actions=
  let hunk_deps deps hunk=
      List.fold_left
        (fun deps (a,b)->
          (* Printf.eprintf "%S %S\n" (to_hex a) (to_hex b);flush stderr; *)
          let deps=
            if is_new b.[0] then deps else
              if streq a 0 root_key 0 key_size=0
              || (if String.length b>=1+key_size then streq b 1 root_key 0 key_size=0 else true)
              then deps else
                S.add (String.sub a 0 hash_size) deps
          in
          if String.length b<1+key_size then
            deps
          else
            let deps=if streq b 1 root_key 0 key_size = 0 then deps else
                S.add (String.sub b 1 hash_size) deps
            in
            if String.length b=1+key_size+hash_size then
              S.add (String.sub b (1+key_size) hash_size) deps
            else
              deps
        ) deps hunk
  in
  let s=List.fold_left hunk_deps S.empty actions in
  s




let delete_edges txn alldb node edges=
  if node="" then edges else
    begin
      let edges=ref edges in
      real_parents_iter
        txn alldb node
        (fun parent->
          parent.[0]<-(change_deleted parent.[0]);
          edges:=(node,parent):: !edges;
        );
      !edges
    end

(* beware of the zombie edge! Invert only edges with the same deletion status as (a,b). *)
let rollback txn repo records patch_id=
  let bb=String.create (1+key_size) in
  with_cursor
    txn repo.dbi_nodes
    (fun curs->
      List.map
        (fun h->
          List.rev
            (List.fold_left
               (fun l (a,b)->
                 let rl=ref l in
                 let akey=if is_new b.[0] then patch_id^a else a in
                 if String.length b=1+line_size then (
                   String.blit patch_id 0 bb 1 hash_size;
                   String.blit b 1 bb (1+hash_size) line_size;
                 ) else (
                   String.blit b 1 bb 1 key_size
                 );
                 neighbors_iter
                   curs akey
                   (fun bb->
                     bb.[0]<-change_deleted bb.[0];
                     rl:= (akey,bb):: !rl)
                   (int_of_char (not_new b.[0]));
                 !rl
               ) [] h)
        ) records
    )

(* unsafe = ne vérifie pas les dépendances. *)
let unsafe_unrecord txn alldb patch patch_id=
  let akey=String.create key_size in
  let ckey=String.create key_size in
  let bkey=String.create (1+hash_size+key_size) in
  let unrecord_edge curs (a,b)=
    if String.length b=1+hash_size+key_size then
      begin
        String.blit b 0 bkey 0 (1+hash_size+key_size);
        let _=Mdb.cursor_get curs a bkey Mdb._GET_BOTH in
        Mdb.cursor_del curs 0;
        bkey.[0]<-change_deleted bkey.[0];
        Mdb.cursor_put curs a bkey 0;

        let akey=node_of_val b in
        bkey.[0]<-change_direction bkey.[0];
        String.blit a 0 bkey 1 key_size;
        let _=Mdb.cursor_get curs akey bkey Mdb._GET_BOTH in
        Mdb.cursor_del curs 0;
        bkey.[0]<-change_deleted bkey.[0];
        Mdb.cursor_put curs akey bkey 0;
      end
    else
      begin
        if is_new b.[0] then (
          String.blit a 0 akey hash_size line_size;
          String.blit patch_id 0 akey 0 hash_size;
        )else (
          String.blit a 0 akey 0 key_size;
        );
        if String.length b=1+line_size then (
          bkey.[0]<-not_new b.[0];
          String.blit b 1 bkey (1+hash_size) line_size;
          String.blit patch_id 0 bkey 1 hash_size;
          String.blit patch_id 0 bkey (1+key_size) hash_size;
        ) else if String.length b=1+key_size then (
          bkey.[0]<-not_new b.[0];
          String.blit b 1 bkey 1 key_size;
          String.blit patch_id 0 bkey (1+key_size) hash_size;
        );
        let _=Mdb.cursor_get curs akey bkey Mdb._GET_BOTH in
        Mdb.cursor_del curs 0;

        String.blit bkey 1 ckey 0 key_size;
        String.blit akey 0 bkey 1 key_size;
        bkey.[0]<-change_direction bkey.[0];
        let _=Mdb.cursor_get curs ckey bkey Mdb._GET_BOTH in
        Mdb.cursor_del curs 0;
      end
  in
  with_cursor
    txn alldb.dbi_nodes
    (fun curs->
      List.iter (List.iter (unrecord_edge curs)) patch.edges;
    );
  with_cursor
    txn alldb.dbi_branches
    (fun curs->
     try let _,a=Mdb.cursor_get curs alldb.current_branch ("\001"^patch_id) Mdb._GET_BOTH_RANGE in
         if streq a 1 patch_id 0 hash_size = 0 then (
           Mdb.cursor_del curs 0;
           with_cursor
             txn alldb.dbi_revbranches
             (fun revcurs->
              let _=
                Mdb.cursor_get
                  revcurs alldb.current_branch ("\001"^String.sub a (1+hash_size) hash_size^patch_id)
                  Mdb._GET_BOTH
              in
              Mdb.cursor_del revcurs 0;
             )
         ) else raise Mdb.Notfound
     with Mdb.Notfound->failwith "unrecord: patch is not applied")


let unrecord_sync txn alldb patch_id=
  with_cursor
    txn alldb.dbi_revinodes
    (fun curs_rev->
     with_cursor
       txn alldb.dbi_inodes
       (fun curs->
        let l=ref [] in
        let rec iterate a b=
          if streq a 0 patch_id 0 hash_size = 0 then (
            l:=(a,b):: !l;
            let a',b'=Mdb.cursor_get curs_rev a b Mdb._NEXT in
            iterate a' b'
          )
        in
        let _=
          try
            let a,b=Mdb.cursor_get curs_rev patch_id "" Mdb._SET_RANGE in
            iterate a b
          with Mdb.Notfound->()
        in
        List.iter
          (fun (a,b)->
           let _=Mdb.cursor_get curs_rev a "" Mdb._SET in
           Mdb.cursor_del curs_rev 0;
           let _=Mdb.cursor_get curs b "" Mdb._SET in
           Mdb.cursor_del curs 0;
          ) !l
       )
    )


exception Unrecord_dependencies of string*string
let unrecord txn alldb patch patch_id=
  with_cursor
    txn alldb.dbi_patches
    (fun curs->
     try
       let rec iterate a b=
         if a=patch_id then (
           if branch_has_patch txn alldb alldb.current_branch b then raise (Unrecord_dependencies (to_hex patch_id,to_hex b))
           else
             let a,b=Mdb.cursor_get curs a b Mdb._NEXT in
             iterate a b
         )
       in
       let a,b=Mdb.cursor_get curs patch_id "" Mdb._SET in
       iterate a b
     with Mdb.Notfound->()
    );
  unsafe_unrecord txn alldb patch patch_id


let write_changes ~pijuldir txn alldb=
  let file,o=Filename.open_temp_file ~mode:[Open_binary;Open_creat;Open_trunc]
                                     ~temp_dir:pijuldir "changes" "" in
  (try
      with_cursor
        txn alldb.dbi_branches
        (fun curs->
         try
           let rec iterate a b=
             output_string o b;
             let a,b=Mdb.cursor_get curs a b Mdb._NEXT in
             iterate a b
           in
           let a,b=Mdb.cursor_get curs alldb.current_branch "" Mdb._SET in
           iterate a b
         with Mdb.Notfound->()
        );
      close_out o
    with e->(close_out o;raise e));
  let targ=changesfile alldb.current_branch in
  Sys.rename file (fc pijuldir targ)



type line={key:string;half_deleted:bool;mutable children:line list;mutable index:int;mutable lowlink:int;
           mutable spit:bool;mutable onstack:bool}
let empty_line={key="";half_deleted=false;children=[];spit=false;
                index=(-1);lowlink=0;onstack=false;
               }

(* Retrieve basically projects the graph into Samuel's category. I (PE) believe this is only needed now for temporarily storing the order numbers, which might be stored more efficiently in a (key->int) map. *)
let retrieve txn alldb key0=
  let sink={empty_line with key=""} in
  let cache=ref (M.singleton "" sink) in
  let rec retrieve curs key=
    (* Printf.eprintf "retrieve : %S\n" (to_hex key);flush stderr; *)
    try
      M.find key !cache
    with
      Not_found->
      begin
        let half_deleted=has_neighbors curs key (parent_flag lor deleted_flag) in
        let e={empty_line with key;half_deleted} in
        cache:=M.add key e !cache;

        real_and_pseudo_children_iter
          txn alldb key
          (fun b->
           let b=node_of_val b in
           e.children<-(retrieve curs b)::(e.children)
          );
        let _=
          match e.children with
            []->e.children<-[sink]
          | _->()
        in
        e
      end
  in
  with_cursor txn alldb.dbi_nodes (fun curs->retrieve curs key0)


let reset_spit line=
  let r=ref S.empty in
  let rec reset_spit t=
    if not (S.mem t.key !r) then (
      t.spit<-false;
      r:=S.add t.key !r;
      List.iter reset_spit t.children
    )
  in
  reset_spit line

let tarjan line=
  let i=ref 0 in
  let stack=ref [] in
  let rec dfs v=
    v.index<- !i;
    v.lowlink<- !i;
    stack:=v:: !stack;
    v.onstack<-true;
    incr i;
    List.iter
      (fun w->
        if w.index<0 then (dfs w; v.lowlink<-min v.lowlink w.lowlink)
        else if w.onstack then v.lowlink<-min v.lowlink w.index
      ) v.children;
    if v.lowlink=v.index then (
      let rec pop st=match st with
          []->[]
        | h::s->(
          h.onstack<-false;
          if h.key=v.key then s else pop s
        )
      in
      stack:=
        match !stack with
          h::s->(
            h.onstack<-false;
            pop s
          )
        | []->[];
    )
  in
  dfs line;
  !i-1




(** outputs a file to the given channel. The optional parameter [store] is meant for {!diff}, which needs to remember what it output. *)
let output_file ?(store=false) txn repo ouch file=
  let maxlevel=tarjan file in
  let a=Array.make (maxlevel+1) 0 in
  let lines=Array.make (maxlevel+1) [] in
  let rec fill_topo line=
    if not line.spit then (
      line.spit<-true;
      a.(line.lowlink)<-a.(line.lowlink)+if line.half_deleted then 2 else 1;
      lines.(line.lowlink)<-line::lines.(line.lowlink);
      List.iter fill_topo line.children
    )
  in
  fill_topo file;
  for i=0 to maxlevel do
    if a.(i)>1 then
      List.iter
        (fun l->
         List.iter
           (fun chi->
            for i=l.lowlink+1 to chi.lowlink-1 do
              a.(i)<-a.(i)+1
            done;
           ) l.children
        ) lines.(i)
  done;
  reset_spit file;

  let arra=ref [] in
  let output_line k cont=
    (* Printf.eprintf "output %d : %S %S\n" __LINE__ (to_hex k) cont;flush stderr; *)
    (* Printf.eprintf "output : %S %S\n" (to_hex k) cont;flush stderr; *)
    if store then arra:=k:: !arra;
    output_string ouch cont;
  in
  let get_conflict l=
    let conflict=ref [] in
    let last=ref empty_line in
    let rec get l nodes s=
      if a.(l.lowlink)<=1 then (
        conflict:=(List.rev nodes):: !conflict;
        last:=l
      ) else (
        if not (S.mem l.key s) then (
          if l.half_deleted then (
            let min_order=List.fold_left (fun m l->min m l.lowlink) max_int l.children in
            if min_order<max_int then
              List.iter (fun c->if c.lowlink=min_order then get c (nodes) (S.add l.key s)) l.children
          );

          let min_order=List.fold_left (fun m l->min m l.lowlink) max_int l.children in
          if min_order<max_int then
            List.iter (fun c->if c.lowlink=min_order then get c (l::nodes) (S.add l.key s)) l.children
        )
      )
    in
    List.iter (fun l->get l [] S.empty) l;
    !conflict, !last
  in
  let rec output_file i=
    if i>=Array.length a then () else
      if a.(i)=0 then ()
      else (
        (* Printf.eprintf "output %d %d\n" i a.(i); *)
        (* List.iter (fun x->Printf.fprintf stderr "\t%S\n" (to_hex x.key)) lines.(i); *)
        if a.(i)=1 then (
          let h=List.hd lines.(i) in
          let k=h.key in
          if String.length k=key_size then (
            let cont=try Mdb.get txn repo.dbi_contents k with Mdb.Notfound->"" in
            output_line k cont;
            h.spit<-true;
          );
          output_file (i+1)
        ) else
          begin
            let confl,next=get_conflict lines.(i) in
            output_line "" ">>>>>>>>>>>>>>>>>>>>>>>>>>>>\n";
            let rec output_conflict con=match con with
                []->()
              | h::s->
                 begin
                   List.iter
                     (fun h->
                      let k=h.key in
                      let cont=try Mdb.get txn repo.dbi_contents k with Mdb.Notfound->"" in
                      output_line k cont
                     ) h;
                   let _=match s with []->() | _->output_line "" "==========================\n" in
                   output_conflict s
                 end
            in
            output_conflict confl;
            output_line "" "<<<<<<<<<<<<<<<<<<<<<<<<<<<<\n";
            (* Zombie lines don't get the same "next" as true
               conflicts: for them, "next" is the last zombie in the
               conflict, which is why we need to make sur i moves forward. *)
            output_file (max (i+1) next.lowlink) 
          end
      )
  in
  output_file 0;
  List.rev !arra



let create_num n=
  let rec int_blit str off i n=
    if i<line_size then (str.[off+i]<-char_of_int (n land 0xff); int_blit str off (i+1) (n lsr 8))
  in
  let c=String.create line_size in
  int_blit c 0 0 n;
  c

let diff txn repo line_num a b=
  if not (Sys.file_exists b) then [] else
    begin
      (* Printf.eprintf "diff %S\n" b;flush stderr; *)
      let inp,out=Unix.open_process (Printf.sprintf "diff - %s" (Filename.quote b)) in
      let arra=output_file ~store:true txn repo out a in
      close_out out;
      let arra=Array.of_list (arra) in

      let rec prev i=
        if i<=0 then a.key else if arra.(i)="" then prev (i-1) else arra.(i)
      in
      let rec next i=if i>=Array.length arra then "" else if arra.(i)="" then next (i+1) else arra.(i) in

      let lines=ref [] in
      let l=ref "" in
      (try
          while true do
            l:=input_line inp;
            (* Printf.eprintf "diff says %S\n" !l;flush stderr; *)
            if !l.[0]<>'\\' then lines:= !l:: (match !lines with []->[] | h::s->(h^"\n")::s)
          done
        with End_of_file->());
      if !l<>"" then
        if !l.[0]<>'\\' then lines:= !l:: (match !lines with []->[] | h::s->(h^"\n")::s);
      let lines=Array.of_list (List.rev !lines) in
      let rec make_hunks i act=
        if i>=Array.length lines then act else
          if Str.string_match (Str.regexp "\\([0-9,]*\\)\\([adc]\\)\\([0-9,]*\\)") lines.(i) 0 then
            let (u,u'),(v,v'),change=
              let range x=
                match Str.split (Str.regexp ",") x with
                  u::v::_->(int_of_string u,int_of_string v)
                | u::_->let uu=int_of_string u in uu,uu
                | _->failwith "range"
              in
              let a=Str.matched_group 1 lines.(i) in
              let b=Str.matched_group 3 lines.(i) in
              range a,range b,Str.matched_group 2 lines.(i)
            in
            let make_add pre i imax=
              let rec make_keys context j keys=
                if j>imax then context,keys else
                  begin
                    let l=create_num !line_num in
                    incr line_num;
                    let cont=lines.(j) in
                    let cont=String.sub cont 2 (String.length cont-2) in
                    make_keys l (j+1) ((l^cont,(str (parent_flag lor new_flag))^context)::keys)
                  end
              in
              let keys=
                let pre=prev pre in
                let cont,keys=make_keys pre (i+1) [] in
                let n=next (u'+1) in
                if n="" then keys else ((n,(str (parent_flag)^cont))::keys)
              in
              List.rev keys
            in
            let make_del ()=
              let rec make_del i del_edges=
                if i>u' then del_edges
                else make_del (i+1) (delete_edges txn repo arra.(i) del_edges)
              in
              let d=make_del (u) [] in
              d
            in
            if change="d" then
              make_hunks (i+u'-u+1) ((make_del ())::act)
            else if change="a" then
              make_hunks (i+v'-v+1) ((make_add (u) i (i+(v'-v)+1))::act)
            else if change="c" then
              make_hunks (i+u'-u+v'-v+3) ((make_del()@make_add (u-1) (i+(u'-u)+2) (i+(u'-u)+(v'-v)+3))::act)
            else
              make_hunks (i+1) act
          else
            make_hunks (i+1) act
      in
      make_hunks 0 []
    end


let curs_iter curs f=
  try
    let a,b=Mdb.cursor_get curs "" "" Mdb._FIRST in
    f a b;
    while true do
      let a,b=Mdb.cursor_get curs "" "" Mdb._NEXT in
      f a b
    done;
  with Mdb.Notfound->()



exception Already_in_repository of string
(* Créer tous les inodes sur le chemin, s'il y a besoin. *)
let add_inode ?(inode="") txn alldb f=
  let rec dfs curs dir path=
    match path with
      []->()
    | h::s->
       begin
         let next=
           try
             Mdb.get txn alldb.dbi_tree (dir^h)
           with
             Mdb.Notfound->
             begin
               let inode=
                 match s with
                   [] when inode<>""->inode
                 | _->create_new_inode curs
               in
               (* Mdb.put txn alldb.dbi_filenames inode h 0; *)
               Mdb.put txn alldb.dbi_tree (dir^h) inode 0;
               (* Printf.eprintf "revtree put: %S %S %S\n" (to_hex inode) (to_hex dir) h;flush stderr; *)
               Mdb.put txn alldb.dbi_revtree inode (dir^h) 0;
               let _=
                 match s with
                   []->()
                 | _::_->Mdb.put txn alldb.dbi_tree inode "" 0
                                 (* Indiquer que c'est un répertoire. *)
               in
               inode
             end
         in
         dfs curs next s
       end
  in
  with_cursor
    txn alldb.dbi_revtree
    (fun curs->dfs curs root_inode f)

let addfile txn alldb f=
  add_inode ~inode:"" txn alldb f


let get_inode txn alldb f=
  let rec dfs parent dir path=
    match path with
      []->parent,dir
    | h::s->(
      (* Printf.eprintf "get_inode %S %S\n" (to_hex dir) h;flush stderr; *)
       let next=Mdb.get txn alldb.dbi_tree (dir^h) in
       dfs dir next s
    )
  in
  dfs root_inode root_inode f



exception Not_in_repository of string
let delfile txn alldb f=
  try
    let _,inode=get_inode txn alldb f in
    let v=Mdb.get txn alldb.dbi_inodes inode in
    v.[0]<-'\002';
    Mdb.put txn alldb.dbi_inodes inode v 0
  with
    Mdb.Notfound->raise (Not_in_repository (String.concat "/" f))



let movefile txn alldb f f'=
  try
    let parent,inode=get_inode txn alldb f in
    let basename=List.hd (List.rev f) in
    with_cursor
      txn alldb.dbi_tree
      (fun curs->
       let _=Mdb.cursor_get curs (parent^basename) "" Mdb._SET in
       Mdb.cursor_del curs 0);
    add_inode ~inode txn alldb f';
    (* Printf.eprintf "movefile? %S\n" (to_hex inode);flush stderr; *)
    let v=Mdb.get txn alldb.dbi_inodes inode in
    (* Printf.eprintf "movefile %S %S\n" (to_hex inode) (to_hex v);flush stderr; *)
    v.[0]<-'\001';
    Mdb.put txn alldb.dbi_inodes inode v 0
  with
    Mdb.Notfound->raise (Not_in_repository (String.concat "/" f))



let inode_iter txn alldb inode f=
  with_cursor
    txn alldb.dbi_tree
    (fun curs->
     let rec iter k v=
       (* Printf.eprintf "inode_iter %S %S\n" (to_hex k) (to_hex inode);flush stderr; *)
       if streq k 0 inode 0 inode_size=0 then
         (f k v;
          let k,v=Mdb.cursor_get curs k v Mdb._NEXT in
          iter k v)
     in
     try
       let k,v=Mdb.cursor_get curs inode "" Mdb._SET_RANGE in
       iter k v
     with
       Mdb.Notfound->()
    )

let tree_iter txn rep acc0 f=
  let rec fold inode acc=
    inode_iter
      txn rep inode
      (fun k v->
       let acc'=f acc (String.sub k inode_size (String.length k-inode_size)) in
       fold v acc'
      )
  in
  fold root_inode acc0


let path_of_inode revcursor inode=
  let rec find_path inode l=
    if inode=root_inode then l else
      let _,inode'=Mdb.cursor_get revcursor inode "" Mdb._SET in
      let parent=String.sub inode' 0 inode_size in
      if String.length inode'>inode_size then
        let base=String.sub inode' inode_size (String.length inode'-inode_size) in
        find_path parent (base::l)
      else l
  in
  find_path inode []

let path_of_folder_node txn repo key=
  let paths=ref [] in
  let rec find_path seen key l=
    if not (S.mem key seen) then
      if key=root_key then paths:=l:: !paths else (
        folder_parents_iter
          txn repo key
          (fun parent->
            let par=node_of_val parent in
            let cont=try Mdb.get txn repo.dbi_contents par with Mdb.Notfound->"" in
            folder_parents_iter
              txn repo par
              (fun grandparent->
                let grpar=node_of_val grandparent in
                find_path (S.add key seen) grpar (String.sub cont key_size (String.length cont-key_size)::l)
              )
          )
      )
  in
  find_path S.empty key [];
  !paths







let record ~working_copy txn alldb=
  let actions=ref [] in
  let line_num=ref 0 in
  let extra_nodes=ref (M.singleton root_inode root_key) in
  let updatables=ref M.empty in
  let rec dfs curs parent cur path real_path basename=
    (* let is_directory=try (Mdb.get txn alldb.dbi_inodes cur) = "" with Mdb.Notfound->false in *)
    if cur<>root_inode then
      begin
        (* Printf.eprintf "dfs %S %S\n" real_path basename;flush stderr; *)
        (* Est-ce que ce fichier est une addition ? *)
        let parent_node=
          try let p=Mdb.get txn alldb.dbi_inodes parent in
              String.sub p 3 key_size
          with Mdb.Notfound->M.find parent !extra_nodes
        in
        try
          let node=Mdb.get txn alldb.dbi_inodes cur in
          (* Printf.eprintf "record node=%S\n" (to_hex node);flush stderr; *)
          let permissions=
            let perm=(Unix.stat real_path).Unix.st_perm in
            let p=String.create 2 in
            p.[0]<-char_of_int ((perm lsr 8) land 0xff);
            p.[1]<-char_of_int (perm land 0xff);
            p
          in
          if node.[0]='\000' && permissions<>String.sub node 1 2 then node.[0]<-'\001';
          (* Printf.eprintf "node.[0]=%S %S %S\n" (String.sub node 0 1) permissions (String.sub node 1 2);flush stderr; *)
          match node.[0] with
            '\001'->
            begin
              (* Ce noeud a été déplacé par rapport au graphe. *)
              let l1=create_num !line_num in
              incr line_num;
              let l2=String.sub node 3 key_size in
              (* Printf.eprintf "1. l1,l2=%S %S\n" (to_hex l1) (to_hex l2);flush stderr; *)
              (* Printf.eprintf "1. parent=%S\n" (to_hex parent_node);flush stderr; *)
              (* Printf.eprintf "cur=%S\n" (to_hex cur);flush stderr; *)
              let ret=retrieve txn alldb l2 in
              let dif=
                if Sys.is_directory real_path then
                  []
                else
                  diff txn alldb line_num ret real_path
              in
              let deleted_other_names=
                let edges=ref [] in
                folder_parents_iter
                  txn alldb l2
                  (fun parent->
                   let par=node_of_val parent in
                   parent.[0]<-change_direction parent.[0];
                   String.blit l2 0 parent 1 key_size;
                   edges:=(par,parent):: !edges;
                   folder_parents_iter
                     txn alldb par
                     (fun grandparent->
                      let grpar=node_of_val grandparent in
                      grandparent.[0]<-change_deleted (change_direction grandparent.[0]);
                      String.blit par 0 grandparent 1 key_size;
                      edges:=(grpar,grandparent):: !edges;
                     )
                  );
                !edges
              in
              actions:=
                ((l1^permissions^basename, str (new_flag lor folder_flag lor parent_flag) ^ parent_node)
                 ::(l2, str (folder_flag lor parent_flag)^l1)
                 ::deleted_other_names)
              ::dif
              @ !actions;
            end
          | '\002'->
             begin
              let l2=String.sub node 3 key_size in
              let deleted_other_names=
                let edges=ref [] in
                folder_parents_iter
                  txn alldb l2
                  (fun parent->
                   let par=node_of_val parent in
                   parent.[0]<-change_direction parent.[0];
                   String.blit l2 0 parent 1 key_size;
                   edges:=(par,parent):: !edges;
                   folder_parents_iter
                     txn alldb par
                     (fun grandparent->
                      let grpar=node_of_val grandparent in
                      grandparent.[0]<-change_direction (change_deleted grandparent.[0]);
                      String.blit par 0 grandparent 1 key_size;
                      edges:=(grpar,grandparent):: !edges;
                     )
                  );
                !edges
              in
              actions:=deleted_other_names :: !actions
             end
          | _->
             if Sys.is_directory real_path then
               ()
             else (
               let ret=retrieve txn alldb (String.sub node 3 key_size) in
               let dif=diff txn alldb line_num ret real_path in
               actions:=dif @ !actions;
             )
        with
          Mdb.Notfound->
          begin
            let l1=create_num !line_num in
            incr line_num;
            let l2=create_num !line_num in
            incr line_num;
            (* Printf.eprintf "nf. l1,l2=%S %S\n" (to_hex l1) (to_hex l2);flush stderr; *)
            (* Printf.eprintf "cur=%S\n" (to_hex cur);flush stderr; *)
            extra_nodes:=M.add cur l2 !extra_nodes;
            updatables:=M.add l2 cur !updatables;
            let dif=
              if Sys.is_directory real_path then
                []
              else (
                let ret={empty_line with key=l2} in
                List.concat (diff txn alldb line_num ret real_path)
              )
            in
            let permissions=
              let perm=(Unix.stat real_path).Unix.st_perm in
              let p=String.create 2 in
              p.[0]<-char_of_int ((perm lsr 8) land 0xff);
              p.[1]<-char_of_int (perm land 0xff);
              p
            in
            actions:=
              ((l1^permissions^basename, str (new_flag lor folder_flag lor parent_flag) ^ parent_node)
               ::(l2, str (new_flag lor folder_flag lor parent_flag)^l1)
               ::dif)
              :: !actions;
          end
      end;
    inode_iter
      txn alldb cur
      (fun dir inode->
       if inode<>"" then (
         let basename=String.sub dir inode_size (String.length dir-inode_size) in
         dfs curs cur inode (if path<>"" then path^"/"^basename else basename)
             (Filename.concat real_path basename) basename
       )
      );
  in
  with_cursor
    txn alldb.dbi_nodes
    (fun curs->dfs curs root_inode root_inode "" working_copy "");
  !actions, !updatables



let sync_files txn alldb patch patch_id updates=
  List.iter
    (fun record->
     match record with
       (kname,h)::(l2,_)::_ when is_folder h.[0] && is_new h.[0]->
       begin
         let name=String.sub kname (line_size) (String.length kname-line_size) in
         let p2=patch_id^l2 in
         let inode_l2=try M.find l2 updates with Not_found->with_cursor txn alldb.dbi_revtree create_new_inode in
         let permissions=String.sub name 0 2 in
         (* Fichier local, il existe dans trees, mais pas dans inodes. *)
         Mdb.put txn alldb.dbi_inodes inode_l2 ("\000"^permissions^p2) 0;
         Mdb.put txn alldb.dbi_revinodes p2 inode_l2 0;
       end
     | (_,h)::(_,_)::_ when is_folder h.[0] && is_deleted h.[0] ->
        begin
          (* Printf.eprintf "sync_files\n"; *)
          List.iter
            (fun (_,b)->
             let key=String.sub b (1+hash_size) key_size in
             (* Printf.eprintf "inode %S\n" (to_hex key);flush stderr; *)
             (* b peut contenir le noeud du nom, ou le noeud de l'inode. Dans le premier cas, le premier appel lève Mdb.Notfound. *)
             try
               let inode=Mdb.get txn alldb.dbi_revinodes key in

               (* Décider si on doit le supprimer, c'est-à-dire si personne ne pointe plus sur lui dans le graphe. *)
               let has_parents=
                 try
                   folder_parents_iter
                     txn alldb key
                     (fun _->raise Found);
                   false
                 with Found->true
               in
               if not has_parents then
                 begin
                   (* Printf.eprintf "inode=%S\n" (to_hex inode);flush stderr; *)
                   let del_keys=ref [] in
                   let del_inodes=ref [] in
                   let rec recursive_del inode=
                     (* Printf.eprintf "deleting inode %S\n" (to_hex inode);flush stderr; *)
                     if inode<>"" then
                       begin
                         del_inodes:=inode:: !del_inodes;
                         inode_iter
                           txn alldb inode
                           (fun k child->
                            del_keys:=k :: !del_keys;
                            recursive_del child
                           )
                       end
                   in
                   recursive_del inode;
                   let parent=Mdb.get txn alldb.dbi_revtree inode in
                   Mdb.del txn alldb.dbi_tree parent None;
                   List.iter (fun a->Mdb.del txn alldb.dbi_tree a None) !del_keys;
                   List.iter (fun a->Mdb.del txn alldb.dbi_revtree a None) !del_inodes;
                 end
             with
               Mdb.Notfound->()
            ) record
        end
     | _->()
    ) patch.edges



let save_patch ~patches_path patch=
  let f,o=Filename.open_temp_file ~mode:[Open_binary;Open_creat;Open_trunc] ~temp_dir:patches_path "pijul" "" in
  json_output_patch o patch;
  close_out o;
  let digest=sha1 f in
  let patchname=fc patches_path (to_hex digest) in
  Unix.rename f patchname;
  digest



let debug_repository o txn alldb=
  let styles=Array.make 16 "" in
  for i=0 to Array.length styles-1 do
    styles.(i)<-"color="^[|"red";"blue";"green";"black"|].((i lsr 1) land 3);
    if is_deleted (c i) then styles.(i)<-styles.(i)^",style=dashed"
    else (if is_pseudo (c i) then styles.(i)<-styles.(i)^",style=dotted")
  done;
  Printf.fprintf o "digraph{\n";
  with_cursor
    txn alldb.dbi_nodes
    (fun curs->
     curs_iter
       curs
       (fun a b->
        (* Printf.eprintf "debug a: %S\n      b: %S\n" (to_hex a) (to_hex b); flush stderr; *)
        let acont=try Mdb.get txn alldb.dbi_contents a with Mdb.Notfound->"" in
        let bb=node_of_val b in
        let flag=int_of_char b.[0] in
        Printf.fprintf o "n_%s[label=%S];\n" (to_hex a) (to_hex a ^ ":"^acont);
        Printf.fprintf o "n_%s->n_%s [%s,label=\"%d\"];\n" (to_hex a) (to_hex bb) styles.(flag land 15) flag
       )
    );

  with_cursor
    txn alldb.dbi_tree
    (fun curs->
     curs_iter
       curs
       (fun a b->
        Printf.fprintf o "//tree: %S %S\n" (to_hex a) (to_hex b);flush stderr;
        (* let aa=String.sub a 0 (min inode_size (String.length a)) in *)
        (* let bb=String.sub b 0 (min inode_size (String.length b)) in *)
        (* Printf.fprintf o "f_%s[label=%S];\n" (to_hex bb) (String.sub a inode_size (max 0 (String.length a-inode_size))); *)
        (* Printf.fprintf o "f_%s->f_%s;\n" (to_hex aa) (to_hex bb) *)
       )
    );
  with_cursor
    txn alldb.dbi_revtree
    (fun curs->
     curs_iter
       curs
       (fun a b->
         Printf.fprintf o "//revtree: %S %S\n" (to_hex a) (to_hex b);flush stderr;
        (* Printf.fprintf stderr "%S %S\n" (to_hex a) (to_hex b);flush stderr; *)
        (* let aa=String.sub a 0 (min inode_size (String.length a)) in *)
        (* let bb=String.sub b 0 (min inode_size (String.length b)) in *)
        (* Printf.fprintf o "f_%s->f_%s[style=dashed];\n" (to_hex aa) (to_hex bb) *)
       )
    );

  with_cursor
    txn alldb.dbi_inodes
    (fun curs->
     curs_iter
       curs
       (fun a b->
        Printf.fprintf o "//inodes: %S %S\n" (to_hex a) (to_hex b);
        (* let aa=String.sub a 0 (min inode_size (String.length a)) in *)
        (* let bb=String.sub b 0 (min inode_size (String.length b)) in *)
        (* Printf.fprintf o "f_%s[label=%S];\n" (to_hex bb) (String.sub a inode_size (max 0 (String.length a-inode_size))); *)
        (* Printf.fprintf o "f_%s->f_%s;\n" (to_hex aa) (to_hex bb) *)
       )
    );
  with_cursor
    txn alldb.dbi_revinodes
    (fun curs->
     curs_iter
       curs
       (fun a b->
        Printf.fprintf o "//revinodes: %S %S\n" (to_hex a) (to_hex b);
        (* let aa=String.sub a 0 (min inode_size (String.length a)) in *)
        (* let bb=String.sub b 0 (min inode_size (String.length b)) in *)
        (* Printf.fprintf o "f_%s[label=%S];\n" (to_hex bb) (String.sub a inode_size (max 0 (String.length a-inode_size))); *)
        (* Printf.fprintf o "f_%s->f_%s;\n" (to_hex aa) (to_hex bb) *)
       )
    );
  Printf.fprintf o "}\n"
(*
let node_contents curs b=
  let k,_=Mdb.cursor_get curs (node_of_val b) "" Mdb._SET_RANGE in
  if streq k 0 b (1+hash_size) key_size <> 0 then raise Mdb.Notfound
  else String.sub k key_size (String.length k-key_size)
*)

let unsafe_output_repository ~working_copy do_output_files txn alldb=
  let cache=ref M.empty in
  let paths=ref M.empty in
  (* Walk down the graph, collecting files, and look them up using the revinodes base (possibly adding new files to that table). *)
  let rec retrieve_paths curs key path inode inode_path=
    if not (M.mem key !cache) then
      begin
        cache:=M.add key () !cache;
        folder_children_iter
          txn alldb key
          (fun b->
           try
             let bv=node_of_val b in
             let cont_b=try Mdb.get txn alldb.dbi_contents bv with Mdb.Notfound->"" in
             let filename=String.sub cont_b 2 (String.length cont_b-2) in
             let perms=((int_of_char cont_b.[0]) lsl 8) lor (int_of_char cont_b.[1]) in
             let path'=if path="" then filename else (path^"/"^filename) in
             folder_children_iter
               txn alldb bv
               (fun c->
                let cv=node_of_val c in
                let c_is_file=
                  try
                    neighbors_iter curs cv (fun _->raise Found) (0);
                    neighbors_iter curs cv (fun _->raise Found) ((pseudo_flag));
                    neighbors_iter curs cv (fun _->raise Found) ((deleted_flag));
                    neighbors_iter curs cv (fun _->raise Found) ((deleted_flag lor pseudo_flag));
                    false
                  with
                    Found->true
                in
                let inode'=try
                    Mdb.get txn alldb.dbi_revinodes cv
                  with
                    Mdb.Notfound->
                    begin
                      (* New file. *)
                      let inode'=with_cursor txn alldb.dbi_revtree create_new_inode in
                      Mdb.put txn alldb.dbi_inodes inode' ("\000"^String.sub cont_b 0 2^cv) 0;
                      Mdb.put txn alldb.dbi_revinodes cv inode' 0;
                      Mdb.put txn alldb.dbi_tree (inode^filename) inode' 0;
                      Mdb.put txn alldb.dbi_revtree inode' (inode^filename) 0;
                      inode'
                    end
                in
                paths:=M.add path' ((cont_b,cv,inode',perms,c_is_file)::(try M.find path' !paths with Not_found->[])) !paths;
                if not c_is_file then (
                  let basename=Mdb.get txn alldb.dbi_revtree inode' in
                  let basename=String.sub basename inode_size (String.length basename-inode_size) in
                  retrieve_paths curs cv path' inode' (Filename.concat inode_path basename);
                )
               );
           with Mdb.Notfound->()
          )
      end
  in
  Mdb.drop txn alldb.dbi_tree false;
  with_cursor
    txn alldb.dbi_nodes
    (fun curs->
     retrieve_paths curs root_key "" root_inode ""
    );
  (* Then output the files. The main challenge here, is that maybe a succession of patches moved several elements of the path to a single file, i.e. x/a became y/b, so we need to take care of this when moving existing files. *)
  M.iter
    (fun k a->
     (* k is now a path, and a is a list of nodes that are supposed to be at that path (of course more than one file in a is a conflict. *)
     let output_file path file=
       let o=open_out path in
       try
         let _=output_file txn alldb o file in
         close_out o
       with e->(close_out o;raise e)
     in
     let output basename key inode perms is_file suffix=
       let rec path_for_inode inode=
         try
           let next=Mdb.get txn alldb.dbi_revtree inode in
           let inode'=String.sub next 0 inode_size in
           let basename=String.sub next inode_size (String.length next-inode_size) in
           Filename.concat (path_for_inode inode') basename
         with
           Mdb.Notfound->""
       in
       let fi=path_for_inode inode in
       let former=fc working_copy fi in
       let newer=(fc working_copy k)^suffix in

       let parent=Mdb.get txn alldb.dbi_revtree inode in
       let parent=String.sub parent 0 inode_size in
       let b=String.sub basename 2 (String.length basename-2) in
       Mdb.put txn alldb.dbi_tree (parent ^ b) inode 0;
       Mdb.put txn alldb.dbi_revtree inode (parent ^ b) 0;

       if is_file then
         begin
           let _=Sys.command ("mkdir -p "^(Filename.quote (Filename.dirname newer))) in
           let _=try Sys.rename former newer with Sys_error _->() in
           if do_output_files then (
             output_file newer (retrieve txn alldb key);
             Unix.chmod newer perms
           );
         end
       else
         begin
           (try Sys.rename former newer with Sys_error _->
              let _=Sys.command ("mkdir -p "^newer) in ());
           Unix.chmod newer perms
         end
     in
     match a with
       []->()
     | [basename,key,inode,perms,is_file]->output basename key inode perms is_file ""
     | _->
        let _=List.fold_left (fun i (basename,key,inode,perms,is_file)->output basename key inode perms is_file (Printf.sprintf "~%d" i);i+1) 0 a in
        ()
    ) !paths


let output_repository ~working_copy env txn alldb edges=
  unsafe_output_repository ~working_copy false txn alldb;
  let t=Mdb.txn_begin env (Some txn) 0 in
  try
    let patch={empty_patch with edges} in
    let patchid=String.make hash_size '\000' in
    unsafe_apply t alldb patch patchid;
    unsafe_output_repository ~working_copy true txn alldb;
    Mdb.txn_abort t;
  with
    e->(Mdb.txn_abort t;raise e)


let patches_topo patchespath patches=
  let rp=ref [] in
  let rec treat m h=
    if S.mem h m then (
      let deps=
        let o=open_in_bin (Filename.concat patchespath (to_hex h)) in
        let p=input_dependencies o in
        close_in o;
        p
      in
      let m'=List.fold_left treat (S.remove h m) deps in
      rp:=h :: !rp;
      m'
    ) else m
  in
  let rec iterate m=
    if not (S.is_empty m) then
      iterate (treat m (S.min_elt m))
  in
  iterate (List.fold_left (fun m h->S.add h m) S.empty patches);
  !rp


let compare_repositories source target=
  let applicable=ref [] in
  let remo=open_in_bin target in
  let loco=open_in_bin source in
  let rem=String.make (1+2*hash_size) '\000' in
  let loc=String.make (1+2*hash_size) '\000' in
  let fini=ref false in
  let _=
    try
      while true do
        let c=compare (String.sub rem 1 hash_size) (String.sub loc 1 hash_size) in
        if c>=0 || !fini then (
          if c>0 || !fini then applicable:=(String.copy loc) :: !applicable;
          really_input loco loc 0 (1+2*hash_size);
        );
        if c<=0 then
          (try really_input remo rem 0 (1+2*hash_size) with End_of_file->fini:=true)
      done
    with
      End_of_file->(close_in remo;close_in loco;)
    | e->(close_in remo;close_in loco;raise e)
  in
  !applicable

   
let apply_recursive ~pijuldir txn alldb patchid=
  let rec apply_recursive patchid=
    (* if not (Pijul.branch_has_patch txn alldb (Pijul.current_branch alldb) patchid) then *)
    begin
      Printf.printf "applying patch %s!\n%!" (to_hex patchid);
      let p=open_in (Filename.concat (patchesdir pijuldir) (to_hex patchid)) in
      let patch=try
          let patch=json_input_patch p in
          close_in p;
          patch
        with e->(close_in p;raise e)
      in
      List.iter (fun dep->apply_recursive dep) patch.dependencies;
      apply txn alldb patch patchid;
    end
  in
  apply_recursive patchid;
  write_changes ~pijuldir txn alldb
