(*
unrecord p: if q depends on p, selecting p must also select q.
push p: if p depends on q, selecting p must also select q.

This function takes care of all this:

"push mode" means include_deps=true, "unrecord mode" means include_deps=false.
 *)
open Pijul

let general_filter l include_deps key dep que=
  let str=String.create 1 in
  let last_asked=ref 1 in
  let rec filter actions past i sel unsel sel_deps unsel_deps=match actions with
      []->(
      if i- !last_asked > 1 then (
      );
      List.fold_left (fun l (u,v,_,_,_,_) -> if u then v::l else l) [] past
    )
    | h::s->
       let k=key h in
       let deps=dep h in
       let selected,direction=
         if if include_deps then Pijul.S.mem k sel_deps else
              List.exists (fun d->Pijul.S.mem d sel) deps
         then
           true,1
         else
           if if include_deps then List.exists (fun d->Pijul.S.mem d unsel) deps
              else Pijul.S.mem k unsel_deps
           then
             false,1
           else
             begin
               if i- !last_asked > 1 then (
                 Printf.printf "Skipping %d\n" (i- !last_asked-1);flush stdout;
               );
               que h i;
               last_asked:=i;
               flush stdout;
               str.[0]<-'\000';
               let _=Unix.read Unix.stdin str 0 1 in
               Printf.fprintf stdout "\n";
               match str with
                 "y"->true,1
               | "n"->false,1
               | "k"->false, -1
               | _->false,0
             end
       in
       if direction<0 then
         match past with
           []->filter actions past i sel unsel sel_deps unsel_deps
         | (_,u,a,b,c,d)::v->filter (u::actions) v (i-1) a b c d
       else if direction=0 then
         filter actions past i sel unsel sel_deps unsel_deps
       else
         let sel',unsel'=if selected then Pijul.S.add k sel,unsel else sel,Pijul.S.add k unsel
         and sel_deps',unsel_deps'=
           if selected then List.fold_left (fun m d->Pijul.S.add d m) sel_deps deps,unsel_deps
           else sel_deps,List.fold_left (fun m d->Pijul.S.add d m) unsel_deps deps
         in
         filter s ((selected,h,sel,unsel,sel_deps,unsel_deps)::past) (i+1) sel' unsel' sel_deps' unsel_deps'
  in
  let tcattr = if Unix.isatty Unix.stdin
	       then
		 let tcattr=Unix.tcgetattr Unix.stdin in
		 Unix.tcsetattr Unix.stdin Unix.TCSADRAIN { tcattr with Unix.c_icanon=false };
		 Some tcattr
	       else
		 None
  in
  let f=filter l [] 1 Pijul.S.empty Pijul.S.empty Pijul.S.empty Pijul.S.empty in
  let () = match tcattr with | Some tcattr -> Unix.tcsetattr Unix.stdin Unix.TCSADRAIN tcattr | None -> () in
  f


let filter_patches pijuldir push_mode action all_patches=
  let n=List.length all_patches in
  general_filter
    all_patches
    push_mode
    (fun h->h)
    (fun h->
     let o=open_in_bin (Filename.concat (Pijul.patchesdir pijuldir) (Pijul.to_hex h)) in
     let p=Pijul.input_dependencies o in
     close_in o;
     p)
    (fun h i->
     let p=
       let o=open_in_bin (Filename.concat (Pijul.patchesdir pijuldir) (Pijul.to_hex h)) in
       let p=Pijul.json_input_patch o in
       close_in o;
       p
     in
     Printf.fprintf stdout "%s %s\n  * %s\nShall I %s this patch? (%d/%d) [ynk]: " p.time p.author p.name action i n
    )


(* blanc fort: 39, gris 30, rouge 31, vert 32 *)
let filter_edges env action record_mode edges updates=
  let plusfile_esc="\027[1;32m" (* foreground: 39, 30-37, 90-97. Style: 1,2,4,5,7,8 *)
  and rmfile_esc="\027[1;31m"
  and movefile_esc="\027[1;33m"
  and plus_esc="\027[32m"
  and minus_esc="\027[31m"
  and reset_esc="\027[0m" in
  let n=List.length edges in
  (* Regrouper toutes les opérations de fichier relatives à une même clef, pour faire les mv/rm/add correctement. *)
  general_filter
    edges record_mode
    (fun h->
      match h with
        (_,h)::(l2,_)::s when Pijul.is_folder h.[0] && not (Pijul.is_deleted h.[0]) -> l2
      | _->""
    ) (* provides *)
    (fun h->
      match h with
        (_,h)::s when Pijul.is_folder h.[0] && not (Pijul.is_deleted h.[0]) &&
            String.length h=1+line_size->[String.sub h 1 line_size]
      | _->[]
    ) (* deps *)
    (fun (h:(string*string) list) i->
      Pijul.with_txn
        env None
        (fun txn->
          let repo=Pijul.open_repository txn in
          Pijul.with_cursor
            txn repo.dbi_nodes
            (fun curs->
              Pijul.with_cursor
                txn repo.dbi_revtree
                (fun revtree_cursor->
                  let print_hunk hunk=
                    List.iter
                      (fun (a,b)->
                        let acont=Pijul.edge_contents txn repo a b in
                        if Pijul.is_deleted b.[0] then (
                          Printf.fprintf stdout "%s- %s%s" minus_esc acont reset_esc;
                          if String.length acont>0 then if acont.[String.length acont-1]<>'\n' then Printf.fprintf stdout "\n"
                        ) else (
                          Printf.fprintf stdout "%s+ %s%s" plus_esc acont reset_esc;
                          if String.length acont>0 then if acont.[String.length acont-1]<>'\n' then Printf.fprintf stdout "\n"
                        )
                      ) hunk;
                  in
                  match h with
                    (kname,h)::(l2,h2)::s when Pijul.is_folder h.[0] && not (Pijul.is_deleted h.[0]) ->
                      begin
                        let name=
                          if Pijul.is_new h.[0] then
                            String.sub kname (Pijul.line_size) (String.length kname-Pijul.line_size)
                          else
                            String.sub kname (Pijul.key_size) (String.length kname-Pijul.key_size)
                        in
                        let inode_l2=
                          if Pijul.is_new h2.[0] then
                            M.find l2 updates
                          else
                            Mdb.get txn repo.dbi_revinodes (String.sub l2 0 Pijul.key_size)
                        in
                        let permissions=String.sub name 0 2 in
                        let path=Pijul.path_of_inode revtree_cursor inode_l2 in
                        if Pijul.is_new h2.[0] then (
                          Printf.fprintf stdout "%sadd file%s %s\n" plusfile_esc reset_esc (String.concat "/" path);
                          print_hunk s;
                        ) else (
                          Printf.fprintf stdout "%smove file%s " movefile_esc reset_esc;
                          match s with
                            _::s->
                              begin
                                let finish names l=
                                  let _=
                                    match names with
                                      [_]->
                                        Printf.fprintf stdout "%s %sto%s %s\n"
                                          (String.concat "," names)
                                          movefile_esc reset_esc
                                          (String.concat "/" path);
                                    | _->
                                       Printf.fprintf stdout "%s %sto%s %s\n"
                                         (String.concat "," names)
                                         movefile_esc reset_esc
                                         (String.concat "/" path);
                                  in
                                  print_hunk l
                                in
                                let rec print_folder_edges (names:string list) l=match l with
                                    []->finish names l
                                  | (_,b)::_ when not (Pijul.is_folder b.[0])->finish names l
                                  | (a,b)::s->
                                     begin
                                       let paths=path_of_folder_node txn repo (String.sub b 1 key_size) in
                                       let paths=List.map (fun p->
                                         String.concat "/" (List.map (fun pp->String.sub pp 2 (String.length pp-2)) p)
                                       ) paths
                                       in
                                       print_folder_edges (paths@names) s
                                     end
                                in
                                print_folder_edges [] s
                              end
                          | _->()
                        )
                      end
                  | (kname,h)::(l2,_)::s when Pijul.is_folder h.[0] && Pijul.is_deleted h.[0]->
                     begin

                       let name=
                         String.sub kname (Pijul.key_size) (String.length kname-Pijul.key_size)
                       in
                       (* Printf.eprintf "kname=%S %S\n" (to_hex kname) (to_hex l2);flush stderr; *)
                       let inode_l2=
                         Mdb.get txn repo.dbi_revinodes (String.sub l2 0 Pijul.key_size)
                       in
                       let permissions=String.sub name 0 2 in
                       let path=Pijul.with_cursor
                         txn repo.dbi_revtree
                         (fun cursor->Pijul.path_of_inode cursor inode_l2)
                       in
                       Printf.fprintf stdout "%sremove file%s %s\n" rmfile_esc reset_esc (String.concat "/" path);
                       print_hunk s;
                     end
                  | s->print_hunk s
                )
            );
          Printf.fprintf stdout "%s (%d/%d) [ynk]: " action i n;flush stdout
        )
    )




let ask_author pijul_path=
  try
    let i=open_in (Filename.concat pijul_path "author") in
    let l=input_line i in
    close_in i;
    if l="" then raise (Sys_error "") else l
  with
    Sys_error _ ->
    begin
      let rec ask_name ()=
        Printf.fprintf stdout "What is your name (example: Marcel Dupont <marcel.dupont@example.org>)? ";
        flush stdout;
        let auth=input_line stdin in
        if auth<>"" then (
          let i=open_out (Filename.concat pijul_path "author") in
          Printf.fprintf i "%s\n" auth;
          close_out i;
          auth
        ) else
          ask_name ()
      in
      ask_name ()
    end


let ask_name ()=
  let rec ask_name ()=
    Printf.fprintf stdout "What is the patch name? ";
    flush stdout;
    let name=input_line stdin in
    if name<>"" then (
      Printf.fprintf stdout "Do you want to add a long comment [yN]?";
      flush stdout;

      let tcattr=Unix.tcgetattr Unix.stdin in
      let _=Unix.tcsetattr Unix.stdin Unix.TCSADRAIN
                           { tcattr with Unix.c_icanon=false }
      in
      let answer=
        let str="n" in
        let _=Unix.read Unix.stdin str 0 1 in
        str
      in
      let _=Unix.tcsetattr Unix.stdin Unix.TCSADRAIN tcattr in
      if answer<>"\n" then Printf.fprintf stdout "\n";
      if answer="n" || answer="N" || answer="\n" then name else (
        flush stdout;
        Printf.fprintf stderr "long comments not implemented\n";
        flush stderr;
        name
      )
    ) else
      ask_name ()
  in
  ask_name ()

exception Nothing_to_record

let now () =
  let i=Unix.open_process_in "date -In" in
  let d=input_line i in
  let _=Unix.close_process_in i in
  d

let polite_record ?(author="") ?(name="") action direction ~ask pijul_path edges updates=
  let edges=
    if ask then
      Pijul.with_env
        (Pijul.pristinedir pijul_path)
        (fun env->
         filter_edges env action direction edges updates
        )
    else
      edges
  in
  match edges with
    []-> raise Nothing_to_record
  | _->
     begin
       let time= now () in
       let author=if author="" then ask_author pijul_path else author in
       let name=if name="" then ask_name () else name in
       let patch={ edges;dependencies=S.elements (dependencies edges); name; author; time } in
       patch
     end
