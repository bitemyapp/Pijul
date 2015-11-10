exception Nothing_to_record
exception Unknown_command of string
type repo_command =
   Record | Unrecord | Rollback | Revert | Add | Remove | Move | Pull | Put | Push | Log
  | Apply | Ls | Debug

type out_of_repo_command = Init | Get

type command = In_repo of repo_command
	     | Out_of_repo of out_of_repo_command

let parse_command = function
    "init" -> Out_of_repo Init
  | "record" -> In_repo Record
  | "unrecord" -> In_repo Unrecord
  | "rollback" -> In_repo Rollback
  | "revert" -> In_repo Revert
  | "add" -> In_repo Add
  | "remove" -> In_repo Remove
  | "mv"
  | "move" -> In_repo Move
  | "get" -> Out_of_repo Get
  | "pull" -> In_repo Pull
  | "put" -> In_repo Put
  | "push" -> In_repo Push
  | "log" | "changes" -> In_repo Log
  | "apply" -> In_repo Apply
  | "ls" -> In_repo Ls
  | "debug" -> In_repo Debug
  | c -> raise (Unknown_command c)

let now () =
  let i=Unix.open_process_in "date -In" in
  let d=input_line i in
  let _=Unix.close_process_in i in
  d

let conclude_patch pijul_path patch update_files=
  let patches_path=Pijul.patchesdir pijul_path in
  let patchid=Pijul.save_patch ~patches_path patch in
  Pijul.with_pijul
    pijul_path
    (fun _ txn->
     let repo=Pijul.open_repository txn in
     let _=Pijul.apply txn repo patch patchid in
     Pijul.sync_files txn repo patch patchid update_files;
     Pijul.write_changes ~pijuldir:pijul_path txn repo;
     ()
    )

let record pijul_path dir ?(author="") ?(name="") ~ask=
  Pijul.with_env
    (Pijul.pristinedir pijul_path)
    (fun env->
     let edges,update_files=
       Pijul.with_txn
         env None
         (fun txn->
          let repo = Pijul.open_repository txn in
          Pijul.record ~working_copy:dir txn repo
         )
     in
     let patch=Interaction.polite_record "record" true ~author ~name ~ask pijul_path edges update_files in
     conclude_patch pijul_path patch update_files
    )

exception Abort
let revert pijul_path dir ~ask=
  try
    Pijul.with_env
      (Pijul.pristinedir pijul_path)
      (fun env->
       let edges,update_files=
         Pijul.with_txn
           env None
           (fun txn->
            let repo = Pijul.open_repository txn in
            Pijul.record ~working_copy:dir txn repo
           )
       in
       let patch=
         if not ask then Pijul.empty_patch else
           try
             Interaction.polite_record "Maintain this change?" true
               pijul_path edges ~ask ~author:"Dummy McRevert" ~name:"reverted hunks" update_files
           with Interaction.Nothing_to_record->Pijul.empty_patch
       in
       let patchid=String.make Pijul.hash_size '\255' in
       Pijul.with_txn
         env None
         (fun txn->
          let repo=Pijul.open_repository txn in
          Pijul.unsafe_apply txn repo patch patchid;
          Pijul.unsafe_output_repository ~working_copy:dir true txn repo;
          raise Abort
         )
      )
  with
    Abort->()


let unrecord pijul_path =
  let patches_path=Pijul.patchesdir pijul_path in
  let patches=
    let p=ref [] in
    Pijul.with_pijul
      pijul_path
      (fun _ txn->
       let repo=Pijul.open_repository txn in
       Pijul.(branch_patches txn repo repo.current_branch (fun pp->p:= pp:: !p));
      );
    !p
  in
  let patches=Interaction.filter_patches pijul_path false "unrecord" patches in
  (* let patches=Pijul.patches_topo patches_path patches in *)
  Pijul.with_pijul
    pijul_path
    (fun _ txn->
     let repo=Pijul.open_repository txn in
     List.iter
       (fun patch_id->
        let o=open_in (Filename.concat patches_path (Pijul.to_hex patch_id)) in
	let p=Pijul.json_input_patch o in
	close_in o;
	Pijul.unsafe_unrecord txn repo p patch_id;
	Pijul.unrecord_sync txn repo patch_id;
       ) patches;
     Pijul.write_changes ~pijuldir:pijul_path txn repo;
    )


let rollback pijul_path ~ask ~author ~name=
  let patches_path=Pijul.patchesdir pijul_path in
  let patches=
    let p=ref [] in
    Pijul.with_pijul
      pijul_path
      (fun _ txn->
       let repo=Pijul.open_repository txn in
       Pijul.(branch_patches txn repo repo.current_branch (fun pp->p:= pp:: !p));
      );
    !p
  in
  let patches=Interaction.filter_patches pijul_path false "rollback" patches in
  (* let patches=Pijul.patches_topo patches_path patches in *)
  let rollback_edges=ref [] in
  Pijul.with_pijul
    pijul_path
    (fun _ txn->
     let repo=Pijul.open_repository txn in
     List.iter
       (fun patch_id->
        let o=open_in (Filename.concat patches_path (Pijul.to_hex patch_id)) in
        let p=Pijul.json_input_patch o in
        close_in o;
        rollback_edges:=Pijul.(rollback txn repo p.edges patch_id) @ !rollback_edges
       ) patches;
    );
  (* Rollback does not introduce new files, hence the M.empty here. *)
  let patch=Interaction.polite_record "rollback" false pijul_path !rollback_edges ~ask ~author ~name Pijul.M.empty in
  conclude_patch pijul_path patch Pijul.M.empty

type tracking_update = Additions | Removals

exception No_update_to_tracked_files

let update_tracked_files (operation: tracking_update) path pijul_path extra =
  match extra with
    []-> raise No_update_to_tracked_files
  | _->
     Pijul.with_pijul pijul_path
       (fun _ txn->
        let alldb=Pijul.open_repository txn in
        List.iter
          (fun file->
           if operation=Additions then
             if Sys.file_exists file then
               let file=Str.split (Str.regexp "/") file in
               Pijul.addfile txn alldb ((path@file))
             else
               failwith ("Fichier inexistant: "^file)
           else
             let file=Str.split (Str.regexp "/") file in
             Pijul.delfile txn alldb ((path@file))
          ) extra;
       )

let add path pijul_path extra = update_tracked_files Additions path pijul_path extra

let remove path pijul_path extra = update_tracked_files Removals path pijul_path extra

let move dir pijul_path extra =
  let fc=Filename.concat in
  let moves=
    match extra with
      _::_::h::s-> (* Au moins trois fichiers : tester si le dernier est un répertoire. *)
      let rec last l=match l with [h]->h | _::s->last s | []->assert false in
      let rec files l=match l with [_]->[] | h::s->h::files s | []->assert false in
      let b=last (h::s) in
      if try Sys.is_directory b with _->false then
        List.map (fun a->a,fc b (Filename.basename a)) (files extra)
      else
        failwith "move 1"
    | a::b::_->
       if try Sys.is_directory b with _->false then
         [a,fc b (Filename.basename a)]
       else
         [a,b]
    | _->failwith "move 2"
  in
  Pijul.with_pijul pijul_path
    (fun _ txn->
     let alldb=Pijul.open_repository txn in
     List.iter (fun (a,b)->
                let f=Str.split (Str.regexp "/") a
                and f'=Str.split (Str.regexp "/") b in
                Pijul.movefile txn alldb f f';
                Sys.rename (fc dir a) (fc dir b);
               ) moves
    )

let fetch_remote_changes ~remote_repo ~remote_branch ~dest=
  let source = Pijul._pijul ^ "/" ^ Pijul.changesfile remote_branch in
  Remote.fetcher ~remote_repo ~source ~dest ~dir_to_dir:false
(*
  let pid=
    Unix.create_process
      "rsync"
      (if host="" then [|"rsync";path^"/"^Pijul._pijul^"/"^Pijul.changesfile remote_branch;local_tmp|]
       else
         [|"rsync";
           "-e";"ssh -o ControlMaster=auto -o ControlPersist=600 -o ControlPath="^socket;
           path^"/"^Pijul._pijul^"/"^Pijul.changesfile remote_branch;local_tmp|])
      Unix.stdin Unix.stdout logchan
  in
  pid
 *)
let gen_pull ?(local_branch="main") ?(remote_branch="main") url target pijul_path ask =
  let localtmp = pijul_path in
  let remote_repo = Remote.open_url ~url ~localtmp in
  let remote_changes=Filename.concat pijul_path (Pijul.to_hex url^"."^Pijul.to_hex remote_branch) in
  let pid_changes= fetch_remote_changes ~remote_repo ~remote_branch ~dest:remote_changes in
  let pid_patches= Remote.fetcher ~remote_repo ~source:(Pijul._pijul^"/patches") ~dest:(target^"/"^Pijul._pijul^"/patches") ~dir_to_dir:true in
  (try
      Remote.waitpid "fetch_remote_changes" pid_changes;
      Remote.waitpid "pull patches" pid_patches;
    with _->());
  

  let applicable=Pijul.compare_repositories remote_changes (Filename.concat pijul_path (Pijul.changesfile local_branch)) in
  let applicable=
    List.map (fun p->String.sub p 1 (Pijul.hash_size))
    @@ List.sort (fun a b->Pijul.streq a (1+Pijul.hash_size) b (1+Pijul.hash_size) Pijul.hash_size) applicable
  in
  let applicable=if ask then Interaction.filter_patches pijul_path true "pull" applicable else applicable in

  Pijul.with_pijul
    pijul_path
    (fun env txn->
     let alldb=Pijul.open_repository txn in
     let records,_=Pijul.record ~working_copy:target txn alldb in
     List.iter (fun p->
                Pijul.apply_recursive ~pijuldir:(Pijul.pijuldir target) txn alldb p;
               ) applicable;
     if applicable<>[] then Pijul.output_repository ~working_copy:target env txn alldb records
    )

let pull dir pijul_path extra ask =
  let url=match extra with [url]->url | []->"" | _->failwith "trop de dépôts" in
  let Some url=Pijul.get_remote dir url in
  gen_pull url dir pijul_path ask


let gen_push ?(local_branch="main") ?(remote_branch="main") source url pijul_path ask=
  let remote_repo = Remote.open_url ~url ~localtmp:pijul_path in
  let remote_changes=Filename.concat pijul_path (Pijul.to_hex url^"."^Pijul.to_hex remote_branch) in
  let pid_changes=fetch_remote_changes ~remote_repo ~remote_branch ~dest:remote_changes in
  let pid_patches = Remote.fetcher ~remote_repo ~source:(Pijul._pijul^"/patches") ~dest:(source^"/"^Pijul._pijul^"/patches") ~dir_to_dir:true in
  (try
      Remote.waitpid "fetch_remote_changes" pid_changes;
      Remote.waitpid "pull patches" pid_patches;
    with _->());

  let applicable=Pijul.compare_repositories (Filename.concat pijul_path (Pijul.changesfile local_branch)) remote_changes in
  let applicable=
    List.map (fun p->String.sub p 1 (Pijul.hash_size))
    @@ List.sort (fun a b->Pijul.streq a (1+Pijul.hash_size) b (1+Pijul.hash_size) Pijul.hash_size) applicable
  in
  let applicable=if ask then Interaction.filter_patches pijul_path true "push" applicable else applicable in

  Printf.printf "got %d patches to apply\n%!" (List.length applicable);

  let pid_send_patches = Remote.sender ~remote_repo ~source:(Pijul.patchesdir pijul_path) ~dest:(Pijul._pijul^"/patches") ~dir_to_dir:true in
  Remote.waitpid "send patches" pid_send_patches;
  
  if remote_repo.Remote.location.Pijul.host=Pijul.Localhost then
    begin
      let path = remote_repo.Remote.location.Pijul.path in
      let targetpijul=Pijul.pijuldir path in
      Pijul.with_pijul
        targetpijul
        (fun env txn->
         let alldb=Pijul.open_repository txn in
         let records,_=Pijul.record ~working_copy:path txn alldb in
         List.iter (fun p->Pijul.apply_recursive ~pijuldir:targetpijul txn alldb p) applicable;
         if applicable<>[] then (
           Pijul.output_repository ~working_copy:path env txn alldb records
         )
        )
    end
  else
    begin
      let socket = remote_repo.Remote.socket_name in
      let Pijul.Ssh_host host = remote_repo.Remote.location.Pijul.host in
      let path = remote_repo.Remote.location.Pijul.path in
      let logchan = Remote.get_logchan remote_repo in
      let pid_apply=
        Unix.create_process
          "ssh"
          [|"ssh";
           "-e";"ssh -o ControlMaster=auto -o ControlPersist=600 -o ControlPath="^socket;
            host;"cd "^path^" && pijul apply "^String.concat " " (List.map Pijul.to_hex applicable);
           |]
          Unix.stdin Unix.stdout logchan
      in
      Remote.waitpid "remote apply" pid_apply;
    end

                   
let push dir pijul_path extra ask =
  let url=match extra with [url]->url | []->"" | _->failwith "trop de dépôts" in
  let Some url=Pijul.get_remote dir url in
  gen_push dir url pijul_path ask

let put url dir pijul_path =
  let remote_repo = Remote.open_url ~url ~localtmp:pijul_path in
  Remote.init dir remote_repo;
  gen_push dir url pijul_path false

let log dir pijul_path =
  Pijul.with_pijul
    pijul_path
    (fun _ txn->
     let repo=Pijul.open_repository txn in
     Pijul.branch_patches
       txn repo (Pijul.current_branch repo)
       (fun h->
	let o=open_in_bin (Filename.concat (Pijul.patchesdir pijul_path) (Pijul.to_hex h)) in
	let p=Pijul.json_input_patch o in
	close_in o;
	Printf.fprintf stdout "%s %s %s\n  * %s\n" p.time p.author (Pijul.to_hex h) p.name
       )
    )

let apply dir pijul_path extra =
  let patches_path=Pijul.patchesdir pijul_path in
  Pijul.with_pijul
    pijul_path
    (fun env txn->
     let alldb=Pijul.open_repository txn in
     let records,_=Pijul.record ~working_copy:dir txn alldb in
     List.iter (fun patchid->
                let o=open_in (Filename.concat patches_path patchid) in
                let p=Pijul.json_input_patch o in
                close_in o;
                Pijul.apply txn alldb p (Pijul.from_hex patchid)
               ) extra;
     Pijul.write_changes ~pijuldir:pijul_path txn alldb;
     Pijul.output_repository ~working_copy:dir env txn alldb records
    )

let ls dir pijul_path =
  Pijul.with_pijul
    pijul_path
    (fun _ txn->
     let rep=Pijul.open_repository txn in
     Pijul.tree_iter
       txn rep ""
       (fun path base->
        let p=Filename.concat path base in
        Printf.printf "%s\n" p;
        p
       )

    )
