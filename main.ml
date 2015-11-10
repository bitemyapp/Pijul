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

open Pijul

let fc = Filename.concat

let dir_perm=0o755

let logchan=ref None
let get_logchan dir=
  match !logchan with
    Some c->c
  | None->
     begin
       let path=Filename.concat (pijuldir dir) "log" in
       let c=Unix.openfile path [Unix.O_WRONLY;Unix.O_APPEND;Unix.O_CREAT;Unix.O_TRUNC] 0o640 in
       logchan:=Some c;
       c
     end

let _=Random.init 2015
let debug dir=
  Pijul.with_pijul
  (pijuldir dir)
  (fun _ txn->
   let alldb=Pijul.open_repository txn in
   let o=open_out "debug" in
   Pijul.debug_repository o txn alldb;
   close_out o)

let main (command_str : string)  (extra : string list) (init_dir : string) ~author ~name ~ask=
  match Commands.parse_command command_str with
  | Commands.Out_of_repo command ->
     begin
       match command with
       | Commands.Init ->
	  begin
            let path=if init_dir="" then Sys.getcwd () else init_dir in
            Repository.init path
	  end
       | Commands.Get ->
	  begin
            match extra with
              url::target->
              begin
                let target=match target with
                    []->
                    (try let i=String.rindex url '/' in String.sub url (i+1) (String.length url-i-1)
                  with Not_found->
                       try let i=String.rindex url ':' in String.sub url (i+1) (String.length url-i-1)
                       with Not_found->failwith "get: mauvaise url")
               | h::_->h
             in
             Unix.mkdir target dir_perm;
             Repository.init target;
             let o=open_out (fc (pijuldir target) Pijul.default_remote_file) in
             output_string o url;
             close_out o;
             (* Printf.fprintf stderr "init done\n";flush stderr; *)
             Commands.gen_pull url target (Pijul.pijuldir target) false;
           end
         | []->failwith "get: pas de source"
       end
     end
  | Commands.In_repo command ->
       let dir, pathRelToRepo = Repository.get_pijul_dir (Sys.getcwd ()) in
       let pijul_path = pijuldir dir in
     let _=
       match command with
       | Commands.Record ->
	  begin
	    try
	      Commands.record pijul_path dir ~author ~name ~ask;
	    with
	    | Interaction.Nothing_to_record -> failwith "Nothing to record"
	  end
       | Commands.Unrecord -> Commands.unrecord pijul_path
       | Commands.Rollback -> Commands.rollback pijul_path ~ask ~author ~name
       | Commands.Revert -> Commands.revert pijul_path dir ~ask
       | Commands.Add -> Commands.add pathRelToRepo pijul_path extra
       | Commands.Remove -> Commands.remove pathRelToRepo pijul_path extra
       | Commands.Move -> Commands.move dir pijul_path extra
       | Commands.Pull -> Commands.pull dir pijul_path extra ask
       | Commands.Push -> Commands.push dir pijul_path extra ask
       | Commands.Put ->
	  begin
	    match extra with
	      [url] ->  Commands.put url dir pijul_path
            | []->failwith "put: pas assez de dépôts"
            | _::_->failwith "put: trop de dépôts"
	  end
       | Commands.Log -> Commands.log dir pijul_path
       | Commands.Apply -> Commands.apply dir pijul_path extra
       | Commands.Ls -> Commands.ls dir pijul_path
       | _ ->

    match command_str with
    | "debug"->
       let dir,_=Repository.get_pijul_dir (Sys.getcwd()) in
       debug dir
    | cmd->
       begin
         failwith ("commande inconnue: "^cmd)
       end;

       in
     (let o=open_out (Filename.concat pijul_path "gc.log") in
      Gc.print_stat o;
      close_out o)

let _=
  Sys.catch_break true;
  let spec=ref [] in
  let command=ref "" in
  let extra=ref [] in
  let init_dir=ref "" in
  let do_ask=ref true in
  let author=ref "" and name=ref "" in
  Arg.parse_dynamic
    spec
    (fun anon->
     if !command="" then
       command:=anon
     else (
       extra:= anon :: !extra
     );
     match !command with
       "init"->spec:=[("-path",Arg.String (fun x->init_dir:=x),"Chemin à initialiser");
                      ("-help",Arg.Unit (fun ()->raise (Arg.Help "")), "Aide")
                     ]
     | "pull" | "push" -> spec:=[("-a",Arg.Unit (fun ()->do_ask:=false), "Ne pas demander")]
     | "record" ->
        spec:=[("-a",Arg.Unit (fun ()->do_ask:=false), "Don't ask");
               ("--author",Arg.String (fun a->author:=a), "Author");
               ("--name",Arg.String (fun a->name:=a), "Name");
              ]
     | "rollback" ->
        spec:=[("--author",Arg.String (fun a->author:=a), "Author");
               ("--name",Arg.String (fun a->name:=a), "Name");
              ]
     | _-> spec:=[("-a",Arg.Unit (fun ()->do_ask:=false), "Don't ask")]
    )
    "usage";
  let extra=List.rev !extra in
  try
    main !command extra !init_dir ~author:(!author) ~name:(!name) ~ask:(!do_ask);
  with
    e->
      begin
        let _=match !logchan with None->() | Some c->Unix.close c in
        match e with
          Sys.Break -> exit 130
        | _->raise e
      end
