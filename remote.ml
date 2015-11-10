open Pijul

type remote_repo =
  { location : repository_ref;
    socket_name : string;
    mutable logchan : Unix.file_descr option;
    tmpdir : string;
  }

let get_logchan remote_repo=
  match remote_repo.logchan with
    Some c->c
  | None->
     begin
       let path=Filename.concat remote_repo.tmpdir "log" in
       let c=Unix.openfile path [Unix.O_WRONLY;Unix.O_APPEND;Unix.O_CREAT;Unix.O_TRUNC] 0o640 in
       remote_repo.logchan <- Some c;
       c
     end

let open_remote_repo ~repo ~localtmp =
  let socket_name = match repo.host with
  | Localhost -> ""
  | Ssh_host _ ->
     let local_mirror_file_name = Pijul.to_hex (url repo^".ssh") in
     (Filename.concat localtmp local_mirror_file_name)
  in
  let logchan = None in
  { location = repo; socket_name; tmpdir = localtmp; logchan }

let open_url ~url ~localtmp =
  open_remote_repo ~repo:(parse_url url) ~localtmp
       
let waitpid comment pid=
  let _,stat=Unix.waitpid [] pid in
  if stat<>Unix.WEXITED 0 then failwith comment

let ssh_control_options ~remote_repo =
  "-o ControlMaster=auto -o ControlPersist=600 -o ControlPath="^remote_repo.socket_name

let ensure_dir path=
  match path.[String.length path - 1] with
  | '/' -> path
  | _ -> path ^ "/"

let fetcher ~remote_repo ~dir_to_dir ~source ~dest =
  let source = if dir_to_dir then ensure_dir source else source in
  let dest = if dir_to_dir then ensure_dir dest else dest in
  Printf.printf "copying %s to %s\n%!" source dest;
  let rsync_command = match remote_repo.location.host with
    | Localhost -> [|"rsync";"-r";remote_repo.location.path^"/"^source;dest|]
    | Ssh_host user_host ->
       [|"rsync";"-r";"-e";
         "ssh "^ ssh_control_options ~remote_repo;
         user_host^":"^remote_repo.location.path^"/"^source;dest
        |]
  in
  Unix.create_process "rsync" rsync_command Unix.stdin Unix.stdout (get_logchan remote_repo)

                                                              
let sender ~remote_repo ~dir_to_dir ~source ~dest =
  let source = if dir_to_dir then ensure_dir source else source in
  let dest = if dir_to_dir then ensure_dir dest else dest in
  Printf.printf "sending %s to %s/%s \n%!" source (Pijul.url remote_repo.location) dest;
  let rsync_command = match remote_repo.location.host with
    | Localhost -> [|"rsync";"-r";source^"/";remote_repo.location.path^"/"^dest^"/"|]
    | Ssh_host user_host ->
       [|"rsync";"-r";"-e";
         "ssh "^ ssh_control_options ~remote_repo;
         source;user_host^":"^remote_repo.location.path^"/"^dest
        |]
  in
  Unix.create_process "rsync" rsync_command Unix.stdin Unix.stdout (get_logchan remote_repo)

let init dir remote_repo=
  let path = remote_repo.location.path in
  match remote_repo.location.host with

  | Ssh_host user_host ->
     let ssh_args =[| "ssh"; "-e"; ssh_control_options ~remote_repo; user_host;
		      "pijul init -path "^(Filename.quote path) |]
     in
     let pid = Unix.create_process "ssh" ssh_args Unix.stdin Unix.stdout (get_logchan remote_repo) in
     waitpid "remote init pid" pid
  | Localhost -> Repository.init path
