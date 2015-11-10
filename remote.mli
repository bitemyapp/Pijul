type remote_repo =   { location : Pijul.repository_ref;
                       socket_name : string;
                       mutable logchan : Unix.file_descr option;
                       tmpdir : string;
                     }

val get_logchan : remote_repo -> Unix.file_descr


val open_remote_repo : repo:Pijul.repository_ref -> localtmp:string -> remote_repo

val open_url : url:string -> localtmp:string -> remote_repo

val waitpid: string -> int -> unit

val fetcher: remote_repo:remote_repo -> dir_to_dir:bool -> source:string -> dest:string -> int

val sender: remote_repo:remote_repo -> dir_to_dir:bool -> source:string -> dest:string -> int

val init : string -> remote_repo -> unit
