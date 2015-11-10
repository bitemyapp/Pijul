let dir_perm=0o755

let get_pijul_dir dir=
  let rec get_it dir l=
    if Sys.file_exists (Pijul.pijuldir dir) then (
      dir,l
    ) else
      let d=Filename.dirname dir in
      let f=Filename.basename dir in
      if d=dir then "",[] else get_it d (f::l)
  in
  get_it dir []



let fc=Filename.concat
let fcc a=
  let rec fcc a r=match a with
      []->r
    | h::s->fcc s (Filename.concat r h)
  in
  match a with h::s->fcc s h | []->"/"


let make_absolute dir=
  if Filename.is_relative dir then
    begin
      let rec path d l=
        let d'=Filename.dirname d in
        if d=d' then d::l else
          let f=Filename.basename d in
          if f=Filename.parent_dir_name then
            path (Filename.dirname d') l
          else
            path d' (f::l)
      in
      let p=path (Filename.concat (Sys.getcwd ()) dir) [] in
      fcc p
    end
  else
    dir


let init dir=
  let dir=make_absolute dir in
  match fst (get_pijul_dir dir) with
  | "" ->
     begin
       try
	 let rec mkdir dir=
           if not (try Sys.is_directory dir with _->false) then (
             let dd=Filename.dirname dir in
             if dd<>dir then mkdir dd;
             Unix.mkdir dir dir_perm
           )
	 in
	 mkdir dir;
	 let pij=Pijul.pijuldir dir  in
	 Unix.mkdir pij dir_perm;
	 Unix.mkdir (Pijul.pristinedir pij) dir_perm;
	 Unix.mkdir (Pijul.patchesdir pij) dir_perm;
	 let o=open_out_bin (Filename.concat pij (Pijul.changesfile Pijul.default_branch)) in
	 close_out o;
       with
	 Unix.Unix_error (Unix.EEXIST,_,x)->
	 failwith ("Le dossier "^x^" existe déjà.")
     end
  | pd ->
    failwith ("Cannot create nested repository "^dir^" of "^pd)
