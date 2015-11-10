let chocolat=
  Array.of_list(
    Str.split (Str.regexp "[, :;\\.]")
      "Le mot chocolat vient, croit-on, de deux mots de la langue mexicaine : choco, son ou bruit et atle, eau, parce que le peuple mexicain le bat dans l'eau pour le faire mousser. Les dames du nouveau monde aiment, paraît-il, le chocolat à la folie et en font un usage considérable. On rapporte que, non contentes d'en prendre chez elles à tout moment de la journée, elles s'en font quelquefois apporter à l'église, sensualité qui leur a souvent attiré la censure et les reproches de leurs confesseurs, qui ont cependant fini par en prendre leur parti, y trouvant leur intérêt d'ailleurs, car ces dames leur faisaient la gracieuseté de leur en offrir de temps en temps une tasse, ce qu'ils se gardaient bien de refuser. Enfin, le révérend père Escobar, dont la métaphysique était aussi subtile que sa morale accommodante, déclara formellement que le chocolat à l'eau ne rompait aucunement le jeune, proclamant ainsi en faveur de ses belles pénitentes l'ancien adage : Liquidum non frangit jejunium"
  )

let pijul=Sys.getenv "pijul"
let git=if Sys.command "git --version" <> 127 then "git" else ""
let darcs=if Sys.command "darcs --version" <> 127 then "darcs" else ""

(* 1. long history 2. large source tree and 3. large single commits. *)
let buf=Buffer.create 1000
let make_line n=
  Buffer.clear buf;
  let wc=n/2+Random.int (n/2) in
  for i=0 to wc-2 do
    Buffer.add_string buf (chocolat.(Random.int (Array.length chocolat)));
    Buffer.add_char buf ' '
  done;
  Buffer.add_string buf (chocolat.(Random.int (Array.length chocolat)));
  Buffer.add_char buf '\n';
  Buffer.contents buf

let add_proba=10
let del_proba=7
let max=100
let rec update_file l f=match l with
    []->
      let r=Random.int max in
      if r<add_proba then
        make_line 100::List.rev f
      else
        List.rev f
  | h::s->
     let r=Random.int max in
     if r<add_proba then
       update_file l (make_line 100::f)
     else if r>=max-del_proba then
       update_file s f
     else
       update_file s (h::f)

let q=Filename.quote

let test_with record_file pijul command=
  let file=ref [make_line 10] in
  let root=Filename.temp_file "pijul-test" "" in
  let alice_pijul=Filename.concat root "alice" in
  let bob_pijul=Filename.concat root "bob" in
  let _=Sys.command (Printf.sprintf "rm -Rf %s" (q root)) in

  let filename="file" in

  Unix.mkdir root 0o755;
  Unix.mkdir alice_pijul 0o755;
  let _=
    Sys.command
      (Printf.sprintf "cd %s;%s init;touch %s;%s add %s" (q alice_pijul) pijul (q filename) pijul (q filename))
  in
  let root=Filename.dirname alice_pijul in
  let _=Sys.command (Printf.sprintf "cd %s;%s get %s %s" (q root) pijul (q alice_pijul) (q bob_pijul)) in
  Printf.fprintf stderr "%S %S\n" alice_pijul bob_pijul;
  let log=open_out record_file in
  Sys.catch_break true;
  try
    for i=0 to 280 do
      let o=open_out (Filename.concat alice_pijul filename) in
      List.iter (fun line->output_string o line) !file;
      close_out o;
      Printf.fprintf stderr "%d updated\n" i;flush stderr;
      let t0=Unix.gettimeofday () in
      let ret=Sys.command (Printf.sprintf "cd %s;%s %s" (q alice_pijul) pijul command) in
      let t1=Unix.gettimeofday () in
      if ret=255 then failwith "interrupted";
      Printf.fprintf log "%d %d %g\n" i (List.length !file) (t1-.t0);flush log;
      (* let _=Sys.command (Printf.sprintf "cd %s;%s debug" (q alice_pijul) pijul) in *)
      (* let _=Sys.command (Printf.sprintf "mv %s %s" (q @@ Filename.concat alice_pijul "ocamlprof.dump") *)
      (*                      (Printf.sprintf "/home/pe/Recherche/pijul/pijul/")) in *)
      file:=update_file !file [];
    done;
    close_out log
  with
    _->(
      let _=Sys.command (Printf.sprintf "rm -Rf %s" (q root)) in ()
    )

let _=
  (* let _=Random.init 7102015 in test_with "darcs_times" "darcs record --name \"name\""; *)
  let _=Random.init 7102015 in test_with "pijul_times" pijul "record -a --author \"author\" --name \"name\"";
  (* let _=Random.init 7102015 in test_with "git_times" "git"  "commit -a -m \"test\" --author \"author <author>\""; *)
  (* let _=Random.init 7102015 in test_with "hg_times" "hg"  "commit -m \"test\" -u \"author\""; *)
  ()
