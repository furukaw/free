(* ファイル名を受け取って、ファイル名を出力して、入力チャンネルを返す *)
(* string -> in_channel *)
(* ファイルが開けなければ実行を終了する *)
let openfile filename =
  try
    let channel = open_in filename in
    print_string "opened: ";
    print_endline filename;
    channel
  with
    Sys_error error_detail -> failwith "No such file"

(* 問題のファイル名を受け取って、問題を解析して、解く過程と答えを出力する *)
(* main : string -> unit *)
let main filename =
  ignore (openfile filename)

let _ =
  print_endline "started";
  main Sys.argv.(1)
