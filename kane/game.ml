open UniverseJs
open World
open Image
open Color

let debugging = true
let rate = 0.5
let time_limit = 1200

type yen_t = Y1 | Y5 | Y10 | Y50 | Y100 | Y500 | No

type cell_t = {
  pos : (int * int);
  yen : yen_t;
  falling : float option;
}

type world_t = {
  cells : cell_t list;
  score : int;
  time : int;
  debug : string option;
}

;;Random.init 0

let int_to_yen i =
  let mod6 = i mod 6 in
  if mod6 = 0 then Y1
  else if mod6 = 1 then Y5
  else if mod6 = 2 then Y10
  else if mod6 = 3 then Y50
  else if mod6 = 4 then Y100
  else Y500

let yen_exist yen = match yen with
  | No -> false
  | a -> true

let second = 1. /. rate

let lines = 8
let rows = 12
let top = 1
let bottom = 0

let length = 64	(* １マスの辺の長さ *)

let width	= rows * length
let height	= (lines + top + bottom) * length

let background_image = read_image "images/background.png"
let yen1_image = read_image "images/yen1.png"
let yen5_image = read_image "images/yen5.png"
let yen10_image = read_image "images/yen10.png"
let yen50_image = read_image "images/yen50.png"
let yen100_image = read_image "images/yen100.png"
let yen500_image = read_image "images/yen500.png"

let yen_to_image yen = match yen with
  | Y1 -> yen1_image
  | Y5 -> yen5_image
  | Y10 -> yen10_image
  | Y50 -> yen50_image
  | Y100 -> yen100_image
  | Y500 -> yen500_image
  | _ -> empty_scene 40. 40.

let rec make_initial_cells x y =
  if rows < x then []
  else if lines < y then make_initial_cells (x + 1) 1
  else {
    pos = (x, y);
    yen = int_to_yen (Random.int 6);
    falling = None;
  } :: make_initial_cells x (y + 1)

let initial_world = {
  cells = make_initial_cells 1 1;
  score = 0;
  time = 0;
  debug = Some "start";
}

let draw ({cells; score; time; debug} : world_t) : Image.t =
  let field = 
    let cells_with_yen = List.filter (fun {yen} -> yen_exist yen) cells in
    let poss =
      List.map
        (fun {pos = (x, y)} ->
           (float_of_int (length * (x - 1)),
            float_of_int (length * (top + y - 1))))
        cells_with_yen in
    let images = List.map (fun {yen} -> yen_to_image yen) cells_with_yen in
    place_images images poss background_image in
  let with_texts =
    let time_text = text (string_of_float (time /. second)) red in
    let score_text = text (string_of_int score) red in
    
  match debug with
  | None -> field
  | Some message ->
    place_image (text message blue) (0., 0.) field

(* ゲーム開始 *)
let _ =
  big_bang initial_world
	   ~name:"kane"
           ~width:width
	   ~height:height
	   ~to_draw:draw
	   (* ~on_key_press:key_draw 
	    * ~on_tick:move_on_tick *)
	   ~rate:rate			(* ゲームの動く速さ *)
