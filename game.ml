open Raylib
open Random

let l = read_int ()
let t = [|Color.blue; Color.yellow; Color.orange; Color.green; Color.red; Color.green|]

let init_array l =
  let g = Array.make l [|0|] in
  for i=0 to l-1 do
    g.(i)<-Array.make l 0
  done;
  g
;;

let g = init_array l

let colors () =
  for i = 0 to l - 1 do
    for j = 0 to l - 1 do
      self_init ();
      g.(i).(j) <- int 5
    done;
  done

let grid () =
  for i = 0 to l - 1 do
    for j = 0 to l - 1 do
        draw_rectangle (100 + j*16) (100 + i*16) 16 16 t.(g.(i).(j))
    done;
  done

let setup () =
  init_window 500 500 "Flood it";
  set_target_fps 60;
  colors ();
  begin_drawing ();
  grid ();
  end_drawing ()

let rec loop () =
  if window_should_close () then close_window ()
  else
    
    begin_drawing ();
    clear_background Color.raywhite;
    grid ();
    end_drawing ();
    loop ()

let () = setup () |> loop
