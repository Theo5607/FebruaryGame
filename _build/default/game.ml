open Raylib
open Random

let t = [|Color.blue; Color.yellow; Color.orange; Color.green; Color.red; Color.green|]
let g = Array.make ((14*14) - 1) Color.white

let colors () =
  for i = 0 to (14*14) - 1 do
    self_init ();
    g.(i) <- t.(int 5)
  done

let grid () =
  for i = 0 to 14 do
    for j = 0 to 14 do
        draw_rectangle (100 + j*16) (100 + i*16) 16 16 g.(i*j)
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