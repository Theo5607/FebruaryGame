open Raylib

let setup () =
  Raylib.init_window 500 500 "Flood it";
  Raylib.set_target_fps 60

let grid () =
  let t = [|Color.blue; Color.yellow; Color.orange; Color.green; Color.red; Color.green|] in
  for i = 0 to 14 do
    draw_rectangle 100 (8*i) 8 8 t.(2)
  done;

let rec loop () =
  if window_should_close () then close_window ()
  else
    begin_drawing ();
    clear_background Color.raywhite;
    grid ();
    end_drawing ();
    loop ()

let () = setup () |> loop