open Raylib
open Random

let () = print_endline "Rentrer la taille de la grille:"
let l = read_int ()
let () = print_endline "Rentrer le nombre de couleurs (max 8):"
let c = read_int ()
let t = [|Color.blue; Color.yellow; Color.orange; Color.green; Color.red; Color.green; Color.brown; Color.pink|]
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
      g.(i).(j) <- int (c - 1)
    done;
  done

let grid () =
  for i = 0 to l - 1 do
    for j = 0 to l - 1 do
        draw_rectangle (((500-l*16)/2) + j*16) (((500-l*16)/2) + i*16) 16 16 t.(g.(i).(j))
    done;
  done

(*
let color_click () =
  if is_mouse_button_pressed Left then g.((get_mouse_x () - 100) / 16).((get_mouse_y () - 100) / 16)
  else -1*)

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
