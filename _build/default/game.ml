open Raylib
open Random

let () = print_endline "Rentrer la taille de la grille:"
let l = read_int ()
let () = print_endline "Rentrer le nombre de couleurs (max 8):"
let c = read_int ()
let a_c = [|Color.blue; Color.yellow; Color.orange; Color.green; Color.red; Color.green; Color.brown; Color.pink|]

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
        draw_rectangle (((500-l*16)/2) + j*16) (((500-l*16)/2) + i*16) 16 16 a_c.(g.(i).(j))
    done;
  done

(*
let color_click () =
  if is_mouse_button_pressed Left then g.((get_mouse_x () - 100) / 16).((get_mouse_y () - 100) / 16)
  else -1*)

let grid_coloring couleur =
  let base_color = g.(0).(0) in
  let rec aux x y =
    if g.(x).(y) = base_color then
      g.(x).(y) <- couleur;
      match x, y with
      | 0, 0 -> aux 1 0; aux 0 1;
      | (l-1), (l-1) -> aux (l-2) (l-1); aux (l-1) (l-2);
      | (l-1), 0 -> aux (l-2) 0; aux (l-1) 1;
      | 0, (l-1) -> aux 1 (l-1); aux 0 (t-2);  
      | (l-1), y -> aux (t-1) (y-1); aux (t-1) (y+1); aux (t-2) y;
      | x, (l-1) -> aux (x-1) (t-1); aux (x+1) (t-1); aux x (t-2);
      | 0, y -> aux 1 y; aux 0 (y-1); aux 0 (y+1);
      | x,0 -> aux x 1; aux (x-1) 0; aux (x+1) 0;
      | x,y -> aux (x-1) y; aux (x+1) y; aux x (y-1); aux x (y+1)
    else ()
  in aux 0 0


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
