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
        draw_rectangle (i*16) (j*16) 16 16 a_c.(g.(i).(j))
    done;
  done

let color_click () =
  if is_mouse_button_pressed Left then g.(get_mouse_x () / 16).(get_mouse_y () / 16)
  else -1

let grid_coloring couleur =
  let base_color = g.(0).(0) in
  let rec aux x y =
    if g.(x).(y) = base_color then(
      g.(x).(y) <- couleur;
      if x = 0 && y = 0 then(aux 1 0; aux 0 1;)
      else if x = (l - 1) && y = (l - 1) then(aux (l - 2) (l - 1); aux (l - 1) (l - 2);)
      else if x = (l - 1) && y = 0 then(aux (l - 2) 0; aux (l - 1) l;)
      else if x = 0 && y = (l - 1) then(aux 1 (l - 1); aux 0 (l - 2);)
      else if y = (l - 1) then(aux (x - 1) (l - 1); aux (x + 1) (l - 1); aux x (l - 2);)
      else if x = 0 then(aux 1 y; aux 0 (y - 1); aux 0 (y + 1);)
      else if y = 0 then(aux x 1; aux (x - 1) 0; aux (x + 1) 0;)
      else if x = (l-1) then(aux (x-1) y; aux x (y-1); aux x (y+1);)
      else aux (x - 1) y; aux (x + 1) y; aux x (y - 1); aux x (y + 1))
    else ()
  in aux 0 0


let setup () =
  init_window (l*16) (l*16) "Flood it";
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
    if color_click () <> -1 then grid_coloring (color_click ());
    grid ();
    end_drawing ();
    loop ()

let () = setup () |> loop
