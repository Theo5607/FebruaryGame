open Raylib
open Random

let () = print_endline "Rentrer la taille de la grille:"
let l = read_int ()
let () = print_endline "Rentrer le nombre de couleurs (max 8):"
let c = read_int ()
let a_c = [|Color.blue; Color.yellow; Color.orange; Color.beige; Color.red; Color.green; Color.lime; Color.pink|]

let init_array l =
  let g = Array.make l [|0|] in
  for i=0 to l-1 do
    g.(i)<-Array.make l 0
  done;
  g

let g = init_array l

let cpt = ref 0
let nombre_coups nb =
  match nb with
    | 0 -> 0
    | 1 -> 1
    | 2 -> 2
    | 3-> 5
    | 4 -> 9
    | 5 -> 11
    | 6 -> 14
    | 7 -> 16
    | 8 -> 18
    | 9 -> 20
    | 10 -> 23
    | 11 -> 26
    | 12 -> 28
    | 13 -> 31
    | 14 -> 33
    | 15 -> 35
    | 16 -> 37
    | 17 -> 39
    | 18 -> 42
    | 19 -> 47
    | 20 -> 52
    | _ -> 60

let nb_max = nombre_coups l

let gagner grille =
  let valeur = ref true in
  let couleur_base = grille.(0).(0) in
  for i = 0 to l-1 do 
    for j = 0 to l-1 do 
      valeur := grille.(i).(j) = couleur_base && (!valeur);
    done;
  done;
  if !valeur=true then 1
  else 0

let check cpt =
  cpt:=!cpt+1;
  if !cpt > nb_max && (gagner g) <> 1 then 0
  else if !cpt < nb_max && (gagner g) = 1 then 2
  else 1

let test=ref 0

let colors () =
  for i = 0 to l - 1 do
    for j = 0 to l - 1 do
      self_init ();
      g.(i).(j) <- int c
    done;
  done

let grid () =
  for i = 0 to l - 1 do
    for j = 0 to l - 1 do
        draw_rectangle (i*32) (j*32) 32 32 a_c.(g.(i).(j))
    done;
  done

let color_click () =
  if is_mouse_button_pressed Left && get_mouse_y () <= (32*l)+5 then g.(get_mouse_x () / 32).(get_mouse_y () / 32)
  else -1

let grid_coloring couleur =
  let base_color = g.(0).(0) in
  if base_color = couleur then ()
  else (let rec aux x y =
    if g.(x).(y) = base_color then(
      g.(x).(y) <- couleur;
      if x = 0 && y = 0 then(aux 1 0; aux 0 1;)
      else if x = (l - 1) && y = (l - 1) then(aux (l - 2) (l - 1); aux (l - 1) (l - 2);)
      else if x = (l - 1) && y = 0 then(aux (l - 2) 0; aux (l - 1) 1;)
      else if x = 0 && y = (l - 1) then(aux 1 (l - 1); aux 0 (l - 2);)
      else if y = (l - 1) then(aux (x - 1) y; aux (x + 1) y; aux x (y - 1);)
      else if x = (l - 1) then(aux x (y - 1); aux x (y + 1); aux (x - 1) y;)
      else if x = 0 then(aux 1 y; aux 0 (y - 1); aux 0 (y + 1);)
      else if y = 0 then(aux x 1; aux (x - 1) 0; aux (x + 1) 0;)
      else (aux (x - 1) y; aux (x + 1) y; aux x (y - 1); aux x (y + 1)))
    else ()
  in aux 0 0)

let setup () =
  init_window (l*32) ((l*32)+32) "Flood it";
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
    if !test=0 then draw_text (String.concat (string_of_int !cpt) ["";(String.concat (string_of_int nb_max) ["/";""])]) ((l-2)*16) (l*32+8) 24 Color.black;
    if !test=1 then draw_text "Perdu ratio" ((l-4)*16) (l*32+8) 24 Color.red;
    if !test=2 then draw_text "Victoire" ((l-3)*16) (l*32+8) 24 Color.lime;
    if color_click () <> -1 then (grid_coloring (color_click ()); let a = check cpt in
                                  if a = 0 then test:=1
                                  else if a = 2 then test:=2);
    end_drawing ();
    loop ()

let () = setup () |> loop
