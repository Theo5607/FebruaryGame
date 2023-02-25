type point = {x:int; y:int};
type case = {pos:point; couleur:int};
type grid = {t_x:int ; t_y:int; points:case array};

let init_array x y : case array =
  let tab = Array.make (x*y) {pos={x=0;y=0};couleur=0} in
  let compt= ref 0 in
  let stadex = ref 0 in
  for i=0 to (x*y)-1 do
    tab.(i)<-{pos={x=(!stadex);y=(!compt)};couleur=0};
    compt:=!compt+1;
    if !compt>(y-1) then (stadex:=!stadex+1; compt:=0);
    done;
  tab
;;
