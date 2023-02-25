type point = {x:int; y;int};
type case = {pos:point; couleur:int};
type grid = {t_x:int ; t_y:int; points:point list};

let init_grid x y liste = {t_x=x;t_y=y;cases=liste}
