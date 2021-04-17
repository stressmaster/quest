let wall = "./wall.png"

and path = "./path.png"

and entrance = "./entrance.png"

and exit_tex = "./exit.png"

and player = "./player.png"

and darkness = "./darkness.png"

and monster = "./monster.png"

let texture_list =
  [
    wall;
    path;
    entrance;
    exit_tex;
    player;
    darkness;
    monster;
    "./font/0.png";
    "./font/1.png";
    "./font/2.png";
    "./font/3.png";
    "./font/4.png";
    "./font/5.png";
    "./font/6.png";
    "./font/7.png";
    "./font/8.png";
    "./font/9.png";
    "./font/a.png";
    "./font/b.png";
    "./font/c.png";
    "./font/d.png";
    "./font/e.png";
    "./font/f.png";
    "./font/g.png";
    "./font/h.png";
    "./font/i.png";
    "./font/j.png";
    "./font/k.png";
    "./font/l.png";
    "./font/m.png";
    "./font/n.png";
    "./font/o.png";
    "./font/p.png";
    "./font/q.png";
    "./font/r.png";
    "./font/s.png";
    "./font/t.png";
    "./font/u.png";
    "./font/v.png";
    "./font/w.png";
    "./font/x.png";
    "./font/y.png";
    "./font/z.png";
    "./font/>.png";
  ]

let w = 500

and h = 500

let x_length = 11

let y_length = 11

let width = float_of_int w /. float_of_int x_length

let height = float_of_int h /. float_of_int y_length

let square_height ~height = float_of_int h /. height
