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
    "./fonts/0.png";
    "./fonts/1.png";
    "./fonts/2.png";
    "./fonts/3.png";
    "./fonts/4.png";
    "./fonts/5.png";
    "./fonts/6.png";
    "./fonts/7.png";
    "./fonts/8.png";
    "./fonts/9.png";
    "./fonts/a.png";
    "./fonts/b.png";
    "./fonts/c.png";
    "./fonts/d.png";
    "./fonts/e.png";
    "./fonts/f.png";
    "./fonts/g.png";
    "./fonts/h.png";
    "./fonts/i.png";
    "./fonts/j.png";
    "./fonts/k.png";
    "./fonts/l.png";
    "./fonts/m.png";
    "./fonts/n.png";
    "./fonts/o.png";
    "./fonts/p.png";
    "./fonts/q.png";
    "./fonts/r.png";
    "./fonts/s.png";
    "./fonts/t.png";
    "./fonts/u.png";
    "./fonts/v.png";
    "./fonts/w.png";
    "./fonts/x.png";
    "./fonts/y.png";
    "./fonts/z.png";
    "./fonts/>.png";
  ]

let w = 500

and h = 500

let x_length = 11

let y_length = 11

let health = 10

let width = float_of_int w /. float_of_int x_length

let height = float_of_int h /. float_of_int y_length

let square_height ~height = float_of_int h /. height
