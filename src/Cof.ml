type ('r, 'v) t =
  | Cof of ('r, ('r, 'v) t) CofFun.t
  | Var of 'v

let var v = Var v
let cof c = Cof c

let eq x y = cof @@ CofFun.eq x y
let bot = cof CofFun.bot
let top = cof CofFun.top
