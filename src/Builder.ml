module Endo =
struct
  module type Param =
  sig
    type dim
    type ddim
    type cof
    val dim0  : dim
    val dim1  : dim
    val ddim0 : ddim
    val ddim1 : ddim
    val equal_dim  : dim -> dim -> bool
    val equal_ddim : ddim -> ddim -> bool
    val cof : (dim, ddim, cof) Syntax.endo -> cof
    val uncof : cof -> (dim, ddim, cof) Syntax.endo option
  end

  module type S =
  sig
    type dim
    type ddim
    type cof
    val cof : (dim, ddim, cof) Syntax.endo -> cof
    val le  : dim -> dim -> cof
    val dle : ddim -> ddim -> cof
    val bot : cof
    val top : cof
    val join : cof list -> cof
    val meet : cof list -> cof
    val eq   : dim -> dim -> cof
    val deq  : ddim -> ddim -> cof
    val eq0  : dim -> cof
    val deq0 : ddim -> cof
    val eq1  : dim -> cof
    val deq1 : ddim -> cof
    val boundary  : dim -> cof
    val dboundary : ddim -> cof
    val forall  : dim * cof -> cof
    val dforall : ddim * cof -> cof
  end

  module Make (P : Param) : S with type dim = P.dim and type ddim = P.ddim and type cof = P.cof =
  struct
    include P

    let (=) = equal_dim
    let (==) = equal_ddim

    let le x y = cof @@
      if dim0 = x || x = y || y = dim1 then
        Syntax.Endo.top
      else if x = dim1 && y = dim0 then
        Syntax.Endo.bot
      else
        Syntax.Endo.le x y

    let dle x y = cof @@
      if ddim0 == x || x == y || y == ddim1 then
        Syntax.Endo.top
      else if x == ddim1 && y == ddim0 then
        Syntax.Endo.bot
      else
        Syntax.Endo.dle x y

    let bot = cof Syntax.Endo.bot
    let top = cof Syntax.Endo.top

    let join phis =
      let is_syntactic_top c = match uncof c with Some (Meet []) -> true | _ -> false in
      if List.exists is_syntactic_top phis then
        top
      else
        let expose phi = match uncof phi with Some (Join phis) -> phis | _ -> [phi] in
        match List.concat_map expose phis with
        | [phi] -> phi
        | l -> cof @@ Syntax.Endo.join l

    let meet phis =
      let is_syntactic_bot c = match uncof c with Some (Join []) -> true | _ -> false in
      if List.exists is_syntactic_bot phis then
        bot
      else
        let expose phi = match uncof phi with Some (Meet phis) -> phis | _ -> [phi] in
        match List.concat_map expose phis with
        | [phi] -> phi
        | l -> cof @@ Syntax.Endo.meet l

    let eq x y = meet [le x y; le y x]
    let deq x y = meet [dle x y; dle y x]

    let eq0 x = eq x dim0
    let deq0 x = deq x ddim0

    let eq1 x = eq x dim1
    let deq1 x = deq x ddim1

    let boundary r = join [eq0 r; eq1 r]
    let dboundary r = join [deq0 r; deq1 r]

    let forall (sym, cof) =
      let rec go cof =
        match uncof cof with
        | None -> cof
        | Some Le (x, y) ->
          begin
            match equal_dim x sym, equal_dim y sym with
            | true, true -> top
            | true, false -> if y = dim1 then top else bot
            | false, true -> if x = dim0 then top else bot
            | _ -> eq x y
          end
        | Some DLe (x, y) -> dle x y
        | Some Meet phis -> meet @@ List.map go phis
        | Some Join phis -> join @@ List.map go phis
      in
      go cof

    let dforall (sym, cof) =
      let rec go cof =
        match uncof cof with
        | None -> cof
        | Some Le (x, y) -> le x y
        | Some DLe (x, y) ->
          begin
            match equal_ddim x sym, equal_ddim y sym with
            | true, true -> top
            | true, false -> if y == ddim1 then top else bot
            | false, true -> if x == ddim0 then top else bot
            | _ -> deq x y
          end
        | Some Meet phis -> meet @@ List.map go phis
        | Some Join phis -> join @@ List.map go phis
      in
      go cof
    end 
end

module Free =
struct
  module type Param =
  sig
    type dim
    type ddim
    type var
    val dim0 : dim
    val dim1 : dim
    val ddim0 : ddim
    val ddim1 : ddim
    val equal_dim : dim -> dim -> bool
    val equal_ddim : ddim -> ddim -> bool
  end

  module type S =
  sig
    type dim
    type ddim
    type var
    type cof = (dim, ddim, var) Syntax.free

    val var : var -> cof
    include Endo.S with type dim := dim and type ddim := ddim and type cof := cof
  end

  module Make (P : Param) : S with type dim = P.dim and type ddim = P.ddim and type var = P.var =
  struct
    open Syntax.Free

    let var = var
    module P = struct
      include P
      type cof = (dim, ddim, var) Syntax.free
      let cof phi = Cof phi
      let uncof phi = match phi with Cof phi -> Some phi | _ -> None
    end

    include P
    include Endo.Make(P)
  end
end
