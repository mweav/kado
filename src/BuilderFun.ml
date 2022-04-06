(** Parameters of smart constructors *)
module type Param =
sig
  (** The interval algebra. *)
  type dim

  (** The type that embeds cofibrations *)
  type cof

  (** The point 0 in the interval algebra. *)
  val dim0 : dim

  (** The point 1 in the interval algebra. *)
  val dim1 : dim

  (** The embedding of cofibrations to [cof]. *)
  val cof : (dim, cof) CofFun.t -> cof

  (** Extract the embedded cofibration, if any. *)
  val uncof : cof -> (dim, cof) CofFun.t option
end

module type S =
sig
  (** The interval algebra. *)
  type dim

  (** The type that embeds cofibrations *)
  type cof

  (** Smarter version of {!val:CofFun.eq} that checks equality. *)
  val eq : dim -> dim -> cof

  (** Smarter version of {!val:CofFun.bot} that is actually the same. *)
  val bot : cof

  (** Smarter version of {!val:CofFun.top} that is actually the same. *)
  val top : cof

  (** Smarter version of {!val:CofFun.join} that simplifies cofibrations using syntactic criteria.
      For example, [join [meet []]] gives [cof (Meet [])].

      Note that the simplification is attempting to strike a balance between optimality and efficiency,
      and thus it will not perform all possible syntactic reduction. For the best result,
      use only smart constructors (instead of raw constructors) to build cofibrations.
  *)
  val join : cof list -> cof

  (** Smarter version of {!val:CofFun.meet} that simplifies cofibrations using syntactic criteria.
      See {!val:join}. *)
  val meet : cof list -> cof

  (** [boundary r] gives a cofibration equivalent to [join [eq r dim0; eq r dim1]] *)
  val boundary : dim -> cof
end

module Make (P : Param) : S with type dim = P.dim and type cof = P.cof =
struct
  open P

  type nonrec dim = dim
  type nonrec cof = cof

  let eq x y = cof @@
    if x = y then
      CofFun.top
    else if (x = dim0 && y = dim1) || (x = dim1 && y = dim0) then
      CofFun.bot
    else
      CofFun.eq x y

  let bot = cof CofFun.bot
  let top = cof CofFun.top

  let join phis =
    let is_syntactic_top c = match uncof c with Some (Meet []) -> true | _ -> false in
    if List.exists is_syntactic_top phis then
      top
    else
      let expose phi = match uncof phi with Some (Join phis) -> phis | _ -> [phi] in
      match List.concat_map expose phis with
      | [phi] -> phi
      | l -> cof @@ CofFun.join l

  let meet phis =
    let is_syntactic_bot c = match uncof c with Some (Join []) -> true | _ -> false in
    if List.exists is_syntactic_bot phis then
      bot
    else
      let expose phi = match uncof phi with Some (Meet phis) -> phis | _ -> [phi] in
      match List.concat_map expose phis with
      | [phi] -> phi
      | l -> cof @@ CofFun.meet l

  let boundary r = join [eq r dim0; eq r dim1]
end