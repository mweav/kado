(** Smart constructors for {!type:Syntax.endo}. *)
module Endo :
sig

  (** Parameters of smart constructors. *)
  module type Param =
  sig
    (** The interval algebra. *)
    type dim
    type ddim

    (** The type that embeds cofibrations. *)
    type cof

    (** The element 0 in the interval algebra. *)
    val dim0 : dim

    (** The element 1 in the interval algebra. *)
    val dim1 : dim

    val ddim0 : ddim
    val ddim1 : ddim

    (** Equality checker for elements in the interval algebra. *)
    val equal_dim : dim -> dim -> bool
    val equal_ddim : ddim -> ddim -> bool

    (** The embedding of cofibrations into [cof]. *)
    val cof : (dim, ddim, cof) Syntax.endo -> cof

    (** Extract the embedded cofibration, if any. *)
    val uncof : cof -> (dim, ddim, cof) Syntax.endo option
  end

  (** The signature of smart constructors. *)
  module type S =
  sig
    (** The interval algebra. *)
    type dim
    type ddim

    (** The type that embeds cofibrations. *)
    type cof

    (** The embedding of cofibrations to [cof]. *)
    val cof : (dim, ddim, cof) Syntax.endo -> cof

    (** Smarter version of {!val:Syntax.Endo.le}. *)
    val le : dim -> dim -> cof
    val dle : ddim -> ddim -> cof

    (** The bottom cofibration. *)
    val bot : cof

    (** The top cofibration. *)
    val top : cof

    (** Smarter version of {!val:Syntax.Endo.join} that simplifies cofibrations using syntactic criteria.
        For example, [join [meet []]] gives [cof (Meet [])].

        Note that the simplification is attempting to strike a balance between optimality and efficiency,
        and thus it will not perform all possible syntactic reduction. To obtain more reduced cofibrations,
        use only smart constructors (instead of raw constructors) to build cofibrations.
    *)
    val join : cof list -> cof

    (** Smarter version of {!val:Syntax.Endo.meet} that simplifies cofibrations using syntactic criteria.
        See {!val:join}. *)
    val meet : cof list -> cof

    (** [eq] is equivalent to [meet [le x y; le y x]]. *)
    val eq : dim -> dim -> cof
    val deq : ddim -> ddim -> cof

    (** [eq0 r] is equivalent to [eq r dim0]. *)
    val eq0 : dim -> cof
    val deq0 : ddim -> cof

    (** [eq1 r] is equivalent to [eq r dim1]. *)
    val eq1 : dim -> cof
    val deq1 : ddim -> cof

    (** [boundary r] is equivalent to [join [eq0 r; eq1 r]]. *)
    val boundary : dim -> cof
    val dboundary : ddim -> cof

    (** [forall (r, cof)] computes [forall r. cof], using the syntactic quantifier elimination
        and potentially other simplification procedures used in {!val:le}, {!val:join}, and {!val:meet}.

        Note: [r] cannot be [dim0] or [dim1].
    *)
    val forall : dim * cof -> cof
    val dforall : ddim * cof -> cof
  end

  (** The implementation of smart constructors. *)
  module Make (P : Param) : S with type dim = P.dim and type ddim = P.ddim and type cof = P.cof
end

(** Smart constructors for {!type:Syntax.free}. *)
module Free :
sig

  (** Parameters of smart constructors. *)
  module type Param =
  sig
    (** The interval algebra. *)
    type dim
    type ddim

    (** The type of cofibration variables. *)
    type var

    (** The element 0 in the interval algebra. *)
    val dim0 : dim

    (** The element 1 in the interval algebra. *)
    val dim1 : dim


    val ddim0 : ddim
    val ddim1 : ddim

    (** Equality checker for elements of the interval algebra. *)
    val equal_dim : dim -> dim -> bool
    val equal_ddim : ddim -> ddim -> bool
  end

  (** The signature of smart constructors. *)
  module type S =
  sig
    (** The interval algebra. *)
    type dim
    type ddim

    (** The type of cofibration variables. *)
    type var

    (** The type of freely generated cofibrations. *)
    type cof = (dim, ddim, var) Syntax.free

    (** Alias of {!val:Syntax.Free.var}. *)
    val var : var -> cof

    (** @open *)
    include Endo.S with type dim := dim and type ddim := ddim and type cof := cof
  end

  (** The implementation of smart constructors. *)
  module Make (P : Param) : S with type dim = P.dim and type ddim = P.ddim and type var = P.var
end
