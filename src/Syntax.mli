(** A family of polynomial endofunctors [('r, -) t] indexed in an interpretation of the interval algebra ['r].
    This is for multiple types (for example, abstract syntax) to {e embed} the langauge of cofibrations. *)
type ('r, 's, 'a) endo =
  | Le of 'r * 'r
  | DLe of 's * 's
  | Join of 'a list
  | Meet of 'a list

(** For each interval algebra ['r], we define the {e free monad} [('r, -) free] on the polynomial endofunctor [('r, -) endo]:
    each [('r, 'v) t] is the language of cofibrations over an interval algebra ['r], with indeterminates drawn from ['v]. *)
type ('r, 's, 'v) free =
  | Cof of ('r, 's, ('r, 's, 'v) free) endo
  | Var of 'v

(** Stupid constructors for {!type:endo}. *)
module Endo :
sig
  (** A family of polynomial endofunctors [('r, -) t] indexed in an interpretation of the interval algebra ['r].
      This is for multiple types (for example, abstract syntax) to {e embed} the langauge of cofibrations.

      @canonical Kado.Syntax.endo
  *)
  type ('r, 's, 'a) t = ('r, 's, 'a) endo =
    | Le of 'r * 'r
    | DLe of 's * 's
    | Join of 'a list
    | Meet of 'a list

  (** [le x y] is [Le (x, y)] *)
  val le : 'r -> 'r -> ('r, 's, 'a) t

  (** [dle x y] is [DLe (x, y)] *)
  val dle : 's -> 's -> ('r, 's, 'a) t

  (** [join phis] is [Join phis] *)
  val join : 'a list -> ('r, 's, 'a) t

  (** [meet phis] is [Meet phis] *)
  val meet : 'a list -> ('r, 's, 'a) t

  (** [bot] is [Join []] *)
  val bot : ('r, 's, 'a) t

  (** [top] is [Meet []] *)
  val top : ('r, 's, 'a) t

  (** [map] is the functoriality of the endo functor [('r, -) t]. *)
  val map : ('a -> 'b) -> ('r, 's, 'a) t -> ('r, 's, 'b) t

  (** Ugly printer. *)
  val dump :
    (Format.formatter -> 'r -> unit) ->
    (Format.formatter -> 's -> unit) ->
    (Format.formatter -> 'a -> unit) ->
    Format.formatter -> ('r, 's, 'a) t -> unit
end

(** Stupid constructors for {!type:free}. *)
module Free :
sig
  (** A family of polynomial endofunctors [('r, -) t] indexed in an interpretation of the interval algebra ['r].
      This is for multiple types (for example, abstract syntax) to {e embed} the langauge of cofibrations.

      @canonical Kado.Syntax.endo
  *)
  type nonrec ('r, 's, 'a) endo = ('r, 's, 'a) endo =
    | Le of 'r * 'r
    | DLe of 's * 's
    | Join of 'a list
    | Meet of 'a list

  (** For each interval algebra ['r], we define the {e free monad} [('r, -) t] on the polynomial endofunctor [('r, -) endo]:
      each [('r, 'v) t] is the language of cofibrations over an interval algebra ['r], with indeterminates drawn from ['v].

      @canonical Kado.Syntax.free
  *)
  type ('r, 's, 'v) t = ('r, 's, 'v) free =
    | Cof of ('r, 's, ('r, 's, 'v) t) endo
    | Var of 'v

  (** [var v] is [Var v] 
      why was this  'v -> ('a, 'v) t   ??? 
  *)
  val var : 'v -> ('r, 's, 'v) t

  (** [cof phi] is [Cof phi] *)
  val cof : ('r, 's, ('r, 's, 'v) t) endo -> ('r, 's, 'v) t

  (** [le x y] is [Le (x, y)] *)
  val le : 'r -> 'r -> ('r, 's, 'v) t

  (** [le x y] is [Le (x, y)] *)
  val dle : 's -> 's -> ('r, 's, 'v) t

  (** [join phis] is [Cof (Join phis)] *)
  val join : ('r, 's, 'v) t list -> ('r, 's, 'v) t

  (** [meet phis] is [Cof (Meet phis)] *)
  val meet : ('r, 's, 'v) t list -> ('r, 's, 'v) t

  (** [bot] is [Cof (Join [])] *)
  val bot : ('r, 's, 'v) t

  (** [top] is [Cof (Meet [])] *)
  val top : ('r, 's, 'v) t

  (** Ugly printer. *)
  val dump :
    (Format.formatter -> 'r -> unit) ->
    (Format.formatter -> 's -> unit) ->
    (Format.formatter -> 'v -> unit) ->
    Format.formatter -> ('r, 's, 'v) t -> unit
end
