(** Representation of a Symbolic Packet (SP). Hash-consing is used in this module to improve efficiency. *)

type sp =
  | Skip
  | Drop
  | Union of Field.t * (sp ref Value.M.t) * sp ref * int

type t = sp ref
(** The actual exposed type of SPs in the hash-consing scheme. *)

(** [of_pk p] is the symbolic packet representing the single concrete packet [p]. *)
val of_pk : Pk.t -> t

(** [get_hash sp] is the hash of the SP [sp] in the hash-consing scheme. *)
val get_hash : sp -> int

(** The comparator for SPs. *)
val compare : t -> t -> int

(** The equality relation for SPs. *)
val eq : t -> t -> bool

(** The less-than-or-equal-to relation for SPs. *)
val le : t -> t -> bool

(** The SPP [⊤]. *)
val skip : t

(** The SP [⊥]. *)
val drop : t

(** [mk f m d] constructs the SP corresponding to the field [f], non-default cases [m] and the default-case [d]. *)
val mk : Field.t * (t Value.M.t) * t -> t

(** [union_pair sp1 sp2] is the union of the SPs [sp1] and [sp2]. *)
val union_pair : t -> t -> t

(** [union [sp1;sp2;...;spn]] is the union of the SPs [sp1], [sp2], ..., and [spn]. *)
val union : t list -> t

(** [seq_pair sp1 sp2] is the concatenation cf the SPs [sp1] and [sp2] in that order. *)
val seq_pair : t -> t -> t

(** [seq [sp1;sp2;...;spn]] is the concatenation cf the SPs [sp1], [sp2], ..., and [spn] in that order. *)
val seq : t list -> t

(** [intersect_pair sp1 sp2] is the intersection of the SPs [sp1] and [sp2]. *)
val intersect_pair : t -> t -> t

(** [intersect [sp1;sp2;...;spn]] is the intersection of the SPs [sp1], [sp2], ..., and [spn]. *)
val intersect : t list -> t

(** [star sp] is the iteration (i.e. [*sp]) of the SP [sp]. *)
val star : t -> t

(** [diff sp1 sp2] is the difference (i.e. [sp1 \ sp2]) of [sp1] and [sp2]. *)
val diff : t -> t -> t

(** [neg sp1] is the negation of [sp1]. *)
val neg : t -> t

(** [xor sp1 sp2] is the symmetric difference [sp1 ⊕ sp2] between [sp1] and [sp2]. *)
val xor : t -> t -> t

(** [rep sp fields] is a packet in the SP [sp] with fields [fields]. *)
val rep : t -> Field.S.t -> Pk.t

(** [rep_over ?domains diversify sp fields] is like [rep], but returns one
    packet per live combination of [diversify] fields in [sp] (bounding
    the resulting list to the product of branching factors of exactly
    those fields), while still picking a single arbitrary representative
    for every other field, as [rep] does. Finds every live combination via
    a full traversal (see [restrict_over]), so a diversify field's value
    is never missed just because it sits behind a branch of some unrelated
    field. With [diversify] empty, this is [rep] wrapped in a singleton
    list. See [diversify_keys] for what [domains] does. *)
val rep_over : ?domains:Value.S.t Field.M.t -> Field.S.t -> t -> Field.S.t -> Pk.t list

(** [restrict_over ?domains diversify sp] partitions [sp] into one sub-SP
    per live combination of values for fields in [diversify], each being
    the exact restriction of [sp] to that combination -- fields outside
    [diversify] (and diversify fields [sp] never actually tests, unless
    covered by [domains]) are left fully symbolic in each result, not
    resolved to a representative value as [rep_over] does. See
    [diversify_keys] for what [domains] does. *)
val restrict_over : ?domains:Value.S.t Field.M.t -> Field.S.t -> t -> t list

(** [tag_origin ?domains shadow_of sp] returns [sp] with each shadow field
    [shadow_of f] additionally set to match [f]'s value, wherever [f] (a
    key of [shadow_of]) is live in [sp]. Fields of [shadow_of]'s domain
    never tested in [sp] are left alone, unless covered by [domains] (see
    [diversify_keys]). Intended for fields that stay unmodified for the
    rest of an exploration, so a later many-to-one rewrite of [f] doesn't
    erase which of [f]'s original values a given branch came from. *)
val tag_origin : ?domains:Value.S.t Field.M.t -> Field.t Field.M.t -> t -> t

(** [is_tested f sp] is [true] iff [sp] branches on [f] anywhere. *)
val is_tested : Field.t -> t -> bool

(** [diversify_keys ?domains diversify spref] finds every live combination
    of values for fields in [diversify] that [spref] actually tests
    somewhere, regardless of what other fields those tests happen to be
    nested beneath -- the shared traversal underlying [restrict_over],
    [rep_over], and [tag_origin]. Without [domains], a diversify field's
    non-drop default branch (matching every value not tested explicitly)
    is tagged with one arbitrary such value, and a field never branched on
    at all is simply absent from a returned [Pk.t] -- both lose every
    other value that region might represent, since [spref] alone gives no
    way to recover them. [domains] plugs that gap for whichever fields
    have an entry: every value in its declared domain not already
    explicit gets its own copy of the relevant continuation, instead of
    being collapsed into (or entirely missing) one representative. The
    caller is responsible for [domains] actually being trustworthy (e.g.
    via [collect_values] over a global reachable set, not [spref] itself)
    -- this has no way to check that itself. *)
val diversify_keys : ?domains:Value.S.t Field.M.t -> Field.S.t -> t -> Pk.t list

(** [collect_values targets spref] is, for each field in [targets], the set
    of values [spref] tests it against anywhere -- one combined traversal
    covering every field in [targets] at once, rather than one traversal
    per field. A field of [targets] that [spref] never tests is simply
    absent from the result (no entry, not an entry mapped to the empty
    set). Meant to build a [domains] argument for [diversify_keys] from a
    [spref] that's a full, global reachable set, not from a single state's
    own local output. *)
val collect_values : Field.S.t -> t -> Value.S.t Field.M.t

(*---------- Output ------------------ *)
(** [to_exp sp] is the NetKAT expression corresponding to the SP [sp]. *)
val to_exp : t -> Nk.t

(** [to_string sp] is a string representation of the NetKAT expression corresponding to the SP [sp]. *)
val to_string : t -> string

(** [dump ()] clears the pool of hash-consed SPs. *)
val dump: unit -> unit
