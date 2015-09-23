(** We hide the context by instanciating a functor: *)
module Z = ZZ3.Make (struct let ctx = Z3.mk_context [("unsat_core", "true")] end)


(** The result of the functor is safe for opening (contains only types and modules. *)
open Z


let () =

  (** We create a solver for future usage. *)
  let solver = Solver.make () in

  (** We create new SMT variables and specify their types. *)
  let x = Symbol.declare Real "x" in
  let y = Symbol.declare Real "y" in
  let z = Symbol.declare Int "z" in

  (** We can define SMT formulas using an OCaml-like syntax.
      [!] transforms a symbol into a term.
  *)
  let t = T.(!y <= int 3 && !x + !y <= rat Q.(5 // 2)) in

  (** We assert the formula in the SMT solver. *)
  Solver.add ~solver t;
  Solver.add_with_label ~solver (T.(!z <= int 0 && !z > int 0), "assert_z");

  (** We can now solve it and extract the result: *)
  let result = Solver.check ~solver [] in

  let model = match result with
    | Unsat _ -> failwith ("UNSAT: " ^ (Solver.get_unsat_core ~solver))
    | Unknown _ -> failwith "UNKNOWN"
    | Sat (lazy model) -> model
  in

(** Finally we easily get back the values in the model as inferred by Z3 without any casting! *)
  let vy = Model.get_value ~model y in
  let vx = Model.get_value ~model x in

  Printf.printf "y = %s \nx = %s\n" (Q.to_string vy) (Q.to_string vx)
