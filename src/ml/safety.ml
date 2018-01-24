open Globals
open EvalValue
open EvalContext
open EvalEncode
open Common
open MacroApi
open Ast
open Type

type safety_message = {
	sm_msg : string;
	sm_pos : pos;
}

type safety_report = {
	mutable sr_errors : safety_message list;
	mutable sr_warnings : safety_message list;
}

type scope_type =
	| STNormal
	| STLoop
	| STClosure

type safety_unify_error =
	| NullSafetyError

exception Safety_error of safety_unify_error

(**
	Terminates compiler process and prints user-friendly instructions about filing an issue in compiler repo.
*)
let fail ?msg hxpos mlpos =
	let msg =
		(Lexer.get_error_pos (Printf.sprintf "%s:%d:") hxpos) ^ ": "
		^ "Haxe-safety: " ^ (match msg with Some msg -> msg | _ -> "unexpected expression.") ^ "\n"
		^ "Submit an issue to https://github.com/RealyUniqueName/Safety/issues with expression example and following information:"
	in
	match mlpos with
		| (file, line, _, _) ->
			Printf.eprintf "%s\n" msg;
			Printf.eprintf "%s:%d\n" file line;
			assert false

(**
	Returns human-readable string representation of specified type
*)
let str_type t = s_type (print_context()) t

(**
	Determines if we have a Null<T> which was not checked against `null` yet.
*)
let rec is_nullable_type t =
	match t with
		| TMono r ->
			(match !r with None -> false | Some t -> is_nullable_type t)
		| TAbstract ({ a_path = ([],"Null") }, _) ->
			true
		| TAbstract (abstr, params) when not (Meta.has Meta.CoreType abstr.a_meta) ->
			is_nullable_type (Abstract.get_underlying_type abstr params)
		| TLazy f ->
			is_nullable_type (lazy_type f)
		| TType (t,tl) ->
			is_nullable_type (apply_params t.t_params tl t.t_type)
		| _ ->
			false

(**
	Check if provided type is `Unsafe<T>`
*)
let is_special_type_unsafe t =
	match t with
		| TType ({ t_path = ([], "Unsafe") }, _) -> true
		| _ -> false

(**
	Checks if execution of provided expression is guaranteed to be terminated with `return`, `throw`, `break` or `continue`.
*)
let rec is_dead_end e =
	match e.eexpr with
		| TThrow _ -> true
		| TReturn _ -> true
		| TBreak -> true
		| TContinue -> true
		| TWhile (_, body, DoWhile) -> is_dead_end body
		| TIf (_, if_body, Some else_body) -> is_dead_end if_body && is_dead_end else_body
		| TBlock exprs -> List.exists is_dead_end exprs
		| TMeta (_, e) -> is_dead_end e
		| TCast (e, _) -> is_dead_end e
		| _ -> false

(**
	If `t` represents `Null<SomeType>` this function returns `SomeType`.
*)
let rec unfold_null t =
	match t with
		| TMono r -> (match !r with None -> t | Some t -> unfold_null t)
		| TAbstract ({ a_path = ([],"Null") }, [t]) -> unfold_null t
		| TLazy f -> unfold_null (lazy_type f)
		| TType (t,tl) -> unfold_null (apply_params t.t_params tl t.t_type)
		| _ -> t

(**
	Shadow Type.error to avoid raising unification errors, which should not be raised from null-safety checks
*)
let safety_error () : unit = raise (Safety_error NullSafetyError)

(**
*
* BEGIN OF COPY-PAST from type.ml with some modifications.
* 	scroll down to END OF COPY-PASTE
*)

(**
	Check if value of type `a` could be passed to type `b`
*)
let rec type_eq param a b =
	let can_follow t = match param with
		| EqCoreType -> false
		| _ -> not (is_nullable_type t)
	in
	if a == b then
		()
	else match a , b with
	| TLazy f , _ -> type_eq param (lazy_type f) b
	| _ , TLazy f -> type_eq param a (lazy_type f)
	| TMono t , _ ->
		(match !t with
		| None -> if param = EqCoreType || not (link t a b) then error [cannot_unify a b]
		| Some t -> type_eq param t b)
	| _ , TMono t ->
		(match !t with
		| None -> if param = EqCoreType || not (link t b a) then error [cannot_unify a b]
		| Some t -> type_eq param a t)
	| TType (t1,tl1), TType (t2,tl2) when (t1 == t2 || (param = EqCoreType && t1.t_path = t2.t_path)) && List.length tl1 = List.length tl2 ->
		List.iter2 (type_eq param) tl1 tl2
	| TType (t,tl) , _ when can_follow a ->
		type_eq param (apply_params t.t_params tl t.t_type) b
	| _ , TType (t,tl) when can_follow b ->
		rec_stack eq_stack (a,b)
			(fun (a2,b2) -> fast_eq a a2 && fast_eq b b2)
			(fun() -> type_eq param a (apply_params t.t_params tl t.t_type))
			(fun l -> error (cannot_unify a b :: l))
	| TEnum (e1,tl1) , TEnum (e2,tl2) ->
		if e1 != e2 && not (param = EqCoreType && e1.e_path = e2.e_path) then error [cannot_unify a b];
		List.iter2 (type_eq param) tl1 tl2
	| TInst (c1,tl1) , TInst (c2,tl2) ->
		if c1 != c2 && not (param = EqCoreType && c1.cl_path = c2.cl_path) && (match c1.cl_kind, c2.cl_kind with KExpr _, KExpr _ -> false | _ -> true) then error [cannot_unify a b];
		List.iter2 (type_eq param) tl1 tl2
	| TFun (l1,r1) , TFun (l2,r2) when List.length l1 = List.length l2 ->
		(try
			type_eq param r1 r2;
			List.iter2 (fun (n,o1,t1) (_,o2,t2) ->
				if o1 <> o2 then error [Not_matching_optional n];
				type_eq param t1 t2
			) l1 l2
		with
			Unify_error l -> error (cannot_unify a b :: l))
	| TDynamic a , TDynamic b ->
		type_eq param a b
	| TAbstract ({a_path=[],"Null"},[t1]),TAbstract ({a_path=[],"Null"},[t2]) ->
		type_eq param t1 t2
	| TAbstract ({a_path=[],"Null"},[t]),_ when param <> EqDoNotFollowNull ->
		(* type_eq param t b *)
		safety_error ()
	| _,TAbstract ({a_path=[],"Null"},[t]) when param <> EqDoNotFollowNull ->
		type_eq param a t
	| TAbstract (a1,tl1) , TAbstract (a2,tl2) ->
		if a1 != a2 && not (param = EqCoreType && a1.a_path = a2.a_path) then error [cannot_unify a b];
		List.iter2 (type_eq param) tl1 tl2
	| TAnon a1, TAnon a2 ->
		(try
			PMap.iter (fun n f1 ->
				try
					let f2 = PMap.find n a2.a_fields in
					if f1.cf_kind <> f2.cf_kind && (param = EqStrict || param = EqCoreType || not (unify_kind f1.cf_kind f2.cf_kind)) then error [invalid_kind n f1.cf_kind f2.cf_kind];
					let a = f1.cf_type and b = f2.cf_type in
					rec_stack eq_stack (a,b)
						(fun (a2,b2) -> fast_eq a a2 && fast_eq b b2)
						(fun() -> type_eq param a b)
						(fun l -> error (invalid_field n :: l))
				with
					Not_found ->
						if is_closed a2 then error [has_no_field b n];
						if not (link (ref None) b f1.cf_type) then error [cannot_unify a b];
						a2.a_fields <- PMap.add n f1 a2.a_fields
			) a1.a_fields;
			PMap.iter (fun n f2 ->
				if not (PMap.mem n a1.a_fields) then begin
					if is_closed a1 then error [has_no_field a n];
					if not (link (ref None) a f2.cf_type) then error [cannot_unify a b];
					a1.a_fields <- PMap.add n f2 a1.a_fields
				end;
			) a2.a_fields;
		with
			Unify_error l -> error (cannot_unify a b :: l))
	| _ , _ ->
		if b == t_dynamic && (param = EqRightDynamic || param = EqBothDynamic) then
			()
		else if a == t_dynamic && param = EqBothDynamic then
			()
		else
			error [cannot_unify a b]

and unify a b =
	(* print_endline ("Checking " ^ (str_type a) ^ " against " ^ (str_type b)); *)
	if a == b then
		()
	else if is_nullable_type a && not (is_nullable_type b) then
		safety_error ()
	else match a, b with
	| TLazy f , _ -> unify (lazy_type f) b
	| _ , TLazy f -> unify a (lazy_type f)
	| TMono t , _ ->
		(match !t with
		| None -> if not (link t a b) then error [cannot_unify a b]
		| Some t -> unify t b)
	| _ , TMono t ->
		(match !t with
		| None -> if not (link t b a) then error [cannot_unify a b]
		| Some t -> unify a t)
	| TType (t,tl) , _ ->
		rec_stack unify_stack (a,b)
			(fun(a2,b2) -> fast_eq a a2 && fast_eq b b2)
			(fun() -> unify (apply_params t.t_params tl t.t_type) b)
			(fun l -> error (cannot_unify a b :: l))
	| _ , TType (t,tl) ->
		rec_stack unify_stack (a,b)
			(fun(a2,b2) -> fast_eq a a2 && fast_eq b b2)
			(fun() -> unify a (apply_params t.t_params tl t.t_type))
			(fun l -> error (cannot_unify a b :: l))
	| TEnum (ea,tl1) , TEnum (eb,tl2) ->
		if ea != eb then error [cannot_unify a b];
		unify_type_params a b tl1 tl2
	| TAbstract ({a_path=[],"Null"},[t]),_ ->
		begin try unify t b
		with Unify_error l -> error (cannot_unify a b :: l) end
	| _,TAbstract ({a_path=[],"Null"},[t]) ->
		begin try unify a t
		with Unify_error l -> error (cannot_unify a b :: l) end
	| TAbstract (a1,tl1) , TAbstract (a2,tl2) when a1 == a2 ->
		begin try
			unify_type_params a b tl1 tl2
		with Unify_error _ as err ->
			(* the type could still have a from/to relation to itself (issue #3494) *)
			begin try
				unify_abstracts a b a1 tl1 a2 tl2
			with Unify_error _ ->
				raise err
			end
		end
	| TAbstract ({a_path=[],"Void"},_) , _
	| _ , TAbstract ({a_path=[],"Void"},_) ->
		error [cannot_unify a b]
	| TAbstract (a1,tl1) , TAbstract (a2,tl2) ->
		unify_abstracts a b a1 tl1 a2 tl2
	| TInst (c1,tl1) , TInst (c2,tl2) ->
		let rec loop c tl =
			if c == c2 then begin
				unify_type_params a b tl tl2;
				true
			end else (match c.cl_super with
				| None -> false
				| Some (cs,tls) ->
					loop cs (List.map (apply_params c.cl_params tl) tls)
			) || List.exists (fun (cs,tls) ->
				loop cs (List.map (apply_params c.cl_params tl) tls)
			) c.cl_implements
			|| (match c.cl_kind with
			| KTypeParameter pl -> List.exists (fun t ->
				match follow t with
				| TInst (cs,tls) -> loop cs (List.map (apply_params c.cl_params tl) tls)
				| TAbstract(aa,tl) -> List.exists (unify_to aa tl b) aa.a_to
				| _ -> false
			) pl
			| _ -> false)
		in
		if not (loop c1 tl1) then error [cannot_unify a b]
	| TFun (l1,r1) , TFun (l2,r2) when List.length l1 = List.length l2 ->
		let i = ref 0 in
		(try
			(match r2 with
			| TAbstract ({a_path=[],"Void"},_) -> incr i
			| _ -> unify r1 r2; incr i);
			List.iter2 (fun (_,o1,t1) (_,o2,t2) ->
				if o1 && not o2 then error [Cant_force_optional];
				unify t1 t2;
				incr i
			) l2 l1 (* contravariance *)
		with
			Unify_error l ->
				let msg = if !i = 0 then "Cannot unify return types" else "Cannot unify argument " ^ (string_of_int !i) in
				error (cannot_unify a b :: Unify_custom msg :: l))
	| TInst (c,tl) , TAnon an ->
		if PMap.is_empty an.a_fields then (match c.cl_kind with
			| KTypeParameter pl ->
				(* one of the constraints must unify with { } *)
				if not (List.exists (fun t -> match follow t with TInst _ | TAnon _ -> true | _ -> false) pl) then error [cannot_unify a b]
			| _ -> ());
		(try
			PMap.iter (fun n f2 ->
				(*
					introducing monomorphs while unifying might create infinite loops - see #2315
					let's store these monomorphs and make sure we reach a fixed point
				*)
				let monos = ref [] in
				let make_type f =
					match f.cf_params with
					| [] -> f.cf_type
					| l ->
						let ml = List.map (fun _ -> mk_mono()) l in
						monos := ml;
						apply_params f.cf_params ml f.cf_type
				in
				let _, ft, f1 = (try raw_class_field make_type c tl n with Not_found -> error [has_no_field a n]) in
				let ft = apply_params c.cl_params tl ft in
				if not (unify_kind f1.cf_kind f2.cf_kind) then error [invalid_kind n f1.cf_kind f2.cf_kind];
				if f2.cf_public && not f1.cf_public then error [invalid_visibility n];

				(match f2.cf_kind with
				| Var { v_read = AccNo } | Var { v_read = AccNever } ->
					(* we will do a recursive unification, so let's check for possible recursion *)
					let old_monos = !unify_new_monos in
					unify_new_monos := !monos @ !unify_new_monos;
					rec_stack unify_stack (ft,f2.cf_type)
						(fun (a2,b2) -> fast_eq b2 f2.cf_type && fast_eq_mono !unify_new_monos ft a2)
						(fun() -> try unify_with_access ft f2 with e -> unify_new_monos := old_monos; raise e)
						(fun l -> error (invalid_field n :: l));
					unify_new_monos := old_monos;
				| Method MethNormal | Method MethInline | Var { v_write = AccNo } | Var { v_write = AccNever } ->
					(* same as before, but unification is reversed (read-only var) *)
					let old_monos = !unify_new_monos in
					unify_new_monos := !monos @ !unify_new_monos;
					rec_stack unify_stack (f2.cf_type,ft)
						(fun(a2,b2) -> fast_eq_mono !unify_new_monos b2 ft && fast_eq f2.cf_type a2)
						(fun() -> try unify_with_access ft f2 with e -> unify_new_monos := old_monos; raise e)
						(fun l -> error (invalid_field n :: l));
					unify_new_monos := old_monos;
				| _ ->
					(* will use fast_eq, which have its own stack *)
					try
						unify_with_access ft f2
					with
						Unify_error l ->
							error (invalid_field n :: l));

				List.iter (fun f2o ->
					if not (List.exists (fun f1o -> type_iseq f1o.cf_type f2o.cf_type) (f1 :: f1.cf_overloads))
					then error [Missing_overload (f1, f2o.cf_type)]
				) f2.cf_overloads;
				(* we mark the field as :?used because it might be used through the structure *)
				if not (Meta.has Meta.MaybeUsed f1.cf_meta) then f1.cf_meta <- (Meta.MaybeUsed,[],f1.cf_pos) :: f1.cf_meta;
				(match f1.cf_kind with
				| Method MethInline ->
					if (c.cl_extern || Meta.has Meta.Extern f1.cf_meta) && not (Meta.has Meta.Runtime f1.cf_meta) then error [Has_no_runtime_field (a,n)];
				| _ -> ());
			) an.a_fields;
			(match !(an.a_status) with
			| Opened -> an.a_status := Closed;
			| Statics _ | EnumStatics _ | AbstractStatics _ -> error []
			| Closed | Extend _ | Const -> ())
		with
			Unify_error l -> error (cannot_unify a b :: l))
	| TAnon a1, TAnon a2 ->
		unify_anons a b a1 a2
	| TAnon an, TAbstract ({ a_path = [],"Class" },[pt]) ->
		(match !(an.a_status) with
		| Statics cl -> unify (TInst (cl,List.map (fun _ -> mk_mono()) cl.cl_params)) pt
		| _ -> error [cannot_unify a b])
	| TAnon an, TAbstract ({ a_path = [],"Enum" },[pt]) ->
		(match !(an.a_status) with
		| EnumStatics e -> unify (TEnum (e,List.map (fun _ -> mk_mono()) e.e_params)) pt
		| _ -> error [cannot_unify a b])
	| TEnum _, TAbstract ({ a_path = [],"EnumValue" },[]) ->
		()
	| TEnum(en,_), TAbstract ({ a_path = ["haxe"],"FlatEnum" },[]) when Meta.has Meta.FlatEnum en.e_meta ->
		()
	| TFun _, TAbstract ({ a_path = ["haxe"],"Function" },[]) ->
		()
	| TInst(c,tl),TAbstract({a_path = ["haxe"],"Constructible"},[t1]) ->
		begin try
			begin match c.cl_kind with
				| KTypeParameter tl ->
					(* type parameters require an equal Constructible constraint *)
					if not (List.exists (fun t -> match follow t with TAbstract({a_path = ["haxe"],"Constructible"},[t2]) -> type_iseq t1 t2 | _ -> false) tl) then error [cannot_unify a b]
				| _ ->
					let _,t,cf = class_field c tl "new" in
					if not cf.cf_public then error [invalid_visibility "new"];
					begin try unify t t1
					with Unify_error l -> error (cannot_unify a b :: l) end
			end
		with Not_found ->
			error [has_no_field a "new"]
		end
	| TDynamic t , _ ->
		if t == a then
			()
		else (match b with
		| TDynamic t2 ->
			if t2 != b then
				(try
					type_eq EqRightDynamic t t2
				with
					Unify_error l -> error (cannot_unify a b :: l));
		| TAbstract(bb,tl) when (List.exists (unify_from bb tl a b) bb.a_from) ->
			()
		| _ ->
			error [cannot_unify a b])
	| _ , TDynamic t ->
		if t == b then
			()
		else (match a with
		| TDynamic t2 ->
			if t2 != a then
				(try
					type_eq EqRightDynamic t t2
				with
					Unify_error l -> error (cannot_unify a b :: l));
		| TAnon an ->
			(try
				(match !(an.a_status) with
				| Statics _ | EnumStatics _ -> error []
				| Opened -> an.a_status := Closed
				| _ -> ());
				PMap.iter (fun _ f ->
					try
						type_eq EqStrict (field_type f) t
					with Unify_error l ->
						error (invalid_field f.cf_name :: l)
				) an.a_fields
			with Unify_error l ->
				error (cannot_unify a b :: l))
		| TAbstract(aa,tl) when (List.exists (unify_to aa tl b) aa.a_to) ->
			()
		| _ ->
			error [cannot_unify a b])
	| TAbstract (aa,tl), _  ->
		if not (List.exists (unify_to aa tl b) aa.a_to) then error [cannot_unify a b];
	| TInst ({ cl_kind = KTypeParameter ctl } as c,pl), TAbstract (bb,tl) ->
		(* one of the constraints must satisfy the abstract *)
		if not (List.exists (fun t ->
			let t = apply_params c.cl_params pl t in
			try unify t b; true with Unify_error _ -> false
		) ctl) && not (List.exists (unify_from bb tl a b) bb.a_from) then error [cannot_unify a b];
	| _, TAbstract (bb,tl) ->
		if not (List.exists (unify_from bb tl a b) bb.a_from) then error [cannot_unify a b]
	| _ , _ ->
		error [cannot_unify a b]

and unify_abstracts a b a1 tl1 a2 tl2 =
	let f1 = unify_to a1 tl1 b in
		let f2 = unify_from a2 tl2 a b in
		if (List.exists (f1 ~allow_transitive_cast:false) a1.a_to)
		|| (List.exists (f2 ~allow_transitive_cast:false) a2.a_from)
		|| (((Meta.has Meta.CoreType a1.a_meta) || (Meta.has Meta.CoreType a2.a_meta))
			&& ((List.exists f1 a1.a_to) || (List.exists f2 a2.a_from))) then
			()
		else
			error [cannot_unify a b]

and unify_anons a b a1 a2 =
	(try
		PMap.iter (fun n f2 ->
		try
			let f1 = PMap.find n a1.a_fields in
			if not (unify_kind f1.cf_kind f2.cf_kind) then
				(match !(a1.a_status), f1.cf_kind, f2.cf_kind with
				| Opened, Var { v_read = AccNormal; v_write = AccNo }, Var { v_read = AccNormal; v_write = AccNormal } ->
					f1.cf_kind <- f2.cf_kind;
				| _ -> error [invalid_kind n f1.cf_kind f2.cf_kind]);
			if f2.cf_public && not f1.cf_public then error [invalid_visibility n];
			try
				unify_with_access (field_type f1) f2;
				(match !(a1.a_status) with
				| Statics c when not (Meta.has Meta.MaybeUsed f1.cf_meta) -> f1.cf_meta <- (Meta.MaybeUsed,[],f1.cf_pos) :: f1.cf_meta
				| _ -> ());
			with
				Unify_error l -> error (invalid_field n :: l)
		with
			Not_found ->
				match !(a1.a_status) with
				| Opened ->
					if not (link (ref None) a f2.cf_type) then error [];
					a1.a_fields <- PMap.add n f2 a1.a_fields
				| Const when Meta.has Meta.Optional f2.cf_meta ->
					()
				| _ ->
					error [has_no_field a n];
		) a2.a_fields;
		(match !(a1.a_status) with
		| Const when not (PMap.is_empty a2.a_fields) ->
			PMap.iter (fun n _ -> if not (PMap.mem n a2.a_fields) then error [has_extra_field a n]) a1.a_fields;
		| Opened ->
			a1.a_status := Closed
		| _ -> ());
		(match !(a2.a_status) with
		| Statics c -> (match !(a1.a_status) with Statics c2 when c == c2 -> () | _ -> error [])
		| EnumStatics e -> (match !(a1.a_status) with EnumStatics e2 when e == e2 -> () | _ -> error [])
		| AbstractStatics a -> (match !(a1.a_status) with AbstractStatics a2 when a == a2 -> () | _ -> error [])
		| Opened -> a2.a_status := Closed
		| Const | Extend _ | Closed -> ())
	with
		Unify_error l -> error (cannot_unify a b :: l))

and unify_from ab tl a b ?(allow_transitive_cast=true) t =
	rec_stack_bool abstract_cast_stack (a,b)
		(fun (a2,b2) -> fast_eq a a2 && fast_eq b b2)
		(fun() ->
			let t = apply_params ab.a_params tl t in
			let unify_func = if allow_transitive_cast then unify else type_eq EqStrict in
			unify_func a t)

and unify_to ab tl b ?(allow_transitive_cast=true) t =
	let t = apply_params ab.a_params tl t in
	let unify_func = if allow_transitive_cast then unify else type_eq EqStrict in
	try
		unify_func t b;
		true
	with Unify_error _ ->
		false

and unify_from_field ab tl a b ?(allow_transitive_cast=true) (t,cf) =
	rec_stack_bool abstract_cast_stack (a,b)
		(fun (a2,b2) -> fast_eq a a2 && fast_eq b b2)
		(fun() ->
			let unify_func = if allow_transitive_cast then unify else type_eq EqStrict in
			match follow cf.cf_type with
			| TFun(_,r) ->
				let monos = List.map (fun _ -> mk_mono()) cf.cf_params in
				let map t = apply_params ab.a_params tl (apply_params cf.cf_params monos t) in
				unify_func a (map t);
				List.iter2 (fun m (name,t) -> match follow t with
					| TInst ({ cl_kind = KTypeParameter constr },_) when constr <> [] ->
						List.iter (fun tc -> match follow m with TMono _ -> raise (Unify_error []) | _ -> unify m (map tc) ) constr
					| _ -> ()
				) monos cf.cf_params;
				unify_func (map r) b;
				true
			| _ -> assert false)

and unify_to_field ab tl b ?(allow_transitive_cast=true) (t,cf) =
	let a = TAbstract(ab,tl) in
	rec_stack_bool abstract_cast_stack (b,a)
		(fun (b2,a2) -> fast_eq a a2 && fast_eq b b2)
		(fun() ->
			let unify_func = if allow_transitive_cast then unify else type_eq EqStrict in
			match follow cf.cf_type with
			| TFun((_,_,ta) :: _,_) ->
				let monos = List.map (fun _ -> mk_mono()) cf.cf_params in
				let map t = apply_params ab.a_params tl (apply_params cf.cf_params monos t) in
				let athis = map ab.a_this in
				(* we cannot allow implicit casts when the this type is not completely known yet *)
				(* if has_mono athis then raise (Unify_error []); *)
				with_variance (type_eq EqStrict) athis (map ta);
				(* immediate constraints checking is ok here because we know there are no monomorphs *)
				List.iter2 (fun m (name,t) -> match follow t with
					| TInst ({ cl_kind = KTypeParameter constr },_) when constr <> [] ->
						List.iter (fun tc -> match follow m with TMono _ -> raise (Unify_error []) | _ -> unify m (map tc) ) constr
					| _ -> ()
				) monos cf.cf_params;
				unify_func (map t) b;
			| _ -> assert false)

and unify_with_variance f t1 t2 =
	let allows_variance_to t tf = type_iseq tf t in
	match follow t1,follow t2 with
	| TInst(c1,tl1),TInst(c2,tl2) when c1 == c2 ->
		List.iter2 f tl1 tl2
	| TEnum(en1,tl1),TEnum(en2,tl2) when en1 == en2 ->
		List.iter2 f tl1 tl2
	| TAbstract(a1,tl1),TAbstract(a2,tl2) when a1 == a2 && Meta.has Meta.CoreType a1.a_meta ->
		List.iter2 f tl1 tl2
	| TAbstract(a1,pl1),TAbstract(a2,pl2) ->
		if (Meta.has Meta.CoreType a1.a_meta) && (Meta.has Meta.CoreType a2.a_meta) then begin
			let ta1 = apply_params a1.a_params pl1 a1.a_this in
			let ta2 = apply_params a2.a_params pl2 a2.a_this in
			type_eq EqStrict ta1 ta2;
		end;
		if not (List.exists (allows_variance_to t2) a1.a_to) && not (List.exists (allows_variance_to t1) a2.a_from) then
			error [cannot_unify t1 t2]
	| TAbstract(a,pl),t ->
		type_eq EqBothDynamic (apply_params a.a_params pl a.a_this) t;
		if not (List.exists (fun t2 -> allows_variance_to t (apply_params a.a_params pl t2)) a.a_to) then error [cannot_unify t1 t2]
	| t,TAbstract(a,pl) ->
		type_eq EqBothDynamic t (apply_params a.a_params pl a.a_this);
		if not (List.exists (fun t2 -> allows_variance_to t (apply_params a.a_params pl t2)) a.a_from) then error [cannot_unify t1 t2]
	| (TAnon a1 as t1), (TAnon a2 as t2) ->
		rec_stack unify_stack (t1,t2)
			(fun (a,b) -> fast_eq a t1 && fast_eq b t2)
			(fun() -> unify_anons t1 t2 a1 a2)
			(fun l -> error l)
	| _ ->
		error [cannot_unify t1 t2]

and unify_type_params a b tl1 tl2 =
	List.iter2 (fun t1 t2 ->
		try
			with_variance (type_eq EqRightDynamic) t1 t2
		with Unify_error l ->
			let err = cannot_unify a b in
			error (err :: (Invariant_parameter (t1,t2)) :: l)
	) tl1 tl2

and with_variance f t1 t2 =
	try
		f t1 t2
	with Unify_error l -> try
		unify_with_variance (with_variance f) t1 t2
	with Unify_error _ ->
		raise (Unify_error l)

and unify_with_access t1 f2 =
	match f2.cf_kind with
	(* write only *)
	| Var { v_read = AccNo } | Var { v_read = AccNever } -> unify f2.cf_type t1
	(* read only *)
	| Method MethNormal | Method MethInline | Var { v_write = AccNo } | Var { v_write = AccNever } -> unify t1 f2.cf_type
	(* read/write *)
	| _ -> with_variance (type_eq EqBothDynamic) t1 f2.cf_type
(**
*
* END OF COPY-PASTE
*
*)

(**
	If `expr` is a TCast or TMeta, returns underlying expression (recursively bypassing nested casts).
	Otherwise returns `expr` as is.
*)
let rec reveal_expr expr =
	match expr.eexpr with
		| TCast (e, _) -> reveal_expr e
		| TMeta (_, e) -> reveal_expr e
		| _ -> expr

let accessed_field_name access =
	match access with
		| FInstance (_, _, { cf_name = name }) -> name
		| FStatic (_, { cf_name = name }) -> name
		| FAnon { cf_name = name } -> name
		| FDynamic name -> name
		| FClosure (_, { cf_name = name }) -> name
		| FEnum (_, { ef_name = name }) -> name

let rec can_pass_type src dst =
	if is_nullable_type src && not (is_nullable_type dst) then
		false
	else
		(* TODO *)
		match dst with
			| TMono r -> (match !r with None -> true | Some t -> can_pass_type src t)
			| TEnum (_, params) -> true
			| TInst _ -> true
			| TType (t, tl) -> can_pass_type src (apply_params t.t_params tl t.t_type)
			| TFun _ -> true
			| TAnon _ -> true
			| TDynamic _ -> true
			| TLazy _ -> true
			| TAbstract ({ a_path = ([],"Null") }, [t]) -> true
			| TAbstract _ -> true

(**
	Collect nullable local vars which are checked against `null`.
	Returns a tuple of (vars_checked_to_be_null * vars_checked_to_be_not_null) in case `condition` evaluates to `true`.
*)
let process_condition condition (is_nullable_expr:texpr->bool) callback =
	let nulls = ref []
	and not_nulls = ref [] in
	let rec traverse e =
		match e.eexpr with
			| TBinop (OpEq, { eexpr = TConst TNull }, { eexpr = TLocal v }) -> nulls := v :: !nulls
			| TBinop (OpEq, { eexpr = TLocal v }, { eexpr = TConst TNull }) -> nulls := v :: !nulls
			| TBinop (OpNotEq, { eexpr = TConst TNull }, { eexpr = TLocal v }) -> not_nulls := v :: !not_nulls
			| TBinop (OpNotEq, { eexpr = TLocal v }, { eexpr = TConst TNull }) -> not_nulls := v :: !not_nulls
			| TBinop (OpEq, e, { eexpr = TLocal v }) when not (is_nullable_expr e) -> not_nulls := v :: !not_nulls
			| TBinop (OpEq, { eexpr = TLocal v }, e) when not (is_nullable_expr e) -> not_nulls := v :: !not_nulls
			| TBinop (OpBoolAnd, left_expr, right_expr) ->
				traverse left_expr;
				traverse right_expr;
			| TParenthesis e -> traverse e
			| _ -> callback e
	in
	traverse condition;
	(!nulls, !not_nulls)

(**
	Check if specified `path` is mentioned in `-D SAFETY=here`
*)
let need_check com type_path file_path =
	let starts_with (haystack:string) (needle:string) :bool =
		(String.length haystack >= String.length needle)
		&& (needle = String.sub haystack 0 (String.length needle))
	in
	match type_path with
		| ([], "Safety") -> false
		| (packages, name) ->
			try
				let file_path = Path.unique_full_path file_path
				and class_path = (String.concat "." packages) ^ (if List.length packages = 0 then "" else ".") ^ name in
				let rec find lst =
					match lst with
						| [] -> false
						| ["ALL"] -> true
						| current :: rest ->
							if starts_with class_path current || starts_with file_path (Path.unique_full_path current) then
								true
							else
								find rest
				and check_list = Str.split (Str.regexp ",") (String.trim (raw_defined_value com "SAFETY")) in
				find check_list
			with
				| Not_found -> false

(**
	Check if specified `field` represents a `var` field which will exist at runtime.
*)
let should_be_initialized field =
	match field.cf_kind with
		| Var { v_read = AccNormal | AccInline | AccNo } | Var { v_write = AccNormal | AccNo } -> true
		| Var _ -> Meta.has Meta.IsVar field.cf_meta
		| _ -> false

(**
	Each loop or function should have its own scope.
*)
class safety_scope (scope_type:scope_type) (safe_locals:(int,tvar) Hashtbl.t) (never_safe:(int,tvar) Hashtbl.t) =
	object (self)
		(* val safe_locals = Hashtbl.create 100
		val never_safe = Hashtbl.create 100 *)
		(** Local vars declared in current scope *)
		val declarations = Hashtbl.create 100
		method get_safe_locals = safe_locals
		method get_never_safe = never_safe
		(* method get_never_safe = safe_locals
		method copy_safety *)
		(* method get_never_safe = never_safe *)
		method get_type = scope_type
		(**
			Should be called for each local var declared
		*)
		method declare_var v =
			Hashtbl.add declarations v.v_id v
		(**
			Check if local var was declared in this scope
		*)
		method owns_var v =
			Hashtbl.mem declarations v.v_id
		(**
			Check if local variable declared in this scope is guaranteed to not have a `null` value.
		*)
		method is_safe local_var =
			not (Hashtbl.mem never_safe local_var.v_id)
			&& (
				Hashtbl.mem safe_locals local_var.v_id
				|| not (is_nullable_type local_var.v_type)
			)
		(**
			Add variable to the list of safe locals.
		*)
		method add_to_safety v =
			Hashtbl.replace safe_locals v.v_id v
		(**
			Remove variable from the list of safe locals.
		*)
		method remove_from_safety ?(forever=false) v =
			Hashtbl.remove safe_locals v.v_id;
			if forever then
				Hashtbl.replace never_safe v.v_id v
	end

(**
	Class to simplify collecting lists of local vars checked against `null`.
*)
class local_vars =
	object (self)
		val mutable scopes = [new safety_scope STNormal (Hashtbl.create 100) (Hashtbl.create 100)]
		(**
			Drop collected data
		*)
		method clear =
			scopes <- [new safety_scope STNormal (Hashtbl.create 100) (Hashtbl.create 100)]
		(**
			Get the latest created scope.
		*)
		method private get_current_scope =
			match scopes with
				| current :: _-> current
				| [] -> fail ~msg:"List of scopes should never end." null_pos __POS__
		(**
			Get a copy of hashtable, which stores currently safe locals
		*)
		method get_safe_locals_copy =
			Hashtbl.copy (self#get_current_scope#get_safe_locals)
		(**
			Should be called upon local function declaration.
		*)
		method function_declared (fn:tfunc) =
			let scope = new safety_scope STClosure (Hashtbl.create 100) (Hashtbl.create 100) in
			scopes <- scope :: scopes;
			List.iter (fun (v, _) -> scope#declare_var v) fn.tf_args
		(**
			Should be called upon entering a loop.
		*)
		method loop_declared e =
			let scope = new safety_scope STLoop self#get_current_scope#get_safe_locals self#get_current_scope#get_never_safe in
			(* let scope = new safety_scope STLoop (Hashtbl.create 100) (Hashtbl.create 100) in *)
			scopes <- scope :: scopes;
			match e.eexpr with
				| TFor (v, _, _) -> scope#declare_var v
				| TWhile _ -> ()
				| _ -> fail ~msg:"Expected TFor or TWhile." e.epos __POS__
		(**
			Should be called upon leaving local function declaration.
		*)
		method scope_closed =
			match scopes with
				| [] -> fail ~msg:"No scopes left." null_pos __POS__
				| [scope] -> fail ~msg:"Cannot close the last scope." null_pos __POS__
				| _ :: rest -> scopes <- rest
		(**
			Should be called for each local var declared
		*)
		method declare_var ?(is_safe=false) (v:tvar) =
			let scope = self#get_current_scope in
			scope#declare_var v;
			if is_safe then scope#add_to_safety v
		(**
			Check if local variable is guaranteed to not have a `null` value.
		*)
		method is_safe local_var =
			if not (is_nullable_type local_var.v_type) then
				true
			else
				let rec traverse scopes =
					match scopes with
						| [] -> false
						| current :: rest ->
							if current#owns_var local_var then
								false
							else if current#get_type = STClosure then
								true
							else
								traverse rest
				in
				let captured = traverse scopes in
				not captured && self#get_current_scope#is_safe local_var
		(**
			This method should be called upon passing `while`.
			It collects locals which are checked against `null` and executes callbacks for expressions with proper statuses of locals.
		*)
		method process_while expr is_nullable_expr (condition_callback:texpr->unit) (body_callback:texpr->unit) =
			match expr.eexpr with
				| TWhile (condition, body, DoWhile) ->
					condition_callback condition;
					body_callback body
				| TWhile (condition, body, NormalWhile) ->
					condition_callback condition;
					let (nulls, not_nulls) = process_condition condition is_nullable_expr (fun _ -> ()) in
					(** execute `body` with known not-null variables *)
					List.iter self#get_current_scope#add_to_safety not_nulls;
					body_callback body;
					List.iter self#get_current_scope#remove_from_safety not_nulls;
				| _ -> fail ~msg:"Expected TWhile" expr.epos __POS__
		(**
			Should be called for bodies of loops (for, while)
		*)
		method process_loop_body (first_check:unit->unit) (second_check:unit->unit) =
			let original_safe_locals = Hashtbl.copy self#get_current_scope#get_safe_locals in
			(** The first check to find out which vars will become unsafe in a loop *)
			first_check();
			(* If local var became safe in a loop, then we need to remove it from safety to make it unsafe outside of a loop again *)
			Hashtbl.iter
				(fun var_id v ->
					if not (Hashtbl.mem original_safe_locals var_id) then
						self#get_current_scope#remove_from_safety v
				)
				(Hashtbl.copy self#get_current_scope#get_safe_locals);
			(** The second check with unsafe vars removed from safety *)
			second_check()
		(**
			This method should be called upon passing `if`.
			It collects locals which are checked against `null` and executes callbacks for expressions with proper statuses of locals.
		*)
		method process_if expr is_nullable_expr (condition_callback:texpr->unit) (body_callback:texpr->unit) =
			match expr.eexpr with
				| TIf (condition, if_body, else_body) ->
					condition_callback condition;
					let (nulls, not_nulls) = process_condition condition is_nullable_expr (fun _ -> ()) in
					(** execute `if_body` with known not-null variables *)
					List.iter self#get_current_scope#add_to_safety not_nulls;
					body_callback if_body;
					List.iter self#get_current_scope#remove_from_safety not_nulls;
					(** execute `else_body` with known not-null variables *)
					let handle_dead_end body safe_vars =
						if is_dead_end body then
							List.iter self#get_current_scope#add_to_safety safe_vars
					in
					(match else_body with
						| None ->
							(** If `if_body` terminates execution, then bypassing `if` means `nulls` are safe now *)
							handle_dead_end if_body nulls
						| Some else_body ->
							List.iter self#get_current_scope#add_to_safety nulls;
							body_callback else_body;
							List.iter self#get_current_scope#remove_from_safety nulls;
							(** If `if_body` terminates execution, then bypassing `if` means `nulls` are safe now *)
							handle_dead_end if_body nulls;
							(** If `else_body` terminates execution, then bypassing `else` means `not_nulls` are safe now *)
							handle_dead_end else_body not_nulls
					);
				| _ -> fail ~msg:"Expected TIf" expr.epos __POS__
		(**
			Handle boolean AND outside of `if` condition.
		*)
		method process_and left_expr right_expr is_nullable_expr (callback:texpr->unit) =
			let (_, not_nulls) = process_condition left_expr is_nullable_expr callback in
			List.iter self#get_current_scope#add_to_safety not_nulls;
			callback right_expr;
			List.iter self#get_current_scope#remove_from_safety not_nulls
		(**
			Handle boolean OR outside of `if` condition.
		*)
		method process_or left_expr right_expr is_nullable_expr (callback:texpr->unit) =
			let (nulls, _) = process_condition left_expr is_nullable_expr callback in
			List.iter self#get_current_scope#add_to_safety nulls;
			callback right_expr;
			List.iter self#get_current_scope#remove_from_safety nulls
		(**
			Remove local var from safety list if a nullable value is assigned to that var
		*)
		method handle_assignment is_nullable_expr left_expr (right_expr:texpr) =
			match (reveal_expr left_expr).eexpr with
				| TLocal v ->
					if is_nullable_expr right_expr then
						begin
							let captured = ref false in
							let rec traverse (lst:safety_scope list) =
								match lst with
									| [] -> ()
									| current :: rest ->
										if current#owns_var v then
											current#remove_from_safety ~forever:!captured v
										else begin
											captured := !captured || current#get_type = STClosure;
											current#remove_from_safety ~forever:!captured v;
											traverse rest
										end
							in
							traverse scopes
						end
					else if is_nullable_type v.v_type then
						self#get_current_scope#add_to_safety v
				| _ -> ()
	end

(**
	This is a base class is used to recursively check typed expressions for null-safety
*)
class expr_checker report =
	object (self)
		val local_safety = new local_vars
		val mutable return_types = []
		val mutable in_closure = false
		(* if this flag is `true` then spotted errors and warnings will not be reported *)
		val mutable is_pretending = false
		(* val mutable cnt = 0 *)
		(**
			Register an error
		*)
		method error msg p =
			if not is_pretending then
				report.sr_errors <- { sm_msg = ("Safety: " ^ msg); sm_pos = p; } :: report.sr_errors;
		(**
			Register an warning
		*)
		method warning msg p =
			if not is_pretending then
				report.sr_warnings <- { sm_msg = ("Safety: " ^ msg); sm_pos = p; } :: report.sr_warnings;
		(**
			Check if `e` is nullable even if the type is reported not-nullable.
			Haxe type system lies sometimes.
		*)
		method is_nullable_expr e =
			match e.eexpr with
				| TConst TNull -> true
				| TConst _ -> false
				(* Safety.unsafe() *)
				| TCall ({ eexpr = TField (_, FStatic ({ cl_path = ([], "Safety")}, { cf_name = "unsafe" })) }, _) -> false
				| TParenthesis e -> self#is_nullable_expr e
				| TMeta (_, e) -> self#is_nullable_expr e
				| TLocal v -> not (local_safety#is_safe v)
				| TThrow _ -> false
				| TBlock exprs ->
					(match exprs with
						| [] -> false
						| _ -> self#is_nullable_expr (List.hd (List.rev exprs))
					)
				| TIf _ ->
					let nullable = ref false in
					let check body = nullable := !nullable || self#is_nullable_expr body in
					local_safety#process_if e self#is_nullable_expr (fun _ -> ()) check;
					!nullable
				| _ -> is_nullable_type e.etype
		(**
			Check if `expr` can be passed to a place where `to_type` is expected.
		*)
		method private can_pass_expr expr to_type p =
			if (is_special_type_unsafe expr.etype) or (is_special_type_unsafe to_type) then
				true
			else if self#is_nullable_expr expr && not (is_nullable_type to_type) then
				false
			else
				let expr_type = unfold_null expr.etype in
				try
					unify expr_type to_type;
					true
				with
					(* Real unification performed by the compiler already passed at this point. So we can face null-safety errors only *)
					| Safety_error err ->
						self#error ("Cannot unify " ^ (str_type expr_type) ^ " with " ^ (str_type to_type)) p;
						(* returning `true` because error is already logged in the line above *)
						true
					(* returning `true` because real unification check is already performed by the compiler at this moment *)
					| _ -> true
				(* can_pass_type expr.etype to_type *)
		(**
			Should be called for the root expressions of a method or for then initialization expressions of fields.
		*)
		method check_root_expr e =
			self#check_expr e;
			local_safety#clear;
			return_types <- [];
			in_closure <- false
		(**
			Recursively checks an expression
		*)
		method private check_expr e =
			match e.eexpr with
				| TConst _ -> ()
				| TLocal _ -> ()
				| TArray (arr, idx) -> self#check_array_access arr idx e.epos
				| TBinop (op, left_expr, right_expr) -> self#check_binop op left_expr right_expr e.epos
				| TField (target, access) -> self#check_field target access e.epos
				| TTypeExpr _ -> ()
				| TParenthesis e -> self#check_expr e
				| TObjectDecl fields -> List.iter (fun (_, e) -> self#check_expr e) fields
				| TArrayDecl items -> self#check_array_decl items e.etype e.epos
				| TCall (callee, args) -> self#check_call callee args
				| TNew ({ cl_constructor = Some ctor }, _, args) -> self#check_constructor ctor args e.epos
				| TNew (_, _, args) -> List.iter self#check_expr args
				| TUnop (_, _, expr) -> self#check_unop expr e.epos
				| TFunction fn -> self#check_function fn
				| TVar (v, init_expr) -> self#check_var v init_expr e.epos
				| TBlock exprs -> List.iter self#check_expr exprs
				| TFor _ -> self#check_for e
				| TIf _ -> self#check_if e
				| TWhile _ -> self#check_while e
				| TSwitch (target, cases, default) -> self#check_switch target cases default
				| TTry (try_block, catches) -> self#check_try try_block catches
				| TReturn (Some expr) -> self#check_return expr e.epos
				| TReturn None -> ()
				| TBreak -> ()
				| TContinue -> ()
				| TThrow expr -> self#check_throw expr e.epos
				| TCast (expr, _) -> self#check_cast expr e.etype e.epos
				| TMeta (_, e) -> self#check_expr e
				| TEnumIndex idx -> self#check_enum_index idx
				| TEnumParameter (e, _, _) -> self#check_expr e (** Checking enum value itself is not needed here because this expr always follows after TEnumIndex *)
				| TIdent _ -> ()
		(**
			Don't allow to use nullable values as items in declaration of not-nullable arrays
		*)
		method private check_array_decl items arr_type p =
			(match Abstract.follow_with_abstracts arr_type with
				| TInst ({ cl_path = ([], "Array") }, [item_type]) ->
					List.iter
						(fun e ->
							if not (self#can_pass_expr e item_type e.epos) then
								self#error ("Cannot use nullable value of " ^ (str_type e.etype) ^ " as an item in Array<" ^ (str_type item_type) ^ ">") e.epos
						)
						items;
				| _ -> ()
			);
			List.iter self#check_expr items
		(**
			Deal with nullable enum values
		*)
		method private check_enum_index idx =
			if self#is_nullable_expr idx then
				self#error "Cannot access nullable enum value." idx.epos;
			self#check_expr idx
		(**
			Check try...catch
		*)
		method private check_try try_block catches =
			self#check_expr try_block;
			List.iter (fun (_, e) -> self#check_expr e) catches
		(**
			Don't use nullable value as a condition in `while`
		*)
		method private check_while e =
			match e.eexpr with
				| TWhile _ ->
					let check_condition condition =
						if self#is_nullable_expr condition then
							self#error "Cannot use nullable value as a condition in \"while\"." condition.epos;
						self#check_expr condition
					in
					local_safety#loop_declared e;
					local_safety#process_while e self#is_nullable_expr check_condition self#check_loop_body;
					local_safety#scope_closed
				| _ -> fail ~msg:"Expected TWhile." e.epos __POS__
		(**
			Don't iterate on nullable values
		*)
		method private check_for e =
			match e.eexpr with
				| TFor (v, iterable, body) ->
					if self#is_nullable_expr iterable then
						self#error "Cannot iterate over nullable value." iterable.epos;
					self#check_expr iterable;
					local_safety#declare_var v;
					local_safety#loop_declared e;
					self#check_loop_body body;
					local_safety#scope_closed
				| _ -> fail ~msg:"Expected TFor." e.epos __POS__
		(**
			Handle safety inside of loops
		*)
		method private check_loop_body body =
			local_safety#process_loop_body
				(* Start pretending to ignore errors *)
				(fun () ->
					is_pretending <- true;
					self#check_expr body
				)
				(* Now we know, which vars will become unsafe in this loop. Stop pretending and check again *)
				(fun () ->
					is_pretending <- false;
					self#check_expr body;
				)
		(**
			Don't throw nullable values
		*)
		method private check_throw e p =
			if self#is_nullable_expr e then
				self#error "Cannot throw nullable value." p;
			self#check_expr e
		(**
			Don't cast nullable expressions to not-nullable types
		*)
		method private check_cast expr to_type p =
			self#check_expr expr;
			if not (self#can_pass_expr expr to_type p) then
				self#error "Cannot cast nullable value to not nullable type." p
		(**
			Check safety in a function
		*)
		method private check_function fn =
			local_safety#function_declared fn;
			return_types <- fn.tf_type :: return_types;
			self#check_expr fn.tf_expr;
			local_safety#scope_closed
		(**
			Don't return nullable values as not-nullable return types.
		*)
		method private check_return e p =
			self#check_expr e;
			match return_types with
				| t :: _ when not (self#can_pass_expr e t p) ->
					self#error ("Cannot return nullable value of " ^ (str_type e.etype) ^ " as " ^ (str_type t)) p
				| _ -> ()
		(**
			Check safety in `switch` expressions.
		*)
		method private check_switch target cases default =
			if self#is_nullable_expr target then
				self#error "Cannot switch on nullable value." target.epos;
			self#check_expr target;
			let rec traverse_cases cases =
				match cases with
					| [] -> ()
					| (_, body) :: rest ->
						self#check_expr body;
						traverse_cases rest
			in
			traverse_cases cases;
			match default with
				| None -> ()
				| Some e -> self#check_expr e
		(**
			Check safety in `if` expressions
		*)
		method private check_if e =
			let check_condition e =
				if self#is_nullable_expr e then
					self#error "Cannot use nullable value as condition in \"if\"." e.epos;
				self#check_expr e
			in
			local_safety#process_if e self#is_nullable_expr check_condition self#check_expr
		(**
			Check array access on nullable values or using nullable indexes
		*)
		method private check_array_access arr idx p =
			if self#is_nullable_expr arr then
				self#error "Cannot perform array access on nullable value." p;
			if self#is_nullable_expr idx then
				self#error "Cannot use nullable value as an index for array access." p;
			self#check_expr arr;
			self#check_expr idx
		(**
			Don't perform unsafe binary operations
		*)
		method private check_binop op left_expr right_expr p =
			let check_both () =
				self#check_expr left_expr;
				self#check_expr right_expr
			in
			match op with
				| OpEq | OpNotEq -> check_both()
				| OpBoolAnd ->
					local_safety#process_and left_expr right_expr self#is_nullable_expr self#check_expr
				| OpBoolOr ->
					local_safety#process_or left_expr right_expr self#is_nullable_expr self#check_expr
				| OpAssign ->
					if not (self#can_pass_expr right_expr left_expr.etype p) then
						begin
							self#error "Cannot assign nullable value here." p;
							check_both()
						end
					else
						begin
							check_both();
							local_safety#handle_assignment self#is_nullable_expr left_expr right_expr;
						end
				| _->
					if self#is_nullable_expr left_expr || self#is_nullable_expr right_expr then
						self#error "Cannot perform binary operation on nullable value." p;
					check_both()
		(**
			Don't perform unops on nullable values
		*)
		method private check_unop e p =
			if self#is_nullable_expr e then
				self#error "Cannot perform unary operation on nullable value." p;
			self#check_expr e
		(**
			Don't assign nullable value to not-nullable variable on var declaration
		*)
		method private check_var v init p =
			match init with
				| None -> local_safety#declare_var v
				| Some e ->
					let is_safe =
						match (reveal_expr e).eexpr with
							| TLocal v2 -> is_nullable_type v.v_type && local_safety#is_safe v2
							| _ -> false
					in
					local_safety#declare_var ~is_safe:is_safe v;
					if not (self#can_pass_expr e v.v_type p) then
						self#error "Cannot assign nullable value to not-nullable variable." p;
					self#check_expr e
		(**
			Make sure nobody tries to access a field on a nullable value
		*)
		method private check_field target access p =
			if self#is_nullable_expr target then
				self#error ("Cannot access \"" ^ accessed_field_name access ^ "\" of a nullable value.") p;
			self#check_expr target
		(**
			Check constructor invocation: dont' pass nulable values to not-nullable arguments
		*)
		method private check_constructor ctor args p =
			let rec traverse t =
				match follow t with
					| TFun (types, _) -> self#check_args args types
					(* | TLazy l -> traverse (lazy_type l)
					| TMono r -> (match !r) *)
					| _ -> fail ~msg:"Unexpected constructor type." p __POS__
			in
			traverse ctor.cf_type

		(**
			Check calls: don't call a nullable value, dont' pass nulable values to not-nullable arguments
		*)
		method private check_call callee args =
			if self#is_nullable_expr callee then
				self#error "Cannot call a nullable value." callee.epos;
			self#check_expr callee;
			List.iter self#check_expr args;
			match callee.eexpr with
				(* Handle `Safety.isSafe(localVar)` *)
				| TField (_, FStatic ({ cl_path = ([], "Safety") }, { cf_name = "_isSafe"})) ->
					(match args with
						| [ e ] -> self#warning (string_of_bool (not (self#is_nullable_expr e))) e.epos
						| _ -> self#error "Invalid arguments for Safety.isSafe*()" callee.epos
					)
				(* Handle other calls *)
				| _ ->
					match follow callee.etype with
						| TFun (types, _) ->
							let fn_name = match (reveal_expr callee).eexpr with
								| TField (_, access) -> field_name access
								| TIdent fn_name -> fn_name
								| TLocal { v_name = fn_name } -> fn_name
								| _ -> ""
							in
							self#check_args ~callee:fn_name args types
						| _ -> ()
		(**
			Check if specified expressions can be passed to a call which expects `types`.
		*)
		method private check_args ?(callee="") args types =
			match (args, types) with
				| (a :: args, (arg_name, optional, t) :: types) ->
					if not optional && not (self#can_pass_expr a t a.epos) then begin
						let fn_str = if callee = "" then "" else " of function \"" ^ callee ^ "\""
						and arg_str = if arg_name = "" then "" else " \"" ^ arg_name ^ "\"" in
						self#error ("Cannot pass nullable value to not-nullable argument" ^ arg_str ^ fn_str ^ ".") a.epos
					end;
					self#check_expr a;
					self#check_args ~callee:callee args types;
				| _ -> ()
	end

class class_checker cls report =
	object (self)
			val checker = new expr_checker report
			val mutable initialized_in_constructor = Hashtbl.create 1
			val mutable constructor_checked = false
		(**
			Entry point for checking a class
		*)
		method check =
			if (not cls.cl_extern) && (not cls.cl_interface) then
				self#check_var_fields;
			let check_field f =
				Option.may checker#check_root_expr f.cf_expr
			in
			Option.may checker#check_root_expr cls.cl_init;
			Option.may (fun field -> Option.may checker#check_root_expr field.cf_expr) cls.cl_constructor;
			List.iter check_field cls.cl_ordered_fields;
			List.iter check_field cls.cl_ordered_statics;
		(**
			Check `var` fields are initialized properly
		*)
		method check_var_fields =
			let check_field is_static field =
				if should_be_initialized field then
					if not (is_nullable_type field.cf_type) then
						match field.cf_expr with
							| None ->
								if is_static then
									checker#error
										("Field \"" ^ field.cf_name ^ "\" is not nullable thus should have an initial value.")
										field.cf_pos
							| Some e ->
								if checker#is_nullable_expr e then
									checker#error ("Cannot set nullable initial value for not-nullable field \"" ^ field.cf_name ^ "\".") field.cf_pos
			in
			List.iter (check_field false) cls.cl_ordered_fields;
			List.iter (check_field true) cls.cl_ordered_statics;
			self#check_fields_initialization_in_constructor ()
		(**
			Check instance fields without initial values are properly initialized in constructor
		*)
		method private check_fields_initialization_in_constructor () =
			let fields_to_initialize = Hashtbl.create 20 in
			List.iter
				(fun f ->
					if not (is_nullable_type f.cf_type) then
						match f.cf_expr with
							| Some _ -> ()
							| None -> Hashtbl.add fields_to_initialize f.cf_name f
				)
				cls.cl_ordered_fields;
			let rec check_unsafe_usage init_list e =
				if Hashtbl.length init_list > 0 then
					match e.eexpr with
						| TField ({ eexpr = TConst TThis }, FInstance (_, _, field)) ->
							if Hashtbl.mem init_list field.cf_name then
								checker#error ("Cannot use field " ^ field.cf_name ^ " until initialization.") e.epos
						| TField ({ eexpr = TConst TThis }, FClosure (_, field)) ->
							checker#error ("Cannot use method " ^ field.cf_name ^ " until all instance fields are initialized.") e.epos;
						| TCall ({ eexpr = TField ({ eexpr = TConst TThis }, FInstance (_, _, field)) }, args) ->
							checker#error ("Cannot call method " ^ field.cf_name ^ " until all instance fields are initialized.") e.epos;
							List.iter (check_unsafe_usage init_list) args
						| TConst TThis ->
							checker#error "Cannot use \"this\" until all instance fields are initialized." e.epos
						| _ ->
							iter (check_unsafe_usage init_list) e
			in
			let rec traverse init_list e =
				(match e.eexpr with
					| TBinop (OpAssign, { eexpr = TField ({ eexpr = TConst TThis }, FInstance (_, _, f)) }, right_expr) ->
						Hashtbl.remove init_list f.cf_name;
						ignore (traverse init_list right_expr)
					| TWhile (condition, body, DoWhile) ->
						check_unsafe_usage init_list condition;
						ignore (traverse init_list body)
					| TBlock exprs ->
						List.iter (fun e -> ignore (traverse init_list e)) exprs
					| TIf (_, if_block, Some else_block) ->
						let if_init_list = traverse (Hashtbl.copy init_list) if_block
						and else_init_list = traverse (Hashtbl.copy init_list) else_block in
						Hashtbl.clear init_list;
						Hashtbl.iter (Hashtbl.replace init_list) if_init_list;
						Hashtbl.iter (Hashtbl.replace init_list) else_init_list
					| _ ->
						check_unsafe_usage init_list e
				);
				init_list
			in
			match cls.cl_constructor with
				| Some { cf_expr = Some { eexpr = TFunction { tf_expr = e } } } ->
					ignore (traverse fields_to_initialize e);
					Hashtbl.iter
						(fun name field ->
							checker#error
								("Field \"" ^ name ^ "\" is not nullable thus should have an initial value or should be initialized in constructor.")
								field.cf_pos
						)
						fields_to_initialize
				| _ -> ()
	end

class plugin =
	object (self)
		val report = { sr_errors = []; sr_warnings = [] }
		(**
			Plugin API: this method should be executed at initialization macro time
		*)
		method run () =
			let com = (get_ctx()).curapi.get_com() in
			add_typing_filter com (fun types ->
				let t = Gencommon.timer ["safety plugin"] in
				let rec traverse com_type =
					match com_type with
						| TEnumDecl enm -> ()
						| TTypeDecl typedef -> ()
						| TAbstractDecl abstr -> ()
						| TClassDecl cls when not (need_check com cls.cl_path cls.cl_pos.pfile) -> ()
						| TClassDecl cls ->
							if raw_defined com "SAFETY_DEBUG" then
								print_endline ("Safety check: " ^ (str_type (TInst (cls, []))));
							(new class_checker cls report)#check
				in
				List.iter traverse types;
				if not (raw_defined com "SAFETY_SILENT") then
					List.iter (fun err -> com.error err.sm_msg err.sm_pos) (List.rev report.sr_errors);
				t();
			);
			(* This is because of vfun0 should return something *)
			vint32 (Int32.of_int 0)
		(**
			Plugin API: returns a list of all errors found during safety checks
		*)
		method get_errors () =
			self#serialize_messages report.sr_errors
		(**
			Plugin API: returns a list of all warnings found during safety checks
		*)
		method get_warnings () =
			self#serialize_messages report.sr_warnings
		(**
			Plugin API: Check if current macro position should be handled by Safety (based on `-D SAFETY=` flag) for preprocessing safe-call operator `!.`
		*)
		method is_in_safety () =
			let api = (get_ctx()).curapi in
			match api.get_local_type() with
				| None -> vfalse
				| Some t ->
					let check type_path file_path =
						if need_check (api.get_com()) type_path file_path then
							vtrue
						else
							vfalse
					in
					match t with
						| TInst (cls, _) -> check cls.cl_path cls.cl_pos.pfile
						| TAbstract (abstr, _) -> check abstr.a_path abstr.a_pos.pfile
						| _ -> vfalse

		method private serialize_messages messages =
			let arr = Array.make (List.length messages) vnull in
			let set_item idx msg p =
				let obj = encode_obj_s
					vnull
					[
						("msg", vstring (Rope.of_string msg));
						("pos", encode_pos p)
					]
				in
				Array.set arr idx obj
			in
			let rec traverse idx errors =
				match errors with
					| err :: errors ->
						set_item idx err.sm_msg err.sm_pos;
						traverse (idx + 1) errors
					| [] -> ()
			in
			traverse 0 (List.rev messages);
			(* VArray arr *)
			VArray (EvalArray.create arr)
	end
;;

let api = new plugin in

EvalStdLib.StdContext.register [
	("run", vfun0 api#run);
	("getErrors", vfun0 api#get_errors);
	("getWarnings", vfun0 api#get_warnings);
	("isInSafety", vfun0 api#is_in_safety);
]