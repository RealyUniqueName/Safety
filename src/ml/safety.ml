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
	mutable sc_errors : safety_message list;
	mutable sc_warnings : safety_message list;
}

type scope_type =
	| STNormal
	| STLoop
	| STClosure

(**
	Terminates compiler process and prints user-friendly instructions about filing an issue in compiler repo.
*)
let fail ?msg hxpos mlpos =
	let msg =
		(Lexer.get_error_pos (Printf.sprintf "%s:%d:") hxpos) ^ ": "
		^ "Haxe-safety: " ^ (match msg with Some msg -> msg | _ -> "unexpected expression.") ^ "\n"
		^ "Submit an issue to https://github.com/RealyUniqueName/Haxe-Safety/issues with expression example and following information:"
	in
	match mlpos with
		| (file, line, _, _) ->
			Printf.eprintf "%s\n" msg;
			Printf.eprintf "%s:%d\n" file line;
			assert false

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

(**
	Determines if we have a Null<T> which was not checked against `null` yet.
*)
let rec is_nullable_type t =
	match t with
		| TMono r ->
			(match !r with None -> false | Some t -> is_nullable_type t)
		| TAbstract ({ a_path = ([],"Null") }, [t]) ->
			true
		| TLazy f ->
			is_nullable_type (lazy_type f)
		| TType (t,tl) ->
			is_nullable_type (apply_params t.t_params tl t.t_type)
		| _ ->
			false

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
let need_check com path =
	match path with
		| ([], "Safety") -> false
		| (packages, name) ->
			try
				let rec find str lst =
					match lst with
						| [] -> false
						| ["ALL"] -> true
						| current :: rest ->
							let contains =
								(String.length str >= String.length current)
								&& (current = String.sub str 0 (String.length current))
							in
							if contains then
								true
							else
								find str rest;
				and check_list = Str.split (Str.regexp ",") (String.trim (raw_defined_value com "SAFETY"))
				and path_str = (String.concat "." packages) ^ (if List.length packages = 0 then "" else ".") ^ name in
				find path_str check_list
			with Not_found -> false

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
class safety_scope (scope_type:scope_type) =
	object (self)
		val safe_locals = Hashtbl.create 100
		val never_safe = Hashtbl.create 100
		(** Local vars declared in current scope *)
		val declarations = Hashtbl.create 100
		method get_safe_locals = safe_locals
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
			Hashtbl.add safe_locals v.v_id v
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
		val mutable scopes = [new safety_scope STNormal]
		(**
			Drop collected data
		*)
		method clear =
			scopes <- [new safety_scope STNormal]
		(**
			Get the latest created scope.
		*)
		method private get_current_scope =
			match scopes with
				| current :: _-> current
				| [] -> fail ~msg:"List of scopes should never end." null_pos __POS__
		(**
			Should be called upon local function declaration.
		*)
		method function_declared (fn:tfunc) =
			let scope = new safety_scope STClosure in
			scopes <- scope :: scopes;
			List.iter (fun (v, _) -> scope#declare_var v) fn.tf_args
		(**
			Should be called upon entering a loop.
		*)
		method loop_declared e =
			let scope = new safety_scope STLoop in
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
		method declare_var (v:tvar) =
			self#get_current_scope#declare_var v
		(**
			Check if local variable is guaranteed to not have a `null` value.
		*)
		method is_safe local_var =
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
					(match else_body with
						| None -> ()
						| Some else_body ->
							List.iter self#get_current_scope#add_to_safety nulls;
							body_callback else_body;
							List.iter self#get_current_scope#remove_from_safety nulls
					)
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
				| TLocal v when is_nullable_expr right_expr ->
					let captured = ref false in
					let rec traverse (lst:safety_scope list) =
						match lst with
							| [] -> ()
							| current :: rest ->
								if current#owns_var v then
									current#remove_from_safety ~forever:!captured v
								else begin
									captured := current#get_type = STClosure;
									current#remove_from_safety ~forever:!captured v;
									traverse rest
								end
					in
					traverse scopes
					(* List.iter (fun scope -> scope#remove_from_safety v) scopes *)
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
		(* val mutable cnt = 0 *)
		(**
			Register an error
		*)
		method error msg p =
			report.sc_errors <- { sm_msg = ("Safety: " ^ msg); sm_pos = p; } :: report.sc_errors;
			(* cnt <- cnt + 1;
			if cnt = 2 then assert false *)
		(**
			Register an warning
		*)
		method warning msg p =
			report.sc_warnings <- { sm_msg = ("Safety: " ^ msg); sm_pos = p; } :: report.sc_warnings;
		(**
			Check if `e` is nullable even if the type is reported not-nullable.
			Haxe type system lies sometimes.
		*)
		method is_nullable_expr e =
			match e.eexpr with
				| TConst TNull -> true
				| TParenthesis e -> self#is_nullable_expr e
				| TLocal v -> not (local_safety#is_safe v)
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
		method private can_pass_expr expr to_type =
			if self#is_nullable_expr expr && not (is_nullable_type to_type) then
				false
			else
				true
				(* can_pass_type expr.etype to_type *)
		(**
			Should be called for the root expressions of a method or for then initialization expressions of fields.
		*)
		method check_root_expr e =
			(* print_endline (s_expr (fun t -> "_t_") e); *)
			self#check_expr e;
			local_safety#clear
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
				| TArrayDecl exprs -> List.iter self#check_expr exprs
				| TCall (callee, args) -> self#check_call callee args
				| TNew ({ cl_constructor = Some ctor }, _, args) -> self#check_constructor ctor args e.epos
				| TNew (_, _, args) -> List.iter self#check_expr args
				| TUnop (_, _, expr) -> self#check_unop expr e.epos
				| TFunction fn -> self#check_function fn
				| TVar (v, Some init_expr) -> self#check_var v init_expr e.epos
				| TVar (_, None) -> ()
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
				| TMeta ((Meta.Custom ":unsafe", _, _), _) -> print_endline "UNSAFE META!"
				| TMeta (_, e) -> self#check_expr e
				| TEnumIndex idx -> self#check_enum_index idx
				| TEnumParameter (e, _, _) -> self#check_expr e (** Checking enum value itself is not needed here because this expr always follows after TEnumIndex *)
				| TIdent _ -> ()
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
							self#error "Cannot use nullable value as condition in \"while\"." condition.epos;
						self#check_expr condition
					in
					local_safety#loop_declared e;
					local_safety#process_while e self#is_nullable_expr check_condition self#check_expr;
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
					self#check_expr body;
					local_safety#scope_closed
				| _ -> fail ~msg:"Expected TFor." e.epos __POS__
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
			if not (self#can_pass_expr expr to_type) then
				self#error "Cannot cast nullable value to not nullable type." p;
			self#check_expr expr
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
				| t :: _ when not (self#can_pass_expr e t) ->
					self#error "Cannot return nullable value from function with not nullable return type." p
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
					if not (self#can_pass_expr right_expr left_expr.etype) then
						self#error "Cannot assign nullable value here." p
					else
						local_safety#handle_assignment self#is_nullable_expr left_expr right_expr;
					check_both()
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
		method private check_var v e p =
			local_safety#declare_var v;
			if not (self#can_pass_expr e v.v_type) then
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
			match ctor.cf_type with
				| TFun (types, _) -> self#check_args args types
				| _ -> fail ~msg:"Unexpected constructor type." p __POS__

		(**
			Check calls: don't call a nullable value, dont' pass nulable values to not-nullable arguments
		*)
		method private check_call callee args =
			if self#is_nullable_expr callee then
				self#error "Cannot call a nullable value." callee.epos;
			self#check_expr callee;
			List.iter self#check_expr args;
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
				| (a :: args, (arg_name, _, t) :: types) ->
					if not (self#can_pass_expr a t) then begin
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
								else if not (self#is_initialized_in_constructor field) then
									checker#error
										("Field \"" ^ field.cf_name ^ "\" is not nullable thus should have an initial value or should be initialized in constructor.")
										field.cf_pos
							| Some e ->
								if checker#is_nullable_expr e then
									checker#error ("Cannot set nullable initial value for not-nullable field \"" ^ field.cf_name ^ "\".") field.cf_pos
			in
			List.iter (check_field false) cls.cl_ordered_fields;
			List.iter (check_field true) cls.cl_ordered_statics
		(**
			Check if `field` is initialized in constructor
		*)
		method private is_initialized_in_constructor field =
			if constructor_checked then
				Hashtbl.mem initialized_in_constructor field.cf_name
			else begin
				constructor_checked <- true;
				let rec traverse e =
					match e.eexpr with
						| TBinop (OpAssign, { eexpr = TField ({ eexpr = TConst TThis }, FInstance (_, _, f)) }, _) ->
							let result = Hashtbl.create 1 in
							Hashtbl.add result f.cf_name f;
							result
						| TWhile (_, body, DoWhile) ->
							traverse body
						| TBlock exprs ->
							let result = Hashtbl.create (List.length exprs) in
							List.iter
								(fun e ->
									Hashtbl.iter
										(fun name f -> Hashtbl.replace result name f)
										(traverse e)
								)
								exprs;
							result
						| TIf (_, if_block, Some else_block) ->
							let result = Hashtbl.create 10
							and initialized_in_if = traverse if_block
							and initialized_in_else = traverse else_block in
							Hashtbl.iter
								(fun name f ->
									if Hashtbl.mem initialized_in_else name then
										Hashtbl.replace result name f
								)
								initialized_in_if;
							Hashtbl.iter
								(fun name f ->
									if Hashtbl.mem initialized_in_if name then
										Hashtbl.replace result name f
								)
								initialized_in_else;
							result
						| _ -> Hashtbl.create 0
				in
				match cls.cl_constructor with
					| Some { cf_expr = Some { eexpr = TFunction { tf_expr = e } } } ->
						initialized_in_constructor <- traverse e;
						Hashtbl.mem initialized_in_constructor field.cf_name
					| _ -> false
			end
	end

class plugin =
	object (self)
		val report = { sc_errors = []; sc_warnings = [] }
		(**
			Plugin API: this method should be executed at initialization macro time
		*)
		method run () =
			let com = (get_ctx()).curapi.get_com() in
			add_typing_filter com (fun types ->
				(* let t = macro_timer ctx ["safety plugin"] in *)
				let rec traverse com_type =
					match com_type with
						| TEnumDecl enm -> ()
						| TTypeDecl typedef -> ()
						| TAbstractDecl abstr -> ()
						| TClassDecl { cl_path = path } when not (need_check com path) -> ()
						| TClassDecl cls -> (new class_checker cls report)#check
				in
				List.iter traverse types;
				if not (raw_defined com "SAFETY_SILENT") then
					List.iter (fun err -> com.error err.sm_msg err.sm_pos) (List.rev report.sc_errors);
				(* t() *)
			);
			(* This is because of vfun0 should return something *)
			vint32 (Int32.of_int 0)
		(**
			Plugin API: returns a list of all errors found during safety checks
		*)
		method get_errors () =
			self#serialize report.sc_errors
		(**
			Plugin API: returns a list of all warnings found during safety checks
		*)
		method get_warnings () =
			self#serialize report.sc_warnings
		method private serialize messages =
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
	("getWarnings", vfun0 api#get_warnings)
]