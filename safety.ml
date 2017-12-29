open EvalValue
open EvalContext
open EvalEncode
open Common
open MacroApi
open Type

let accessed_field_name access =
	match access with
		| FInstance (_, _, { cf_name = name }) -> name
		| FStatic (_, { cf_name = name }) -> name
		| FAnon { cf_name = name } -> name
		| FDynamic name -> name
		| FClosure (_, { cf_name = name }) -> name
		| FEnum (_, { ef_name = name }) -> name

(**
	This class is used to recursively check typed expressions for null-safety
*)
class null_checker com =
	object (self)
	method check e =
		match e.eexpr with
			| TConst _ -> ()
			| TLocal _ -> ()
			| TArray _ -> ()
			| TBinop _ -> ()
			| TField (target, access) ->
				if is_explicit_null target.etype then
					com.error ("Cannot access \"" ^ accessed_field_name access ^ "\" of a nullable value") target.epos
			| TTypeExpr _ -> ()
			| TParenthesis _ -> ()
			| TObjectDecl _ -> ()
			| TArrayDecl _ -> ()
			| TCall (target, args) ->
				self#check target;
				List.iter self#check args
			| TNew _ -> ()
			| TUnop _ -> ()
			| TFunction fn -> self#check fn.tf_expr
			| TVar _ -> ()
			| TBlock exprs -> List.iter self#check exprs
			| TFor _ -> ()
			| TIf _ -> ()
			| TWhile _ -> ()
			| TSwitch _ -> ()
			| TTry _ -> ()
			| TReturn _ -> ()
			| TBreak -> ()
			| TContinue -> ()
			| TThrow _ -> ()
			| TCast _ -> ()
			| TMeta _ -> ()
			| TEnumParameter _ -> ()
			| TEnumIndex _ -> ()
			| TIdent _ -> ()
end

let check_class com cls =
	let check_field f =
		Option.may (new null_checker com)#check f.cf_expr
	in
	List.iter check_field cls.cl_ordered_fields;
	List.iter check_field cls.cl_ordered_statics;
	match cls.cl_init with
		| Some e -> (new null_checker com)#check e
		| None -> ()

let run = vfun0 (fun () ->
	let com = (get_ctx()).curapi.get_com() in
	add_typing_filter com (fun types ->
		(* let t = macro_timer ctx ["safety plugin"] in *)
		let rec traverse com_type =
			match com_type with
				| TClassDecl cls -> check_class com cls;
				| TEnumDecl enm -> ();
				| TTypeDecl typedef -> ();
				| TAbstractDecl abstr -> ()
		in
		List.iter traverse types;
		(* t() *)
	);
	(* This is because of vfun0 should return something *)
	vint32 (Int32.of_int 0);
)
;;

EvalStdLib.StdContext.register ["run",run]