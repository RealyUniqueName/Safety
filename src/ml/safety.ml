open Globals
open EvalValue
open EvalContext
open EvalEncode
open Common
open MacroApi
open Type

type safety_error = {
	se_msg : string;
	se_pos : pos;
}

let errors:safety_error list ref = ref []

let accessed_field_name access =
	match access with
		| FInstance (_, _, { cf_name = name }) -> name
		| FStatic (_, { cf_name = name }) -> name
		| FAnon { cf_name = name } -> name
		| FDynamic name -> name
		| FClosure (_, { cf_name = name }) -> name
		| FEnum (_, { ef_name = name }) -> name

(**
	This is a base class is used to recursively check typed expressions for null-safety
*)
class virtual base_checker com =
	object (self)
	(**
		Entry point for checking a all expression in current type
	*)
	method virtual check : unit
	(**
		Register an error
	*)
	method error msg p =
		errors := { se_msg = msg; se_pos = p; } :: !errors
	(**
		Recursively checks an expression
	*)
	method private check_expr e =
		match e.eexpr with
			| TConst _ -> ()
			| TLocal _ -> ()
			| TArray _ -> ()
			| TBinop _ -> ()
			| TField (target, access) -> self#check_field target access
			| TTypeExpr _ -> ()
			| TParenthesis _ -> ()
			| TObjectDecl _ -> ()
			| TArrayDecl _ -> ()
			| TCall (target, args) ->
				self#check_expr target;
				List.iter self#check_expr args
			| TNew _ -> ()
			| TUnop _ -> ()
			| TFunction fn -> self#check_expr fn.tf_expr
			| TVar _ -> ()
			| TBlock exprs -> List.iter self#check_expr exprs
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
	(**
		Make sure nobody tries to access a field on a nullable value
	*)
	method private check_field target access =
		if is_explicit_null target.etype then
			self#error ("Cannot access \"" ^ accessed_field_name access ^ "\" of a nullable value") target.epos
end

class class_checker cls com =
	object (self)
		inherit base_checker com
	(**
		Entry point for checking a class
	*)
	method check =
		let check_field f =
			Option.may self#check_expr f.cf_expr
		in
		List.iter check_field cls.cl_ordered_fields;
		List.iter check_field cls.cl_ordered_statics;
		match cls.cl_init with
			| Some e -> self#check_expr e
			| None -> ()
end

let run = vfun0 (fun () ->
	errors := [];
	let com = (get_ctx()).curapi.get_com() in
	add_typing_filter com (fun types ->
		(* let t = macro_timer ctx ["safety plugin"] in *)
		let rec traverse com_type =
			match com_type with
				| TClassDecl cls -> (new class_checker cls com)#check
				| TEnumDecl enm -> ()
				| TTypeDecl typedef -> ()
				| TAbstractDecl abstr -> ()
		in
		List.iter traverse types;
		if not (raw_defined com "SAFETY_SILENT") then
			List.iter (fun err -> com.error err.se_msg err.se_pos) !errors;
		(* t() *)
	);
	(* This is because of vfun0 should return something *)
	vint32 (Int32.of_int 0);
)

let get_errors = vfun0 (fun () ->
	let arr = Array.make (List.length !errors) vnull in
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
				set_item idx err.se_msg err.se_pos;
				traverse (idx + 1) errors
			| [] -> ()
	in
	traverse 0 !errors;
	(* VArray arr *)
	VArray (EvalArray.create arr)
)
;;

EvalStdLib.StdContext.register [("run", run); ("getErrors", get_errors)]