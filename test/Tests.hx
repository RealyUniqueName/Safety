package;

import Validator.shouldFail;

using Safety;

private enum DummyEnum {
	DummyOne;
	DummyTwo(a:Int, ?b:String);
}

@:build(Validator.checkFields())
class Tests
{
	@:shouldFail var notInitializedField:Int;
	@:shouldFail var notInitializedProperty(default,null):Float;
	@:shouldFail @:isVar var notInitializedIsVar(get,set):String;
	function get_notInitializedIsVar() return notInitializedIsVar;
	function set_notInitializedIsVar(v) return notInitializedIsVar = v;

	var initialized:Bool = false;
	var initializedInConstructor:String;

	/**
	 *  Null safety should work in __init__ functions
	 */
	static function __init__() {
		var s:Null<String> = 'hello';
		shouldFail(s.length);
	}

	/**
	 *  Null safety should work in constructors
	 */
	function new(a:String) {
		var s:Null<String> = 'hello';
		shouldFail(s.length);
	}

	static function fieldAccess_onNullableValue_shouldFail():Void {
		var a:Null<String> = "hello";
		shouldFail(a.length);
	}

	static function fieldAccess_onNullableValueInIfCondition_shouldFail():Void {
		var a:Null<String> = "hello";
		shouldFail(if(a.length == 0) {});
	}

	static function fieldAccess_onNotNullableValue_shouldPass():Void {
		var a:String = "hello";
		a.length;
	}

	static function fieldAccess_onOptionalNullableValue_shouldFail(a:String = null, ?b:String):Void {
		shouldFail(a.length);
		shouldFail(b.length);
	}

	static function fieldAccess_onOptionalNotNullableValue_shouldPass(a:String = 'hello'):Void {
		a.length;
	}

	static function call_onNullableValue_shouldFail() {
		var fn:Null<Void->Void> = function() {}
		shouldFail(fn());
	}

	static function call_onNotNullableValue_shouldPass() {
		var fn:Void->Void = function() {}
		fn();
	}

	static function call_nullableValueToNotNullableArgument_shouldFail() {
		var fn = function(a:String) {}
		var v:Null<String> = 'hello';
		shouldFail(fn(v));
		shouldFail(new Tests(v));
	}

	static function call_nullableValueToOptionalArgument_shouldPass() {
		var fn = function(?a:Int) {}
		var v:Null<Int> = 1;
		fn(v);
	}

	static function varDecl_assignNullableValueToNotNullableVar_shouldFail() {
		var v:Null<String> = 'hello';
		shouldFail(var s:String = v);
		shouldFail(var s:String = null);
	}

	static function assign_nullableValueToNotNullable_shouldFail() {
		var a:Null<Int> = 0;
		var b = 10;
		shouldFail(b = a);
	}

	static function assign_notNullableValueToNullable_shouldPass() {
		var a:Null<Int> = null;
		var b = 10;
		a = b;
	}

	static function binop_withNullableValue_shouldFail() {
		var a:Null<Int> = 0;
		var b = 10;
		shouldFail(a + b);
	}

	static function binop_comparisonWithNullableValue_shouldPass() {
		var a:Null<Int> = 0;
		var b = 10;
		a == b;
		a != b;
	}

	static function unop_nullableValue_shouldFail() {
		var a:Null<Int> = 0;
		shouldFail(a++);
	}

	static function ternary_nullableElse_assignToNotNullableValue_shouldFail() {
		var v:Null<String> = 'a';
		var a:String;
		shouldFail((true ? 'hello' : v).length);
	}

	static function arrayAccess_nullableArray_shouldFail() {
		var a:Null<Array<Int>> = [];
		shouldFail(a[0]);
	}

	static function arrayAccess_usingNullableIndex_shouldFail() {
		var a = [0];
		var idx:Null<Int> = 0;
		shouldFail(a[idx]);
	}

	static function if_nullableCondition_shouldFail() {
		var s:Null<Bool> = false;
		shouldFail(if(s) {});
	}

	static function typeInference_arrayAccess_fieldOnNullableItem_shouldFail() {
		var a:Array<Null<String>> = [];
		shouldFail(a[0].length);
	}

	static function typeInference_assignNullableValueToVariableWithoutExplicitTyping_shouldPass(nullable:String = null) {
		var s = nullable;
	}

	static function typeInference_fieldAccessOnInferredNullableType_shouldFail(nullable:Null<String>) {
		var s = nullable;
		shouldFail(s.length);
	}

	static var notNullableSetter(default,set):String = 'hello';
	static function set_notNullableSetter(v) return notNullableSetter = v;
	static function setter_passNullableValueToNotNullableSetter_shouldFail(?v:String) {
		shouldFail(notNullableSetter = v);
	}

	static function checkAgainstNull_transferSafeNullableLocalToNotNullable_shouldPass(?a:String) {
		var s:String;
		if(a == null) {} else s = a;
		if(null == a) {} else s = a;
		if(a != null) s = a;
		if(null != a) s = a;
		s = (a == null ? 'hello' : a);
		s = (null == a ? 'hello' : a);
		s = (a != null ? a : 'hello');
		s = (null != a ? a : 'hello');
		s = if(a == null) {
			'hello';
		} else {
			'other expressions';
			a;
		}
	}

	static function checkAgainstNull_checkAndFieldAccess(?a:String) {
		var s:Null<String> = 'hello';
		if(s != null && s.length == 0) {}
		if(s == null || s.length == 0) {}
		s != null && s.length == 0;
		s == null || s.length == 0;

		shouldFail(if(s != null || s.length == 0) {});
		shouldFail(if(s == null && s.length == 0) {});
		shouldFail(s != null || s.length == 0);
		shouldFail(s == null && s.length == 0);

		//checked against not-nullable value, so it's not null
		var nullable:Null<String> = 'hello';
		var s = 'world';
		if(nullable == s) {
			s = nullable;
		} else {
			shouldFail(s = nullable);
		}
	}

	static function checkAgainstNull_complexConditions() {
		var nullable:Null<String> = 'hello';
		var s:String;
		if(nullable != null && true) {
			s = nullable;
		}
		else {
			shouldFail(s = nullable);
		}
		if(false && (true || false) && null == nullable) {
			shouldFail(s = nullable);
		} else {
			s = nullable;
		}
		if(true || nullable != null) {
			shouldFail(s = nullable);
		} else {
			shouldFail(s = nullable);
		}
	}

	static function return_nullableValueFromNotNullableResult_shouldFail(?a:String):String {
		function local():String {
			shouldFail(return a);
		}
		shouldFail(return a);
	}

	static function objectDecl_fieldsExpressions_shouldBeChecked(?a:String) {
		var s:String;
		var o = {
			field: shouldFail(s = a)
		}
	}

	// static function objectDecl_passObjWithNullabelFieldToObjWithNotNullableField_shouldFail(?a:String) {
	// 	shouldFail(var o:{field:String} = {field:a});
	// }

	static function for_iterateOverNullableValue_shouldFail(?a:Iterable<Int>) {
		for(i in shouldFail(a)) {}
	}

	static function while_nullableCondition_shouldFail(?a:Bool) {
		shouldFail(while(a) {});
	}

	static function while_checkAgainstNullInConditionAndUseInBody(?a:Bool) {
		var b:Bool;
		while(a != null) b = a;
		do shouldFail(b = a) while(a != null);
		while(a == null) shouldFail(b = a);
	}

	static function throw_nullableValue_shouldFail() {
		var s:Null<String> = 'hello';
		shouldFail(throw s);
	}

	static function arrayDeclaration_shouldCheck(?a:String) {
		var s:String;
		shouldFail([s = a]);
	}

	static function tryCatch_shouldCheck(?a:String) {
		var s:String;
		try {
			shouldFail(s = a);
		} catch(e:Dynamic) {
			shouldFail(s = a);
		}
	}

	static function cast_nullableExprToNotNullableType_shouldFail() {
		var s:Null<String> = 'hello';
		shouldFail((s:String));
		shouldFail(cast(s, String));
	}

	static function cast_nullableExprToNullableType_shouldPass() {
		var s:Null<String> = 'hello';
		function dummy(?a:String) {}
		dummy(cast s);
	}

	static function enum_switchOnNullableEnum_shouldFail(e:Null<DummyEnum>) {
		switch shouldFail(e) {
			case DummyOne:
			case DummyTwo(a, b):
		}
	}

	// TODO far far future
	// static function checkAgainstNull_assignCheckedValueToVarWithoutExplicitType_shouldTypeAsNotNullable() {
	// 	var nullable:Null<String> = 'hello';
	// 	var v = nullable != null ? nullable : 'world';
	// 	var s:String = v;
	// }

	// static function switch_onNullableValue_shouldFail() {
	// 	var nullable:Null<String> = 'hello';

	// 	shouldFail(switch(nullable) {
	// 		case _:
	// 	});

	// 	// switch(nullable) {
	// 	// 	case v if(Std.random(2) == 1):
	// 	// 		shouldFail(s = v);
	// 	// 		shouldFail(s = nullable);
	// 	// 	case null:
	// 	// 		shouldFail(s = nullable);
	// 	// 	case v if(Std.random(2) == 1):
	// 	// 		s = v;
	// 	// 		s = nullable;
	// 	// 	case v:
	// 	// 		s = v;
	// 	// 		s = nullable;
	// 	// }
	// }

	// static function switch_checkedAgainstNullInGuard() {
	// 	var nullable:Null<String> = 'hello';
	// 	var s:String;
	// 	switch('world') {
	// 		case v if(nullable == 'rnd'):
	// 			shouldFail(s = v);
	// 			shouldFail(s = nullable);
	// 		case v if(nullable != null):
	// 			shouldFail(s = v);
	// 			s = nullable;
	// 		case null:
	// 			shouldFail(s = nullable);
	// 		case v if(nullable == 'rnd'):
	// 			s = v;
	// 		case v
	// 	}
	// }

	// static function abstractOverNul_consideredAsNullable() {
	// 	var s:String
	// }
}

