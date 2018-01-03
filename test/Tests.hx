package;

import Validator.shouldFail;

using Safety;

class Tests
{
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
	function new() {
		var s:Null<String> = 'hello';
		shouldFail(s.length);
	}

	static function fieldAccess_onNullableValue_shouldFail():Void {
		var a:Null<String> = "hello";
		shouldFail(a.length);
	}

	static function fieldAccess_onNonNullableValue_shouldPass():Void {
		var a:String = "hello";
		a.length;
	}

	static function fieldAccess_onOptionalNullableValue_shouldFail(a:String = null, ?b:String):Void {
		shouldFail(a.length);
		shouldFail(b.length);
	}

	static function fieldAccess_onOptionalNonNullableValue_shouldPass(a:String = 'hello'):Void {
		a.length;
	}

	static function call_onNullableValue_shouldFail() {
		var fn:Null<Void->Void> = function() {}
		shouldFail(fn());
	}

	static function call_onNonNullableValue_shouldPass() {
		var fn:Void->Void = function() {}
		fn();
	}

	static function call_nullableValueToNonNullableArgument_shouldFail() {
		var fn = function(a:String) {}
		var v:Null<String> = 'hello';
		shouldFail(fn(v));
		// shouldFail(new Tests(v));
	}

	static function call_nullableValueToOptionalArgument_shouldPass() {
		var fn = function(?a:Int) {}
		var v:Null<Int> = 1;
		fn(v);
	}

	static function varDecl_assignNullableValueToNonNullableVar_shouldFail() {
		var v:Null<String> = 'hello';
		shouldFail(var s:String = v);
		shouldFail(var s:String = null);
	}

	static function assign_nullableValueToNonNullable_shouldFail() {
		var a:Null<Int> = 0;
		var b = 10;
		shouldFail(b = a);
	}

	static function assign_nonNullableValueToNullable_shouldPass() {
		var a:Null<Int> = null;
		var b = 10;
		a = b;
	}

	static function binop_withNullableValue_shouldFail() {
		var a:Null<Int> = 0;
		var b = 10;
		shouldFail(a + b);
	}

	static function unop_nullableValue_shouldFail() {
		var a:Null<Int> = 0;
		shouldFail(a++);
	}

	static function ternary_nullableElse_assignToNonNullableValue_shouldFail() {
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

	// TODO
	// static function typeInference_arrayAccess_fieldOnNullableItem_shouldFail() {
	// 	var a:Array<Null<String>> = [];
	// 	shouldFail(a[0].length);
	// }

	// static function typeInference_fieldAccessOnInferredNullableType_shouldFail() {
	// 	var nullable:Null<String> = 'hello';
	// 	var s = nullable;
	// 	shouldFail(s.length);
	// }

	// static function typeInference_assignNullableValueToVariableWithoutExplicitTyping_shouldPass() {
	// 	var nullable:Null<String> = 'hello';
	// 	var s = nullable;
	// }
}