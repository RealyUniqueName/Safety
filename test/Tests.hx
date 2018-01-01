package;

import Validator.shouldFail;

using Safety;

class Tests
{
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

}