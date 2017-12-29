package;

import Validator.shouldFail;

using Safety;

class Tests
{
	static function fieldAccess_onNullableValue_shouldFail():Void {
		var a:Null<String> = "hello";
		shouldFail(a).length;
	}
}