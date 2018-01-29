package cases;

import utest.Assert;
import safety.NullPointerException;

using Safety;

class TestSafeApi extends BaseCase {
	static public function strArg(str:String) {}
	static public function intArg(int:Int) {}
	static public function nullArg(b:Null<Bool>) {}
	static public function optArg(str:String = null) {}
	static public function optNoTypeArg(i = 0) {}

	public function testPassingNullToNotNullableArg_throwsNullPointerException() {
		Assert.raises(
			() -> strArg((null:Unsafe<String>)),
			NullPointerException
		);
	}

	#if !static
	static public function testPassingNull_toBasicType_throwsNullPointerException() {
		Assert.raises(
			() -> intArg((null:Unsafe<Int>)),
			NullPointerException
		);
	}

	static public function testPassingNull_toOptionalArgumentWithoutType_doesNotThrow() {
		optNoTypeArg(null);
		Assert.pass();
	}
	#end

	static public function testPassingNull_toNullType_doesNotThrow() {
		nullArg(null);
		Assert.pass();
	}

	static public function testPassingNull_toOptionalArgument_doesNotThrow() {
		optArg(null);
		Assert.pass();
	}
}