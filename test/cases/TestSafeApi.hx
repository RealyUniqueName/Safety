package cases;

import utest.Assert;
import haxe.Unsafe;
import safety.IllegalArgumentException;

class TestSafeApi extends BaseCase {
	static public function strArg(str:String) {}
	static public function intArg(int:Int) {}
	static public function nullArg(b:Null<Bool>) {}
	static public function noTypeArg(a) { a == 1; /* infer Int */ }
	static public function optArg(str:String = null) {}
	static public function optNoTypeArg(i = 0) {}

	public function testPassingNullToNotNullableArg_throwsNullPointerException() {
		Assert.raises(
			function() strArg((null:Unsafe<String>)),
			IllegalArgumentException
		);
	}

#if !(cross || flash || cpp || cs || hl || java)
	static public function testPassingNull_toBasicType_throwsNullPointerException() {
		Assert.raises(
			function() intArg((null:Unsafe<Int>)),
			IllegalArgumentException
		);
	}

	static public function testPassingNull_toOptionalArgumentWithoutType_doesNotThrow() {
		optNoTypeArg(null);
		Assert.pass();
	}

	//this test is under `#if !static` because `noTypeArg()` has Int argument, and `null` is not allowed for `Int` on static targets.
	public function testPassingNullToNoTypeArg_throwsNullPointerException() {
		Assert.raises(
			function() noTypeArg((null:Unsafe<Dynamic>)),
			IllegalArgumentException
		);
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