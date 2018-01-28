package cases;

import utest.Assert;
import safety.NullPointerException;

using Safety;

class TestSafeApi extends BaseCase {
	static public function dummy(str:String, i:Int, b:Null<Bool>, ?opt:String) {}

	public function testPassingNullToNotNullableArg_throwsNullPointerException() {
		Assert.raises(
			() -> dummy((null:Unsafe<String>), 0, false),
			NullPointerException
		);
	}
}