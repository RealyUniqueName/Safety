package cases;

import utest.Assert;

using Safety;

private typedef Dummy = {
	nullable:Null<String>
}

class TestSafeNavigation extends BaseCase {
#if !SAFETY_DISABLE_SAFE_NAVIGATION
	public function testSafeAccess_onNullValue_returnsNull() {
		var o:Null<Dummy> = null;
		Assert.isTrue(null == o!.nullable!.length);
	}

	public function testSafeAccess_onNotNull_returnsActualValue() {
		var value = 'hello';
		var o:Dummy = { nullable:value };
		Assert.isTrue(value.length == o!.nullable!.length.unsafe());
	}
#end
}