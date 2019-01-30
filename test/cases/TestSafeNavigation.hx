package cases;

import utest.Assert;

using Safety;

private typedef Dummy = {
	nullable:Null<String>
}

class TestSafeNavigation extends BaseCase {
	public function testSafeAccess_onNullValue_returnsNull() {
		var o:Null<Dummy> = null;
		Assert.isTrue(null == o!.nullable!.length);
	}

	public function testSafeAccess_onNotNull_returnsActualValue() {
		var value = 'hello';
		var o:Dummy = { nullable:value };
		Assert.isTrue(value.length == o!.nullable!.length.unsafe());
	}

	public function testSafeCall_onNullValue_returnsNull() {
		var o:Null<Dummy> = null;
		Assert.isTrue(null == o!.nullable!.toUpperCase());
	}

	public function testSafeArrayAccess_onNullValue_returnsNull() {
		var o:Null<{array:Array<String>}> = null;
		Assert.isTrue(null == o!.array[0]);
	}
}