package cases;

import utest.Assert;

using Safety;

class TestSafetyExtensions extends BaseCase {
	static var tmp:Any = {};
	static function dummyCall(fn:Any) {
		tmp = fn;
	}

	public function testOr() {
		var s:Null<String> = null;
		Assert.equals('hello', s.or('hello'));
		s = 'world';
		Assert.equals('world', s.or('hello'));
		dummyCall(() -> s = null);
		if(s != null) {
			var n:String = s.or('fd');
		}
	}

	public function testSure() {
		var s:Null<String> = null;
		Assert.raises(function() s.sure(), safety.NullPointerException);
		s = 'hello';
		Assert.equals('hello', s.sure());
	}

	public function testUnsafe() {
		var nullable:Null<String> = null;
		var s:String = nullable.unsafe();
		Assert.isTrue(null == s);
	}

	public function testCheck() {
		var s:Null<String> = null;
		Assert.isFalse(s.check(s -> true));

		s = 'hello';
		Assert.isFalse(s.check(s -> s == 'world'));
		Assert.isTrue(s.check(s -> s == 'hello'));
	}

	public function testLet() {
		var s:Null<String> = null;
		var result = s.let(function(_) {
			Assert.fail();
			return 'wrong';
		});
		Assert.isTrue(null == result);

		s = 'hello';
		result = s.let(function(h) return '$h, world');
		Assert.isTrue('hello, world' == result);
	}

	public function testRun() {
		var s:Null<String> = null;
		s.run(function(_) Assert.fail());

		s = 'hello';
		var invoked = false;
		s.run(function(h) {
			invoked = true;
			Assert.equals('hello', h);
		});
		Assert.isTrue(invoked);
	}

	public function testApply() {
		var s:Null<String> = null;
		var result = s.apply(function(v) dummyCall(v));
		Assert.isTrue(result == s);

		s = 'hello';
		var invoked = false;
		result = s.apply(function(h) {
			invoked = true;
			// Assert.isTrue(s == h);
		});
		Assert.isTrue(s == result);
		Assert.isTrue(invoked);
	}
}