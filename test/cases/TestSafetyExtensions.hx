package cases;

using Safety;

class TestSafetyExtensions extends BaseCase {
	public function testOr() {
		var s:Null<String> = null;
		assert.equals('hello', s.or('hello'));
		s = 'world';
		assert.equals('world', s.or('hello'));
	}

	public function testSure() {
		var s:Null<String> = null;
		assert.raises(() -> s.sure(), safety.NullPointerException);
		s = 'hello';
		assert.equals('hello', s.sure());
	}

	public function testUnsafe() {
		var nullable:Null<String> = null;
		var s:String = nullable.unsafe();
		assert.isTrue(null == s);
	}

	public function testLet() {
		var s:Null<String> = null;
		var result = s.let(_ -> {
			assert.fail();
			'wrong';
		});
		assert.isTrue(null == result);

		s = 'hello';
		result = s.let(h -> '$h, world');
		assert.isTrue('hello, world' == result);
	}

	public function testRun() {
		var s:Null<String> = null;
		s.run(_ -> assert.fail());

		s = 'hello';
		var invoked = false;
		s.run(h -> {
			invoked = true;
			assert.equals('hello', h);
		});
		assert.isTrue(invoked);
	}

	public function testApply() {
		var s:Null<String> = null;
		var result = s.apply(_ -> assert.fail());
		assert.isTrue(result == s);

		s = 'hello';
		var invoked = false;
		result = s.apply(h -> {
			invoked = true;
			assert.isTrue(s == h);
		});
		assert.isTrue(s == result);
		assert.isTrue(invoked);
	}
}