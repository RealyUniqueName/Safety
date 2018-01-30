package cases;

import utest.Assert;
import safety.OutOfBoundsException;

class TestSafeArray extends BaseCase {

	public function testRead_outOfBounds_shouldThrow() {
		var a:SafeArray<Int> = [];
		Assert.raises(function() a[0], OutOfBoundsException);
		Assert.raises(function() a[-1], OutOfBoundsException);
	}

	public function testWrite_outOfBounds_shouldThrow() {
		var a:SafeArray<Int> = [];
		Assert.raises(function() a[10] = 2, OutOfBoundsException);
		Assert.raises(function() a[-1] = 2, OutOfBoundsException);
	}

	public function testWrite_atLength_shouldPass() {
		var a:SafeArray<Int> = [];
		a[a.length] = 5;
		Assert.same([5], a);
	}

	public function testArrayDeclaration_automaticallyConvertedToSafeArray() {
		var a = ["hello", "wtf"];
		//If `a` was automatically typed as `SafeArray` out-of-bounds reading will throw
		Assert.raises(function() a[10], OutOfBoundsException);
	}

	public function testArrayDeclaration_inSwitchAndCase_shouldNotBeConverted() {
		//this test should pass compilation
		switch([Std.random(2), Std.random(2)]) {
			case [0, 1]: Assert.pass();
			case [1, 0]: Assert.pass();
			case _: Assert.pass();
		}
	}

	public function testMapDeclaration_shouldNotBeAffected() {
		var map:Map<String,String> = ['hello' => 'world'];
		Assert.isTrue(map.exists('hello'));
	}

	public function testMapDeclaration_withArrayComprehension_shouldNotBeAffected() {
		var map:Map<String,String> = [for(i in 0...1) 'hello' => 'world'];
		Assert.isTrue(map.exists('hello'));
	}
}