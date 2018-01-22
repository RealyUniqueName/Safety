import safety.OutOfBoundsException;

/**
 *  `SafeArray` throws `safety.OutOfBoundsException` when trying to read or write out of array bounds.
 *  Writing at an index of array's length is allowed.
 *  ```
 *  	var a:SafeArray<String> = ["hello"];
 *  	trace(a[100]); //exception
 *  	a[1] = "world"; //ok
 *  	a[100] = "too far"; //exception
 *  ```
 */
@:forward
abstract SafeArray<T>(Array<T>) from Array<T> to Array<T> {
	@:arrayAccess inline function get(index:Int):T {
		if(index < 0 || index >= this.length) {
			throw new OutOfBoundsException('Reading out of array bounds. Array length: ${this.length}. Accessed index: $index.');
		}
		return this[index];
	}

	@:arrayAccess inline function set(index:Int, value:T):T {
		if(index < 0 || index > this.length) {
			throw new OutOfBoundsException('Writing out of array bounds. Array length: ${this.length}. Accessed index: $index.');
		}
		return this[index] = value;
	}

	public inline function iterator():SafeArrayIterator<T> {
		return new SafeArrayIterator(this);
	}
}

private class SafeArrayIterator<T> {
	var array:Array<T>;
	var current:Int = 0;
	public inline function new(array:Array<T>) this.array = array;
	public inline function hasNext() return current < array.length;
	public inline function next() return array[current++];
}