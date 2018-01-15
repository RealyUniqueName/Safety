package safety;

class NullPointerException {
	public var message(default,null):String;

	public function new(msg:String = "Null pointer") {
		message = msg;
	}
}