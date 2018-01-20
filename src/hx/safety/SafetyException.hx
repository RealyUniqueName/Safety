package safety;

class SafetyException {
	public var message(default,null):String;

	public function new(msg:String) {
		message = msg;
	}
}