package safety;

#if (haxe >= version('4.1.0-rc.1'))
class SafetyException extends haxe.Exception {}
#else
class SafetyException {
	public var message:String;

	public function new(msg:String) {
		message = msg;
	}

	public function toString():String {
		return message;
	}
}
#end