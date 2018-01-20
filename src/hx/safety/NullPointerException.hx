package safety;

class NullPointerException extends SafetyException {
	public function new(msg:String = "Null pointer") {
		super(msg);
	}
}