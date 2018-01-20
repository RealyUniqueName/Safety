package safety;

class OutOfBoundsException extends SafetyException {
	public function new(msg:String = "Out of bounds") {
		super(msg);
	}
}