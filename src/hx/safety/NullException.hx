package safety;

import haxe.PosInfos;

class NullException extends Exception {
	public function new(msg:String = "Cannot perform safe operation on null value", ?pos:PosInfos) {
		super(msg, pos);
	}
}