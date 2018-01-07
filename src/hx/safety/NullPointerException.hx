package safety;

import haxe.PosInfos;

class NullPointerException extends Exception {
	public function new(msg:String = "Null pointer exception", ?pos:PosInfos) {
		super(msg, pos);
	}
}