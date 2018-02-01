package safety.macro;

import haxe.macro.Expr;

typedef SafetyPluginApi = {
	/** This method should be executed at initialization macro time */
	function run():Void;
	/** Check specified dot-path for null safety. */
	function addPath(path:String):Void;
	/** Returns a list of all errors found during safety checks */
	function getErrors():Array<{msg:String, pos:Position}>;
	/** Returns a list of all warnings found during safety checks */
	function getWarnings():Array<{msg:String, pos:Position}>;
	/** Calls `callback` when all safety checks are finished */
	function onComplete(callback:Void->Void):Void;
}