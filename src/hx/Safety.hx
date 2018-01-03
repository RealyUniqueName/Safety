#if macro
import haxe.macro.PositionTools;
import haxe.macro.Context;
import haxe.macro.Expr;
import haxe.io.Path;
import eval.vm.Context in EvalContext;

using haxe.macro.PositionTools;
using haxe.io.Path;

private typedef PluginApi = {
	/** This method should be executed at initialization macro time */
	function run():Void;
	/** Returns a list of all errors found during safety checks */
	function getErrors():Array<{msg:String, pos:Position}>;
}
#end

class Safety {
#if macro
	static public var plugin:PluginApi = EvalContext.loadPlugin(getPluginPath());

	static public function register() {
		if(haxe.macro.Context.defined('display')) {
			return;
		}
		plugin.run();
	}

	static public function getPluginPath():String {
		var pos = Context.getPosInfos(PositionTools.here());
		var srcDir = pos.file.directory().directory();
		return Path.join([srcDir, 'ml', 'safety.cmxs']);
	}
#else

	/**
	 *  Returns `value` if it is not `null`. Otherwise returns `defaultValue.
	 */
	static public inline function or<T>(value:Null<T>, defaultValue:T):T {
		return (value == null ? defaultValue : value);
	}

	/**
	 *  Returns `value` if it is not `null`. Otherwise throws an exception.
	 *  @throws NullException if `value` is `null`.
	 */
	static public inline function safe<T>(value:Null<T>):T {
		return (value == null ? throw new safety.NullException() : value);
	}

	/**
	 *  Always returns `value`, but typed as non-nullable. Use at your own risk.
	 */
	static public inline function unsafe<T>(value:Null<T>):T {
		return (value:T);
	}
#end
}
