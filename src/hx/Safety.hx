#if macro
import haxe.macro.PositionTools;
import haxe.macro.Context;
import haxe.io.Path;
import eval.vm.Context in EvalContext;

using haxe.macro.PositionTools;
using haxe.io.Path;
#end

class Safety {
#if macro
	static public function register() {
		if(haxe.macro.Context.defined('display')) {
			return;
		}

		var module:{run:Void->Void} = EvalContext.loadPlugin(getPluginPath());
		module.run();
	}

	static public function getPluginPath():String {
		var pos = Context.getPosInfos(PositionTools.here());
		var srcDir = pos.file.directory().directory();
		return Path.join([srcDir, 'ml', 'safety.cmxs']);
	}
#else

	static public inline function or<T>(value:Null<T>, defaultValue:T):T {
		return (value == null ? defaultValue : value);
	}

	static public inline function unsafe<T>(value:Null<T>):T {
		return (value:T);
	}
#end
}
