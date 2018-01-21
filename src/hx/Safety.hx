#if macro
import haxe.macro.PositionTools;
import haxe.macro.Context;
import haxe.macro.Compiler;
import haxe.macro.Expr;
import haxe.io.Path;
import eval.vm.Context in EvalContext;

using haxe.macro.PositionTools;
using haxe.io.Path;
using sys.FileSystem;

typedef SafetyPluginApi = {
	/** This method should be executed at initialization macro time */
	function run():Void;
	/** Returns a list of all errors found during safety checks */
	function getErrors():Array<{msg:String, pos:Position}>;
	/** Returns a list of all warnings found during safety checks */
	function getWarnings():Array<{msg:String, pos:Position}>;
	/** Check if current macro position should be handled by Safety (based on `-D SAFETY=` flag) for preprocessing safe-call operator `!.` */
	function isInSafety():Bool;
}
#end

class Safety {
#if macro
	static public var plugin(get,never):SafetyPluginApi;
	static var _plugin:SafetyPluginApi;
	static function get_plugin():SafetyPluginApi {
		if(_plugin == null) {
			try {
				_plugin = EvalContext.loadPlugin(getPluginPath());
			} catch(e:Dynamic) {
				#if SAFETY_DEBUG
				trace('Failed to load plugin: $e');
				#end
				Context.error('Current build of Safety is not compatible with your build of Haxe compiler. You need to rebuild Safety (see README.md)', Context.currentPos());
			}
		}
		return _plugin;
	}

	static public function register() {
		#if !(SAFETY_DISABLE_SAFE_NAVIGATION || SAFETY_DISABLE_SAFE_ARRAY)
		Compiler.addGlobalMetadata('', '@:build(safety.macro.SafeAst.build())');
		#end
		if(haxe.macro.Context.defined('display')) {
			return;
		}
		if(!Context.defined('SAFETY')) {
			Context.error('-D SAFETY is not defined. Define it like "-D SAFETY=SomeClass,my.pack,another.pack.AnotherClass,/path/to/dir"', Context.currentPos());
		}
		plugin.run();
	}

	static public function getPluginPath():String {
		var pos = Context.getPosInfos(PositionTools.here());
		var srcDir = pos.file.directory().directory();
		var path = Path.join([srcDir, 'ml', 'safety.cmxs']); //development path
		//if development binary does not exist, use pre built one
		if(!path.exists()) {
			path = Path.join([srcDir, 'ml', 'bin', Sys.systemName(), 'safety.cmxs']);
		}
		return path;
	}
#else

	/**
	 *  Returns `value` if it is not `null`. Otherwise returns `defaultValue.
	 */
	static public function or<T>(value:Null<T>, defaultValue:T):T {
		return value == null ? defaultValue : value;
	}

	/**
	 *  Returns `value` if it is not `null`. Otherwise throws an exception.
	 *  @throws NullPointerException if `value` is `null`.
	 */
	static public function sure<T>(value:Null<T>):T {
		return value == null ? throw new safety.NullPointerException() : value;
	}

	/**
	 *  Just returns `value` without any checks, but typed as not-nullable. Use at your own risk.
	 */
	static public function unsafe<T>(value:Null<T>):T {
		return value;
	}

	/**
	 *  Applies `callback` to `value` and returns the result if `value` is not `null`.
	 *  Returns `null` otherwise.
	 */
	static public inline function let<T,V>(value:Null<T>, callback:T->V):Null<V> {
		return value == null ? null : callback(unsafe(value));
	}

	/**
	 *  Passes `value` to `callback` if `value` is not null.
	 */
	static public inline function run<T>(value:Null<T>, callback:T->Void) {
		if(value != null) callback(unsafe(value));
	}

	/**
	 *  Applies `callback` to `value` if `value` is not `null`.
	 *  Returns `value`.
	 */
	static public inline function apply<T>(value:Null<T>, callback:T->Void):Null<T> {
		if(value != null) callback(unsafe(value));
		return value;
	}

	/**
	 *  TODO
	 *  Elvis operator implementation.
	 *  Or maybe make it a build macro which transforms postifx `!` operator.
	 */
	static public inline function sh<T>(value:Null<T>):T {
		throw "TODO";
	}
#end
}
