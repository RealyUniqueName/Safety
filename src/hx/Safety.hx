#if macro
import haxe.macro.PositionTools;
import haxe.macro.Context;
import haxe.macro.Compiler;
import haxe.macro.Expr;
import haxe.io.Path;
import eval.vm.Context in EvalContext;
import safety.macro.SafeAst;

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
	/** Check if current macro position should be handled by Safety (based on `-D SAFETY=` flag) */
	function isInSafety():Bool;
}
#end

class Safety {
	/**
	 *  Prints `true` at compile time if provided expression can not be evaluated to `null` at runtime. Prints `false` otherwise.
	 */
	macro static public function isSafe(expr:Expr):ExprOf<Void> {
		return macro @:pos(expr.pos) @:privateAccess Safety._isSafe($expr);
	}
	static function _isSafe(ident:Dynamic):Void {}; //Handled in plugin

#if macro
	/**
	 *  Add this call to hxml to make public methods in specified `path` to throw `NullPointerException`
	 *  if someone passes `null` as an argument value if that argument is not nullable.
	 *  E.g.:
	 *  ```
	 *  --macro Safety.safeApi('my.pack', true)
	 *  ```
	 *  @param path - Dot-path of a package or a fully qualified type name.
	 *  @param recursive - Should we also apply to all sub-packages of `path`?
	 */
	static public function safeApi(path:String, recursive:Bool = false) {
		SafeAst.addSafeApi(path, recursive);
	}

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
		#if (!SAFETY_DISABLE_SAFE_NAVIGATION || !SAFETY_DISABLE_SAFE_ARRAY)
		Compiler.addGlobalMetadata('', '@:build(safety.macro.SafeAst.buildSafeNavigationAndArray())');
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
			path = Path.join([srcDir, 'bin', Sys.systemName(), 'safety.cmxs']);
		}
		return path;
	}
#else

	/**
	 *  Returns `value` if it is not `null`. Otherwise returns `defaultValue`.
	 */
	static public inline function or<T>(value:Null<T>, defaultValue:T):T {
		return value == null ? defaultValue : (value:Unsafe<T>);
	}

	/**
	 *  Returns `value` if it is not `null`. Otherwise throws an exception.
	 *  @throws NullPointerException if `value` is `null`.
	 */
	static public inline function sure<T>(value:Null<T>):T {
		return value == null ? throw new safety.NullPointerException('Null pointer in .sure() call') : (value:Unsafe<T>);
	}

	/**
	 *  Just returns `value` without any checks, but typed as not-nullable. Use at your own risk.
	 */
	static public inline function unsafe<T>(value:Null<T>):T {
		return (value:Unsafe<T>);
	}

	/**
	 *  Applies `callback` to `value` and returns the result if `value` is not `null`.
	 *  Returns `null` otherwise.
	 */
	static public inline function let<T,V>(value:Null<T>, callback:T->V):Null<V> {
		return value == null ? null : callback((value:Unsafe<T>));
	}

	/**
	 *  Passes `value` to `callback` if `value` is not null.
	 */
	static public inline function run<T>(value:Null<T>, callback:T->Void) {
		if(value != null) callback((value:Unsafe<T>));
	}

	/**
	 *  Applies `callback` to `value` if `value` is not `null`.
	 *  Returns `value`.
	 */
	static public inline function apply<T>(value:Null<T>, callback:T->Void):Null<T> {
		if(value != null) callback((value:Unsafe<T>));
		return value;
	}
#end
}
