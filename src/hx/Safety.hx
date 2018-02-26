#if macro
import haxe.macro.PositionTools;
import haxe.macro.Context;
import haxe.macro.Expr;
import haxe.io.Path;
import safety.macro.SafetyPluginApi;
import safety.macro.SafeAst;
import safety.macro.PluginLoadingException;

using haxe.io.Path;
using sys.FileSystem;
#end

class Safety {

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

	/**
	 *  Prints `true` at compile time if provided expression can not be evaluated to `null` at run time. Prints `false` otherwise.
	 *  Always prints `false` if invoked outside of a path passed to `Safety.enable()`
	 */
	macro static public function isSafe(expr:Expr):ExprOf<Void> {
		return macro @:pos(expr.pos) @:privateAccess Safety._isSafe($expr);
	}
	static function _isSafe(ident:Dynamic):Void {}; //Handled in plugin

#if macro
	/**
	 *  Enable null safety checking for specified dot-path.
	 *  @param path - Dot-path of a package or a fully qualified type name.
	 *  @param enableAdditionalFeatures - Enable all additional features on that `path`. You can enable each feature separately instead. See `Safety.safe*()` methods.
	 */
	static public function enable(path:String, enableAdditionalFeatures:Bool = true) {
		if(enableAdditionalFeatures) {
			SafeAst.addSafeApi(path, true);
			SafeAst.addSafeNavigation(path, true);
			SafeAst.addSafeArray(path, true);
		}

		if(Context.defined('display')) {
			return;
		}

		#if (haxe_ver < '4.0.0')
		Context.warning('Null safety is disabled: at least Haxe 4.0.0-preview.3 is required.', Context.currentPos());
		return;
		#end

		try {
			plugin.addPath(path);
		} catch(e:PluginLoadingException) {
			#if SAFETY_DEBUG
			trace('Failed to load plugin: ${e.message}');
			#end
			Context.warning('Null safety is disabled: current build of Safety is not compatible with your build of Haxe compiler. You may want to rebuild Safety to enable null safety (see README.md).', Context.currentPos());
		}
	}

	/**
	 *  Add this call to hxml to make public methods in specified `path` to throw `NullPointerException`
	 *  if someone passes `null` as an argument value and that argument is not nullable.
	 *  ```
	 *  public function job(str:String) {...}
	 *  <...>
	 *  job(null); //throws safety.NullPointerException
	 *  ```
	 *  Example for hxml:
	 *  ```
	 *  --macro Safety.safeApi('my.pack', true)
	 *  ```
	 *  @param path - Dot-path of a package or a fully qualified type name.
	 *  @param recursive - Should we also apply to all sub-packages of `path`?
	 */
	static public function safeApi(path:String, recursive:Bool = true) {
		SafeAst.addSafeApi(path, recursive);
	}

	/**
	 *  Add this call to hxml to enable safe navigation operator `!.`:
	 *  ```
	 *  var s:Null<String> = null;
	 *  trace(s!.length); //null
	 *  s = 'wow';
	 *  trace(s!.length); //3
	 *  ```
	 *  Example for hxml:
	 *  ```
	 *  --macro Safety.safeNavigation('my.pack', true)
	 *  ```
	 *  @param path - Dot-path of a package or a fully qualified type name.
	 *  @param recursive - Should we also apply to all sub-packages/sub-types of `path`?
	 */
	static public function safeNavigation(path:String, recursive:Bool = true) {
		SafeAst.addSafeNavigation(path, recursive);
	}

	/**
	 *  Add this call to hxml to enable auto-casting all array declarations to `SafeArray`:
	 *  Example for hxml:
	 *  ```
	 *  --macro Safety.safeArray('my.pack', true)
	 *  ```
	 *  @param path - Dot-path of a package or a fully qualified type name.
	 *  @param recursive - Should we also apply to all sub-packages/sub-types of `path`?
	 */
	static public function safeArray(path:String, recursive:Bool = true) {
		SafeAst.addSafeArray(path, recursive);
	}

	static public var plugin(get,never):SafetyPluginApi;
	static var _plugin:SafetyPluginApi;
	static function get_plugin():SafetyPluginApi {
		#if (haxe_ver < '4.0.0')
		throw new PluginLoadingException('Haxe >= 4.0.0 is required to load Safety plugin.');
		#else
		if(_plugin == null) {
			try {
				_plugin = eval.vm.Context.loadPlugin(getPluginPath());
			} catch(e:Dynamic) {
				throw new PluginLoadingException(Std.string(e));
			}
		}
		#end
		return _plugin;
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

	static public function register() {
		if(Context.defined('display')) {
			return;
		}

		try {
			plugin.run();
		} catch(e:PluginLoadingException) {
			//Ignore plugin loading errors at this point. Will handle them on `Safety.enable()`
			#if SAFETY_DEBUG
			trace('Failed to load plugin: ${e.message}');
			#end
		}
	}
#end
}