#if macro
import haxe.macro.Context;
import haxe.macro.Expr;
import safety.macro.PluginLoadingException;

typedef SafetyMessage = {msg:String, pos:Position}
typedef ExpectedMessage = {symbol:String, pos:Position}
#end

class Validator {
#if macro
	static var expectedErrors:Array<ExpectedMessage> = [];
	static var expectedWarnings:Array<ExpectedMessage> = [];

	static public function register() {
		if(Context.defined('display')) return;
		try {
			Safety.plugin.onComplete(validate);
		} catch(e:PluginLoadingException) {
			//ignore this exception. It should be handled in Safety.hx at this point.
			#if SAFETY_DEBUG
			trace('Failed to load plugin: ${e.message}');
			#end
		}
		expectedErrors = [];
		expectedWarnings = [];
	}

	static public function checkFields():Array<Field> {
		for(field in Context.getBuildFields()) {
			for(meta in field.meta) {
				if(meta.name == ':shouldFail') {
					expectedErrors.push({symbol: field.name, pos:field.pos});
					break;
				}
				if(meta.name == ':shouldWarn') {
					expectedWarnings.push({symbol: field.name, pos:field.pos});
					break;
				}
			}
		}
		return null;
	}

	static function validate() {
		try {
			var errors = check(expectedErrors, Safety.plugin.getErrors(), 'fail');
			var warnings = check(expectedWarnings, Safety.plugin.getWarnings(), 'warn');
			if(errors.ok && warnings.ok) {
				Sys.println('${warnings.passed} expected warnings spotted');
				Sys.println('${errors.passed} expected errors spotted');
				Sys.println('Compile-time tests passed.');
			} else {
				if(!Context.defined('VALIDATOR_DONT_FAIL')) {
					Context.error('Tests failed. See warnings.', Context.currentPos());
				}
			}
		} catch(e:PluginLoadingException) {
			//ignore this exception. It should be handled in Safety.hx at this point.
			#if SAFETY_DEBUG
			trace('Failed to load plugin: ${e.message}');
			#end
		}
	}

	static function check(expected:Array<ExpectedMessage>, actual:Array<SafetyMessage>, expectation:String):{ok:Bool, passed:Int} {
		var passed = 0;
		var i = 0;
		while(i < actual.length) {
			var actualEvent = actual[i];
			var wasExpected = false;
			for(expectedEvent in expected) {
				if(posContains(expectedEvent.pos, actualEvent.pos)) {
					expected.remove(expectedEvent);
					wasExpected = true;
					break;
				}
			}
			if(wasExpected) {
				actual.remove(actualEvent);
				++passed;
			} else {
				++i;
			}
		}

		for(event in actual) {
			Context.warning(event.msg, event.pos);
		}
		for(event in expected) {
			Context.warning('${event.symbol} was expected to $expectation, but it did not $expectation.', event.pos);
		}
		return {
			ok: actual.length == 0 && expected.length == 0,
			passed: passed
		}
	}

	static function posContains(pos:Position, subPos:Position):Bool {
		var infos = Context.getPosInfos(pos);
		var subInfos = Context.getPosInfos(subPos);
		return infos.file == subInfos.file && infos.min <= subInfos.min && infos.max >= subInfos.max;
	}
#end

	macro static public function shouldFail(expr:Expr):Expr {
		if(!Context.defined('display')) {
			expectedErrors.push({symbol:Context.getLocalMethod(), pos:expr.pos});
		}
		return expr;
	}

	macro static public function shouldWarn(expr:Expr):Expr {
		if(!Context.defined('display')) {
			expectedWarnings.push({symbol:Context.getLocalMethod(), pos:expr.pos});
		}
		return expr;
	}
}