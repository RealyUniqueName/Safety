package safety;

import haxe.macro.Context;
import haxe.macro.Expr;

using haxe.macro.ExprTools;

class SafeNavigationOperator {
	static var foundSafeCall:Bool = false;

	macro static public function build():Null<Array<Field>> {
		if(!Safety.plugin.isInSafety()) {
			return null;
		}

		var fields = [];
		for(field in Context.getBuildFields()) {
			var expr = switch(field.kind) {
				case FVar(_, e): e;
				case FProp(_, _, _, e): e;
				case FFun(_.expr => e): e;
			}
			if(expr != null) {
				expr.expr = transformSafeCall(expr).expr;
			}
			fields.push(field);
		}
		if(foundSafeCall) {
			foundSafeCall = false;
			return fields;
		} else {
			return null;
		}
	}

	static function transformSafeCall(e:Expr):Expr {
		switch(e) {
			case macro $target!.$field:
				foundSafeCall = true;
				e = macro @:pos(e.pos) {
					var _v_ = $target;
					_v_ == null ? null : _v_.$field;
				};
			case _:
		}
		return e.map(transformSafeCall);
	}
}