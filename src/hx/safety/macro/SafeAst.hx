package safety.macro;

import haxe.macro.Context;
import haxe.macro.Expr;

using haxe.macro.ExprTools;

class SafeAst {
	static var transformed:Bool = false;

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
				expr.expr = transform(expr).expr;
			}
			fields.push(field);
		}

		if(transformed) {
			transformed = false;
			return fields;
		} else {
			return null;
		}
	}

	static function transform(e:Expr):Expr {
		return switch(e) {
			#if !SAFETY_DISABLE_SAFE_NAVIGATION
			case macro $target!.$field:
				transformed = true;
				e = macro @:pos(e.pos) {
					var _v_ = $target;
					_v_ == null ? null : _v_.$field;
				};
				e.map(transform);
			#end
			#if !SAFETY_DISABLE_SAFE_ARRAY
			//don't touch array declaration if a user is casting it manually
			case macro ([$a{exprs}]:$type):
				exprs = exprs.map(transform);
				macro @:pos(e.pos) ([$a{exprs}]:$type);
			//otherwise transform to ([...]:SafeArray<T>);
			case macro [$a{exprs}]:
				transformed = true;
				exprs = exprs.map(transform);
				macro @:pos(e.pos) ([$a{exprs}]:SafeArray<safety.macro.Monomorph<0>>);
			#end
			case _:
				e.map(transform);
		}
	}
}