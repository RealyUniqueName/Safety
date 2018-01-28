package safety.macro;

import haxe.macro.Context;
import haxe.macro.Expr;

using haxe.macro.ExprTools;

class SafeAst {
	static var safeApiList:Array<String> = [];

	static public function addSafeAst(path:String) {
		safeApiList.push(path);
	}

	static var transformed:Bool = false;

	macro static public function buildSafeNavigationAndArray():Null<Array<Field>> {
		if(!Safety.plugin.isInSafety()) {
			return null;
		}

		transformed = false;

		var fields = [];
		var typeName = Context.getLocalClass().toString();
		for(field in Context.getBuildFields()) {
			var expr = switch(field.kind) {
				case FVar(_, e): e;
				case FProp(_, _, _, e): e;
				case FFun(fn):
					if(field.access.indexOf(APublic) >= 0) {
						injectArgumentsChecking(fn, field.pos, typeName, field.name);
					}
					fn.expr;
			}
			if(expr != null) {
				expr.expr = transform(expr).expr;
			}
			fields.push(field);
		}

		return transformed ? fields : null;
	}

	static function injectArgumentsChecking(fn:Function, pos:Position, typeName:String, fieldName:String) {
		if(fn.args.length == 0) {
			return;
		}

		var checks = [];
		for(arg in fn.args) {
			if(arg.opt) continue;
			var argType = arg.type;
			checks.push(macro @:pos(pos) new safety.macro.NullCheck<$argType>($i{arg.name}, $v{typeName}, $v{fieldName}, $v{arg.name}));
		}
		if(checks.length == 0) {
			return;
		}

		switch(fn.expr) {
			case null:
			case macro $b{exprs}:
				transformed = true;
				fn.expr = macro @:pos(fn.expr.pos) $b{checks.concat(exprs)};
			case expr:
				transformed = true;
				fn.expr = macro @:pos(fn.expr.pos) {
					$b{checks};
					$expr;
				}
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
			//don't touch patterns in `case`
			case macro ${{expr:ESwitch(target, cases, defaultCase)}}:
				target = switch(target) {
					case macro [$a{exprs}]:
						macro @:pos(target.pos) [$a{exprs.map(transform)}];
					case macro ([$a{exprs}]):
						macro @:pos(target.pos) ([$a{exprs.map(transform)}]);
					case _:
						transform(target);
				}
				for(c in cases) {
					if(c.guard != null) c.guard = transform(c.guard);
					if(c.expr != null) c.expr = transform(c.expr);
				}
				if(defaultCase != null && defaultCase.expr != null) {
					defaultCase = transform(defaultCase);
				}
				return {expr:ESwitch(target, cases, defaultCase), pos:e.pos};
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