package;

using Safety;

class Main
{
	var field:Int = 0;

	static function main():Void {
		function some(?a:Main) {
			trace(a.field);
		}
		var a:Null<String> = "hello";
		trace(a.unsafe());
		trace(a.length);
		// some(null);
	}
}