# Safety

This is a plugin for Haxe compiler. It's an attempt to push Haxe towards null safety implementation in the compiler.
Use this plugin, report bugs and share your thoughts in [issues](https:/github.com/RealyUniqueName/Safety/issues).
Hopefully we can find the best approach to null safety together. And then with all the collected experience we will be able to propose a solid implementation to the compiler.

## Installation

Minimum supported Haxe version is `4.0.0-preview.2`
```
haxelib git safety https://github.com/RealyUniqueName/Safety.git
```
If you want to use this plugin with another version of Haxe you need to setup desired version of Haxe for development (see [Building Haxe from source](https://haxe.org/documentation/introduction/building-haxe.html)) and then
```
cd path/to/haxe-source/
PLUGIN=path/to/haxelib/safety/git/src/ml/safety make plugin
```

## Usage

Add `-lib safety` to you hxml file.
Use following flags:

* `-D SAFETY=location1,location2` (required) - Use this flag to specify which location(s) you want plugin to check for null safety. This is a comma-separated list of packages, class names and filesystem paths. E.g. `-D SAFETY=Main,some.pack,another.pack.AnotherClass,path/to/src`. You can specify `-D SAFETY=ALL` instead which will check all the code, even std lib (not recommended)
* `-D SAFETY_ENABLE_SAFE_NAVIGATION` (optional) - Enables [safe navigation operator](https://en.wikipedia.org/wiki/Safe_navigation_operator) `!.` (Disabled by default. Does not provide code completion. Implemented via build macro which means penalties for compilation speed.)
* `-D SAFETY_SILENT` (optional) - do not abort compilation on safety errors. You can handle safety errors manually in `Context.onAfterTyping(_ -> trace(Safety.plugin.getErrors()))`
* `-D SAFETY_DEBUG` (optional) - prints additional information during safety checking.

## Features

* Safety makes sure you will not pass nullable values to places which are not explicitly declared with `Null<SomeType>` (assignments, return statements, array access etc.);
```haxe
function fn(s:String) {}
var nullable:Null<String> = 'hello';
var str:String = null; //Compilation error
str = nullable; //Compilation error
fn(nullable); //Compilation error. Function argument was not declared with `Null<String>`
```
* Using nullables with unary and binary operators (except `==` and `!=`) is not allowed;
* If a field is declared without `Null<>` then it should have an initial value or it should be initialized in a constructor (for instance fields);
* Passing an instance of parameterized type with nullable type parameter to a place with the same type, but with not-nullable type parameter is not allowed:
```haxe
var nullables:Array<Null<String>> = ['hello', null, 'world'];
var a:Array<Int> = nullables; //Compilation error. Array<Null<String>> cannot be assigned to Array<String>
```
* Local variables checked against `null` are considered safe inside of a scope covered with that null-check:
```haxe
var nullable:Null<String> = getSomeStr();
var s:String = nullable; //Compilation error
if(nullable != null) {
	s = nullable; //OK
}
s = nullable; //Compilation error
s = (nullable == null ? 'hello' : nullable); //OK
```
* Static extensions for convenience:
```haxe
using Safety;

var nullable:Null<String> = getSomeStr();
var s:String = nullable.or('hello');
```
Available extensions:
```haxe
/**
*  Returns `value` if it is not `null`. Otherwise returns `defaultValue.
*/
static public function or<T>(value:Null<T>, defaultValue:T):T;
/**
*  Returns `value` if it is not `null`. Otherwise throws an exception.
*  @throws NullPointerException if `value` is `null`.
*/
static public function sure<T>(value:Null<T>):T;
/**
*  Just returns `value` without any checks, but typed as not-nullable. Use at your own risk.
*/
static public function unsafe<T>(value:Null<T>):T;
/**
*  Applies `callback` to `value` and returns the result if `value` is not `null`.
*  Returns `null` otherwise.
*/
static public inline function let<T,V>(value:Null<T>, callback:T->V):Null<V>;
/**
*  Passes `value` to `callback` if `value` is not null.
*/
static public inline function run<T>(value:Null<T>, callback:T->Void):Void;
/**
*  Applies `callback` to `value` if `value` is not `null`.
*  Returns `value`.
*/
static public inline function apply<T>(value:Null<T>, callback:T->Void):Null<T>;
```
* Safe navigation operator (disabled by default; see [Usage](#Usage))
```haxe
var obj:Null<{ field:Null<String> } = null;
trace(obj!.field!.length); //null
obj = { field:'hello' };
trace(obj!.field!.length); //5
```

## Limitations

* Haxe was not designed with null safety in mind, so it's always possible `null` will come to your code from 3rd-party code or even from std lib.
Safety doesn't perform automatic runtime checks for any values which you get from any code.
* Safety runs after compiler typing phase. At this point everything is already typed. That means if another var infers a nullable type of null-checked var, then that new var is also nullable, but it's not automatically safe:
```haxe
var n1:Null<String> = getSomeStr();
var s:String;
if(n1 != null) {
	s = n1; //OK
	var n2 = n1;
	$type(n2); //Null<String>
	s = n2; //Compilation error
	if(n2 != null) {
		s = n2; //OK
	}
}
```
* Nullable fields and properties are not considered null-safe even after checking against `null`. Use safety extensions instead:
```haxe
using Safety;

class Main {
	var nullable:Null<String>;
	function new() {
		var str:String;
		if(nullable != null) {
			str = nullable; //Compilation error.
		}
		str = nullable.or('hello');
	}
}
```
* If a local var is captured and modified in a closure that var cannot be safe anymore:
```haxe
var nullable:Null<String> = getSomeStr();
var str:String;
if(nullable != null) {
	str = nullable; //OK
	doStuff(function() nullable = getSomeStr());
	str = nullable; //Compilation error
}
```
