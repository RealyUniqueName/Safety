# Safety [![Build Status](https://travis-ci.org/RealyUniqueName/Safety.svg?branch=master)](https://travis-ci.org/RealyUniqueName/Safety)

This is a null safety implementation as a plugin for Haxe compiler. It's an attempt to push Haxe towards null safety implementation in the compiler.
Use this plugin, report bugs and share your thoughts in [issues](https://github.com/RealyUniqueName/Safety/issues).
Hopefully we can find the best approach to null safety together. And then with all the collected experience we will be able to propose a solid implementation to the compiler.

## Installation

Minimum supported Haxe version is `4.0.0-preview.2`, but pre-built binaries are only compatible with commit ceeba64 (and a few later) of development branch of Haxe.

Compatible compiler nightlies for x64 systems: [Windows](http://hxbuilds.s3-website-us-east-1.amazonaws.com/builds/haxe/windows64-installer/haxe_2018-01-23_development_ceeba64.zip), [Mac](http://hxbuilds.s3-website-us-east-1.amazonaws.com/builds/haxe/mac-installer/haxe_2018-01-25_development_ceeba64.tar.gz), [Linux](http://hxbuilds.s3-website-us-east-1.amazonaws.com/builds/haxe/linux64/haxe_2018-01-23_development_ceeba64.tar.gz).

Install Safety:
```
haxelib git safety https://github.com/RealyUniqueName/Safety.git
```
If you want to use this plugin with another OS, arch or another version of Haxe you need to setup desired version of Haxe for development (see [Building Haxe from source](https://haxe.org/documentation/introduction/building-haxe.html)) and then
```
cd path/to/haxe-source/
make PLUGIN=path/to/safety/src/ml/safety plugin
```

## Usage

Add `-lib safety` to your hxml file.
Use following compiler arguments:

* `--macro Safety.enable(dotPath, enableAdditionalFeatures)` - Enable null safety for the specified path. E.g. `--macro Safety.enable('my.pack')` to enable it for all the types in `my.pack` and in subpackages. Or `--macro Safety.enable('my.pack.MyClass')` to enable it for `my.pack.MyClass` only. Additional features are safe navigation operator, safe api and SafeArray. Each feature can be enabled separately if you pass `false` to `enableAdditionalFeatures`. The default value is `true`. More on the features below.
* `--macro Safety.safeNavigation(dotPath, recursive)` - Enables [safe navigation operator](https://en.wikipedia.org/wiki/Safe_navigation_operator) `!.` in the specified path. if `recursive` is `true` (it is by default), then `!.` operator is also enabled for subpackages in `dotPath`.
* `--macro Safety.safeArray(dotPath, recursive)` - Makes all array declarations to be typed as `SafeArray`. See [feature description](#safe-array) for details.
* `--macro Safety.safeApi(dotPath, recursive)` - Adds runtime checking for not-nullable arguments of public methods in the specified path. If `null` is passed to such an argument, then `safety.IllegalArgumentException` is thrown. [Details](#safe-api)
* `-D SAFETY_SILENT` - do not abort compilation on safety errors. You can handle safety errors manually in `Context.onAfterTyping(_ -> trace(Safety.plugin.getErrors()))`
* `-D SAFETY_DEBUG` - prints additional information during safety checking.

All `--macro Safety.*` arguments can be used multiple times with different `dotPath` values.

You can pass empty string `""` as any `dotPath` to apply a feature to the whole codebase (not recommended, because a lot of compilation errors will come from std lib).

## Features

### Compile time safety checking

* Safety makes sure you will not pass nullable values to the places which are not explicitly declared with `Null<SomeType>` (assignments, return statements, array access etc.);
```haxe
function fn(s:String) {}
var nullable:Null<String> = getNullableStr();
var str:String = null; //Compilation error
str = nullable; //Compilation error
fn(nullable); //Compilation error. Function argument `str` is not nullable
```
* Using nullables with unary and binary operators (except `==` and `!=`) is not allowed;
* If a field is declared without `Null<>` then it should have an initial value or it should be initialized in a constructor (for instance fields);
* Passing an instance of parameterized type with nullable type parameter to a place with the same type, but with not-nullable type parameter is not allowed:
```haxe
var nullables:Array<Null<String>> = ['hello', null, 'world'];
var a:Array<String> = nullables; //Compilation error. Array<Null<String>> cannot be assigned to Array<String>
```
* Local variables checked against `null` are considered _safe_ inside of a scope covered with that null-check:
```haxe
var nullable:Null<String> = getSomeStr();
var s:String = nullable; //Compilation error
if(nullable != null) {
    s = nullable; //OK
}
s = nullable; //Compilation error
s = (nullable == null ? 'hello' : nullable); //OK
switch(nullable) {
    case null:
    case _: s = nullable; //OK
}
```
* Control flow is also taken into account:
```haxe
function doStuff(a:Null<String>) {
    if(a == null) {
        return;
    }
    //From here `a` is safe, because function execution will continue only if `a` is not null
    var s:String = a; //OK
}
```

### Safe navigation operator

Adds safe navigation operator `!.` to Haxe syntax. Compiler argument to enable it: `--macro Safety.safeNavigation(my.pack)`

```haxe
var obj:Null<{ field:Null<String> }> = null;
trace(obj!.field!.length); //null
obj = { field:'hello' };
trace(obj!.field!.length); //5
```

### Safe array

`SafeArray<T>` (abstract over `Array<T>`) behaves exactly like `Array<T>` except it prevents out-of-bounds reading/writing (throws `safety.OutOfBoundsException`). See [Limitations](#limitations) to find out why you need it.

If `--macro Safety.safeArray(my.pack)` is in effect, then all array declarations become `SafeArray`:
```haxe
var a = ['hello', 'world'];
$type(a); //SafeArray<String>
```
You can add explicit typing to `Array<T>` (or another type) if you want to disable `SafeArray` for a single expression:
```haxe
var a = (['hello', 'world']:Array<String>);
$type(a); //Array<String>
```

### Safe api

Compiler argument to enable this feature: `--macro Safety.safeApi(my.pack)`.

If enabled it adds runtime checking against `null` for all not-nullable arguments of public methods:
```haxe
public function method(arg:String) {}
<...>
method(null); //throws safety.IllegalArgumentException
```
It's pretty cheap performance wise, because it just adds a simple line to the body of a method: `if(arg == null) throw new IllegalArgumentException();`

If argument is nullable, no check is generated.

Also safe api does not generate such checks for `Int`, `Float`, `Bool` (and other basic types) on static targets, because it's impossible to assign `null` to such types on static targets.

This feature is especially useful if you are creating a library, and you don't want your users to pass `null`s to your API. You can add `--macro Safety.safeApi('my.lib')` to `extraParams.hxml`.

### Static extensions for convenience

```haxe
using Safety;

var nullable:Null<String> = getSomeStr();
var s:String = nullable.or('hello');
```
Available extensions:
```haxe
/**
*  Returns `value` if it is not `null`. Otherwise returns `defaultValue`.
*/
static public inline function or<T>(value:Null<T>, defaultValue:T):T;
/**
*  Returns `value` if it is not `null`. Otherwise throws an exception.
*  @throws NullPointerException if `value` is `null`.
*/
static public inline function sure<T>(value:Null<T>):T;
/**
*  Just returns `value` without any checks, but typed as not-nullable. Use at your own risk.
*/
static public inline function unsafe<T>(value:Null<T>):T;
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
/**
*  Prints `true` if provided expression can not evaluate to `null` at runtime. Prints `false` otherwise.
*/
macro static public function isSafe(expr:Expr):ExprOf<Void>
```

## Limitations

* Safe navigation operator `!.` does not provide code completion.
* Haxe was not designed with null safety in mind, so it's always possible `null` will come to your code from 3rd-party code or even from std lib.
* Out-of-bounds array read returns `null`, but Haxe types it without `Null<>`. ([PR to the compiler to fix this issue](https://github.com/HaxeFoundation/haxe/pull/6825))
```haxe
var a:Array<String> = ["hello"];
$type(a[100]); // String
trace(a[100]); // null
var s:String = a[100]; // Safety does not complain here, because `a[100]` is not `Null<String>`, but just `String`
```
* Out-of-bounds array write fills all positions between the last defined index and the newly written one with `null`. Safety cannot save you in this case.
```haxe
var a:Array<String> = ["hello"];
a[2] = "world";
trace(a); //["hello", null, "world"]
var s:String = a[1]; //Safety cannot check this
trace(s); //null
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
        str = nullable.sure();
        str = nullable.or('hello');
    }
}
```
* If a local var is captured in a closure, it cannot be safe inside that closure:
```haxe
var a:Null<String> = getSomeStr();
var fn = function() {
    if(a != null) {
        var s:String = a; //Compilation error
    }
}
```
* If a local var is captured and modified in a closure with a nullable value, that var cannot be safe anymore:
```haxe
var nullable:Null<String> = getSomeNullableStr();
var str:String;
if(nullable != null) {
    str = nullable; //OK
    doStuff(function() nullable = getSomeNullableStr());
    if(nullable != null) {
        str = nullable; //Compilation error
    }
}
```
