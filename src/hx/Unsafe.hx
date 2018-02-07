/**
 *  Special type which is handled in Safety to force using of nullable values as not-nullable.
 *  Nullable values will be passed to/from this type without any checks.
 *  Also expressions like `(expr:Unsafe<T>)` are not checked for null safety.
 *  You should avoid using it directly.
 */
typedef Unsafe<T> = T;