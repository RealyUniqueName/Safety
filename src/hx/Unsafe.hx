/**
 *  Special type which is handled in Safety to force using of nullable values as not-nullable.
 *  Nullable values will be passed to/from this type without any checks.
 */
typedef Unsafe<T> = T;