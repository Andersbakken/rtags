#include <stdint.h>
#include <stdio.h>

#define NULL ((void *)0)
typedef int PyObject; // to make self contained

typedef PyObject *(*getter)(PyObject *, void *);
typedef int (*setter)(PyObject *, PyObject *, void *);

typedef struct PyGetSetDef {
    char *name;
    getter get;
    setter set;
    char *doc;
    void *closure;
} PyGetSetDef;

const char *Vector_axis_x_doc = "";

static PyObject *Vector_swizzle_get(PyObject *self, void *type)
{
	(void)self;
	(void)type;
	return (void *)0;
}

static int Vector_swizzle_set(PyObject *self, PyObject *value, void *type)
{
	(void)self;
	(void)value;
	(void)type;
	return 0;
}

#define _VA_NARGS_GLUE(x, y) x y
#define _VA_NARGS_RETURN_COUNT(\
	_1_, _2_, _3_, _4_, _5_, _6_, _7_, _8_, _9_, _10_, _11_, _12_, _13_, _14_, _15_, _16_, \
	_17_, _18_, _19_, _20_, _21_, _22_, _23_, _24_, _25_, _26_, _27_, _28_, _29_, _30_, _31_, _32_, \
	_33_, _34_, _35_, _36_, _37_, _38_, _39_, _40_, _41_, _42_, _43_, _44_, _45_, _46_, _47_, _48_, \
	_49_, _50_, _51_, _52_, _53_, _54_, _55_, _56_, _57_, _58_, _59_, _60_, _61_, _62_, _63_, _64_, \
	count, ...) count
#define _VA_NARGS_EXPAND(args) _VA_NARGS_RETURN_COUNT args
#define _VA_NARGS_OVERLOAD_MACRO2(name, count) name##count
#define _VA_NARGS_OVERLOAD_MACRO1(name, count) _VA_NARGS_OVERLOAD_MACRO2(name, count)
#define _VA_NARGS_OVERLOAD_MACRO(name,  count) _VA_NARGS_OVERLOAD_MACRO1(name, count)
/* --- expose for re-use --- */
/* 64 args max */
#define VA_NARGS_COUNT(...) _VA_NARGS_EXPAND((__VA_ARGS__, \
	64, 63, 62, 61, 60, 59, 58, 57, 56, 55, 54, 53, 52, 51, 50, 49, \
	48, 47, 46, 45, 44, 43, 42, 41, 40, 39, 38, 37, 36, 35, 34, 33, \
	32, 31, 30, 29, 28, 27, 26, 25, 24, 23, 22, 21, 20, 19, 18, 17, \
	16, 15, 14, 13, 12, 11, 10,  9,  8,  7,  6,  5,  4,  3,  2, 1, 0))
#define VA_NARGS_CALL_OVERLOAD(name, ...) \
	_VA_NARGS_GLUE(_VA_NARGS_OVERLOAD_MACRO(name, VA_NARGS_COUNT(__VA_ARGS__)), (__VA_ARGS__))


/* Swizzle axes get packed into a single value that is used as a closure. Each
 * axis uses SWIZZLE_BITS_PER_AXIS bits. The first bit (SWIZZLE_VALID_AXIS) is
 * used as a sentinel: if it is unset, the axis is not valid. */
#define SWIZZLE_BITS_PER_AXIS 3
#define SWIZZLE_VALID_AXIS 0x4
#define SWIZZLE_AXIS       0x3



/* XYZW -> 0123 */
#define AXIS_FROM_CHAR(a) (((a) != 'W') ? ((a) - 'X') : 3)

#define _VA_SWIZZLE_1(a) ( \
	((AXIS_FROM_CHAR(a) | SWIZZLE_VALID_AXIS)))
#define _VA_SWIZZLE_2(a, b) (_VA_SWIZZLE_1(a) | \
	((AXIS_FROM_CHAR(b) | SWIZZLE_VALID_AXIS) << (SWIZZLE_BITS_PER_AXIS)))
#define _VA_SWIZZLE_3(a, b, c) (_VA_SWIZZLE_2(a, b) | \
	((AXIS_FROM_CHAR(c) | SWIZZLE_VALID_AXIS) << (SWIZZLE_BITS_PER_AXIS * 2)))
#define _VA_SWIZZLE_4(a, b, c, d) (_VA_SWIZZLE_3(a, b, c) | \
	((AXIS_FROM_CHAR(d) | SWIZZLE_VALID_AXIS) << (SWIZZLE_BITS_PER_AXIS * 3)))

#define SET_INT_IN_POINTER(i)    ((void *)(intptr_t)(i))

#define SWIZZLE(...) SET_INT_IN_POINTER(VA_NARGS_CALL_OVERLOAD(_VA_SWIZZLE_, __VA_ARGS__))

/*****************************************************************************/
/* Python attributes get/set structure:                                      */
/*****************************************************************************/
static PyGetSetDef Vector_getseters[] = {
	{(char *)"xx",   (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('X', 'X')},
	{(char *)"xxx",  (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('X', 'X', 'X')},
	{(char *)"xxxx", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('X', 'X', 'X', 'X')},
	{(char *)"xxxy", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('X', 'X', 'X', 'Y')},
	{(char *)"xxxz", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('X', 'X', 'X', 'Z')},
	{(char *)"xxxw", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('X', 'X', 'X', 'W')},
	{(char *)"xxy",  (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('X', 'X', 'Y')},
	{(char *)"xxyx", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('X', 'X', 'Y', 'X')},
	{(char *)"xxyy", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('X', 'X', 'Y', 'Y')},
	{(char *)"xxyz", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('X', 'X', 'Y', 'Z')},
	{(char *)"xxyw", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('X', 'X', 'Y', 'W')},
	{(char *)"xxz",  (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('X', 'X', 'Z')},
	{(char *)"xxzx", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('X', 'X', 'Z', 'X')},
	{(char *)"xxzy", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('X', 'X', 'Z', 'Y')},
	{(char *)"xxzz", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('X', 'X', 'Z', 'Z')},
	{(char *)"xxzw", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('X', 'X', 'Z', 'W')},
	{(char *)"xxw",  (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('X', 'X', 'W')},
	{(char *)"xxwx", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('X', 'X', 'W', 'X')},
	{(char *)"xxwy", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('X', 'X', 'W', 'Y')},
	{(char *)"xxwz", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('X', 'X', 'W', 'Z')},
	{(char *)"xxww", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('X', 'X', 'W', 'W')},
	{(char *)"xy",   (getter)Vector_swizzle_get, (setter)Vector_swizzle_set, NULL, SWIZZLE('X', 'Y')},
	{(char *)"xyx",  (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('X', 'Y', 'X')},
	{(char *)"xyxx", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('X', 'Y', 'X', 'X')},
	{(char *)"xyxy", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('X', 'Y', 'X', 'Y')},
	{(char *)"xyxz", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('X', 'Y', 'X', 'Z')},
	{(char *)"xyxw", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('X', 'Y', 'X', 'W')},
	{(char *)"xyy",  (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('X', 'Y', 'Y')},
	{(char *)"xyyx", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('X', 'Y', 'Y', 'X')},
	{(char *)"xyyy", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('X', 'Y', 'Y', 'Y')},
	{(char *)"xyyz", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('X', 'Y', 'Y', 'Z')},
	{(char *)"xyyw", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('X', 'Y', 'Y', 'W')},
	{(char *)"xyz",  (getter)Vector_swizzle_get, (setter)Vector_swizzle_set, NULL, SWIZZLE('X', 'Y', 'Z')},
	{(char *)"xyzx", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('X', 'Y', 'Z', 'X')},
	{(char *)"xyzy", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('X', 'Y', 'Z', 'Y')},
	{(char *)"xyzz", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('X', 'Y', 'Z', 'Z')},
	{(char *)"xyzw", (getter)Vector_swizzle_get, (setter)Vector_swizzle_set, NULL, SWIZZLE('X', 'Y', 'Z', 'W')},
	{(char *)"xyw",  (getter)Vector_swizzle_get, (setter)Vector_swizzle_set, NULL, SWIZZLE('X', 'Y', 'W')},
	{(char *)"xywx", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('X', 'Y', 'W', 'X')},
	{(char *)"xywy", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('X', 'Y', 'W', 'Y')},
	{(char *)"xywz", (getter)Vector_swizzle_get, (setter)Vector_swizzle_set, NULL, SWIZZLE('X', 'Y', 'W', 'Z')},
	{(char *)"xyww", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('X', 'Y', 'W', 'W')},
	{(char *)"xz",   (getter)Vector_swizzle_get, (setter)Vector_swizzle_set, NULL, SWIZZLE('X', 'Z')},
	{(char *)"xzx",  (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('X', 'Z', 'X')},
	{(char *)"xzxx", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('X', 'Z', 'X', 'X')},
	{(char *)"xzxy", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('X', 'Z', 'X', 'Y')},
	{(char *)"xzxz", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('X', 'Z', 'X', 'Z')},
	{(char *)"xzxw", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('X', 'Z', 'X', 'W')},
	{(char *)"xzy",  (getter)Vector_swizzle_get, (setter)Vector_swizzle_set, NULL, SWIZZLE('X', 'Z', 'Y')},
	{(char *)"xzyx", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('X', 'Z', 'Y', 'X')},
	{(char *)"xzyy", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('X', 'Z', 'Y', 'Y')},
	{(char *)"xzyz", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('X', 'Z', 'Y', 'Z')},
	{(char *)"xzyw", (getter)Vector_swizzle_get, (setter)Vector_swizzle_set, NULL, SWIZZLE('X', 'Z', 'Y', 'W')},
	{(char *)"xzz",  (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('X', 'Z', 'Z')},
	{(char *)"xzzx", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('X', 'Z', 'Z', 'X')},
	{(char *)"xzzy", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('X', 'Z', 'Z', 'Y')},
	{(char *)"xzzz", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('X', 'Z', 'Z', 'Z')},
	{(char *)"xzzw", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('X', 'Z', 'Z', 'W')},
	{(char *)"xzw",  (getter)Vector_swizzle_get, (setter)Vector_swizzle_set, NULL, SWIZZLE('X', 'Z', 'W')},
	{(char *)"xzwx", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('X', 'Z', 'W', 'X')},
	{(char *)"xzwy", (getter)Vector_swizzle_get, (setter)Vector_swizzle_set, NULL, SWIZZLE('X', 'Z', 'W', 'Y')},
	{(char *)"xzwz", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('X', 'Z', 'W', 'Z')},
	{(char *)"xzww", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('X', 'Z', 'W', 'W')},
	{(char *)"xw",   (getter)Vector_swizzle_get, (setter)Vector_swizzle_set, NULL, SWIZZLE('X', 'W')},
	{(char *)"xwx",  (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('X', 'W', 'X')},
	{(char *)"xwxx", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('X', 'W', 'X', 'X')},
	{(char *)"xwxy", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('X', 'W', 'X', 'Y')},
	{(char *)"xwxz", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('X', 'W', 'X', 'Z')},
	{(char *)"xwxw", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('X', 'W', 'X', 'W')},
	{(char *)"xwy",  (getter)Vector_swizzle_get, (setter)Vector_swizzle_set, NULL, SWIZZLE('X', 'W', 'Y')},
	{(char *)"xwyx", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('X', 'W', 'Y', 'X')},
	{(char *)"xwyy", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('X', 'W', 'Y', 'Y')},
	{(char *)"xwyz", (getter)Vector_swizzle_get, (setter)Vector_swizzle_set, NULL, SWIZZLE('X', 'W', 'Y', 'Z')},
	{(char *)"xwyw", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('X', 'W', 'Y', 'W')},
	{(char *)"xwz",  (getter)Vector_swizzle_get, (setter)Vector_swizzle_set, NULL, SWIZZLE('X', 'W', 'Z')},
	{(char *)"xwzx", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('X', 'W', 'Z', 'X')},
	{(char *)"xwzy", (getter)Vector_swizzle_get, (setter)Vector_swizzle_set, NULL, SWIZZLE('X', 'W', 'Z', 'Y')},
	{(char *)"xwzz", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('X', 'W', 'Z', 'Z')},
	{(char *)"xwzw", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('X', 'W', 'Z', 'W')},
	{(char *)"xww",  (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('X', 'W', 'W')},
	{(char *)"xwwx", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('X', 'W', 'W', 'X')},
	{(char *)"xwwy", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('X', 'W', 'W', 'Y')},
	{(char *)"xwwz", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('X', 'W', 'W', 'Z')},
	{(char *)"xwww", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('X', 'W', 'W', 'W')},
	{(char *)"yx",   (getter)Vector_swizzle_get, (setter)Vector_swizzle_set, NULL, SWIZZLE('Y', 'X')},
	{(char *)"yxx",  (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Y', 'X', 'X')},
	{(char *)"yxxx", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Y', 'X', 'X', 'X')},
	{(char *)"yxxy", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Y', 'X', 'X', 'Y')},
	{(char *)"yxxz", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Y', 'X', 'X', 'Z')},
	{(char *)"yxxw", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Y', 'X', 'X', 'W')},
	{(char *)"yxy",  (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Y', 'X', 'Y')},
	{(char *)"yxyx", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Y', 'X', 'Y', 'X')},
	{(char *)"yxyy", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Y', 'X', 'Y', 'Y')},
	{(char *)"yxyz", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Y', 'X', 'Y', 'Z')},
	{(char *)"yxyw", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Y', 'X', 'Y', 'W')},
	{(char *)"yxz",  (getter)Vector_swizzle_get, (setter)Vector_swizzle_set, NULL, SWIZZLE('Y', 'X', 'Z')},
	{(char *)"yxzx", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Y', 'X', 'Z', 'X')},
	{(char *)"yxzy", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Y', 'X', 'Z', 'Y')},
	{(char *)"yxzz", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Y', 'X', 'Z', 'Z')},
	{(char *)"yxzw", (getter)Vector_swizzle_get, (setter)Vector_swizzle_set, NULL, SWIZZLE('Y', 'X', 'Z', 'W')},
	{(char *)"yxw",  (getter)Vector_swizzle_get, (setter)Vector_swizzle_set, NULL, SWIZZLE('Y', 'X', 'W')},
	{(char *)"yxwx", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Y', 'X', 'W', 'X')},
	{(char *)"yxwy", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Y', 'X', 'W', 'Y')},
	{(char *)"yxwz", (getter)Vector_swizzle_get, (setter)Vector_swizzle_set, NULL, SWIZZLE('Y', 'X', 'W', 'Z')},
	{(char *)"yxww", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Y', 'X', 'W', 'W')},
	{(char *)"yy",   (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Y', 'Y')},
	{(char *)"yyx",  (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Y', 'Y', 'X')},
	{(char *)"yyxx", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Y', 'Y', 'X', 'X')},
	{(char *)"yyxy", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Y', 'Y', 'X', 'Y')},
	{(char *)"yyxz", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Y', 'Y', 'X', 'Z')},
	{(char *)"yyxw", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Y', 'Y', 'X', 'W')},
	{(char *)"yyy",  (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Y', 'Y', 'Y')},
	{(char *)"yyyx", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Y', 'Y', 'Y', 'X')},
	{(char *)"yyyy", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Y', 'Y', 'Y', 'Y')},
	{(char *)"yyyz", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Y', 'Y', 'Y', 'Z')},
	{(char *)"yyyw", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Y', 'Y', 'Y', 'W')},
	{(char *)"yyz",  (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Y', 'Y', 'Z')},
	{(char *)"yyzx", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Y', 'Y', 'Z', 'X')},
	{(char *)"yyzy", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Y', 'Y', 'Z', 'Y')},
	{(char *)"yyzz", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Y', 'Y', 'Z', 'Z')},
	{(char *)"yyzw", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Y', 'Y', 'Z', 'W')},
	{(char *)"yyw",  (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Y', 'Y', 'W')},
	{(char *)"yywx", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Y', 'Y', 'W', 'X')},
	{(char *)"yywy", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Y', 'Y', 'W', 'Y')},
	{(char *)"yywz", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Y', 'Y', 'W', 'Z')},
	{(char *)"yyww", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Y', 'Y', 'W', 'W')},
	{(char *)"yz",   (getter)Vector_swizzle_get, (setter)Vector_swizzle_set, NULL, SWIZZLE('Y', 'Z')},
	{(char *)"yzx",  (getter)Vector_swizzle_get, (setter)Vector_swizzle_set, NULL, SWIZZLE('Y', 'Z', 'X')},
	{(char *)"yzxx", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Y', 'Z', 'X', 'X')},
	{(char *)"yzxy", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Y', 'Z', 'X', 'Y')},
	{(char *)"yzxz", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Y', 'Z', 'X', 'Z')},
	{(char *)"yzxw", (getter)Vector_swizzle_get, (setter)Vector_swizzle_set, NULL, SWIZZLE('Y', 'Z', 'X', 'W')},
	{(char *)"yzy",  (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Y', 'Z', 'Y')},
	{(char *)"yzyx", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Y', 'Z', 'Y', 'X')},
	{(char *)"yzyy", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Y', 'Z', 'Y', 'Y')},
	{(char *)"yzyz", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Y', 'Z', 'Y', 'Z')},
	{(char *)"yzyw", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Y', 'Z', 'Y', 'W')},
	{(char *)"yzz",  (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Y', 'Z', 'Z')},
	{(char *)"yzzx", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Y', 'Z', 'Z', 'X')},
	{(char *)"yzzy", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Y', 'Z', 'Z', 'Y')},
	{(char *)"yzzz", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Y', 'Z', 'Z', 'Z')},
	{(char *)"yzzw", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Y', 'Z', 'Z', 'W')},
	{(char *)"yzw",  (getter)Vector_swizzle_get, (setter)Vector_swizzle_set, NULL, SWIZZLE('Y', 'Z', 'W')},
	{(char *)"yzwx", (getter)Vector_swizzle_get, (setter)Vector_swizzle_set, NULL, SWIZZLE('Y', 'Z', 'W', 'X')},
	{(char *)"yzwy", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Y', 'Z', 'W', 'Y')},
	{(char *)"yzwz", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Y', 'Z', 'W', 'Z')},
	{(char *)"yzww", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Y', 'Z', 'W', 'W')},
	{(char *)"yw",   (getter)Vector_swizzle_get, (setter)Vector_swizzle_set, NULL, SWIZZLE('Y', 'W')},
	{(char *)"ywx",  (getter)Vector_swizzle_get, (setter)Vector_swizzle_set, NULL, SWIZZLE('Y', 'W', 'X')},
	{(char *)"ywxx", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Y', 'W', 'X', 'X')},
	{(char *)"ywxy", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Y', 'W', 'X', 'Y')},
	{(char *)"ywxz", (getter)Vector_swizzle_get, (setter)Vector_swizzle_set, NULL, SWIZZLE('Y', 'W', 'X', 'Z')},
	{(char *)"ywxw", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Y', 'W', 'X', 'W')},
	{(char *)"ywy",  (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Y', 'W', 'Y')},
	{(char *)"ywyx", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Y', 'W', 'Y', 'X')},
	{(char *)"ywyy", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Y', 'W', 'Y', 'Y')},
	{(char *)"ywyz", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Y', 'W', 'Y', 'Z')},
	{(char *)"ywyw", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Y', 'W', 'Y', 'W')},
	{(char *)"ywz",  (getter)Vector_swizzle_get, (setter)Vector_swizzle_set, NULL, SWIZZLE('Y', 'W', 'Z')},
	{(char *)"ywzx", (getter)Vector_swizzle_get, (setter)Vector_swizzle_set, NULL, SWIZZLE('Y', 'W', 'Z', 'X')},
	{(char *)"ywzy", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Y', 'W', 'Z', 'Y')},
	{(char *)"ywzz", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Y', 'W', 'Z', 'Z')},
	{(char *)"ywzw", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Y', 'W', 'Z', 'W')},
	{(char *)"yww",  (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Y', 'W', 'W')},
	{(char *)"ywwx", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Y', 'W', 'W', 'X')},
	{(char *)"ywwy", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Y', 'W', 'W', 'Y')},
	{(char *)"ywwz", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Y', 'W', 'W', 'Z')},
	{(char *)"ywww", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Y', 'W', 'W', 'W')},
	{(char *)"zx",   (getter)Vector_swizzle_get, (setter)Vector_swizzle_set, NULL, SWIZZLE('Z', 'X')},
	{(char *)"zxx",  (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Z', 'X', 'X')},
	{(char *)"zxxx", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Z', 'X', 'X', 'X')},
	{(char *)"zxxy", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Z', 'X', 'X', 'Y')},
	{(char *)"zxxz", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Z', 'X', 'X', 'Z')},
	{(char *)"zxxw", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Z', 'X', 'X', 'W')},
	{(char *)"zxy",  (getter)Vector_swizzle_get, (setter)Vector_swizzle_set, NULL, SWIZZLE('Z', 'X', 'Y')},
	{(char *)"zxyx", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Z', 'X', 'Y', 'X')},
	{(char *)"zxyy", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Z', 'X', 'Y', 'Y')},
	{(char *)"zxyz", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Z', 'X', 'Y', 'Z')},
	{(char *)"zxyw", (getter)Vector_swizzle_get, (setter)Vector_swizzle_set, NULL, SWIZZLE('Z', 'X', 'Y', 'W')},
	{(char *)"zxz",  (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Z', 'X', 'Z')},
	{(char *)"zxzx", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Z', 'X', 'Z', 'X')},
	{(char *)"zxzy", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Z', 'X', 'Z', 'Y')},
	{(char *)"zxzz", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Z', 'X', 'Z', 'Z')},
	{(char *)"zxzw", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Z', 'X', 'Z', 'W')},
	{(char *)"zxw",  (getter)Vector_swizzle_get, (setter)Vector_swizzle_set, NULL, SWIZZLE('Z', 'X', 'W')},
	{(char *)"zxwx", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Z', 'X', 'W', 'X')},
	{(char *)"zxwy", (getter)Vector_swizzle_get, (setter)Vector_swizzle_set, NULL, SWIZZLE('Z', 'X', 'W', 'Y')},
	{(char *)"zxwz", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Z', 'X', 'W', 'Z')},
	{(char *)"zxww", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Z', 'X', 'W', 'W')},
	{(char *)"zy",   (getter)Vector_swizzle_get, (setter)Vector_swizzle_set, NULL, SWIZZLE('Z', 'Y')},
	{(char *)"zyx",  (getter)Vector_swizzle_get, (setter)Vector_swizzle_set, NULL, SWIZZLE('Z', 'Y', 'X')},
	{(char *)"zyxx", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Z', 'Y', 'X', 'X')},
	{(char *)"zyxy", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Z', 'Y', 'X', 'Y')},
	{(char *)"zyxz", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Z', 'Y', 'X', 'Z')},
	{(char *)"zyxw", (getter)Vector_swizzle_get, (setter)Vector_swizzle_set, NULL, SWIZZLE('Z', 'Y', 'X', 'W')},
	{(char *)"zyy",  (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Z', 'Y', 'Y')},
	{(char *)"zyyx", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Z', 'Y', 'Y', 'X')},
	{(char *)"zyyy", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Z', 'Y', 'Y', 'Y')},
	{(char *)"zyyz", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Z', 'Y', 'Y', 'Z')},
	{(char *)"zyyw", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Z', 'Y', 'Y', 'W')},
	{(char *)"zyz",  (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Z', 'Y', 'Z')},
	{(char *)"zyzx", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Z', 'Y', 'Z', 'X')},
	{(char *)"zyzy", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Z', 'Y', 'Z', 'Y')},
	{(char *)"zyzz", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Z', 'Y', 'Z', 'Z')},
	{(char *)"zyzw", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Z', 'Y', 'Z', 'W')},
	{(char *)"zyw",  (getter)Vector_swizzle_get, (setter)Vector_swizzle_set, NULL, SWIZZLE('Z', 'Y', 'W')},
	{(char *)"zywx", (getter)Vector_swizzle_get, (setter)Vector_swizzle_set, NULL, SWIZZLE('Z', 'Y', 'W', 'X')},
	{(char *)"zywy", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Z', 'Y', 'W', 'Y')},
	{(char *)"zywz", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Z', 'Y', 'W', 'Z')},
	{(char *)"zyww", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Z', 'Y', 'W', 'W')},
	{(char *)"zz",   (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Z', 'Z')},
	{(char *)"zzx",  (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Z', 'Z', 'X')},
	{(char *)"zzxx", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Z', 'Z', 'X', 'X')},
	{(char *)"zzxy", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Z', 'Z', 'X', 'Y')},
	{(char *)"zzxz", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Z', 'Z', 'X', 'Z')},
	{(char *)"zzxw", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Z', 'Z', 'X', 'W')},
	{(char *)"zzy",  (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Z', 'Z', 'Y')},
	{(char *)"zzyx", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Z', 'Z', 'Y', 'X')},
	{(char *)"zzyy", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Z', 'Z', 'Y', 'Y')},
	{(char *)"zzyz", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Z', 'Z', 'Y', 'Z')},
	{(char *)"zzyw", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Z', 'Z', 'Y', 'W')},
	{(char *)"zzz",  (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Z', 'Z', 'Z')},
	{(char *)"zzzx", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Z', 'Z', 'Z', 'X')},
	{(char *)"zzzy", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Z', 'Z', 'Z', 'Y')},
	{(char *)"zzzz", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Z', 'Z', 'Z', 'Z')},
	{(char *)"zzzw", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Z', 'Z', 'Z', 'W')},
	{(char *)"zzw",  (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Z', 'Z', 'W')},
	{(char *)"zzwx", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Z', 'Z', 'W', 'X')},
	{(char *)"zzwy", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Z', 'Z', 'W', 'Y')},
	{(char *)"zzwz", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Z', 'Z', 'W', 'Z')},
	{(char *)"zzww", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Z', 'Z', 'W', 'W')},
	{(char *)"zw",   (getter)Vector_swizzle_get, (setter)Vector_swizzle_set, NULL, SWIZZLE('Z', 'W')},
	{(char *)"zwx",  (getter)Vector_swizzle_get, (setter)Vector_swizzle_set, NULL, SWIZZLE('Z', 'W', 'X')},
	{(char *)"zwxx", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Z', 'W', 'X', 'X')},
	{(char *)"zwxy", (getter)Vector_swizzle_get, (setter)Vector_swizzle_set, NULL, SWIZZLE('Z', 'W', 'X', 'Y')},
	{(char *)"zwxz", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Z', 'W', 'X', 'Z')},
	{(char *)"zwxw", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Z', 'W', 'X', 'W')},
	{(char *)"zwy",  (getter)Vector_swizzle_get, (setter)Vector_swizzle_set, NULL, SWIZZLE('Z', 'W', 'Y')},
	{(char *)"zwyx", (getter)Vector_swizzle_get, (setter)Vector_swizzle_set, NULL, SWIZZLE('Z', 'W', 'Y', 'X')},
	{(char *)"zwyy", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Z', 'W', 'Y', 'Y')},
	{(char *)"zwyz", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Z', 'W', 'Y', 'Z')},
	{(char *)"zwyw", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Z', 'W', 'Y', 'W')},
	{(char *)"zwz",  (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Z', 'W', 'Z')},
	{(char *)"zwzx", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Z', 'W', 'Z', 'X')},
	{(char *)"zwzy", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Z', 'W', 'Z', 'Y')},
	{(char *)"zwzz", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Z', 'W', 'Z', 'Z')},
	{(char *)"zwzw", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Z', 'W', 'Z', 'W')},
	{(char *)"zww",  (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Z', 'W', 'W')},
	{(char *)"zwwx", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Z', 'W', 'W', 'X')},
	{(char *)"zwwy", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Z', 'W', 'W', 'Y')},
	{(char *)"zwwz", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Z', 'W', 'W', 'Z')},
	{(char *)"zwww", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('Z', 'W', 'W', 'W')},
	{(char *)"wx",   (getter)Vector_swizzle_get, (setter)Vector_swizzle_set, NULL, SWIZZLE('W', 'X')},
	{(char *)"wxx",  (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('W', 'X', 'X')},
	{(char *)"wxxx", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('W', 'X', 'X', 'X')},
	{(char *)"wxxy", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('W', 'X', 'X', 'Y')},
	{(char *)"wxxz", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('W', 'X', 'X', 'Z')},
	{(char *)"wxxw", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('W', 'X', 'X', 'W')},
	{(char *)"wxy",  (getter)Vector_swizzle_get, (setter)Vector_swizzle_set, NULL, SWIZZLE('W', 'X', 'Y')},
	{(char *)"wxyx", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('W', 'X', 'Y', 'X')},
	{(char *)"wxyy", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('W', 'X', 'Y', 'Y')},
	{(char *)"wxyz", (getter)Vector_swizzle_get, (setter)Vector_swizzle_set, NULL, SWIZZLE('W', 'X', 'Y', 'Z')},
	{(char *)"wxyw", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('W', 'X', 'Y', 'W')},
	{(char *)"wxz",  (getter)Vector_swizzle_get, (setter)Vector_swizzle_set, NULL, SWIZZLE('W', 'X', 'Z')},
	{(char *)"wxzx", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('W', 'X', 'Z', 'X')},
	{(char *)"wxzy", (getter)Vector_swizzle_get, (setter)Vector_swizzle_set, NULL, SWIZZLE('W', 'X', 'Z', 'Y')},
	{(char *)"wxzz", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('W', 'X', 'Z', 'Z')},
	{(char *)"wxzw", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('W', 'X', 'Z', 'W')},
	{(char *)"wxw",  (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('W', 'X', 'W')},
	{(char *)"wxwx", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('W', 'X', 'W', 'X')},
	{(char *)"wxwy", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('W', 'X', 'W', 'Y')},
	{(char *)"wxwz", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('W', 'X', 'W', 'Z')},
	{(char *)"wxww", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('W', 'X', 'W', 'W')},
	{(char *)"wy",   (getter)Vector_swizzle_get, (setter)Vector_swizzle_set, NULL, SWIZZLE('W', 'Y')},
	{(char *)"wyx",  (getter)Vector_swizzle_get, (setter)Vector_swizzle_set, NULL, SWIZZLE('W', 'Y', 'X')},
	{(char *)"wyxx", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('W', 'Y', 'X', 'X')},
	{(char *)"wyxy", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('W', 'Y', 'X', 'Y')},
	{(char *)"wyxz", (getter)Vector_swizzle_get, (setter)Vector_swizzle_set, NULL, SWIZZLE('W', 'Y', 'X', 'Z')},
	{(char *)"wyxw", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('W', 'Y', 'X', 'W')},
	{(char *)"wyy",  (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('W', 'Y', 'Y')},
	{(char *)"wyyx", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('W', 'Y', 'Y', 'X')},
	{(char *)"wyyy", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('W', 'Y', 'Y', 'Y')},
	{(char *)"wyyz", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('W', 'Y', 'Y', 'Z')},
	{(char *)"wyyw", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('W', 'Y', 'Y', 'W')},
	{(char *)"wyz",  (getter)Vector_swizzle_get, (setter)Vector_swizzle_set, NULL, SWIZZLE('W', 'Y', 'Z')},
	{(char *)"wyzx", (getter)Vector_swizzle_get, (setter)Vector_swizzle_set, NULL, SWIZZLE('W', 'Y', 'Z', 'X')},
	{(char *)"wyzy", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('W', 'Y', 'Z', 'Y')},
	{(char *)"wyzz", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('W', 'Y', 'Z', 'Z')},
	{(char *)"wyzw", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('W', 'Y', 'Z', 'W')},
	{(char *)"wyw",  (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('W', 'Y', 'W')},
	{(char *)"wywx", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('W', 'Y', 'W', 'X')},
	{(char *)"wywy", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('W', 'Y', 'W', 'Y')},
	{(char *)"wywz", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('W', 'Y', 'W', 'Z')},
	{(char *)"wyww", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('W', 'Y', 'W', 'W')},
	{(char *)"wz",   (getter)Vector_swizzle_get, (setter)Vector_swizzle_set, NULL, SWIZZLE('W', 'Z')},
	{(char *)"wzx",  (getter)Vector_swizzle_get, (setter)Vector_swizzle_set, NULL, SWIZZLE('W', 'Z', 'X')},
	{(char *)"wzxx", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('W', 'Z', 'X', 'X')},
	{(char *)"wzxy", (getter)Vector_swizzle_get, (setter)Vector_swizzle_set, NULL, SWIZZLE('W', 'Z', 'X', 'Y')},
	{(char *)"wzxz", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('W', 'Z', 'X', 'Z')},
	{(char *)"wzxw", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('W', 'Z', 'X', 'W')},
	{(char *)"wzy",  (getter)Vector_swizzle_get, (setter)Vector_swizzle_set, NULL, SWIZZLE('W', 'Z', 'Y')},
	{(char *)"wzyx", (getter)Vector_swizzle_get, (setter)Vector_swizzle_set, NULL, SWIZZLE('W', 'Z', 'Y', 'X')},
	{(char *)"wzyy", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('W', 'Z', 'Y', 'Y')},
	{(char *)"wzyz", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('W', 'Z', 'Y', 'Z')},
	{(char *)"wzyw", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('W', 'Z', 'Y', 'W')},
	{(char *)"wzz",  (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('W', 'Z', 'Z')},
	{(char *)"wzzx", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('W', 'Z', 'Z', 'X')},
	{(char *)"wzzy", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('W', 'Z', 'Z', 'Y')},
	{(char *)"wzzz", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('W', 'Z', 'Z', 'Z')},
	{(char *)"wzzw", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('W', 'Z', 'Z', 'W')},
	{(char *)"wzw",  (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('W', 'Z', 'W')},
	{(char *)"wzwx", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('W', 'Z', 'W', 'X')},
	{(char *)"wzwy", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('W', 'Z', 'W', 'Y')},
	{(char *)"wzwz", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('W', 'Z', 'W', 'Z')},
	{(char *)"wzww", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('W', 'Z', 'W', 'W')},
	{(char *)"ww",   (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('W', 'W')},
	{(char *)"wwx",  (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('W', 'W', 'X')},
	{(char *)"wwxx", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('W', 'W', 'X', 'X')},
	{(char *)"wwxy", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('W', 'W', 'X', 'Y')},
	{(char *)"wwxz", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('W', 'W', 'X', 'Z')},
	{(char *)"wwxw", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('W', 'W', 'X', 'W')},
	{(char *)"wwy",  (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('W', 'W', 'Y')},
	{(char *)"wwyx", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('W', 'W', 'Y', 'X')},
	{(char *)"wwyy", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('W', 'W', 'Y', 'Y')},
	{(char *)"wwyz", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('W', 'W', 'Y', 'Z')},
	{(char *)"wwyw", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('W', 'W', 'Y', 'W')},
	{(char *)"wwz",  (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('W', 'W', 'Z')},
	{(char *)"wwzx", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('W', 'W', 'Z', 'X')},
	{(char *)"wwzy", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('W', 'W', 'Z', 'Y')},
	{(char *)"wwzz", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('W', 'W', 'Z', 'Z')},
	{(char *)"wwzw", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('W', 'W', 'Z', 'W')},
	{(char *)"www",  (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('W', 'W', 'W')},
	{(char *)"wwwx", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('W', 'W', 'W', 'X')},
	{(char *)"wwwy", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('W', 'W', 'W', 'Y')},
	{(char *)"wwwz", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('W', 'W', 'W', 'Z')},
	{(char *)"wwww", (getter)Vector_swizzle_get, (setter)NULL, NULL, SWIZZLE('W', 'W', 'W', 'W')},

#undef AXIS_FROM_CHAR
#undef SWIZZLE
#undef _VA_SWIZZLE_1
#undef _VA_SWIZZLE_2
#undef _VA_SWIZZLE_3
#undef _VA_SWIZZLE_4

	{NULL, NULL, NULL, NULL, NULL}  /* Sentinel */
};

/* so we're used */
extern void placeholder_func(void);
void placeholder_func(void)
{
	printf("%d %s\n", (int)sizeof(Vector_getseters), Vector_getseters[0].name);
}
