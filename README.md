# compiler-workout

Supplementary repository for compiler course.

Prerequisites: ocaml [http://ocaml.org], opam [http://opam.ocaml.org].

Building:

* `opam pin add GT https://github.com/dboulytchev/GT.git`
* `opam pin add ostap https://github.com/dboulytchev/ostap.git`
* `opam install ostap`
* `opam install GT`
* To build the sources: `make` from the top project directory
* To test: `test.sh` from `regression` subfolder

### Data flow analysis
Implemented a simple example of a constant propagation data flow analysis
using forward monotone framework.
To run an example: `./rc.opt -a [filename]`

Example:
```
-- file: example.expr
w := 640;
h := 480;
size := w * h;
if size < 1000 then
    a := 1
else
    a := 2
fi;
while a > 0 do
    size := size / 10;
    a := a - 1
od;
write (w);
write (h);
write (size)
```

Run `./rc.opt -a example.expr`

Result:
```
AAssign [w = 640]
AAssign [h = 480, w = 640]
AAssign [size = 307200, h = 480, w = 640]
// if
AAssign [a = 1, size = 307200, h = 480, w = 640]
// else
AAssign [a = 2, size = 307200, h = 480, w = 640]
// if-end [a = 1, size = 307200, h = 480, w = 640, a = 2]
// while
AAssign [size = None, a = 1, h = 480, w = 640, a = 2, a = None]
AAssign [a = None, size = None, h = 480, w = 640]
// while-end [a = None, size = None, h = 480, w = 640, a = 1, size = 307200, a = 2]
AWrite [a = None, size = None, h = 480, w = 640, a = 1, size = 307200, a = 2]
AWrite [a = None, size = None, h = 480, w = 640, a = 1, size = 307200, a = 2]
AWrite [a = None, size = None, h = 480, w = 640, a = 1, size = 307200, a = 2]

Result:
Seq (Assign ("w", Const (640)), Seq (Assign ("h", Const (480)), Seq (Assign ("size", Const (307200)), Seq (If (Const (0), Assign ("a", Const (1)), Assign ("a", Const (2))), Seq (While (Binop (">", Var ("a"), Const (0)), Seq (Assign ("size", Const (30720)), Assign ("a", Binop ("-", Var ("a"), Const (1))))), Seq (Write (Const (640)), Seq (Write (Const (480)), Write (Var ("size")))))))))
```

Pretty result:
```
w := 640;
h := 480;
size := 307200;
if 0 then
    a := 1
else
    a := 2
fi;
while a > 0 do
    size := size / 10;
    a := a - 1;
od;
write (640);
write (480);
write (size)
```