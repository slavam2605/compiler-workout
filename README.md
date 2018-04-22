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
Implemented a monotone framework for forward and backward data flow analysis.
Using this framework implemented constant propagation _forward_ analysis and
live variables _backward_ analysis. To demonstrate the results some optimization
transformations were implemented:
* Constant propagation & folding 
* Elimination of unused structures (like `if 1 then ...` or `while 0 do ...`)
* Elimination of unused assignments

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
Propagate constants:
AAssign [] -> [w = 640]
AAssign [w = 640] -> [h = 480, w = 640]
AAssign [h = 480, w = 640] -> [size = 307200, h = 480, w = 640]
// if
AAssign [size = 307200, h = 480, w = 640] -> [a = 1, size = 307200, h = 480, w = 640]
// else
AAssign [size = 307200, h = 480, w = 640] -> [a = 2, size = 307200, h = 480, w = 640]
// if-end [size = 307200, h = 480, w = 640] -> [a = 1, size = 307200, h = 480, w = 640, a = 2]
// while
AAssign [a = 1, size = 307200, h = 480, w = 640, a = 2, a = None, size = None] -> [size = None, a = 1, h = 480, w = 640, a = 2, a = None]
AAssign [size = None, a = 1, h = 480, w = 640, a = 2, a = None] -> [a = None, size = None, h = 480, w = 640]
// while-end [a = 1, size = 307200, h = 480, w = 640, a = 2] -> [a = 1, size = 307200, h = 480, w = 640, a = 2, a = None, size = None]
AWrite [a = 1, size = 307200, h = 480, w = 640, a = 2, a = None, size = None] -> [a = 1, size = 307200, h = 480, w = 640, a = 2, a = None, size = None]
AWrite [a = 1, size = 307200, h = 480, w = 640, a = 2, a = None, size = None] -> [a = 1, size = 307200, h = 480, w = 640, a = 2, a = None, size = None]
AWrite [a = 1, size = 307200, h = 480, w = 640, a = 2, a = None, size = None] -> [a = 1, size = 307200, h = 480, w = 640, a = 2, a = None, size = None]

Live variables:
AAssign [] -> [w]
AAssign [w] -> [w, h]
AAssign [w, h] -> [size, w, h]
// if
AAssign [size, w, h] -> [a, size, w, h]
// else
AAssign [size, w, h] -> [a, size, w, h]
// if-end [size, w, h] -> [a, size, w, h]
// while
AAssign [size, a, w, h] -> [a, w, h, size]
AAssign [a, w, h, size] -> [w, h, size, a]
// while-end [a, size, w, h] -> [w, h, size, a]
AWrite [w, h, size] -> [h, size]
AWrite [h, size] -> [size]
AWrite [size] -> []

True expressions:
AAssign [] -> []
AAssign [] -> []
AAssign [] -> []
// if
AAssign [size < 1000] -> [size < 1000]
// else
AAssign [size >= 1000] -> [size >= 1000]
// if-end [] -> []
// while
AAssign [a > 0] -> [a > 0]
AAssign [a > 0] -> []
// while-end [] -> [a <= 0]
AWrite [a <= 0] -> [a <= 0]
AWrite [a <= 0] -> [a <= 0]
AWrite [a <= 0] -> [a <= 0]

Interval analysis:
AAssign [] -> [(w, [640, 640])]
AAssign [(w, [640, 640])] -> [(h, [480, 480]), (w, [640, 640])]
AAssign [(h, [480, 480]), (w, [640, 640])] -> [(size, [307200, 307200]), (h, [480, 480]), (w, [640, 640])]
// if
AAssign [(size, [307200, 307200]), (h, [480, 480]), (w, [640, 640])] -> [(a, [1, 1]), (size, [307200, 307200]), (h, [480, 480]), (w, [640, 640])]
// else
AAssign [(size, [307200, 307200]), (h, [480, 480]), (w, [640, 640])] -> [(a, [2, 2]), (size, [307200, 307200]), (h, [480, 480]), (w, [640, 640])]
// if-end [(size, [307200, 307200]), (h, [480, 480]), (w, [640, 640])] -> [(a, [1, 2]), (size, [307200, 307200]), (h, [480, 480]), (w, [640, 640])]
// while
AAssign [(a, [1, 2]), (size, [-30720, 307200]), (h, [480, 480]), (w, [640, 640])] -> [(a, [1, 2]), (size, [-30720, 30720]), (h, [480, 480]), (w, [640, 640])]
AAssign [(a, [1, 2]), (size, [-30720, 30720]), (h, [480, 480]), (w, [640, 640])] -> [(a, [0, 1]), (size, [-30720, 30720]), (h, [480, 480]), (w, [640, 640])]
// while-end [(a, [1, 2]), (size, [307200, 307200]), (h, [480, 480]), (w, [640, 640])] -> [(a, [0, 0]), (size, [-30720, 307200]), (h, [480, 480]), (w, [640, 640])]
AWrite [(a, [0, 0]), (size, [-30720, 307200]), (h, [480, 480]), (w, [640, 640])] -> [(a, [0, 0]), (size, [-30720, 307200]), (h, [480, 480]), (w, [640, 640])]
AWrite [(a, [0, 0]), (size, [-30720, 307200]), (h, [480, 480]), (w, [640, 640])] -> [(a, [0, 0]), (size, [-30720, 307200]), (h, [480, 480]), (w, [640, 640])]
AWrite [(a, [0, 0]), (size, [-30720, 307200]), (h, [480, 480]), (w, [640, 640])] -> [(a, [0, 0]), (size, [-30720, 307200]), (h, [480, 480]), (w, [640, 640])]

Result:
size := 307200;
a := 2;
while a > 0 do
    size := size / 10;
    a := a - 1
od;
write (640);
write (480);
write (size)
```