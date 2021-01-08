# ComputorV2

`bc` like calculator, and Polynomial equations of degrees less than or equal to 2

### Make
```bash make.sh```

### Usage
```
Usage: java -jar ./ComputorV2
```

Supported data types: `RealNumber`, `ComplexNumber`, `Matrix` and `Function`.

#### RealNumber:
```
a = 2
b = 4.242
4.242
c = -4.3
-4.3
a = c
-4.3
```
#### ComplexNumber
```
a = 2*i + 3
3 + 2i
b = -4i - 4
-4 - 4i
```
#### Matrix
```
varA = [[2,3];[4,3]]
[ 2 , 3 ]
[ 4 , 3 ]
varB = [[3,4]]
[ 3 , 4 ]
```
#### Functions

Lib functions
```
sqrt(-16) = ?
0 + 4i
cos(0) = ?
1
```
User Defined Functions
```
funA(x) = 2*x^5 + 4x^2 - 5*x + 4
2 * x^5 + 4 * x^2 - 5*x + 4
funB(y) = 43 * y / (4 % 2 * y)
43 * y / (4 % 2 * y)
funC(z) = -2 * z - 5
-2 * z - 5
```

Polynomial functions solution
```
f(x) = 3x^2 - 2x + x + abs(-40)   
3 * x^2 - 2 * x + x + 40
y = 40
40
f(x) = y ?
Reduced form: 3 * x^2 + -1 * x = 0.0
Polynomial degree: 2.0
Discriminant is strictly positive, the two solutions are:
x1 = 0.333333
x2 = 0.0
```

Supported operators ```+ - / * % ^ **```

### Bonus
1. Predefined functions `sin, cos, tan, fact, sqrt, etc.` and variables `Pi, e`.
2. Functions with more than one argument: `pow(a,b) = a ^ b`
3. Show history `/hist`, common variables `/v` and functions `/f`.
