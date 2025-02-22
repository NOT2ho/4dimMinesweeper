# 4 dim minesweeper (not tested)
difficulty: inverse, 3-15 recommanded.  
don`t make map too big (big o and screen size issue)

## play 

```
Active code page: 65001
welcome to the 4 dim minesweeper.
i don`t know this works well cuz i can`t test this


============  legend  ============
| symbol |         meaning       |
|    *   |          mine         |
|    !   |          flag         |
|    n   |  num of 80-side mine  |


x size: 4
y size: 4
z size: 2
w size: 2
difficulty(inverse, 3-15 recommanded.): 6
random seed: 4294
mines exist: 10

============W axis(0)============




-----------Z axis (0)------------
- - -X axis- - >



 *| *| *| *|
 *| *| *| *|
 *| *| *| *|
 *| *| *| *|
                  ↓ Y axis

-----------Z axis (1)------------
- - -X axis- - >



 *| *| *| *|
 *| *| *| *|
 *| *| *| *|
 *| *| *| *|
                  ↓ Y axis

============W axis(1)============




-----------Z axis (0)------------
- - -X axis- - >



 *| *| *| *|
 *| *| *| *|
 *| *| *| *|
 *| *| *| *|
                  ↓ Y axis

-----------Z axis (1)------------
- - -X axis- - >



 *| *| *| *|
 *| *| *| *|
 *| *| *| *|
 *| *| *| *|
                  ↓ Y axis


=========== output END =============
== you have to input (or you die) ==

do you want to flag? (y / n)
n
map size : 4 * 4 * 2 * 2
next x: 0
next y: 0
next z: 1
next w: 1
mines exist: 10
cell selected by you: fromList [(0,0,1,1)]
cell extended: fromList [(0,0,0,0),(0,0,0,1),(0,0,1,0),(0,0,1,1),(0,1,0,0),(0,1,0,1),(0,1,1,0),(0,1,1,1),(1,0,0,1),(1,0,1,0),(1,0,1,1),(1,1,0,0),(1,1,0,1),(1,1,1,0),(1,1,1,1)]
cell you flagged: fromList []

============W axis(0)============




-----------Z axis (0)------------
- - -X axis- - >



 1| *| *| *|
 6| 7| *| *|
 *| *| *| *|
 *| *| *| *|
                  ↓ Y axis

-----------Z axis (1)------------
- - -X axis- - >



 1| 1| *| *|
 6| 7| *| *|
 *| *| *| *|
 *| *| *| *|
                  ↓ Y axis

============W axis(1)============




-----------Z axis (0)------------
- - -X axis- - >



 1| 1| *| *|
 6| 7| *| *|
 *| *| *| *|
 *| *| *| *|
                  ↓ Y axis

-----------Z axis (1)------------
- - -X axis- - >



 1| 1| *| *|
 6| 7| *| *|
 *| *| *| *|
 *| *| *| *|
                  ↓ Y axis


=========== output END =============
== you have to input (or you die) ==

do you want to flag? (y / n)
y
map size : 4 * 4 * 2 * 2
flag x: 1
flag y: 0
flag z: 0
flag w: 0
do you want to flag? (y / n)
n
map size : 4 * 4 * 2 * 2
next x: 3
next y: 3
next z: 3
out of index try again
3
out of index try again
1
next w: 1
mines exist: 10
cell selected by you: fromList [(0,0,1,1),(3,3,1,1)]
cell extended: fromList [(0,0,0,0),(0,0,0,1),(0,0,1,0),(0,0,1,1),(0,1,0,0),(0,1,0,1),(0,1,1,0),(0,1,1,1),(1,0,0,1),(1,0,1,0),(1,0,1,1),(1,1,0,0),(1,1,0,1),(1,1,1,0),(1,1,1,1),(2,2,0,1),(2,2,1,0),(2,2,1,1),(2,3,0,0),(2,3,0,1),(2,3,1,1),(3,2,0,0),(3,2,0,1),(3,2,1,0),(3,2,1,1),(3,3,0,0),(3,3,0,1),(3,3,1,1)]
cell you flagged: fromList [(1,0,0,0)]

============W axis(0)============




-----------Z axis (0)------------
- - -X axis- - >



 1| !| *| *|
 6| 7| *| *|
 *| *| *| 4|
 *| *| 5| 3|
                  ↓ Y axis

-----------Z axis (1)------------
- - -X axis- - >



 1| 1| *| *|
 6| 7| *| *|
 *| *| 6| 4|
 *| *| *| *|
                  ↓ Y axis

============W axis(1)============




-----------Z axis (0)------------
- - -X axis- - >



 1| 1| *| *|
 6| 7| *| *|
 *| *| 6| 4|
 *| *| 5| 3|
                  ↓ Y axis

-----------Z axis (1)------------
- - -X axis- - >



 1| 1| *| *|
 6| 7| *| *|
 *| *| 6| 4|
 *| *| 5| 3|
                  ↓ Y axis


=========== output END =============
== you have to input (or you die) ==

do you want to flag? (y / n)
n
map size : 4 * 4 * 2 * 2
next x: 2
next y: 2
next z: 0
next w: 0

============W axis(0)============




-----------Z axis (0)------------
- - -X axis- - >



0|1|0|0|
0|0|0|0|
0|0|1|0|
0|0|0|0|
                  ↓ Y axis

-----------Z axis (1)------------
- - -X axis- - >



0|0|0|0|
0|0|0|0|
1|1|0|0|
0|0|1|1|
                  ↓ Y axis

============W axis(1)============




-----------Z axis (0)------------
- - -X axis- - >



0|0|0|0|
0|0|0|1|
1|1|0|0|
0|0|0|0|
                  ↓ Y axis

-----------Z axis (1)------------
- - -X axis- - >



0|0|0|0|
0|0|0|0|
1|0|0|0|
0|0|0|0|
                  ↓ Y axis


=========== output END =============
== you have to input (or you die) ==


you doomed this is answer
game over. regame to restart the exe.
```
