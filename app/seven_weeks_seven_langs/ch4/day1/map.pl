% 地图着色问题
different(red, green).
different(red, blue).
different(green, red).
different(green, blue).
different(blue, red).
different(blue, green).

% 相邻的州需要使用不同的颜色
coloring(Alabama, Mississippi, Georgia, Tennessee, Florida) :- 
    different(Mississippi, Tennessee),
    different(Mississippi, Alabama),
    different(Alabama, Tennessee),
    different(Alabama, Mississippi),
    different(Alabama, Georgia),
    different(Alabama, Florida),
    different(Georgia, Florida),
    different(Georgia, Tennessee).

/*
| ?- ['day1/map.pl'].
compiling D:/dsa4js/app/seven_weeks_seven_langs/ch4/day1/map.pl for byte code...
D:/dsa4js/app/seven_weeks_seven_langs/ch4/day1/map.pl compiled, 18 lines read - 1759 bytes written, 4 ms

(16 ms) yes
| ?- coloring(Alabama, Mississippi, Georgia, Tennessee, Florida).

Alabama = blue
Florida = green
Georgia = red
Mississippi = red
Tennessee = green ? a 得到了一种可能的答案，输入 a 可以得到另外的几个答案

Alabama = green
Florida = blue
Georgia = red
Mississippi = red
Tennessee = blue

Alabama = blue
Florida = red
Georgia = green
Mississippi = green
Tennessee = red

Alabama = red
Florida = blue
Georgia = green
Mississippi = green
Tennessee = blue

Alabama = green
Florida = red
Georgia = blue
Mississippi = blue
Tennessee = red

Alabama = red
Florida = green
Georgia = blue
Mississippi = blue
Tennessee = green

(109 ms) no
| ?- 
*/
