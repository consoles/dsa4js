// 写一个程序，提供10次尝试机会，猜一个1～100之间的随机数。如果你愿意的话，可以在第一次猜测之后，提示猜大了还是猜小了

// eerie install https://github.com/IoLanguage/Random.git
rand := ((Random value) * 100) ceil
rand println

i := 0
num := 0
while(i < 10 and num != rand,
    ("Guess a number between 1 to 100: (" .. i + 1 .. "/10)") println
    num = Readline readline
    num = if (num asNumber isNan, 0, num asNumber)
    if (num > read, "Too big" println)
    if (num < read, "Too small" println)
    i = i + 1
)

if (num == read, "You win!" println, "You lose~" println)

// eerie install https://github.com/IoLanguage/ReadLine.git 报错：
// sudo apt-get install libreadline-dev 即可
