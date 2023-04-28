% 建立一个描述音乐家和乐器的知识库，同时也描述出音乐家以及他们的音乐风格。
play(langlang, piano).
play(langlang, guzheng).
play(mozhate, violin).
play(beiduofen, piano).
play(dongming, dizi).
play(yanran, guzheng).

style(piano, foreign).
style(violin, foreign).
style(dizi, chinese).
style(guzheng, chinese).

% 音乐家 X 具有风格 Z => 音乐家 X 使用乐器 Y，并且乐器 Y 具有风格 Z
player_style(X, Z) :- play(X, Y), style(Y, Z).

% player_style(langlang, What).

% What = foreign ? a

% What = chinese
