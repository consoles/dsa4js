-module(word_count).
-export([count_words/1]).

count_words("") -> 0;
count_words(Sentence) ->
    case skip_spaces(Sentence) of
        "" -> 0;
        Rest -> 1 + count_words(skip_word(Rest))
    end.

skip_spaces(Sentence) ->
    case Sentence of
        "" -> "";
        [Char | Rest] when Char == $  ; Char == $\n; Char == $\t ->
            skip_spaces(Rest);
        _ -> Sentence
    end.

skip_word(Sentence) ->
    case Sentence of
        "" -> "";
        [Char | Rest] when Char /= $  ; Char /= $\n; Char /= $\t ->
            skip_word(Rest);
        _ -> Sentence
    end.
