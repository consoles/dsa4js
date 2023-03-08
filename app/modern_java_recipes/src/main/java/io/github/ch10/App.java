package io.github.ch10;

import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class App {
    public static void main(String[] args) {
//        List<Integer> list = List.of(1, 2, 3);
//        list.add(5); // java9 不可变集合，UnsupportedOperationException

        List<String> strings = Stream.of("this", "is", "a", "list", "of", "strings")
            .takeWhile(s -> !s.equals("of"))
            .collect(Collectors.toList());
        System.out.println(strings); // [this, is, a, list]，当不再满足谓词的时候，返回谓词之前的元素，优点在于这是一个短路操作（相对于 filter 而言，不必遍历每个元素）
    }
}
