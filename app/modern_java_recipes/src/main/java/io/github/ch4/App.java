package io.github.ch4;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;

public class App {

    /**
     * 创建不可变列表，可以直接使用 List.of 工厂方法来解决
     *
     * @param elements
     * @param <T>
     * @return
     */
    public static final <T> List<T> createImmutableList(T... elements) {
        return Arrays.stream(elements)
            .collect(Collectors.collectingAndThen(Collectors.toList(), Collections::unmodifiableList));
    }

    public static void main(String[] args) {
        List<Book> books = Arrays.asList(
            new Book(1, "Effective java", 50),
            new Book(2, "Nodejs", 51),
            new Book(3, "php", 52),
            new Book(4, ".net", 53),
            new Book(5, "golang", 54),
            new Book(6, "python", 55)
        );
        Map<Integer, Book> bookMap = books.stream()
            .collect(Collectors.toMap(Book::getId, b -> b));
        bookMap = books.stream()
            .collect(Collectors.toMap(Book::getId, Function.identity())); // 可以使用静态方法 Function.identity 替代 b -> b
        System.out.println(bookMap);
        System.out.println("-------------------");

        System.out.println("按照偶数或者奇数长度对字符串进行分区");
        List<String> strings = Arrays.asList("this", "is", "a", "long", "list", "of", "strings", "to", "use", "as", "a", "demo");
        Map<Boolean, List<String>> lengthMap = strings.stream()
            .collect(Collectors.partitioningBy(s -> s.length() % 2 == 0));
        lengthMap.forEach((key, value) -> System.out.printf("%5s: %s%n", key, value));
        System.out.println("-------------------");

        System.out.println("根据长度对字符串分组");
        Map<Integer, List<String>> lengthMap2 = strings.stream()
            .collect(Collectors.groupingBy(String::length));
        lengthMap2.forEach((key, value) -> System.out.printf("%d: %s%n", key, value));
        System.out.println("-------------------");

        System.out.println("对已经分区的字符串进行计数（使用下游收集器,对分区操作完成之后进行后期处理）");
        Map<Boolean, Long> numberLengthMap = strings.stream()
            .collect(Collectors.partitioningBy(s -> s.length() % 2 == 0, Collectors.counting()));
        numberLengthMap.forEach((key, value) -> System.out.printf("%5s: %s%n", key, value));
        System.out.println("-------------------");


    }
}
