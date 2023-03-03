package io.github.ch2;

import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import java.util.function.Consumer;
import java.util.function.DoubleSupplier;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * 函数接口
 */
public class App {
    public static void main(String[] args) {
        // Consumer
        // 打印集合中的元素
        List<String> strings = Arrays.asList("this", "is", "a", "list", "of", "strings");
        System.out.println("匿名内部类");
        strings.forEach(new Consumer<String>() {
            @Override
            public void accept(String s) {
                System.out.println(s);
            }
        });
        System.out.println("lambda 表达式");
        strings.forEach(s -> System.out.println(s));
        System.out.println("方法引用");
        strings.forEach(System.out::println);
        System.out.println("-----------------------------------");

        // Supplier
        System.out.println("使用 Math.random 作为 Supplier");
        // 匿名内部类实现
        DoubleSupplier randomSupplier = new DoubleSupplier() {
            @Override
            public double getAsDouble() {
                return Math.random();
            }
        };
        // lambda 表达式
        randomSupplier = () -> Math.random();
        // 方法引用
        randomSupplier = Math::random;
        System.out.println("在集合中查找名称");
        List<String> names = Arrays.asList("Mal", "Wash", "Kaylee", "Inara",
            "Zoë", "Jayne", "Simon", "River", "Shepherd Book");
        Optional<String> first = names.stream()
            .filter(name -> name.startsWith("C"))
            .findFirst();
        System.out.println("first:" + first); // Optional.empty
        System.out.println("orElse:" + first.orElse("None")); // None
        // 即便没有找到指定名称，仍然使用逗号分隔集合
        System.out.println(first.orElse(
            String.format("No result found in %s", names.stream().collect(Collectors.joining(",")))
        ));
        // 仅仅当 Optional 为空的时候才使用逗号分隔集合（必要的时候才会创建完整字符串）
        System.out.println(first.orElseGet(() ->
            String.format("No result found in %s", names.stream().collect(Collectors.joining(",")))
        ));
        System.out.println("-----------------------------------");

        // Predicate: 主要用于流的筛选

        // Function: 最常见的用法是作为 Stream.map 方法的一个参数
        List<Integer> nameLengths = names.stream()
            .map(String::length)
            .collect(Collectors.toList());
        System.out.printf("nameLengths: %s%n", nameLengths);
    }
}
