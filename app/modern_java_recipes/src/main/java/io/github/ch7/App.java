package io.github.ch7;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Comparator;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class App {
    public static void main(String[] args) {
        // 流是在 try-with-resource 代码块中打开的，当 try 代码块完成的时候系统将自动关闭流和文件
        try (Stream<String> lines = Files.lines(Paths.get("D:/logs/pm2/golang-helper-err.log"))) {
            lines.filter(s -> s.length() > 20)
                .sorted(Comparator.comparingInt(String::length).reversed())
                .limit(100)
                .forEach(w -> System.out.printf("%s (%d)%n", w, w.length()));
        } catch (IOException e) {
            e.printStackTrace();
        }
        System.out.println("以流的方式检索文件");
        try (Stream<String> lines = new BufferedReader(new FileReader("D:/logs/pm2/golang-helper-err.log")).lines()) {
            Map<Integer, Long> map = lines.filter(s -> s.length() > 20)
                .collect(Collectors.groupingBy(String::length, Collectors.counting()));
            map.entrySet().stream()
                .sorted(Map.Entry.comparingByKey(Comparator.reverseOrder()))
                .limit(100)
                .forEach(e -> System.out.printf("Length: %d: %d words%n", e.getKey(), e.getValue()));
        } catch (IOException e) {
            e.printStackTrace();
        }
        System.out.println("以流的方式检索文件2");
        try (Stream<Path> list = Files.list(Paths.get("D:/"))) {
            list.forEach(System.out::println);
        } catch (IOException e) {
            e.printStackTrace();
        }
        System.out.println("文件树遍历");
        try (Stream<Path> paths = Files.walk(Paths.get("D:/code/solid"))) {
            paths.forEach(System.out::println);
        } catch (IOException e) {
            e.printStackTrace();
        }
        System.out.println("查找文件树中给定属性的文件");
        try (Stream<Path> paths = Files.find(
            Paths.get("D:/code/solid"),
            Integer.MAX_VALUE,
            (path, attributes) -> !attributes.isDirectory() && path.toString().contains("batch"))) {
            paths.forEach(System.out::println);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
