package io.github.ch5;

import java.util.LinkedList;
import java.util.List;
import java.util.Random;
import java.util.function.Function;
import java.util.function.IntPredicate;
import java.util.logging.Logger;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

public class App {

    public static boolean isPrefect(int x) {
        return Math.sqrt(x) % 1 == 0;
    }

    public static boolean isTriangular(int x) {
        double val = (Math.sqrt(8 * x + 1) - 1) / 2;
        return val % 1 == 0;
    }

    private static <T, R, E extends Exception> Function<T, R> wrapper(FunctionWithException<T, R, E> fe) {
        return arg -> {
            try {
                // 捕获所有 Checked Exception 向外抛出 Unchecked Exception
                return fe.apply(arg);
            } catch (Exception e) {
                throw new RuntimeException(e);
            }
        };
    }

    public static void main(String[] args) {
        System.out.println("随机数");
        Random r = new Random();
        r.ints(5)
            .sorted()
            .forEach(System.out::println);
        r.doubles(5, 0.1, 0.5)
            .sorted()
            .forEach(System.out::println);
        List<Long> longs = r.longs(5)
            .boxed()
            .collect(Collectors.toList());
        System.out.println(longs);
        LinkedList<Integer> listOfInts = r.ints(5, 10, 100)
            .collect(LinkedList::new, LinkedList::add, LinkedList::addAll);
        System.out.println(listOfInts);
        System.out.println("---------------------");

        System.out.println("日志");
        Logger logger = Logger.getLogger(App.class.getName());
        List<String> data = List.of("this", "is", "list", "of", "string");
        logger.info("The data is: " + data.toString()); // 无论日志级别如何，都会进行字符串的拼接
        logger.info(() -> "The data is: " + data.toString()); // 仅仅当日志级别大于 info 的时候才会进行字符串的拼接

        System.out.println("闭包复合");
        Function<Integer, Integer> add2 = x -> x + 2;
        Function<Integer, Integer> mult3 = x -> x * 3;
        Function<Integer, Integer> mult3add2 = add2.compose(mult3);
        Function<Integer, Integer> add2mult3 = add2.andThen(mult3);
        System.out.println("mult3 -> add2: " + mult3add2.apply(1)); // (1 * 3) + 2 = 5
        System.out.println("add2 -> mult3: " + add2mult3.apply(1)); // (1 + 2) * 3 = 9

        IntPredicate prefect = App::isPrefect;
        IntPredicate triangular = App::isTriangular;
        IntPredicate both = prefect.and(triangular);
        IntStream.rangeClosed(1, 10000)
            .filter(both)
            .forEach(System.out::println);
        System.out.println("---------------------");
    }
}
