package io.github.ch1;

/**
 * 回文检查器
 */
@FunctionalInterface
public interface PalindromeChecker {
    boolean isPalidrome(String s);

    default String sayHello() {
        return "接口中的默认方法，用户希望在接口中提供方法的实现!";
    }

    static void myStaticMethod() {
        System.out.println("I'm a static method in an interface");
    }
}
