package io.github.ch5;

@FunctionalInterface
public interface FunctionWithException<T,R,E extends Exception> {
    R apply(T t) throws E;
}
