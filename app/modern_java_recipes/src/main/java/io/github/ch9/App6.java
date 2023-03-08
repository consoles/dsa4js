package io.github.ch9;

import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

public class App6 {
    private static String sleepThenReturnString() {
        try {
            Thread.sleep(100);
        } catch (InterruptedException e) {
            throw new RuntimeException(e);
        }
        return "42";
    }

    /**
     * 2 个 future 的复合
     *
     * @param x
     * @param y
     * @return
     * @throws ExecutionException
     * @throws InterruptedException
     */
    private static int addByFutureCompose(int x, int y) throws ExecutionException, InterruptedException {
        CompletableFuture<Integer> future = CompletableFuture.supplyAsync(() -> x)
            .thenCompose(n -> CompletableFuture.supplyAsync(() -> n + y));
        return future.get();
    }

    /**
     * 2 个 future 的合并
     * @param x
     * @param y
     * @return
     * @throws ExecutionException
     * @throws InterruptedException
     */
    private static int addByFutureCombine(int x, int y) throws ExecutionException, InterruptedException {
        CompletableFuture<Integer> future = CompletableFuture.supplyAsync(() -> x)
            .thenCombine(CompletableFuture.supplyAsync(() -> y), (n1, n2) -> n1 + n2);
        return future.get();
    }

    public static void main(String[] args) throws ExecutionException, InterruptedException {
        // 利用 CompletableFuture 协调多个任务(采用默认的 ForkJoinPool)
        CompletableFuture.supplyAsync(App6::sleepThenReturnString) // Supplier
            .thenApply(Integer::parseInt) // Function
            .thenApply(x -> 2 * x) // Function
            .thenAccept(System.out::println) // Consumer
            .join(); // 阻塞
        System.out.println("Running...");

        // 在单独的线程池中运行 CompletableFuture 任务
        ExecutorService service = Executors.newFixedThreadPool(4);
        CompletableFuture.supplyAsync(App6::sleepThenReturnString, service)
            .thenApply(Integer::parseInt)
            .thenApply(x -> 2 * x)
            .thenAccept(System.out::println)
            .join();
        System.out.println("Running2 ... ");

        System.out.println("addByFutureCompose 5 + 6 = " + addByFutureCompose(5, 6));
        System.out.println("addByFutureCombine 5 + 6 = " + addByFutureCombine(5, 6));
    }
}
