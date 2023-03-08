package io.github.ch9;

import java.time.LocalDateTime;
import java.util.concurrent.*;

public class App4 {
    public static void getIfNotCanceled(Future<String> future) {
        if (!future.isCancelled()) { // 检查 future 状态
            try {
                System.out.println(future.get()); // 通过阻塞调用来检索 future 的值
            } catch (InterruptedException | ExecutionException e) {
                throw new RuntimeException(e);
            }
        } else {
            System.out.println("Canceled");
        }
    }

    public static void main(String[] args) {
        ExecutorService service = Executors.newCachedThreadPool();
        Future<String> future = service.submit(() -> {
            Thread.sleep(100);
            return LocalDateTime.now().toString();
        });
//        future.cancel(true); // 取消 future
        System.out.println("Processing");
        // 忙等待（busy waiting），该方法可能产生数百万调用，应该尽量避免
        // CompletableFuture 类可以在 Future 完成的之后提供更好的处理方式
        while (!future.isDone()) {
            System.out.println("Waiting...");
        }
        getIfNotCanceled(future);
    }
}
