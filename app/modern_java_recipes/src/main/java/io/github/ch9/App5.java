package io.github.ch9;

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.logging.Logger;

class Product {
    private Integer id;
    private String name;

    public Product(Integer id, String name) {
        this.id = id;
        this.name = name;
    }

    @Override
    public String toString() {
        return "Product{" +
            "name='" + name + '\'' +
            ", id=" + id +
            '}';
    }
}

public class App5 {

    private Map<Integer, Product> cache = new HashMap<>();
    private Logger logger = Logger.getLogger(this.getClass().getName());

    private Product getLocal(int id) {
        return cache.get(id);
    }

    private Product getRemote(int id) {
        try {
            Thread.sleep(100);
            if (id == 666) {
                throw new RuntimeException("Evil request");
            }
        } catch (InterruptedException ignored) {

        }
        return new Product(id, "name");
    }

    public CompletableFuture<Product> getProduct(int id) {
        try {
            Product product = getLocal(id);
            if (product == null) {
                return CompletableFuture.completedFuture(null);
            }
            CompletableFuture<Product> future = new CompletableFuture<>();
            product = getRemote(id);
            cache.put(id, product);
            future.complete(product);
            return future;
        } catch (Exception e) {
            CompletableFuture<Product> future = new CompletableFuture<>();
            future.completeExceptionally(e);
            return future;
        }
    }

    public CompletableFuture<Product> getProductAsync(int id) {
        try {
            Product product = getLocal(id);
            if (product != null) {
                logger.info("getLocal with id=" + id);
                return CompletableFuture.completedFuture(product);
            }
            logger.info("getRemote with id=" + id);
            return CompletableFuture.supplyAsync(() -> {
                Product p = getRemote(id);
                cache.put(id, p);
                return p;
            });
        } catch (Exception e) {
            logger.warning("exception thrown");
            CompletableFuture<Product> future = new CompletableFuture<>();
            future.completeExceptionally(e);
            return future;
        }
    }

    public static void main(String[] args) {

    }
}
