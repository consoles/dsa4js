# 在非函数式语言中实现一个monad（参见Ruby中的有关monad的文章系列①）

class MyMonad
    def initialize(value)
      @value = value
    end

    def value
      @value
    end
  
    def bind(&block)
      block.call(@value)
    end
  
    def self.unit(value)
      MyMonad.new(value)
    end
  end
  
# 示例用法
result = MyMonad.unit(5)
.bind { |x| MyMonad.unit(x * 2) }
.bind { |x| MyMonad.unit(x + 3) }
.bind { |x| MyMonad.unit(x / 2.0) } # 如果是除以 2 就是整数除法了

# 6.5
puts result.value
