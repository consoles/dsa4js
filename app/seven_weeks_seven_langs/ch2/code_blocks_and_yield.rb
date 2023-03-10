# 代码块和 yield
3.times {puts 'hello'}

# 代码块有2种，大括号包围（单行）的或者 do-end 包围（多行）
['lions','trigers','bears'].each {|a| puts a}

# 在现有类上拓展一个方法
class Integer
    def my_times
        i = self
        while i > 0
            i = i - 1
            yield # 调用代码块
        end
    end
end            
3.my_times {puts 'moon'}

def call_block(&block) # 将代码块作为闭包传递给函数
    block.call
end
def pass_block(&block)
    call_block(&block)
end
pass_block {puts 'hello blocks'}
