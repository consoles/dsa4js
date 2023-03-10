module ToFile
    def filename
        "object_#{self.object_id}.txt"
    end
    
    def to_f
        File.open(filename, 'w') {|f| f.write(to_s)}
    end
end

class Person
    include ToFile # 这个类本身没有写文件的功能，但是通过 mixin 有了写文件的能力
    attr_accessor :name

    def initialize(name)
        @name = name
    end

    def to_s
        name
    end

end

Person.new('matz').to_f
# to_s 在模块中使用，在类中实现，但是在定义模块的时候，实现它的类甚至还没有定义！这说明模块与包含它的类之间是以一种相当隐蔽的方式相互作用的

# ruby 中有 2 个至关重要的 mixin: enumerable, comparable
# 如果想让类可枚举，必须实现 each 方法，如果想让类可比较，必须实现 `<=>` 操作符

puts 'begin' <=> 'end'
puts 'same' <=> 'same'
puts '----'
a = [5,3,1,4]
puts a.sort # 整数实现了 `<=>` 操作符，所以可以排序，可以调用 max,min
puts a.any? {|i| i > 6}
puts a.any? {|i| i > 4}
puts a.all? {|i| i > 4}
puts a.collect {|i| i * 2} # like js Array.map
puts '----'
puts a.select {|i| i % 2 == 1} # like js Array.filter
puts '-----'
puts a.max
puts a.member?(2) # like js Array.includes

puts '----'
puts a.inject(0) {|sum, i| sum + i} # 13
puts a.inject {|sum, i| sum + i} # 13
puts a.inject {|product, i| product * i} # 60
a.inject(0) do |sum, i|
    puts "sum: #{sum} i: #{i} sum+i: #{sum + i}"
    sum + i
end    
