class ActAsCsv
    def read
        file = File.new('D:/Downloads/downloaded-logs-20230216-094547.csv')
        @headers = file.gets.chomp.split(', ')

        file.each do |row|
            @result << row.chomp.split(', ')
        end
    end
    
    def headers
        @headers
    end

    def csv_contents
        @result
    end

    def initialize
        @result = []
        read
    end
end

class RubyCsv < ActAsCsv
end

m = RubyCsv.new
puts m.headers.inspect
puts m.csv_contents.inspect
puts '------------'

# 元编程发生在 acts_as_csv 宏当中，它对我们想添加到目标类上的所有方法都调用了 define_method
# 当目标类调用 acts_as_csv 时，宏代码会为目标类定义 4 个方法
class ActAsCsv2
    def self.acts_as_csv
        define_method 'read' do
            file = File.new('D:/Downloads/downloaded-logs-20230216-094547.csv')
            @headers = file.gets.chomp.split(', ')

            file.each do |row|
                @result << row.chomp.split(', ')
            end
        end
        
        define_method 'headers' do
            @headers
        end
        
        define_method 'csv_contents' do
            @result
        end
        
        define_method 'initialize' do
            @result = []
            read
        end
    end
end    

class RubyCsv2 < ActAsCsv2
    acts_as_csv
end

m = RubyCsv2.new
puts m.headers.inspect
puts m.csv_contents.inspect
puts '------------'

# 使用模块，其他类中可以直接 include 模块，使用 mixin 的方式使类具有某种能力，而不是使用继承
module ActAsCsv3
    def self.included(bash)
        bash.extend(ClassMethods)
    end

    module ClassMethods
        def acts_as_csv
            include InstanceMethods
        end
    end

    module InstanceMethods
        def read
            @csv_contents = []
            file = File.new('D:/Downloads/downloaded-logs-20230216-094547.csv')
            @headers = file.gets.chomp.split(', ')

            file.each do |row|
                @csv_contents << row.chomp.split(', ')
            end
        end

        attr_accessor :headers, :csv_contents

        def initialize
            read
        end
    end

end

class RubyCsv3 # 没有继承，可以自由添加
    include ActAsCsv3
    acts_as_csv
end

m = RubyCsv3.new
puts m.headers.inspect
puts m.csv_contents.inspect
puts '------------'

