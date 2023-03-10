# 散列表
numbers = {1 => 'one', 2 => 'two'}
puts numbers

# 使用 symbol 作为键，在给事物和概念命名的时候非常好用
stuff = {:array => [1,2,3], :string => 'Hello, tom!'}
puts stuff

puts 'string'.object_id #60
puts 'string'.object_id #80
puts :string.object_id #305628
puts :string.object_id #305628
