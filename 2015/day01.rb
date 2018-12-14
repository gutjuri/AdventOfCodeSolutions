# Day 1

input = File.read('inp1a.txt')

# Part 1

puts input.chars.map { |x| x == '(' ? 1 : -1 }.sum

# Part 2

puts Array.new(input.size) { |i| input[0..i].chars.map { |x| x == '(' ? 1 : -1 } }.map(&:sum).find_index(-1) + 1
