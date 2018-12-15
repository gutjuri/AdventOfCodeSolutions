# Day 2

input = File.read('inp2a.txt').split("\n").map { |l| l.split('x').map(&:to_i) }

# Part 1

puts input.map { |x, y, z| 2*x*y + 2*y*z + 2*z*x + [x*y, y*z, z*x].min }.sum

# Part 2

puts input.map { |x, y, z| x*y*z + [2*x + 2*y, 2*y + 2*z, 2*z + 2*x].min }.sum
