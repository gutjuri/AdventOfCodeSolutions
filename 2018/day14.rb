# Day 14

inp_str = '633601'
inp = inp_str.to_i

# Part 1

scores = [3, 7]
e1 = 0
e2 = 1

while scores.size < inp + 10
  new_rec = scores[e1] + scores[e2]
  scores << new_rec / 10 if new_rec > 9

  break if scores.size == inp + 10
  scores << new_rec % 10
  
  e1 = (e1 + 1 + scores[e1]) % scores.size
  e2 = (e2 + 1 + scores[e2]) % scores.size
  
end

puts scores[-10..-1].join

# Part 2

scores = [3, 7]
e1 = 0
e2 = 1

inp_ary = inp_str.chars.map!(&:to_i)
last_n = Array.new(inp_str.size, 0)

loop do
  new_rec = scores[e1] + scores[e2]
  if new_rec > 9
    scores << new_rec / 10
    last_n.shift
    last_n << new_rec / 10
    break if last_n == inp_ary
  end
  
  scores << new_rec % 10
  last_n.shift
  last_n << new_rec % 10
  break if last_n == inp_ary
  
  e1 = (e1 + 1 + scores[e1]) % scores.size
  e2 = (e2 + 1 + scores[e2]) % scores.size
end

puts scores.size - inp_str.size
