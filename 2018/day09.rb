# Day 9

require 'algorithms'

def rotate deque, n
  while n > 0
    deque.push_back deque.pop_front
    n -= 1
  end
  while n < 0
    deque.push_front deque.pop_back
    n += 1
  end
end

last_marble = 71657*100
players = 476

n = 1

scores = Hash.new 0
circle = Containers::CDeque.new
circle.push_front 0

while n <= last_marble
  if n % 23 == 0 
    rotate circle, (-7)
    scores[n%players] += circle.pop_front + n
  else
    rotate circle, 2
    circle.push_front n
  end
  n += 1
end

p scores.max_by { |_, x| x }
