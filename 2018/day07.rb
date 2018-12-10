# Day 7

def deep_copy(o)
  Marshal.load(Marshal.dump(o))
end

rnodes = {}

('A'..'Z').each { |x| rnodes[x] = []}

File.open('inp7a.txt').each do |line|
  from, to = line[1..-1].scan(/[[:upper:]]/)
  rnodes[to] += [from]
end

# Part 1

q = deep_copy rnodes
vis = []

while !q.empty?
  q = q.sort_by { |k, v| [v.size, k] }
  next_n = q.shift
  vis << next_n
  q.each { |k, d| d.delete next_n[0] }
end

p vis.join

# Part 2

t = -1
q = rnodes
worked_on = []

while !q.empty? || !worked_on.empty?
  t += 1
  
  worked_on.map! { |a, x| [a, x-1] }
  
  worked_on.select { |_, x| x == 0 }.each do |r|
    q.each { |_, d| d.delete r[0] }
  end
  
  worked_on.select! { |_, x| x != 0 }
  
  q = q.sort_by { |k, v| [v.size, k] }
  
  while worked_on.size < 5 && q.size > 0 && q[0][1].size == 0
    q = q.sort_by { |k, v| [v.size, k] }
    next_n = q.shift
    worked_on << [next_n[0], next_n[0].ord - "A".ord + 61]
  end
end

p t
