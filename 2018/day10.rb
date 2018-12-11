# Day 10

require 'set'

points = []

def check_vertical_lines n, points
  points.group_by { |x, _| x }.values.map { |v| v.size }.max >= n
end

def draw_points points
  xs = points.map { |x,_| x }
  ys = points.map { |_,y,_| y }
  bounds = [xs.min, ys.min, xs.max, ys.max]
  
  set = Set.new points.map { |x, y, _| [x, y] }
  
  (bounds[1]..bounds[3]).each do |y|
    (bounds[0]..bounds[2]).each do |x|
      print ((set.member? [x, y]) ? '#' : '*')
    end
    print "\n"
  end
end

def next_step points
  points.map! { |x, y, dx, dy| [x+dx, y+dy, dx, dy] }
end

File.open('inp10a.txt').each do |line|
  points << line.scan(/[0-9\-]+/).map(&:to_i)
end

w = 1000
i = 0

loop do
  next_step points
  ys = points.map { |_,y,_| y }
  w = ys.max - ys.min
  i += 1
  
  if (check_vertical_lines 8, points ) && (w < 15)
    draw_points points
    puts i
    break
  end
end
