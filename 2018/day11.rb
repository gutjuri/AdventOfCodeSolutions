# Day 11

$inp = 9306

def power_level x, y
  return 0 if x == 0 && y == 0
  rID = x + 10
  (((y * rID + $inp) * rID) / 100) % 10 - 5
end

cells = Array.new(301) { |x| Array.new (301) { |y| power_level x, y } }
summed_area = Array.new(301) { Array.new(301, 0) }

# Make summed area table

(1..300).each do |x|
  (1..300).each do |y|
    summed_area[x][y] = cells[x][y]
    summed_area[x][y] += summed_area[x][y-1]
    summed_area[x][y] += summed_area[x-1][y]
    summed_area[x][y] -= summed_area[x-1][y-1]
  end
end

def maxSquare size, cells
  max_sum = 0
  max_point = [-1, -1]
  (1..(300-size+1)).each do |x|
    (1..(300-size+1)).each do |y|
    
      sum = cells[x+size-1][y+size-1] 
      sum -= cells[x-1][y+size-1]
      sum -= cells[x+size-1][y-1]
      sum += cells[x-1][y-1]
      
      if sum > max_sum
        max_sum = sum
        max_point = [x, y]
      end
    end
  end
  [max_sum, max_point]
end


# Part 1

p maxSquare(3, summed_area)[1].join ','

# Part 2

p (1..300).to_a.map { |n| [maxSquare(n, summed_area), n] }.max.flatten[1..3].join ','
