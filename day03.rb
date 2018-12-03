# Day 03
# Assumes that the input is stored as "inp3s.txt"

fields = Array.new(1000) { |x| Array.new(1000, 0) }

regex = /.([0-9]*).{3}([0-9]*).([0-9]*).{2}([0-9]*).([0-9]*)/

patches = File.read('inp3a.txt').split("\n").map { |l| l.match(regex).captures.map(&:to_i) }

patches.each do |area|
  area[3].times do |x|
    area[4].times do |y| 
      fields[area[1] + x][area[2] + y] += 1
    end
  end
end

p "Overlapping inches: " + fields.flatten.keep_if { |x| x > 1 }.count.to_s

patches.each do |area|
  overlap = false
  area[3].times do |x|
    area[4].times do |y| 
      overlap = true if fields[area[1] + x][area[2] + y] > 1
    end
  end
  p "Non-overlapping patch is patch nr. " + area[0].to_s if !overlap
end
