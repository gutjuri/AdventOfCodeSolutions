# Day 4
# Assumes that the input is stored in inp4a.txt

require 'Date'

format = '%Y-%m-%d %H:%M'

log = File.read('inp4a.txt').split("\n").sort { |a, b| DateTime.strptime(a[1..16], format) <=> DateTime.strptime(b[1..16], format)}

guard_log = Hash.new Array.new(60, 0)

guard = 0
log.each do |line|
  case line
  when /.*#([0-9]*) begins shift/
    guard = $1.to_i
  when /.*:([0-9]{2}).{2}wakes up/
    guard_log[guard] = guard_log[guard].map.with_index { |x, i| i >= $1.to_i ? x-1 : x}
  when /.*:([0-9]{2}).{2}falls asleep/
    guard_log[guard] = guard_log[guard].map.with_index { |x, i| i >= $1.to_i ? x+1 : x}
  end
end

guard_sleeping_most = guard_log.transform_values{ |arr| arr.sum }.max_by { |k, v| v }[0]

puts "part 1: #{guard_sleeping_most * guard_log[guard_sleeping_most].each_with_index.max[1].to_i}"

freq_asleep = guard_log.transform_values { |arr| arr.each_with_index.max }.max_by { |k, v| v }

puts "part 2: #{freq_asleep[0] * freq_asleep[1][1]}"
