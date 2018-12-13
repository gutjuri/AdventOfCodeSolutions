# Day 13

rails = Array.new
trains = []
char_mappings = Hash.new { |h, k| h[k] = k }
char_mappings['>'] = '-'
char_mappings['<'] = '-'
char_mappings['^'] = '|'
char_mappings['v'] = '|'

$DIRS = ['>', 'v', '<', '^']

File.readlines('inp13a.txt').each_with_index do |line, i|
  rails[i] = []
  line.chomp.each_char.with_index do |c, ci|
    rails[i] << char_mappings[c]
    trains << [[i, ci], c, 0] if $DIRS.member? c
  end
end

def move_fwd pos, dir
  case dir
    when '^' then [pos[0]-1, pos[1]]
    when '<' then [pos[0], pos[1]-1]
    when '>' then [pos[0], pos[1]+1]
    when 'v' then [pos[0]+1, pos[1]]
  end
end

def left dir
  $DIRS[($DIRS.index(dir)+3)%4]
end

def right dir
  $DIRS[($DIRS.index(dir)+1)%4]
end

def switch train
  train[2] += 1
  case (train[2]-1)%3
  when 0 then left train[1]
  when 1 then train[1]
  when 2 then right train[1]
  end
end

def new_dir train, rail
  case rail
    when '+' then switch train
    when '/' 
      case train[1]
      when />|</ then left train[1]
      else right train[1]
      end
    when '\\' 
      case train[1]
      when />|</ then right train[1]
      else left train[1]
      end
    else train[1]
  end
end

def step_and_check rails, trains
  trains.sort!
  coll = nil
  to_rem = []
  trains.each do |t|
    t[0] = move_fwd t[0], t[1]
    t[1] = new_dir t, rails[t[0][0]][t[0][1]]
    if trains.select { |o| o[0] == t[0] }.size > 1
      coll = t[0]
      to_rem << trains.select { |o| o[0] == t[0] }[0]
      to_rem << trains.select { |o| o[0] == t[0] }[1]
    end
  end
  trains.delete_if { |o| to_rem.member? o }
  coll
end

# Part 1

coll = nil
while coll.nil?
  coll = step_and_check rails, trains
end

p [coll[1], coll[0]]

# Part 2

while trains.size != 1
  step_and_check rails, trains
end

p [trains[0][0][1], trains[0][0][0]]
