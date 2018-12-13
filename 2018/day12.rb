# Day 12

require 'set'

inp = File.readlines('inp12a.txt')

state = inp[0].chomp.gsub 'initial state: ', ''
rules = Hash[inp.drop(2).map{ |x| x.chomp.split ' => '}]
rules.default = '.'

def substrings size, str
  len = str.size
  (0..(len-size)).to_a.map { |i| str[i, size] }
end

def next_step state, rules
  new_state = ""
  lpad = 4
  rpad = 4

  state = ('.'*lpad) + state + ('.'*rpad)

  substrings(5, state).each do |sub|
    plant = rules[sub]
    new_state << plant
  end
  
  new_state.sub!(/\.*$/, '').sub!(/(\.*)/, '')
  
  [new_state, (lpad-2)-Regexp.last_match(1).size]
end

def run_n_times n, state, rules
  pos_of_zero = 0
  n.times do
    state, zero_shift = next_step state, rules
    pos_of_zero += zero_shift
  end
  [state, pos_of_zero]
end

def score str, pos_of_zero
  str.each_char.with_index.map{ |c, i| c == '#' ? (i - pos_of_zero) : 0}.sum
end

# Part 1

state_20, pos_of_zero = run_n_times 20, state, rules

puts "Part 1: #{score state_20, pos_of_zero}" 

# Part 2

g = 200 # we are sure that by generation g the periodical behaviour has begun.

s1, z1 = run_n_times (g+1), state, rules
s2, z2 = run_n_times g, state, rules

dv = (score s1, z1) - (score s2, z2)


state_n, pos_of_zero1 = run_n_times g, state, rules

score_n = score state_n, pos_of_zero1

puts "Part 2: #{score_n + ((50000000000-g)*dv)}" 
