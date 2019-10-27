require 'digest'

prefix = 'yzbqklnj'

puts (1..Float::INFINITY).lazy.find { |i| Digest::MD5.hexdigest("#{prefix}#{i}").start_with?('000000') }