#!/usr/bin/env ruby
# frozen_string_literal: true

mapping = {
  'A' => 'A',
  'B' => 'X',
  'C' => 'J',
  'D' => 'E',
  'E' => '.',
  'F' => 'Y',
  'G' => 'I',
  'H' => 'D',
  'I' => 'C',
  'J' => 'H',
  'K' => 'T',
  'L' => 'N',
  'M' => 'M',
  'N' => 'B',
  'O' => 'R',
  'P' => 'L',
  'Q' => "'",
  'R' => 'P',
  'S' => 'O',
  'T' => 'Y',
  'U' => 'G',
  'V' => 'K',
  'W' => ',',
  'X' => 'J',
  'Y' => 'F',
  'Z' => ';',
}.freeze

xs = mapping.reject do |k, v|
  v !~ /^[a-z]$/i
end.flat_map do |k, v|
  [[k.upcase, v.upcase], [k.downcase, v.downcase]]
end.map do |k, v|
  [k.codepoints.first, v.codepoints.first]
end.each_with_object([]) do |(i, v), xs|
  xs[i] = v
end

puts xs.map { |x| x || 0 }
