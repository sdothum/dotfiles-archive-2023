#
# @file Calculate Levenshtein distance
#
# @copyright (c) 2010-2011, Christoph Kappel <unexist@dorfelite.net>
# @version $Id: ruby/levenshtein.rb,v 117 2012/06/28 11:57:12 unexist $
#
# This program can be distributed under the terms of the GNU GPLv2.
# See the file COPYING for details.
#

module Levenshtein
  MAX_LENGTH = 255

   ## distance {{{
   # @brief Calculate the Levenshtein Distance
   # @param [String]  s1        First string
   # @param [String]  s2        Second string
   # @param [Fixnum]  cost_ins  Cost for insertion
   # @param [Fixnum]  cost_rep  Cost for replace
   # @param [Fixnum]  cost_del  Cost for deletion
   # @param [Array]   a1        First array
   # @param [Array]   a2        Second array
   # @raise [String]  Error
   # @return [Fixnum] Calculated distance
   ##

  def self.distance(s1, s2, cost_ins = 1, cost_rep = 1,
      cost_del = 1, a1 = nil, a2 = nil)
    # Step 1: Check string length
    l1 = s1.length
    l2 = s2.length

    # Check length
    return l2 * cost_ins if 0 == l1
    return l1 * cost_del if 0 == l2

    raise "Max length" if l1 > MAX_LENGTH || l2 > MAX_LENGTH

    # Step 2: Create and init arrays
    p1 = a1 || Array.new(l2 + 1, 0)
    p2 = a2 || Array.new(l2 + 1, 0)

    (0..l2).each { |i| p1[i] = i * cost_ins }

    # Step 3: Iterate over string s1
    (0..(l1 - 1)).each do |i|
      p2[0] = p1[0] + cost_del

      # Step 4: Iterate over string s2
      (0..(l2 - 1)).each do |j|
        # Step 5: Get cost
        c0 = p1[j] + ((s1[i] == s2[j]) ? 0 : cost_rep)
        c1 = p1[j + 1] + cost_del
        c0 = c1 if c1 < c0

        c2 = p2[j] + cost_ins
        c0 = c2 if c2 < c0

        # Step 6: Store min value in matrix
        p2[j + 1] = c0
      end

      # Swap arrays
      tmp = p1
      p1  = p2
      p2  = tmp
    end

    # Step 7: Return distance
    c0 = p1[l2]

    c0
  end # }}}
end # }}}
