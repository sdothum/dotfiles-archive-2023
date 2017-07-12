#!/usr/bin/ruby
#
# @file Vitag
#
# @copyright (c) 2010-2011, Christoph Kappel <unexist@dorfelite.net>
# @version $Id: ruby/vitag.rb,v 109 2011/11/05 21:34:39 unexist $
#
# This program can be distributed under the terms of the GNU GPLv2.
# See the file COPYING for details.
#
# Vitag is a helper to edit window/view tagging with any $EDITOR
#
# http://subforge.org/projects/subtle-contrib/wiki/Vitag
#

require 'tempfile'
require 'digest/md5'

begin
  require 'subtle/subtlext'
rescue LoadError
  puts ">>> ERROR: Couldn't find subtlext"
  exit
end

# Check if $EDITOR is set
if ENV['EDITOR'].nil?
  puts <<-EOF
>>> ERROR: Couldn't find $EDITOR envorinment variable
>>>        Please set it like this: export EDITOR=vim
    EOF
  exit
end

# Check whether subtle is running
unless(Subtlext::Subtle.running?)
  puts ">>> ERROR: Couldn't find running subtle"
  exit
end

# Check for subtlext version
major, minor, teeny = Subtlext::VERSION.split('.').map(&:to_i)
if major == 0 and minor == 10 and 3104 > teeny
  puts ">>> ERROR: vitag needs at least subtle `0.10.3104' (found: %s)" % [
    Subtlext::VERSION
   ]
  exit
end

# Collect views and clients
views   = Subtlext::View.all
clients = Subtlext::Client.all

# Create temp file
temp = Tempfile.new('vitag-')

# Fill in tags
temp.puts('# Views')

views.each do |v|
  temp.puts('@%s %s' % [
    v.name,
    v.tags.map { |t| '#%s' % [ t ] }.join(' ')
  ])
end

# Fill in tags
temp.puts('')
temp.puts('# Clients')

clients.each do |c|
  # Remove hashes from string
  name = c.to_str.split('#').first

  temp.puts('%s (%s) %s' % [
    name, c.instance, c.tags.map { |t| '#%s' % [ t ] }.join(' ')
  ])
end

temp.flush

# Store checksum for check
md5 = Digest::MD5.file(temp.path)

# Start editor
system('$EDITOR %s' % [ temp.path ])

temp.rewind

# Check for changes
if md5 != Digest::MD5.file(temp.path)

  # Read temp file
  temp.readlines.each do |line|

    # Handle lines
    case line[0]
      when '@'            then cur = views.shift
      when '#', ' ', "\n" then next
      else                     cur = clients.shift
    end

    # Select tags and sanitize
    tags = line.split('#')[1..-1].map(&:rstrip)

    # Check for valid object
    if cur and tags

      # Find or create tags
      tags.map! do |name|
        tag = Subtlext::Tag.first(name) || Subtlext::Tag.new(name)
        tag.save

        tag
      end

      # Finally assign tags
      cur.tags = tags

      cur = nil
    end
  end
end

temp.close

# vim:ts=2:bs=2:sw=2:et:fdm=marker
