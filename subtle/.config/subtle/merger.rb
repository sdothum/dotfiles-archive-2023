#!/usr/bin/ruby
#
# @file Merger
#
# @copyright (c) 2011, Christoph Kappel <unexist@dorfelite.net>
# @version $Id: ruby/merger.rb,v 105 2011/10/07 18:51:24 unexist $
#
# This program can be distributed under the terms of the GNU GPLv2.
# See the file COPYING for details.
#
# Select and tag/untag visible views of current client window
#
# Colors:
#
# Focus    - Currently selected view
# View     - Other views
# Occupied - Views client is visible
# Urgent   - Selected views
#
# Keys:
#
# Left, Up    - Move to left
# Right, Down - Move to right
# Escape      - Hide/exit
# Space       - Select view
# Return      - Tag/untag selected views and exit hide/exit
#
# http://subforge.org/projects/subtle-contrib/wiki/Positioner
#

require "singleton"

begin
  require "subtle/subtlext"
rescue LoadError
  puts ">>> ERROR: Couldn't find subtlext"
  exit
end

# Check for subtlext version
major, minor, teeny = Subtlext::VERSION.split(".").map(&:to_i)
if(major == 0 and minor == 10 and 3006 > teeny)
  puts ">>> ERROR: merger needs at least subtle `0.10.3006' (found: %s)" % [
    Subtlext::VERSION
   ]
  exit
end

# Merger class
module Subtle # {{{
  module Contrib # {{{
    class Merger # {{{
      include Singleton

      # Default values
      @@font = "-*-*-medium-*-*-*-14-*-*-*-*-*-*-*"

      # Singleton methods

      ## fonts {{{
      # Set font strings
      # @param [String]  fonts  Fonts array
      ##

      def self.font=(font)
        @@font = font
      end # }}}

      ## run {{{
      # Run expose
      ##

      def self.run
        self.instance.run
      end # }}}

      # Instance methods

      ## initialize {{{
      # Create expose instance
      ##

      def initialize
        # Values
        @colors = Subtlext::Subtle.colors
        @merged = {}
        @backup = {}

        # Create main window
        @win = Subtlext::Window.new(:x => 0, :y => 0, :width => 1, :height => 1) do |w|
          w.name        = "Merger"
          w.font        = @@font
          w.foreground  = @colors[:title_fg]
          w.background  = @colors[:title_bg]
          w.border_size = 0
        end

        # Font metrics
        @font_height = @win.font_height + 6
        @font_y      = @win.font_y

        # Handler
        @win.on :key_down, method(:key_down)
        @win.on :draw, method(:redraw)
      end # }}}

      ## run {{{
      # Show and run positioner
      ##

      def run
        update
        show
        hide
      end # }}}

      private

      ## key_down {{{
      # Key down handler
      # @param [String]  key  Pressed key
      ##

      def key_down(key)
        ret = true

        case key
          when :left, :up # {{{
            idx        = @views.index(@selected)
            idx       -= 1 if(1 < idx)
            @selected  = @views[idx] # }}}
          when :right, :down # {{{
            idx        = @views.index(@selected)
            idx       += 1 if(idx < (@views.size - 1))
            @selected  = @views[idx] # }}}
          when :space # {{{
            if(@merged[@current.name].include?(@selected))
              @merged[@current.name].delete(@selected)
            else
              @merged[@current.name] << @selected
            end # }}}

            p @merged
          when :return # {{{
            # Restore tags or update
            if(@merged[@current.name].empty?)
              @merged.delete(@current.name)
              @current.tags = @backup[@current.name]
            else
              @current.tags = @merged[@current.name].inject(
                @backup[@current.name]) { |r, v| r | v.tags }
            end

            ret = false # }}}
          when :escape # {{{
            @merged.delete(@current.name)
            ret = false # }}}
        end

        redraw(@win) if(ret)

        ret
      end # }}}

      ## update # {{{
      # Update clients and windows
      ##

      def update
        @current  = Subtlext::View.current
        @views    = Subtlext::View.all.select { |v| v != @current }
        @selected = @views.first

        @views.unshift(@current)

        # Backup tags of current view
        unless(@backup.keys.include?(@current.name))
          @backup[@current.name] = @current.tags
        end

        # Create empty array
        unless(@merged.keys.include?(@current.name))
          @merged[@current.name] = []
        end

        arrange
      end # }}}

      ## arrange {{{
      # Arrange window and subwindows
      ##

      def arrange
        geo     = Subtlext::Screen.current.geometry
        width   = geo.width * 50 / 100 #< Max width
        height  = @font_height
        wx      = 0
        wy      = 0
        len     = 0
        wwidth  = 0

        # Arrange client windows
        @views.each_with_index do |v, i|
          len = @win.font_width(v.name) + 6

          # Wrap lines
          if(wx + len > width)
            wwidth  = wx if(wx > wwidth)
            wx      = 0
            wy     += @font_height
          end

          wx += len
        end

        # Update geometry
        width   = 0 == wwidth ? wx : wwidth
        height += wy
        x       = geo.x + ((geo.width - width) / 2)
        y       = geo.y + ((geo.height - height) / 2)

        @win.geometry = [ x , y, width, height ]
      end # }}}

      ## redraw {{{
      # Redraw window content
      # @param [Window]  w  Window instance
      ##

      def redraw(w)
        @win.clear

        wx  = 0
        wy  = 0
        len = 0

        @views.each_with_index do |v, i|
          len = @win.font_width(v.name) + 6

          # Select color
          if(v == @selected)
            fg = @colors[:focus_fg]
            bg = @colors[:focus_bg]
          elsif(v == @current)
            fg = @colors[:occupied_fg]
            bg = @colors[:occupied_bg]
          else
            fg = @colors[:unoccupied_fg]
            bg = @colors[:unoccupied_bg]
          end

          if(@merged[@current.name].include?(v))
            fg = @colors[:urgent_fg]
          end

          @win.draw_rect(wx, wy, len, @font_height, bg, true)
          @win.draw_text(wx + 3, wy + @font_y + 3, v.name, fg)

          wx += len
        end
      end # }}}

      ## show {{{
      # Show launcher
      ##

      def show
        @win.show
      end # }}}

      ## hide # {{{
      # Hide launcher
      ##

      def hide
        @win.hide
      end # }}}

    end # }}}
  end # }}}
end # }}}

# Implicitly run
if(__FILE__ == $0)
  # Set font
  #Subtle::Contrib::Merger.font =
  # "xft:DejaVu Sans Mono:pixelsize=80:antialias=true"

  Subtle::Contrib::Merger.run
end

# vim:ts=2:bs=2:sw=2:et:fdm=marker
