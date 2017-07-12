#!/usr/bin/ruby
#
# @file Positioner
#
# @copyright (c) 2011-2012, Christoph Kappel <unexist@dorfelite.net>
# @version $Id: ruby/positioner.rb,v 119 2012/07/10 13:55:05 unexist $
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

require 'singleton'

begin
  require 'subtle/subtlext'
rescue LoadError
  puts ">>> ERROR: Couldn't find subtlext"
  exit
end

# Check for subtlext version
major, minor, teeny = Subtlext::VERSION.split('.').map(&:to_i)
if major == 0 and minor == 10 and 3216 > teeny
  puts ">>> ERROR: positioner needs at least subtle `0.10.3216' (found: %s)" % [
    Subtlext::VERSION
   ]
  exit
end

# Positioner class
module Subtle # {{{
  module Contrib # {{{
    class Positioner # {{{
      include Singleton

      # Default values
      @@font = '-*-*-medium-*-*-*-14-*-*-*-*-*-*-*'

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

        # Create main window
        @win = Subtlext::Window.new(:x => 0, :y => 0, :width => 1, :height => 1) do |w|
          w.name        = 'Positioner'
          w.font        = @@font
          w.foreground  = @colors[:title_fg]
          w.background  = @colors[:title_bg]
          w.border_size = 0
        end

        # Font metrics
        @font_height = @win.font_height + 6
        @font_y      = @win.font_y

        # Handler
        @win.on(:key_down, method(:key_down))
        @win.on(:draw,     method(:redraw))
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

      def key_down(key, mods)
        ret = true

        case key
          when :left, :up # {{{
            idx       = @views.index(@cur_sel)
            idx      -= 1 if 0 < idx
            @cur_sel  = @views[idx] # }}}
          when :right, :down # {{{
            idx       = @views.index(@cur_sel)
            idx      += 1 if idx < (@views.size - 1)
            @cur_sel  = @views[idx] # }}}
          when :space # {{{
            if @selected.include?(@cur_sel)
              @selected.delete(@cur_sel)
              @unselected << @cur_sel
            else
              @selected << @cur_sel
            end # }}}
          when :return # {{{
            tags = @cur_client.tags

            # Add view tags
            @selected.each do |sel|
              unless @cur_views.include?(sel)
                # Find or create tag
                tag = Subtlext::Tag.first(sel.name) || Subtlext::Tag.new(sel.name)
                tag.save

                # Add tag to view
                sel.tag(tag) unless sel.tags.include?(sel.name)

                tags << tag
              end
            end

            # Remove unselected views from tags
            tags -= @unselected.map(&:name).map { |n| Subtlext::Tag.first(n) }

            # Finally apply tags
            @cur_client.tags = tags

            ret = false # }}}
          when :escape # {{{
            ret = false # }}}
        end

        redraw(@win) if ret

        ret
      end # }}}

      ## update # {{{
      # Update clients and windows
      ##

      def update
        @views      = Subtlext::View.all
        @selected   = []
        @unselected = []
        @cur_client = Subtlext::Client.current
        @cur_views  = @cur_client.views
        @cur_sel    = Subtlext::View.current

        names = @views.map(&:name)

        # Updated selected list
        @cur_client.tags.each do |t|
          if names.include?(t.name)
            @selected << @views[names.index(t.name)]
          end
        end

        arrange
      end # }}}

      ## arrange {{{
      # Arrange window and subwindows
      ##

      def arrange
        geo     = @cur_client.geometry
        width   = geo.width #< Max width
        height  = @font_height
        wx      = 0
        wy      = 0
        len     = 0
        wwidth  = 0

        # Arrange client windows
        @views.each_with_index do |v, i|
          len = @win.font_width(v.name) + 6

          # Wrap lines
          if wx + len > width
            wwidth  = wx if wx > wwidth
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
          if @views[i] == @cur_sel
            fg = @colors[:focus_fg]
            bg = @colors[:focus_bg]
          elsif @cur_views.include?(@views[i])
            fg = @colors[:occupied_fg]
            bg = @colors[:occupied_bg]
          else
            fg = @colors[:views_fg]
            bg = @colors[:views_bg]
          end

          if @selected.include?(@views[i])
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
if __FILE__ == $0
  # Set font
  #Subtle::Contrib::Merger.font =
  # 'xft:DejaVu Sans Mono:pixelsize=80:antialias=true'

  Subtle::Contrib::Positioner.run
end

# vim:ts=2:bs=2:sw=2:et:fdm=marker
