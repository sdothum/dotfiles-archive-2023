#!/usr/bin/ruby
#
# @file Selector
#
# @copyright (c) 2011-2012, Christoph Kappel <unexist@dorfelite.net>
# @version $Id: ruby/selector.rb,v 118 2012/07/10 13:50:28 unexist $
#
# Client selector that works like the subscription selector in google reader.
#
# Colors:
#
# Focus      - Currently selected client
# Occupied   - Visible clients on current views
# Unoccupied - Currently no visible clients
#
# Keys:
#
# Left, Up          - Move to left
# Right, Down       - Move to right
# Tab               - Cycle through windows/matches
# Escape            - Leave input mode/exit selector
# Return            - Focus currently selected and hide/exit selector
# Any capital/digit - Select client prefixed with capital letter/digit
# Any text          - Select client with matching instance name
#
# http://subforge.org/projects/subtle-contrib/wiki/Selector
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
  puts ">>> ERROR: selector needs at least subtle `0.10.3216' (found: %s)" % [
    Subtlext::VERSION
   ]
  exit
end

# Launcher class
module Subtle # {{{
  module Contrib # {{{
    class Selector # {{{
      include Singleton

      # Prefix letters
      LETTERS = ((49..57).to_a|(65..89).to_a).map(&:chr)

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
        @colors   = Subtlext::Subtle.colors
        @expanded = false
        @buffer   = ''
        @x        = 0
        @y        = 0
        @width    = 0
        @height   = 0

        # Create main window
        @win = Subtlext::Window.new(:x => 0, :y => 0, :width => 1, :height => 1) do |w|
          w.name        = 'Selector'
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
      # Show and run launcher
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
            idx       = @clients.index(@current) || 0
            idx      -= 1 if 0 < idx
            @current  = @clients[idx] # }}}
          when :right, :down # {{{
            idx       = @clients.index(@current) || 0
            idx      += 1 if idx < (@clients.size - 1)
            @current  = @clients[idx] # }}}
          when :return # {{{
            @current.focus

            ret = false # }}}
          when :escape # {{{
            if @expanded
              @buffer   = ''
              @expanded = false
            else
              ret = false
            end

            arrange # }}}
          when :backspace # {{{
            if @expanded
              @expanded = (0 < @buffer.chop!.size)

              arrange
            end # }}}
          when :tab # {{{
            if @buffer.empty?
              clients = @clients
            else
              # Select matching clients
              clients = @clients.select do |c|
                c.instance.downcase.start_with?(@buffer)
              end
            end

            unless (idx = clients.index(@current)).nil?
              # Cycle between clients
              if idx < (clients.size - 1)
                idx += 1
              else
                idx = 0
              end

              @current = clients[idx]
            end # }}}
          else # {{{
            str = key.to_s

            if !(idx = LETTERS.index(str)).nil? and idx < @clients.size
              @clients[idx].focus

              ret = false
            elsif !str.empty?
              @buffer << str.downcase

              @clients.each do |c|
                if c.instance.downcase.start_with?(@buffer)
                  @current = c

                  break
                end
              end

              arrange
            end # }}}
        end

        redraw(@win) if ret

        ret
      end # }}}

      ## update # {{{
      # Update clients and windows
      ##

      def update
        @buffer  = ''
        @clients = Subtlext::Client.all
        @visible = Subtlext::Client.visible
        @current = Subtlext::Client.current rescue nil

        arrange
      end # }}}

      ## arrange {{{
      # Arrange window
      ##

      def arrange
        geo     = Subtlext::Screen.current.geometry
        @width  = geo.width * 50 / 100 #< Max width
        @height = @font_height
        wx      = 0
        wy      = 0
        len     = 0
        wwidth  = 0

        # Toggle expand
        if @buffer.empty?
          @height   = @font_height
          @expanded = false
        else
          @height   += @font_height
          @expanded  = true
        end

        # Calculate window width
        @clients.each_with_index do |c, i|
          str  = '%s:%s' % [ LETTERS[i], c.instance ]
          len  = @win.font_width(str) + 6

          # Wrap lines
          if wx + len > @width
            wwidth  = wx if wx > wwidth
            wx      = 0
            wy     += @font_height
          end

          wx += len
        end

        # Update window geometry
        @width   = 0 == wwidth ? wx : wwidth
        @height += wy
        @x       = geo.x + ((geo.width - @width) / 2)
        @y       = geo.y + ((geo.height - @height) / 2)

        @win.geometry = [ @x , @y, @width, @height ]
      end # }}}

      ## redraw {{{
      # Redraw window content
      # @param [Window]  w  Window instance
      ##

      def redraw(w)
        wx     = 0
        wy     = 0
        len    = 0
        wwidth = 0

        @win.clear

        # Render window
        @clients.each_with_index do |c, i|
          str  = '%s:%s' % [ LETTERS[i], c.instance ]
          len  = @win.font_width(str) + 6

          # Wrap lines
          if wx + len > @width
            wwidth  = wx if wx > wwidth
            wx      = 0
            wy     += @font_height
          end

          # Select color
          if @clients[i] == @current
            fg = @colors[:focus_fg]
            bg = @colors[:focus_bg]
          elsif @visible.include?(@clients[i])
            fg = @colors[:occupied_fg]
            bg = @colors[:occupied_bg]
          else
            fg = @colors[:views_fg]
            bg = @colors[:views_bg]
          end

          @win.draw_rect(wx, wy, len, @font_height, bg, true)
          @win.draw_text(wx + 3, wy + @font_y + 3, str, fg)

          wx += len
        end

        # Draw input buffer
        unless @buffer.empty?
          @win.draw_text(6, @height - @font_height + @font_y + 3,
            'Input: %s' % [ @buffer ])
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
  #Subtle::Contrib::Selector.font =
  # 'xft:DejaVu Sans Mono:pixelsize=80:antialias=true'

  Subtle::Contrib::Selector.run
end

# vim:ts=2:bs=2:sw=2:et:fdm=marker
