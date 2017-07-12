#!/usr/bin/ruby
#
# @file Launcher
#
# @copyright (c) 2010-2012, Christoph Kappel <unexist@dorfelite.net>
# @version $Id: ruby/launcher.rb,v 121 2012/08/26 10:48:23 unexist $
#
# This program can be distributed under the terms of the GNU GPLv2.
# See the file COPYING for details.
#
# Launcher that combines modes/tagging of subtle and a browser search bar.
#
# It opens uris with your default browser via xdg-open. Easiest way to set
# it is to define $BROWSER in your shell rc files.
#
# Thanks, fauno, for your initial work!
#
# Examples:
#
# :urxvt                - Call methods defined in the config
# g subtle wm           - Change to browser view and search for 'subtle wm' via Google
# urxvt @editor         - Open urxvt on view @editor with random tag
# urxvt @editor #work   - Open urxvt on view @editor with tag #work
# urxvt #work           - Open urxvt and tag with tag #work
# urxvt -urgentOnBell   - Open urxvt with the urgentOnBell option
# +urxvt                - Open urxvt and set full mode
# ^urxvt                - Open urxvt and set floating mode
# *urxvt                - Open urxvt and set sticky mode
# =urxvt                - Open urxvt and set zaphod mode
# urx<Tab>              - Open urxvt (tab completion)
#
# Keys:
#
# Up/Down               - Cycle through history (per runtime)
# Left/Right            - Move text cursor
# ESC                   - 1) Hide/exit launcher 2) stop reverse history search
# Enter                 - Run command
# ^R                    - Reverse history search
#
# http://subforge.org/projects/subtle-contrib/wiki/Launcher
#

require 'singleton'
require 'uri'

begin
  require 'subtle/subtlext'
rescue LoadError
  puts ">>> ERROR: Couldn't find subtlext"
  exit
end

# Check for subtlext version
major, minor, teeny = Subtlext::VERSION.split('.').map(&:to_i)
if major == 0 and minor == 10 and 3203 > teeny
  puts ">>> ERROR: launcher needs at least subtle `0.10.3203' (found: %s)" % [
    Subtlext::VERSION
   ]
  exit
end

begin
  require_relative 'levenshtein.rb'
rescue LoadError => err
  puts ">>> ERROR: Couldn't find `levenshtein.rb'"
  exit
end

# Launcher class
module Subtle # {{{
  module Contrib # {{{
    # Precompile regexps
    RE_COMMAND = Regexp.new(/^([+\^\*]*[A-Za-z0-9_\-\/''\s]+)(\s[@#][A-Za-z0-9_-]+)*$/)
    RE_MODES   = Regexp.new(/^([+\^\*]*)([A-Za-z0-9_\-\/''\s]+)/)
    RE_SEARCH  = Regexp.new(/^[gs]\s+(.*)/)
    RE_METHOD  = Regexp.new(/^[:]\s*(.*)/)
    RE_URI     = Regexp.new(/^(http|https):\/\/[a-z0-9]+([\-\.]{1}[a-z0-9]+)*\.[a-z]{2,5}(([0-9]{1,5})?\/.*)?$/ix)
    RE_BROWSER = Regexp.new(/(chrom[e|ium]|iron|navigator|firefox|opera)/i)

    # For history search
    CTRL_R     = "\x12".to_sym

    # Launcher class
    class Launcher # {{{
      include Singleton

      # Default values
      @@font_big   = '-*-*-*-*-*-*-40-*-*-*-*-*-*-*'
      @@font_small = '-*-*-*-*-*-*-14-*-*-*-*-*-*-*'
      @@paths      = '/usr/bin'
      @@screen_num = 0

      # Singleton methods

      ## fonts {{{
      # Set font strings
      # @param [Array]  fonts  Fonts array
      ##

      def self.fonts=(fonts)
        if fonts.is_a?(Array)
          @@font_big   = fonts.first if(1 <= fonts.size)
          @@font_small = fonts.last  if(2 <= fonts.size)
        end
      end # }}}

      ## paths {{{
      # Set launcher path separated by colons
      # @param [String, Array]  paths  Path list separated by colon or array
      ##

      def self.paths=(paths)
        if paths.is_a?(String)
          @@paths = paths
        elsif paths.is_a?(Array)
          @@paths = paths.join(':')
        end
      end # }}}

      ## browser_screen_num {{{
      # Set screen num to show browser view
      # @param [Fixnum]  num  Screen number
      ##

      def self.browser_screen_num=(num)
        @screen_num = num if num.is_a?(Fixnum)
      end # }}}

      ## run {{{
      # Run the launcher
      ##

      def self.run
        self.instance.run
      end # }}}

      # Instance methods

      ## initialize {{{
      # Create launcher instance
      ##

      def initialize
        @candidate = nil
        @browser   = nil
        @view      = nil
        @x         = 0
        @y         = 0
        @width     = 0
        @height    = 0
        @completed = nil
        @reverse   = nil
        @input_pos = 0

        # Buffer
        @input_buf = ''
        @buf_info  = ''

        # Parsed data
        @parsed_tags  = []
        @parsed_views = []
        @parsed_app   = ''
        @parsed_modes = ''

        # Cached data
        @cached_tags    = Subtlext::Tag.all.map(&:name)
        @cached_views   = Subtlext::View.all.map(&:name)
        @cached_apps    = {}
        @cached_history = [ ]

        # FIXME: Find config instance
        if defined?(Subtle::Config)
          ObjectSpace.each_object(Subtle::Config) do |c|
            @cached_sender  = c
            @cached_methods = c.methods(false).map(&:to_s)
          end
        end

        # Something close to a skiplist
        @@paths.split(':').each do |path|
          if Dir.exist?(path)
            Dir.foreach(File.expand_path(path)) do |entry|
              file = File.basename(entry)
              sym  = file[0].to_sym

              # Sort in
              if @cached_apps.has_key?(sym)
                @cached_apps[sym] << file
              else
                @cached_apps[sym] = [ file ]
              end
            end
          else
            puts ">>> ERROR: Skipping non-existing path `%s'" % [ path ]
          end
        end

        # Init for performance
        @array1 = Array.new(20, 0)
        @array2 = Array.new(20, 0)

        # Get colors
        colors = Subtlext::Subtle.colors

        # Create input window
        @input = Subtlext::Window.new(:x => 0, :y => 0,
            :width => 1, :height => 1) do |w|
          w.name        = 'Launcher: Input'
          w.font        = @@font_big
          w.foreground  = colors[:focus_fg]
          w.background  = colors[:focus_bg]
          w.border_size = 0
        end

        # Get font height and y offset of input window
        @font_height1 = @input.font_height + 6
        @font_y1      = @input.font_y

        # Key down and redraw wrappers
        @input.on :key_down do |key, mods|
          begin
            Launcher.instance.key_down(key, mods)
          rescue => err
            puts err, err.backtrace
          end
        end

        @input.on :redraw do
          begin
            Launcher.instance.redraw
          rescue => err
            puts err, err.backtrace
          end
        end

        # Create info window
        @info = Subtlext::Window.new(:x => 0, :y => 0,
            :width => 1, :height => 1) do |w|
          w.name        = 'Launcher: Info'
          w.font        = @@font_small
          w.foreground  = colors[:stipple]
          w.background  = colors.has_key?(:panel_top) ?
            colors[:panel_top] : colors[:panel]
          w.border_size = 0
        end

        # Get font height and y offset of info window
        @font_height2 = @info.font_height + 6
        @font_y2      = @info.font_y
      end # }}}

      ## key_down {{{
      # Key down handler
      # @param  [String]  key   Input key
      # @param  [Array]   mods  Modifier list
      ##

      def key_down(key, mods)
        ret = true

        # Handle keys
        case key
          when :up, :down # {{{
            idx        = (@cached_history.index(@input_buf) +
              (:up == key ? -1 : 1)) rescue -1
            @input_buf = @cached_history[idx] || "" # }}}
            @input_pos = @input_buf.size
          when :left # {{{
            @input_pos -= 1 if 0 < @input_pos # }}}
          when :right # {{{
            if (@reverse and @input_pos < @reverse.size) or
                @input_pos < @input_buf.size
              @input_pos += 1 
           end # }}}
          when :tab # {{{
            complete # }}}
          when :escape # {{{
            # Stop reverse-search with ESC
            if @reverse
              @reverse = nil
            else
              @input_buf = ""
              @buf_info  = ""
              @reverse   = nil
              @candidate = nil
              ret        = false
            end # }}}
          when :backspace # {{{
            if @reverse
              @reverse.chop!
              buf      = @reverse
              chopped  = @reverse.slice(0, @input_pos).chop!
              @reverse = chopped unless chopped.nil?

              if (@input_pos - 1) < buf.size
                slice = buf.slice(@input_pos..-1)
                @reverse += slice unless slice.nil?
              end

              reverse_complete
            else
              buf        = @input_buf
              slice      = @input_buf.slice(0, @input_pos)
              @input_buf = slice.chop unless slice.nil?

              if (@input_pos - 1) < buf.size
                slice = buf.slice(@input_pos..-1)
                @input_buf += slice unless slice.nil?
              end
            end

            @input_pos -= 1 if 0 < @input_pos# }}}
          when :space # {{{
            @input_buf << " "
            @input_pos += 1 # }}}
          when :return # {{{
            @cached_history << @input_buf
            @input_buf      =  ""
            @buf_info       =  ""
            @reverse        =  nil
            ret             =  false # }}}
          when CTRL_R # {{{
            if mods.is_a?(Array) and mods.include?(:control)
              @reverse   = ""
              @input_pos = 0

              reverse_complete
            else
              @input_buf << key.to_s
            end # }}}
          else # {{{
            if @reverse
              @reverse << key.to_s

              reverse_complete
            else
              if @input_pos < @input_buf.size - 1
                slice1 = @input_buf.slice(0, @input_pos)
                slice2 = @input_buf.slice(@input_pos..-1)

                @input_buf = (slice1 || "") + key.to_s + (slice2 || "")
              else
                @input_buf << key.to_s
              end

              @input_pos += 1
            end # }}}
        end

        # Reset completed buffer
        @completed = nil unless :tab == key

        parse

        ret
      end # }}}

      ## redraw {{{
      # Redraw window contents
      ##

      def redraw
        # Fill input window
        @input.clear

        # Add underscore
        slice1 = @input_buf.slice(0, @input_pos)
        slice2 = @input_buf.slice(@input_pos..-1)

        @input.draw_text(3, @font_y1 + 3,
          (slice1 || "") + "_" + (slice2 || ""))

        # Assemble info string
        str =  @buf_info.empty? ? 'Ready..' : @buf_info
        str << ", reverse-search: " + @reverse if @reverse

        # Fill info window
        @info.clear
        @info.draw_text(3, @font_y2 + 3, str)
      end # }}}

      ## move {{{
      # Move launcher windows to current screen
      ##

      def move
        # Geometry
        geo     = Subtlext::Screen.current.geometry
        @width  = geo.width * 80 / 100
        @x      = geo.x + ((geo.width - @width) / 2)
        @y      = geo.y + geo.height - @font_height1 - @font_height2 - 40

        @input.geometry = [ @x, @y, @width, @font_height1 ]
        @info.geometry  = [ @x, @y + @font_height1, @width, @font_height2 ]
      end # }}}

      ## show {{{
      # Show launcher
      ##

      def show
        move

        # Show info first because input blocks
        @info.show
        @input.show
      end # }}}

      ## hide # {{{
      # Hide launcher
      ##

      def hide
        @input.hide
        @info.hide
      end # }}}

      ## run {{{
      # Show and run launcher
      ##

      def run
        show
        hide

        # Check if we have a candidate
        case @candidate
          when Symbol #{{{
            @cached_sender.send(@candidate) # }}}
          when String # {{{
            # Find or create tags
            @parsed_tags.map! do |t|
              tag = Subtlext::Tag.first(t) || Subtlext::Tag.new(t)
              tag.save

              tag
            end

            # Find or create view and add tag
            @parsed_views.each do |v|
              view = Subtlext::View.first(v) || Subtlext::View.new(v)
              view.save

              view.tag(@parsed_tags) unless view.nil? or @parsed_tags.empty?
            end

            # Spawn app, tag it and set modes
            unless (client = Subtlext::Subtle.spawn(@parsed_app)).nil?
              client.tags  = @parsed_tags unless @parsed_tags.empty?

              # Set modes
              unless @parsed_modes.empty?
                flags = []

                # Translate modes
                @parsed_modes.each_char do |c|
                  case c
                    when '+' then flags << :full
                    when '^' then flags << :float
                    when '*' then flags << :stick
                    when '=' then flags << :zaphod
                  end
                end

                client.flags = flags
              end
            end # }}}
          when URI # {{{
            find_browser

            unless @browser.nil?
              Subtlext::Screen[@@screen_num].view = @view
              system("xdg-open '%s' &>/dev/null" % [ @candidate.to_s ])
              @browser.focus
            end # }}}
        end

        @candidate = nil
      end # }}}

      private

      def reverse_complete # {{{
        # Handle reverse search
        if @reverse and not @reverse.empty? and @cached_history.any?
          matches    = @cached_history.reverse.select { |h| h =~ /#{@reverse}/ }
          @input_buf = matches.first || ""
          @input_pos = @input_buf.size
        end
      end # }}}

      def complete # {{{
        guesses = []
        lookup = nil

        # Clear info field
        if @input_buf.empty? or @input_buf.nil?
          redraw
          return
        end

        # Store curret buffer
        @completed = @input_buf if @completed.nil?

        # Select lookup cache
        last = @completed.split(' ').last rescue @completed
        case last[0]
          when '#'
            lookup = @cached_tags
            prefix = '#'
          when '@'
            lookup = @cached_views
            prefix = '@'
          when ':'
            lookup = @cached_methods
            prefix = ':'
          when '+', '^', '*'
            lookup = @cached_apps[last[@parsed_modes.size].to_sym]
            prefix = @parsed_modes
          else
            lookup = @cached_apps[last[0].to_sym]
            prefix = ''
        end

        # Collect guesses
        unless lookup.nil?
          lookup.each do |l|
            guesses << [
              '%s%s' %[ prefix, l ],
              Levenshtein::distance(last.gsub(/^[@#:]/, ''),
                l, 1, 8, 5, @array1, @array2)
            ]
          end

          # Sort by distance and remove it afterwards
          guesses.sort! { |a, b| a[1] <=> b[1] }
          guesses.map! { |a| a.first }

          last = @input_buf.split(' ').last rescue @input_buf
          idx  = (guesses.index(last) + 1) % guesses.size rescue 0

          @candidate  = guesses[idx]
          @input_pos += guesses[idx].size
          @input_buf.gsub!(/#{last}$/, guesses[idx])

          # Convert to symbol if methods are guessed
          @candidate = @candidate.delete(':').to_sym if ':' == prefix
        end
      rescue => err
        puts err, err.backtrace
      end # }}}

      def parse # {{{
        # Handle input
        unless @input_buf.empty? or @input_buf.nil?
          if RE_URI.match(@input_buf)
            @candidate = URI.parse(@input_buf)
            @buf_info  = 'Goto %s' % [ @candidate.to_s ]
          elsif RE_SEARCH.match(@input_buf)
            @candidate = URI.parse(
              'http://www.google.com/#q=%s' % [ URI.escape($1) ]
            )
            @buf_info  = 'Goto %s' % [ @candidate.to_s ]
          elsif RE_METHOD.match(@input_buf)
            @candidate = $1.to_sym
            @buf_info  = 'Call :%s' % [ @candidate ]
          elsif RE_COMMAND.match(@input_buf)
            @candidate    = @input_buf
            @parsed_tags  = []
            @parsed_views = []
            @parsed_app   = ''
            @parsed_modes = ''

            # Parse args
            @candidate.split.each do |arg|
              case arg[0]
                when '#' then @parsed_tags  << arg[1..-1]
                when '@' then @parsed_views << arg[1..-1]
                when '+', '^', '*'
                  app, @parsed_modes, @parsed_app = RE_MODES.match(arg).to_a
                else
                  if @parsed_app.empty?
                    @parsed_app += arg
                  else
                    @parsed_app += ' ' + arg
                  end
              end
            end

            # Add an ad-hoc tag if we don't have any and need one
            if @parsed_views.any? and not @parsed_app.empty? and
                @parsed_tags.empty?
              @parsed_tags << 'tag_%d' % [ rand(1337) ]
            end

            if @parsed_views.any?
              @buf_info = 'Launch %s%s on %s (via %s)' % [
                modes2text(@parsed_modes),
                @parsed_app,
                @parsed_views.join(', '),
                @parsed_tags.join(', ')
              ]
            elsif @parsed_tags.any?
              @buf_info = 'Launch %s%s (via %s)' % [
                modes2text(@parsed_modes),
                @parsed_app,
                @parsed_tags.join(', ')
              ]
            else
              @buf_info = 'Launch %s%s' % [
                modes2text(@parsed_modes), @parsed_app
              ]
            end
          end
        else
          @buf_info = ""
        end

        redraw
      end # }}}

      def modes2text(modes) # {{{
        ret = []

        # Collect mode verbs
        modes.each_char do |c|
          case c
            when '+' then ret << 'full'
            when '^' then ret << 'floating'
            when '*' then ret << 'sticky'
            when '=' then ret << 'zaphod'
          end
        end

        ret.any? ? '%s ' % [ ret.join(', ') ] : ''
      end # }}}

      def find_browser # {{{
        begin
          if @browser.nil?
            Subtlext::Client.all.each do |c|
              if c.klass.match(RE_BROWSER)
                @browser = c
                @view    = c.views.first
                return
              end
            end

            puts '>>> ERROR: No supported browser found'
            puts '           (Supported: Chrome, Firefox and Opera)'
          end
        rescue
          @browser = nil
          @view    = nil
        end
      end # }}}
    end # }}}
  end # }}}
end # }}}

# Implicitly run
if __FILE__ == $0
  # Set fonts
  #Subtle::Contrib::Launcher.fonts = [
  #  'xft:DejaVu Sans Mono:pixelsize=80:antialias=true',
  #  'xft:DejaVu Sans Mono:pixelsize=12:antialias=true'
  #]

  # Set paths
  # Subtle::Contrib::Launcher.paths = [ '/usr/bin', '~/bin' ]

  # Set browser screen
  Subtle::Contrib::Launcher.browser_screen_num = 0

  Subtle::Contrib::Launcher.run
end

# vim:ts=2:bs=2:sw=2:et:fdm=marker
