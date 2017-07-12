#!/usr/bin/ruby
#
# @file Styler
#
# @copyright (c) 2010-2011, Christoph Kappel <unexist@dorfelite.net>
# @version $Id: ruby/styler.rb,v 117 2012/06/28 11:57:12 unexist $
#
# This program can be distributed under the terms of the GNU GPLv2.
# See the file COPYING for details.
#
# Styler is a helper to create or change subtle color themes
#
# http://subforge.org/projects/subtle-contrib/wiki/Styler
#

require 'singleton'

begin
  require 'subtle/subtlext'
rescue LoadError
  puts ">>> ERROR: Couldn't find subtlext"
  exit
end

begin
  require 'gtk2'
rescue LoadError
    puts <<EOF
>>> ERROR: Couldn't find the gem `gtk2'
>>>        Please install it with following command:
>>>        gem install gtk2
EOF
  exit
end

# Check whether subtle is running
unless(Subtlext::Subtle.running?)
  puts ">>> WARNING: Couldn't find running subtle"
  #exit
end

# Check for subtlext version
major, minor, teeny = Subtlext::VERSION.split(".").map(&:to_i)
if(major == 0 and minor == 10 and 2945 > teeny)
  puts ">>> ERROR: styler needs at least subtle `0.10.2945' (found: %s)" % [
    Subtlext::VERSION
   ]
  exit
end

# Styler class
module Subtle # {{{
  module Contrib # {{{
    class ColorButton
      DEFAULT_COLOR = '#000000'

      attr_reader :value

      ## initialize {{{
      # Create a new color button
      # @param [String]  color  Default color
      ##

      def initialize(color = DEFAULT_COLOR, &block)
        @value    = color
        @original = color
        @callback = nil

        # Create color button
        @button = Gtk::ColorButton.new(
          Gdk::Color.parse(color)
        )

        # Signal handler
        @button.signal_connect('color-set') do |button|
          # Assemble hex color
          @value = '#%02x%02x%02x' % [
            factor_round((button.color.red.to_f   / 65535), 255),
            factor_round((button.color.green.to_f / 65535), 255),
            factor_round((button.color.blue.to_f  / 65535), 255)
          ]

          @callback.call(@value) unless(@callback.nil?)

          Styler.instance.render
        end
      end # }}}

      ## value= {{{
      # Set value
      # @param  [Fixnum]  value  New value
      ##

      def value=(value)
        @value = value
        @button.set_color(Gdk::Color.parse(@value))
      end # }}}

      ## attach {{{
      # Attach color button to table
      # @param [Table]   table  A #Gtk::Table
      # @param [Fixnum]  x      X slot
      # @param [Fixnum]  y      Y slot
      ##

      def attach(table, x, y)
        table.attach(@button, x, x + 1, y, y + 1, Gtk::FILL, Gtk::SHRINK)
      end # }}}

      ## reset {{{
      # Reset color button to default color
      ##

      def reset
        @value = @original
        @button.set_color(Gdk::Color.parse(@original))
      end # }}}

      ## callback {{{
      # Add event callback
      ##

      def callback(&block)
        @callback = block
      end # }}}

      private

      def factor_round(val, factor) # {{{
        val = (val * factor + 0.5).floor
        val = val > 0      ? val    : 0   #< Min
        val = val > factor ? factor : val #< Max

        val
      end # }}}
    end

    class SpinButton
      attr_reader :value

      ## initialize {{{
      # Create a new spin button
      # @param [Fixnum]  value  Default value
      # @param [Fixnum]  min    Minimum value
      # @param [Fixnum]  max    Maximum value
      ##

      def initialize(value = 0, min = 0, max = 100)
        @value    = value
        @original = value
        @callback = nil

        # Create adjustment
        adjustment = Gtk::Adjustment.new(value, min, max, 1, 5, 0)

        # Create spin button
        @spinner = Gtk::SpinButton.new(adjustment, 0, 0)
        @spinner.set_size_request(40, -1)

        # Signal handler
        @spinner.signal_connect('value_changed') do |spinner|
          @value = spinner.value_as_int

          @callback.call(@value) unless(@callback.nil?)

          Styler.instance.render
        end
      end # }}}

      ## value= {{{
      # Set value
      # @param  [Fixnum]  value  New value
      ##

      def value=(value)
        @value = value
        @spinner.value = value
      end # }}}

      ## attach {{{
      # Attach spin button to table
      # @param [Table]   table  A #Gtk::Table
      # @param [Fixnum]  x      X slot
      # @param [Fixnum]  y      Y slot
      ##

      def attach(table, x, y)
        table.attach(@spinner, x, x + 1, y, y + 1, Gtk::FILL, Gtk::SHRINK)
      end # }}}

      ## reset {{{
      # Reset spin button to default value
      ##

      def reset
        @value         = @original
        @spinner.value = @original
      end # }}}

      ## callback {{{
      # Add event callback
      ##

      def callback(&block)
        @callback = block
      end # }}}
    end

    class Style
      attr_accessor :buttons

      ## reset {{{
      # Reset elements of style
      ##

      def reset
        @buttons.each do |k, v|
          if v.respond_to?(:reset)
            v.reset
          elsif(v.is_a?(Hash))
            v.each do |side, spin|
              spin.reset if(spin.respond_to?(:reset))
            end
          end
        end
      end # }}}

      ## append {{{
      # Append to notebook
      # @param [Notebook]  notebook A #Gtk::Notebook
      ##

      def append(notebook)
        label  = Gtk::Label.new(@name)
        notebook.append_page(@table, label)
      end # }}}

      ## [] {{{
      # Acces values
      # @param [Symbol]  name  Value name
      ##

      def [](name)
        case @buttons[name]
          when ColorButton then @buttons[name].value
          when Hash        then @buttons[name]
        end
      end # }}}

      ## attach_label {{{
      # Attach a label to the table
      # @param [Fixnum]  x        X slot
      # @param [Fixnum]  y        Y slot
      # @param [String]  caption  Label caption
      ##

      def attach_label(x, y, caption)
        label = Gtk::Label.new(caption)
        @table.attach(label, x, x + 1, y, y + 1)
      end # }}}

      ## sum {{{
      # Get total width of given styles
      # @param  [Array]  List of styles
      # @return [Fixnum] Width in pixel
      ##

      def sum(list)
        width = 0

        [ :border, :padding, :margin ].each do |button|
          list.each do |l|
            if(@buttons[button][l].respond_to?(:value))
              width += @buttons[button][l].value
            end
          end
        end

        width
      end # }}}

      private

      def compact(name) # {{{
        ret = []
        val = @buttons[name]

        # Compact padding/margin values
        if(val[:top].value == val[:bottom].value and
            val[:top].value == val[:right].value and
            val[:top].value == val[:left].value)
          ret << val[:top].value
        elsif(val[:top].value == val[:bottom].value and
            val[:left].value == val[:right].value)
          ret << val[:top].value
          ret << val[:left].value
        elsif(val[:left].value == val[:right].value)
          ret << val[:top].value
          ret << val[:left].value
          ret << val[:bottom].value
        else
          ret = val.values.map(&:value)
        end

        ret.join(', ')
      end # }}}
    end

    class StyleNormal < Style

      ## initialize {{{
      # Create a new style element
      # @param  [String]  name    Element name
      # @param  [Array]   colors  Color array
      ##

      def initialize(name, colors)
        @name    = name
        @buttons = {}

        # Table
        @table = Gtk::Table.new(5, 7)

        # Labels: Vertical
        attach_label(0, 1, 'Foreground')
        attach_label(0, 2, 'Background')
        attach_label(0, 3, 'Top')
        attach_label(0, 4, 'Right')
        attach_label(0, 5, 'Bottom')
        attach_label(0, 6, 'Left')

        # Labels: Horizontal
        attach_label(1, 0, 'Color')
        attach_label(2, 0, 'Border')
        attach_label(3, 0, 'Padding')
        attach_label(4, 0, 'Margin')

        # Color buttons
        y = 1

        {
           :fg    => 'fg',       :bg     => 'bg',        :top  => 'bo_top',
           :right => 'bo_right', :bottom => 'bo_bottom', :left => 'bo_left'
        }.each do |name, suffix|
          @buttons[name] = ColorButton.new(
            colors[('%s_%s' % [ @name.downcase, suffix ]).to_sym] ||
              ColorButton::DEFAULT_COLOR
          )
          @buttons[name].attach(@table, 1, y)

          y += 1
        end

        # Spin buttons
        x = 2

        [ :border, :padding, :margin ].each do |name|
          @buttons[name] = {}
          y              = 3

          [ :top, :right, :bottom, :left ].each do |side|
            @buttons[name][side] = SpinButton.new(0)
            @buttons[name][side].attach(@table, x, y)

            y += 1
          end

          x += 1
        end
      end # }}}

      ## dump {{{
      # Print style element
      ##

      def dump
        puts "style :#{@name.downcase} do"
        puts "  foreground    '#{@buttons[:fg].value}'"
        puts "  background    '#{@buttons[:bg].value}'"

        # Compact borders
        if(@buttons[:top].value == @buttons[:right].value and
            @buttons[:top].value == @buttons[:bottom].value and
            @buttons[:top].value == @buttons[:left].value and
            @buttons[:border][:top].value == @buttons[:border][:left].value and
            @buttons[:border][:top].value == @buttons[:border][:bottom].value and
            @buttons[:border][:top].value == @buttons[:border][:right].value)
          puts "  border        '#{@buttons[:top].value}', #{@buttons[:border][:top].value}"
        else
          puts "  border_top    '#{@buttons[:top].value}', #{@buttons[:border][:top].value}"
          puts "  border_right  '#{@buttons[:right].value}', #{@buttons[:border][:right].value}"
          puts "  border_bottom '#{@buttons[:bottom].value}', #{@buttons[:border][:bottom].value}"
          puts "  border_left   '#{@buttons[:left].value}', #{@buttons[:border][:left].value}"
        end

        puts "  padding       #{compact(:padding)}"
        puts "  margin        #{compact(:margin)}"
        puts "end"
        puts
      end # }}}
    end

    class StyleClients < Style

      ## initialize {{{
      # Create a new other element
      # @param  [String]  name    Element name
      # @param  [Array]   colors  Color array
      ##

      def initialize(name, colors)
        @name    = name
        @table   = Gtk::Table.new(5, 7)
        @buttons = {}

        # Labels: Vertical
        attach_label(0, 1, 'Client active')
        attach_label(0, 2, 'Client inactive')
        attach_label(0, 3, 'Top')
        attach_label(0, 4, 'Right')
        attach_label(0, 5, 'Bottom')
        attach_label(0, 6, 'Left')

        # Labels: Horizontal
        attach_label(1, 0, 'Color')
        attach_label(2, 0, 'Border')
        attach_label(3, 0, 'Padding')
        attach_label(4, 0, 'Margin')

        # Buttons
        y = 1

        [ :active, :inactive ].each do |name|
          # Color button
          @buttons[name] = ColorButton.new(
            colors[('client_%s' % name).to_sym]
          )
          @buttons[name].attach(@table, 1, y)

          # Border spinner
          sym = ('%s_border' % [ name ]).to_sym
          @buttons[sym] = SpinButton.new(2)
          @buttons[sym].attach(@table, 2, y)

          y += 1
        end

        # Margin
        @buttons[:margin] = {}

        [ :top, :right, :bottom, :left ].each do |side|
          @buttons[:margin][side] = SpinButton.new(0)
          @buttons[:margin][side].attach(@table, 4, y)

          y += 1
        end

        # Fill remaining fields
        @buttons[:fg]      = @buttons[:active]
        @buttons[:bg]      = @buttons[:inactive]
        @buttons[:border ] = {
          top:    @buttons[:active_border],
          right:  @buttons[:active_border],
          bottom: @buttons[:active_border],
          left:   @buttons[:active_border]
        }

        # Just for text placement
        @buttons[:padding] = { top: 5, right: 0, bottom: 0, left: 5 }
      end # }}}

      ## dump {{{
      # Print style element
      ##

      def dump
        puts <<STYLE
style :#{@name.downcase} do
  active       '#{@buttons[:active].value}'
  inactive     '#{@buttons[:inactive].value}'
  margin       #{compact(:margin)}
end\n
STYLE
      end # }}}
    end

    class StyleSubtle < Style

      ## initialize {{{
      # Create a new other element
      # @param  [String]  name    Element name
      # @param  [Array]   colors  Color array
      ##

      def initialize(name, colors)
        @name    = name
        @table   = Gtk::Table.new(5, 7)
        @buttons = {}

        # Labels: Vertical
        attach_label(0, 1, 'Panel top')
        attach_label(0, 2, 'Panel bottom')
        attach_label(0, 3, 'Background')
        attach_label(0, 4, 'Stipple')
        attach_label(0, 5, 'Top')
        attach_label(0, 6, 'Right')
        attach_label(0, 7, 'Bottom')
        attach_label(0, 8, 'Left')

        # Labels: Horizontal
        attach_label(1, 0, 'Color')
        attach_label(2, 0, 'Border')
        attach_label(3, 0, 'Padding')
        attach_label(4, 0, 'Margin')

        # Buttons
        y = 1

        [ :panel_top, :panel_bottom, :background, :stipple ].each do |name|
          # Color button
          @buttons[name] = ColorButton.new(colors[name])
          @buttons[name].attach(@table, 1, y)

          y += 1
        end

        # Padding
        @buttons[:padding] = {}

        [ :top, :right, :bottom, :left ].each do |side|
          @buttons[:padding][side] = SpinButton.new(0)
          @buttons[:padding][side].attach(@table, 3, y)

          y += 1
        end
      end # }}}

      ## dump {{{
      # Print style element
      ##

      def dump
        puts "style :#{@name.downcase} do"

        # Compact panel colors
        if(@buttons[:panel_top].value == @buttons[:panel_bottom].value)
          puts "  panel        '#{@buttons[:panel_top].value}'"
        else
          puts "  panel_top    '#{@buttons[:panel_top].value}'"
          puts "  panel_bottom '#{@buttons[:panel_bottom].value}'"
        end

        puts "  background   '#{@buttons[:background].value}'"
        puts "  stipple      '#{@buttons[:stipple].value}'"
        puts "  padding      #{compact(:padding)}"
        puts "end"
        puts
      end # }}}
    end

    class Styler < Gtk::Window
      include Singleton

      BORDER_WIDTH  = 2
      WINDOW_WIDTH  = 790
      AREA_WIDTH    = WINDOW_WIDTH - 16 #< Padding + spacing
      CLIENT_WIDTH  = (AREA_WIDTH - 2 * BORDER_WIDTH) / 2

      WINDOW_HEIGHT = 540
      AREA_HEIGHT   = 150

      ## initialize {{{
      # Init window
      ##

      def initialize
        super

        # Options
        set_title('Styler for subtle #{Subtlext::VERSION}')
        set_wmclass('styler', 'subtle')
        set_resizable(false)
        set_keep_above(true)
        set_size_request(WINDOW_WIDTH, WINDOW_HEIGHT)
        set_window_position(Gtk::Window::POS_CENTER)
        stick

        # Signals {{{
        signal_connect('delete_event') do
          false
        end

        signal_connect('destroy') do
          Gtk.main_quit
        end # }}}

        # Alignment
        align = Gtk::Alignment.new(0.5, 0.5, 0.5, 0.5)
        align.set_padding(7, 7, 7, 7)
        add(align)

        # Vbox
        vbox = Gtk::VBox.new()
        align.add(vbox)

        # Frame {{{
        frame = Gtk::Frame.new
        frame.set_border_width(0)
        frame.set_shadow_type(Gtk::SHADOW_NONE)
        vbox.pack_start(frame) # }}}

        # Area {{{
        @area = Gtk::DrawingArea.new
        @area.set_size_request(AREA_WIDTH, AREA_HEIGHT)
        @area.signal_connect('expose_event') do
          expose(@area.window.create_cairo_context)
        end
        frame.add(@area) # }}}

        # Notebook {{{
        notebook = Gtk::Notebook.new
        notebook.set_tab_pos(Gtk::PositionType::LEFT)
        vbox.pack_start(notebook, true, false, 5) # }}}

        # Styles {{{
        colors = {}
        Subtlext::Subtle.colors.map { |k, v| colors[k] = v.to_hex }
        @styles = {}

        # Styles and pages
        [
          'all', 'title', 'views', 'focus', 'urgent',
          'occupied', 'sublets', 'separator'
        ].each do |caption|
          sym          = caption.to_sym
          @styles[sym] = StyleNormal.new(caption.capitalize, colors)
          @styles[sym].append(notebook)
        end

        # Clients
        @styles[:clients] = StyleClients.new('Clients', colors)
        @styles[:clients].append(notebook)

        # Subtle
        @styles[:subtle] = StyleSubtle.new('Subtle', colors)
        @styles[:subtle].append(notebook)

        # All: Update border, padding and margin
        [ :border, :padding, :margin ].each do |name|
          [ :top, :right, :bottom, :left ].each do |side|
            @styles[:all].buttons[name][side].callback do |value|
              [
                :title, :views, :focus, :urgent,
                :occupied, :sublets, :separator
              ].each do |style|
                @styles[style].buttons[name][side].value = value
              end
            end
          end
        end

        # All: Update colors
        [ :fg, :bg, :top, :right, :bottom, :left ].each do |name|
          @styles[:all].buttons[name].callback do |value|
            [
              :title, :views, :focus, :urgent,
              :occupied, :sublets, :separator
            ].each do |style|
              @styles[style].buttons[name].value = value
            end
          end
        end # }}}

        # Hbox
        hbox = Gtk::HButtonBox.new
        vbox.pack_start(hbox, false, false, 5)

        # Print button {{{
        button = Gtk::Button.new('Print')
        hbox.pack_start(button, false, false, 2)
        button.signal_connect('clicked') do
          @styles.select { |k, v| :all != k }.each do |k, v|
            v.dump
          end
        end # }}}

        # Reset button {{{
        button = Gtk::Button.new('Reset')
        hbox.pack_start(button, false, false, 2)
        button.signal_connect('clicked') do
          @styles.each do |k, v|
            if v.respond_to? :reset
              v.reset
            end
          end

          @area.signal_emit('expose-event', nil)
        end # }}}

        # Exit button {{{
        button = Gtk::Button.new('Exit')
        hbox.pack_start(button, false, false, 2)
        button.signal_connect('clicked') do
          Gtk.main_quit
        end # }}}

        show_all
      end # }}}

      ## render {{{
      # Render preview
      ##

      def render
        @area.signal_emit('expose-event', nil)
      end # }}}

      private

      def expose(cr) # {{{
        # Font face, size and height
        cr.select_font_face('Arial', 'normal')
        cr.set_font_size(12)

        extents = cr.font_extents
        @font_height = extents.ascent + extents.descent + 2;
        @font_y      = (extents.height - 2 + extents.ascent) / 2

        # Calculate item heights
        panel_height = @font_height

        [
          :title, :views, :focus, :urgent,
          :occupied, :sublets, :separator
        ].each do |panel|
          value = @font_height + @styles[panel].sum([ :top, :bottom ])

          panel_height = value if(value > panel_height)
        end

        client_height = AREA_HEIGHT - 2 * panel_height - 2 * BORDER_WIDTH

        # Border
        cr.set_source_rgb(1.0, 0.0, 0.0)
        cr.rectangle(0, 0, AREA_WIDTH, AREA_HEIGHT)
        cr.fill

        # Background
        draw_rect(cr, BORDER_WIDTH, BORDER_WIDTH,
          AREA_WIDTH - 2 * BORDER_WIDTH, AREA_HEIGHT - 2 * BORDER_WIDTH,
          @styles[:subtle][:background])

        # Panels
        draw_rect(cr, BORDER_WIDTH, BORDER_WIDTH,
          AREA_WIDTH - 2 * BORDER_WIDTH, panel_height,
          @styles[:subtle][:panel_top]
        )
        draw_rect(cr, BORDER_WIDTH, BORDER_WIDTH + panel_height + client_height,
          AREA_WIDTH - 2 * BORDER_WIDTH, panel_height,
          @styles[:subtle][:panel_bottom]
        )

        # Clients
        draw_box(cr, BORDER_WIDTH, BORDER_WIDTH + panel_height, CLIENT_WIDTH,
          client_height, 'active', @styles[:clients])

        draw_box(cr, BORDER_WIDTH + CLIENT_WIDTH, BORDER_WIDTH + panel_height,
          CLIENT_WIDTH, client_height, 'inactive', @styles[:clients])

        # Panel items
        x = BORDER_WIDTH

        [
          'focus', 'views', 'urgent', 'occupied', 'title'
        ].each do |name|
          sym     = name.to_sym
          extents = cr.text_extents(name)
          width   = @styles[sym].sum([ :left, :right ]) + extents.x_advance

          draw_box(cr, x, BORDER_WIDTH, width,
            panel_height, name, @styles[sym])

          x += width
        end

        # Sublets
        x = BORDER_WIDTH
        {
          'sublet1' => :sublets, '|' => :separator, 'sublet2' => :sublets
        }.each do |name, style|
          extents = cr.text_extents(name)
          width   = @styles[style].sum([ :left, :right ]) + extents.x_advance

          draw_box(cr, x,  BORDER_WIDTH + panel_height + client_height,
            width, panel_height, name, @styles[style])

          x += width
        end
      end # }}}

      def draw_text(cr, x, y, text, color) # {{{
        cr.set_source_color(Gdk::Color.parse(color))
        cr.move_to(x, y + @font_y)
        cr.show_text(text)
        cr.stroke
      end # }}}

      def draw_rect(cr, x, y, width, height, color) # {{{
        cr.set_source_color(Gdk::Color.parse(color))
        cr.rectangle(x, y, width, height)
        cr.fill
      end # }}}

      def draw_box(cr, x, y, width, height, text, style) # {{{
        mw      = style[:margin][:left].value + style[:margin][:right].value
        mh      = style[:margin][:top].value + style[:margin][:bottom].value

        # Filling
        draw_rect(cr, x + style[:margin][:left].value,
          y + style[:margin][:top].value, width - mw, height - mh, style[:bg])

        # Borders
        draw_rect(cr, x + style[:margin][:left].value,
          y + style[:margin][:top].value, width - mw,
          style[:border][:top].value,
          text.end_with?('active') ? style[text.to_sym] : style[:top])

        draw_rect(cr, x + width - style[:border][:right].value -
          style[:margin][:right].value, y + style[:margin][:top].value,
          style[:border][:right].value, height - mh,
          text.end_with?('active') ? style[text.to_sym] : style[:right])

        draw_rect(cr, x + style[:margin][:left].value, y + height -
          style[:border][:bottom].value - style[:margin][:bottom].value,
          width - mw, style[:border][:bottom].value,
          text.end_with?('active') ? style[text.to_sym] : style[:bottom])

        draw_rect(cr, x + style[:margin][:left].value,
          y + style[:margin][:top].value, style[:border][:left].value,
          height - mh,
          text.end_with?('active') ? style[text.to_sym] : style[:left])

        # Text
        draw_text(cr, x + style.sum([ :left ]),
          y + style.sum([ :top ]), text, style[:fg]
        )
      end # }}}
    end
  end # }}}
end # }}}

# Implicitly run<
if __FILE__ == $0
  Gtk.init
  Subtle::Contrib::Styler.instance
  Gtk.main
end

# vim:ts=2:bs=2:sw=2:et:fdm=marker
