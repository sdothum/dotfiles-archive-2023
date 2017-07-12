#!/usr/bin/ruby
#
# @file TermStyler
#
# @copyright (c) 2011, Christoph Kappel <unexist@dorfelite.net>
# @version $Id: ruby/termstyler.rb,v 117 2012/06/28 11:57:12 unexist $
#
# TermStyler is a helper to create or change terminal color themes.
#
# http://subforge.org/projects/subtle-contrib/wiki/TermStyler
#

begin
  require "gtk2"
rescue LoadError
    puts <<EOF
>>> ERROR: Couldn't find the gem `gtk2'
>>>        Please install it with following command:
>>>        gem install gtk2
EOF
  exit
end

# TermStyler class
module Subtle # {{{
  module Contrib # {{{
    class TermStyler < Gtk::Window # {{{
      WINDOW_WIDTH  = 720
      WINDOW_HEIGHT = 420
      AREA_WIDTH    = 404
      AREA_HEIGHT   = 365
      REGEXP        = Regexp.new("^[^!#]*(color[0-9]+|foreground|background)\s*:\s*(#[0-9a-zA-Z]*)")

      ## initialize {{{
      # Init window
      ##

      def initialize
        super

        # Options
        set_title("Styler for terminals")
        set_wmclass("termstyler", "subtle")
        set_resizable(false)
        set_keep_above(true)
        set_size_request(WINDOW_WIDTH, WINDOW_HEIGHT)
        set_window_position(Gtk::Window::POS_CENTER)
        stick

        # Signals
        signal_connect("delete_event") do
          false
        end

        signal_connect("destroy") do
          Gtk.main_quit
        end

        # Alignment
        align = Gtk::Alignment.new(0.5, 0.5, 0.5, 0.5)
        align.set_padding(7, 7, 7, 7)

        add(align)

        # Vbox
        vbox = Gtk::VBox.new
        align.add(vbox)

        # HBox
        hbox = Gtk::HBox.new
        vbox.pack_start(hbox)

        # Table
        table = Gtk::Table.new(4, 9)
        hbox.pack_start(table, true, false, 5)

        # Frame
        frame = Gtk::Frame.new
        frame.set_border_width(0)
        frame.set_shadow_type(Gtk::SHADOW_NONE)
        hbox.pack_start(frame)

        # Area
        @area = Gtk::DrawingArea.new
        @area.set_size_request(AREA_WIDTH, AREA_HEIGHT)
        @area.signal_connect("expose_event") do
          expose(@area.window.create_cairo_context)
        end
        frame.add(@area)

        # Get colors
        load_colors

        # Color buttons
        @buttons = {}
        row      = 0
        idx1     = 0
        idx2     = 8

        @buttons["foreground"] = color_button(table, row, 0, "foreground")
        @buttons["background"] = color_button(table, row, 2, "background")

        8.times do |i|
          row   += 1
          name1  = "color#{idx1 + i}"
          name2  = "color#{idx2 + i}"

          @buttons[name1] = color_button(table, row, 0, name1)
          @buttons[name2] = color_button(table, row, 2, name2)
        end

        # Hbox
        hbox = Gtk::HButtonBox.new
        vbox.pack_start(hbox, false, false, 5)

        # Print button
        button = Gtk::Button.new("Print")
        hbox.pack_start(button, false, false, 2)
        button.signal_connect("clicked") do
          print_pair("",        "foreground", "background")
          print_pair("black",   "color0",     "color8")
          print_pair("red",     "color1",     "color9")
          print_pair("green",   "color2",     "color10")
          print_pair("yellow",  "color3",     "color11")
          print_pair("blue",    "color4",     "color12")
          print_pair("magenta", "color5",     "color13")
          print_pair("cyan",    "color6",     "color14")
          print_pair("white",   "color7",     "color15")
        end

        # Reset button
        button = Gtk::Button.new("Reset")
        hbox.pack_start(button, false, false, 2)
        button.signal_connect("clicked") do
          load_colors

          # Reset color buttons
          @colors.each do |k, v|
            @buttons[k].set_color(Gdk::Color.parse(v))
          end

          @area.signal_emit("expose-event", nil)
        end

        # Exit button
        button = Gtk::Button.new("Exit")
        hbox.pack_start(button, false, false, 2)
        button.signal_connect("clicked") do
          Gtk.main_quit
        end

        show_all
      end # }}}

      private

      def print_pair(name, col1, col2) # {{{
        puts "!#{name}" unless(name.empty?)
        puts "*%-11s %s" % [ col1 + ":", @colors[col1] ]
        puts "*%-11s %s" % [ col2 + ":", @colors[col2] ]
      end # }}}

      def load_colors # {{{
        @colors = Hash[*(16.times.to_a.map { |i|
          [ "color#{i}", "#000000" ]
        } << ["forground", "#ffffff", "background", "#000000"]).flatten]

        # Load and parse Xdefaults
        File.open("#{ENV["HOME"]}/.Xdefaults") do |f|
          while(line = f.gets)
            if line.match(REGEXP)
              @colors[$~[1]] = $~[2]
            end
          end
        end
      rescue
      end # }}}

      def scale_round(val, factor) # {{{
        val = (val * factor + 0.5).floor
        val = val > 0      ? val    : 0   #< Min
        val = val > factor ? factor : val #< Max

        val
      end # }}}

      def color_button(table, row, col, name) # {{{
        caption  = name.to_s.split(/[^a-z0-9]/i).map { |w|
          w.capitalize
        }.join(" ")
        label    = Gtk::Label.new(caption)
        button   = Gtk::ColorButton.new(
          Gdk::Color.parse(@colors[name])
        )

        # Align
        align = Gtk::Alignment.new(1.0, 0.0, 0, 0)
        align.add(label)

        # Signal handler
        button.signal_connect("color-set") do |button|
          begin
            # Assemble and update hex color
            @colors[name] = "#%02X%02X%02X" % [
              scale_round((button.color.red.to_f   / 65535), 255),
              scale_round((button.color.green.to_f / 65535), 255),
              scale_round((button.color.blue.to_f  / 65535), 255)
            ]

            @area.signal_emit("expose-event", nil)
          rescue => error
            puts error, error.backtrace
          end
        end

      # Attach to table
      table.attach(align, 0 + col, 1 + col, 0 + row, 1 + row,
        Gtk::SHRINK, Gtk::SHRINK, 5, 5)
      table.attach(button, 1 + col, 2 + col, 0 + row, 1 + row,
        Gtk::SHRINK, Gtk::SHRINK, 5, 5)

        return button
      end # }}}

      def expose(cr) # {{{
        # Border
        cr.set_source_rgb(1.0, 0.0, 0.0)
        cr.rectangle(0, 0, AREA_WIDTH, AREA_HEIGHT)
        cr.fill

       # Rects
        width = (AREA_WIDTH - 4) / 8

        8.times do |i|
          draw_rect(cr, 2 + width * i, 2, width, AREA_HEIGHT - 4, @colors["color#{i}"])
        end

        # Fore-/background
        draw_rect(cr, 2, 2, AREA_WIDTH - 4, 38, @colors["background"])

        # Labels
        xary = 8.times.map { |i| 10 + width * i }
        y    = 0

        8.times do |i|
          draw_text(cr, "Norm", xary, 60 + y, @colors["color#{i}"])
          draw_text(cr, "Bold", xary, 75 + y, @colors["color#{8 + i}"], true)

          y += 40
        end

        draw_text(cr, "Col", xary, 24, @colors["foreground"])
      end # }}}

      def draw_rect(cr, x, y, width, height, color) # {{{
        cr.set_source_color(Gdk::Color.parse(color))
        cr.rectangle(x, y, width, height)
        cr.fill
      end # }}}

      def draw_text(cr, text, xary, y, color, bold = false) # {{{
        cr.set_source_color(Gdk::Color.parse(color))
        cr.select_font_face("Arial", Cairo::FONT_SLANT_NORMAL,
          bold ? Cairo::FONT_WEIGHT_BOLD : Cairo::FONT_WEIGHT_NORMAL
        )
        cr.set_font_size(14)

        xary = [ xary ] if(xary.is_a?(Fixnum))

        # Draw text on each xary pos
        xary.each do |x|
          cr.move_to(x, y)
          cr.show_text(text)
        end

        cr.stroke
      end # }}}
    end # }}}
  end # }}}
end # }}}

# Implicitly run<
if __FILE__ == $0
  Gtk.init
  Subtle::Contrib::TermStyler.new
  Gtk.main
end

# vim:ts=2:bs=2:sw=2:et:fdm=marker
