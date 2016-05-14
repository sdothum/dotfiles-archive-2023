#!/usr/bin/python
# sdothum - 2016 (c) wtfpl

# Terminal
# ▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂

# ....................................................................... Colors

# show ncurses colors
# template from http://stackoverflow.com/questions/18551558/how-to-use-terminal-color-palette-with-curses

import curses
import sys

def main(stdscr):
    try:
        columns=int(sys.argv[1])
    except:
        columns=16
    if columns == 0:
        columns=256
    curses.start_color()
    curses.use_default_colors()
    for i in range(0, curses.COLORS):
        curses.init_pair(i + 1, i, -1)
    try:
        j=0
        for i in range(0, 255):
            stdscr.addstr(' %03i ' % i, curses.color_pair(i+1))
            j+=1
            if j == columns:
                stdscr.addstr('\n')
                j=0
        # 255 (#eeeeee) most closely matches 015 (#ffffff) if .Xresourses set as such, otherwise..
        stdscr.addstr(' %03i ' % 255, curses.color_pair(255))
    except curses.ERR:
        # End of screen reached
        pass
    stdscr.getch()

curses.wrapper(main)
