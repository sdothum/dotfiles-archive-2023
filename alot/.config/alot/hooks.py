def post_buffer_focus(ui=None, dbm=None, buf=None, success=None):
    if buf.modename == 'search':
        fp = buf.body.focus_position
        buf.rebuild()
        ui.update()
        line_num = len(buf.threadlist.get_lines())
        # ui.notify('fp {} count {}'.format(fp, line_num), 'error')
        if fp >= line_num:
            # buf.focus_last()
            buf.body.set_focus(max(line_num - 1, 0))
        else:
            buf.body.set_focus(fp)
