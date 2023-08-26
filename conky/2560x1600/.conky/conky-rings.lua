-- sdothum - 2016 (c) wtfpl

-- X11 Desktop
-- ═════════════════════════════════════════════════════════════════════════════

-- ....................................................................... Conky

-- see bin/functions/conky/theme

require 'cairo'

LUARINGS=tonumber(io.popen('conky color LUARINGS'):read(), 16)
default_color=tonumber(io.popen('conky color default_color'):read(), 16)
color0=tonumber(io.popen('conky color color0'):read(), 16)
color6=tonumber(io.popen('conky color color6'):read(), 16)

--------------------------------------------------------------------------------
--                                                                    clock DATA
-- HOURS
clock_h = {
    {
    name='time',                   arg='%l',                        max_value=12,
    x=115,                         y=125,
    graph_radius=65,
    graph_thickness=3,
    graph_unit_angle=30,           graph_unit_thickness=30,
    graph_bg_colour=LUARINGS,      graph_bg_alpha=0,
    graph_fg_colour=color0,        graph_fg_alpha=0.66,
    txt_radius=50,
    txt_weight=0,                  txt_size=12.0,
    txt_fg_colour=LUARINGS,        txt_fg_alpha=0.6,
    graduation_radius=55,
    graduation_thickness=0,        graduation_mark_thickness=1,
    graduation_unit_angle=30,
    graduation_fg_colour=LUARINGS, graduation_fg_alpha=0.3,
    },
}
-- MINUTES
clock_m = {
    {
    name='time',                   arg='%M',                        max_value=60,
    x=115,                         y=125,
    graph_radius=69,
    graph_thickness=2,
    graph_unit_angle=6,            graph_unit_thickness=6,
    graph_bg_colour=LUARINGS,      graph_bg_alpha=0.05,
    graph_fg_colour=color0,        graph_fg_alpha=0.33,
    txt_radius=81,
    txt_weight=0,                  txt_size=10.0,
    txt_fg_colour=LUARINGS,        txt_fg_alpha=0.4,
    graduation_radius=59,
    graduation_thickness=0,        graduation_mark_thickness=2,
    graduation_unit_angle=30,
    graduation_fg_colour=LUARINGS, graduation_fg_alpha=0.3,
    },
}
-- SECONDS
clock_s = {
    {
    name='time',                   arg='%S',                        max_value=60,
    x=115,                         y=125,
    graph_radius=62,
    graph_thickness=2,
    graph_unit_angle=6,            graph_unit_thickness=2,
    graph_bg_colour=LUARINGS,      graph_bg_alpha=0,
    graph_fg_colour=LUARINGS,      graph_fg_alpha=0.22,
    txt_radius=53,
    txt_weight=0,                  txt_size=8.0,
    txt_fg_colour=LUARINGS,        txt_fg_alpha=0.3,
    graduation_radius=0,
    graduation_thickness=0,        graduation_mark_thickness=1,
    graduation_unit_angle=0,
    graduation_fg_colour=LUARINGS, graduation_fg_alpha=0,
    },
}

--------------------------------------------------------------------------------
--                                                                    gauge DATA
temp = {
{
    name='exec',                   arg="sensors | grep 'Core .:' | sed -e 's/^.*  [+]//' -e 's/...C  *[(].*$//' | sort | tail -1", max_value=100,
    x=94,                          y=539,
    graph_radius=25,
    graph_thickness=3,
    graph_start_angle=180,
    graph_unit_angle=2.7,          graph_unit_thickness=2.7,
    graph_bg_colour=LUARINGS,      graph_bg_alpha=0.06,
    graph_fg_colour=color0,        graph_fg_alpha=0.66,
    hand_fg_colour=color0,         hand_fg_alpha=0,
    txt_radius=17,
    txt_weight=0,                  txt_size=8.0,
    txt_fg_colour=LUARINGS,        txt_fg_alpha=0.5,
    graduation_radius=28,
    graduation_thickness=0,        graduation_mark_thickness=1,
    graduation_unit_angle=27,
    graduation_fg_colour=LUARINGS, graduation_fg_alpha=0.3,
    caption='',
    caption_weight=1,              caption_size=8.0,
    caption_fg_colour=LUARINGS,    caption_fg_alpha=0.3,
},
-- {
--     name='exec',                   arg="sensors | grep 'Core .:' | sed -e 's/^.*  [+]//' -e 's/...C  *[(].*$//' | sort | head -1", max_value=100,
--     x=94,                          y=539,
--     graph_radius=25,
--     graph_thickness=3,
--     graph_start_angle=180,
--     graph_unit_angle=2.7,          graph_unit_thickness=2.7,
--     graph_bg_colour=LUARINGS,      graph_bg_alpha=0,
--     graph_fg_colour=color6,        graph_fg_alpha=0.66,
--     hand_fg_colour=color0,         hand_fg_alpha=0,
--     txt_radius=17,
--     txt_weight=0,                  txt_size=8.0,
--     txt_fg_colour=LUARINGS,        txt_fg_alpha=0.5,
--     graduation_radius=28,
--     graduation_thickness=0,        graduation_mark_thickness=1,
--     graduation_unit_angle=27,
--     graduation_fg_colour=LUARINGS, graduation_fg_alpha=0.3,
--     caption='',
--     caption_weight=1,              caption_size=8.0,
--     caption_fg_colour=LUARINGS,    caption_fg_alpha=0.3,
-- },
}

gauge = {
{
    name='exec',                   arg='conky volume',              max_value=100,
    x=94,                          y=335,
    graph_radius=29,
    graph_thickness=1,
    graph_start_angle=180,
    graph_unit_angle=2.7,          graph_unit_thickness=2.7,
    graph_bg_colour=LUARINGS,      graph_bg_alpha=0.06,
    graph_fg_colour=default_color, graph_fg_alpha=0.88,
    hand_fg_colour=color0,         hand_fg_alpha=0,
    txt_radius=38,
    txt_weight=0,                  txt_size=8.0,
    txt_fg_colour=LUARINGS,        txt_fg_alpha=0.5,
    graduation_radius=28,
    graduation_thickness=0,        graduation_mark_thickness=1,
    graduation_unit_angle=27,
    graduation_fg_colour=LUARINGS, graduation_fg_alpha=0.3,
    caption='',
    caption_weight=1,              caption_size=8.0,
    caption_fg_colour=LUARINGS,    caption_fg_alpha=0.3,
},
{
    name='mpd_percent',            arg='',                          max_value=100,
    x=94,                          y=335,
    graph_radius=25,
    graph_thickness=3,
    graph_start_angle=180,
    graph_unit_angle=2.7,          graph_unit_thickness=2.7,
    graph_bg_colour=LUARINGS,      graph_bg_alpha=0.06,
    graph_fg_colour=color0,        graph_fg_alpha=0.66,
    hand_fg_colour=color0,         hand_fg_alpha=0,
    txt_radius=17,
    txt_weight=0,                  txt_size=8.0,
    txt_fg_colour=LUARINGS,        txt_fg_alpha=0.5,
    graduation_radius=28,
    graduation_thickness=0,        graduation_mark_thickness=1,
    graduation_unit_angle=27,
    graduation_fg_colour=LUARINGS, graduation_fg_alpha=0.3,
    caption='',
    caption_weight=1,              caption_size=8.0,
    caption_fg_colour=LUARINGS,    caption_fg_alpha=0.3,
},
{
    name='cpu',                    arg='cpu0',                      max_value=100,
    x=94,                          y=539,
    graph_radius=29,
    graph_thickness=1,
    graph_start_angle=180,
    graph_unit_angle=2.7,          graph_unit_thickness=2.7,
    graph_bg_colour=LUARINGS,      graph_bg_alpha=0.06,
    graph_fg_colour=default_color, graph_fg_alpha=0.88,
    hand_fg_colour=color0,         hand_fg_alpha=0,
    txt_radius=38,
    txt_weight=0,                  txt_size=8.0,
    txt_fg_colour=LUARINGS,        txt_fg_alpha=0.5,
    graduation_radius=28,
    graduation_thickness=0,        graduation_mark_thickness=1,
    graduation_unit_angle=27,
    graduation_fg_colour=LUARINGS, graduation_fg_alpha=0.3,
    caption='',
    caption_weight=1,              caption_size=8.0,
    caption_fg_colour=LUARINGS,    caption_fg_alpha=0.3,
},
{
    name='memperc',                arg='',                          max_value=100,
    x=94,                          y=745,
    graph_radius=29,
    graph_thickness=1,
    graph_start_angle=180,
    graph_unit_angle=2.7,          graph_unit_thickness=2.7,
    graph_bg_colour=LUARINGS,      graph_bg_alpha=0.06,
    graph_fg_colour=default_color, graph_fg_alpha=0.88,
    hand_fg_colour=color0,         hand_fg_alpha=0,
    txt_radius=38,
    txt_weight=0,                  txt_size=8.0,
    txt_fg_colour=LUARINGS,        txt_fg_alpha=0.5,
    graduation_radius=23,
    graduation_thickness=0,        graduation_mark_thickness=1,
    graduation_unit_angle=27,
    graduation_fg_colour=LUARINGS, graduation_fg_alpha=0.5,
    caption='',
    caption_weight=1,              caption_size=8.0,
    caption_fg_colour=LUARINGS,    caption_fg_alpha=0.3,
},
{
    name='fs_used_perc',           arg='/tmp',                      max_value=100,
    x=94,                          y=745,
    graph_radius=25,
    graph_thickness=3,
    graph_start_angle=180,
    graph_unit_angle=2.7,          graph_unit_thickness=2.7,
    graph_bg_colour=LUARINGS,      graph_bg_alpha=0.06,
    graph_fg_colour=color0,        graph_fg_alpha=0.66,
    hand_fg_colour=color0,         hand_fg_alpha=0,
    txt_radius=17,
    txt_weight=0,                  txt_size=8.0,
    txt_fg_colour=LUARINGS,        txt_fg_alpha=0.5,
    graduation_radius=28,
    graduation_thickness=0,        graduation_mark_thickness=1,
    graduation_unit_angle=27,
    graduation_fg_colour=LUARINGS, graduation_fg_alpha=0.3,
    caption='',
    caption_weight=1,              caption_size=8.0,
    caption_fg_colour=LUARINGS,    caption_fg_alpha=0.3,
},
{
    -- name='fs_used_perc',           arg='/',                         max_value=100,
    name='exec',                   arg='conky btrfs / percent',     max_value=100,
    x=94,                          y=905,
    graph_radius=29,
    graph_thickness=1,
    graph_start_angle=180,
    graph_unit_angle=2.7,          graph_unit_thickness=2.7,
    graph_bg_colour=LUARINGS,      graph_bg_alpha=0.06,
    graph_fg_colour=default_color, graph_fg_alpha=0.88,
    hand_fg_colour=color0,         hand_fg_alpha=0,
    txt_radius=38,
    txt_weight=0,                  txt_size=8.0,
    txt_fg_colour=LUARINGS,        txt_fg_alpha=0.5,
    graduation_radius=28,
    graduation_thickness=0,        graduation_mark_thickness=1,
    graduation_unit_angle=27,
    graduation_fg_colour=LUARINGS, graduation_fg_alpha=0.3,
    caption='',
    caption_weight=1,              caption_size=8.0,
    caption_fg_colour=LUARINGS,    caption_fg_alpha=0.3,
},
{
    -- name='fs_used_perc',           arg='/net',                      max_value=100,
    name='exec',                   arg='conky btrfs /net percent',  max_value=100,
    x=94,                          y=905,
    graph_radius=25,
    graph_thickness=3,
    graph_start_angle=180,
    graph_unit_angle=2.7,          graph_unit_thickness=2.7,
    graph_bg_colour=LUARINGS,      graph_bg_alpha=0.06,
    graph_fg_colour=color0,        graph_fg_alpha=0.66,
    hand_fg_colour=color0,         hand_fg_alpha=0,
    txt_radius=17,
    txt_weight=0,                  txt_size=8.0,
    txt_fg_colour=LUARINGS,        txt_fg_alpha=0.5,
    graduation_radius=28,
    graduation_thickness=0,        graduation_mark_thickness=1,
    graduation_unit_angle=27,
    graduation_fg_colour=LUARINGS, graduation_fg_alpha=0.3,
    caption='',
    caption_weight=1,              caption_size=8.0,
    caption_fg_colour=LUARINGS,    caption_fg_alpha=0.3,
},
{
    name='swapperc',               arg='',                          max_value=100,
    x=94,                          y=905,
    graph_radius=25,
    graph_thickness=3,
    graph_start_angle=180,
    graph_unit_angle=2.7,          graph_unit_thickness=2.7,
    graph_bg_colour=LUARINGS,      graph_bg_alpha=0,
    graph_fg_colour=color6,        graph_fg_alpha=0.66,
    hand_fg_colour=color0,         hand_fg_alpha=0,
    txt_radius=17,
    txt_weight=0,                  txt_size=8.0,
    txt_fg_colour=LUARINGS,        txt_fg_alpha=0.5,
    graduation_radius=28,
    graduation_thickness=0,        graduation_mark_thickness=1,
    graduation_unit_angle=27,
    graduation_fg_colour=LUARINGS, graduation_fg_alpha=0.3,
    caption='',
    caption_weight=1,              caption_size=8.0,
    caption_fg_colour=LUARINGS,    caption_fg_alpha=0.3,
},
{
    name='wireless_link_qual_perc', arg='wlp4s0',                   max_value=100,
    x=94,                          y=1144,
    graph_radius=29,
    graph_thickness=1,
    graph_start_angle=180,
    graph_unit_angle=2.7,          graph_unit_thickness=2.7,
    graph_bg_colour=LUARINGS,      graph_bg_alpha=0.06,
    graph_fg_colour=default_color, graph_fg_alpha=0.88,
    hand_fg_colour=color0,         hand_fg_alpha=0,
    txt_radius=38,
    txt_weight=0,                  txt_size=8.0,
    txt_fg_colour=LUARINGS,        txt_fg_alpha=0.5,
    graduation_radius=28,
    graduation_thickness=0,        graduation_mark_thickness=1,
    graduation_unit_angle=27,
    graduation_fg_colour=LUARINGS, graduation_fg_alpha=0.3,
    caption='',
    caption_weight=1,              caption_size=8.0,
    caption_fg_colour=LUARINGS,    caption_fg_alpha=0.3,
},
}

-------------------------------------------------------------------------------
--                                                                 rgb_to_r_g_b
-- converts color in hexa to decimal
--
function rgb_to_r_g_b(colour, alpha)
    return ((colour / 0x10000) % 0x100) / 255., ((colour / 0x100) % 0x100) / 255., (colour % 0x100) / 255., alpha
end

-------------------------------------------------------------------------------
--                                                            angle_to_position
-- convert degree to rad and rotate (0 degree is top/north)
--
function angle_to_position(start_angle, current_angle)
    local pos = current_angle + start_angle
    return ( ( pos * (2 * math.pi / 360) ) - (math.pi / 2) )
end

-------------------------------------------------------------------------------
--                                                              draw_clock_ring
-- displays clock
--
function draw_clock_ring(display, data, value)
    local max_value = data['max_value']
    local x, y = data['x'], data['y']
    local graph_radius = data['graph_radius']
    local graph_thickness, graph_unit_thickness = data['graph_thickness'], data['graph_unit_thickness']
    local graph_unit_angle = data['graph_unit_angle']
    local graph_bg_colour, graph_bg_alpha = data['graph_bg_colour'], data['graph_bg_alpha']
    local graph_fg_colour, graph_fg_alpha = data['graph_fg_colour'], data['graph_fg_alpha']

    -- background ring
    cairo_arc(display, x, y, graph_radius, 0, 2 * math.pi)
    cairo_set_source_rgba(display, rgb_to_r_g_b(graph_bg_colour, graph_bg_alpha))
    cairo_set_line_width(display, graph_thickness)
    cairo_stroke(display)

    -- arc of value
    local val = (value % max_value)
    local i = 1
    while i <= val do
        cairo_arc(display, x, y, graph_radius,(  ((graph_unit_angle * i) - graph_unit_thickness)*(2*math.pi/360)  )-(math.pi/2),((graph_unit_angle * i) * (2*math.pi/360))-(math.pi/2))
        cairo_set_source_rgba(display,rgb_to_r_g_b(graph_fg_colour,graph_fg_alpha))
        cairo_stroke(display)
        i = i + 1
    end
    local angle = (graph_unit_angle * i) - graph_unit_thickness

    -- graduations marks
    local graduation_radius = data['graduation_radius']
    local graduation_thickness, graduation_mark_thickness = data['graduation_thickness'], data['graduation_mark_thickness']
    local graduation_unit_angle = data['graduation_unit_angle']
    local graduation_fg_colour, graduation_fg_alpha = data['graduation_fg_colour'], data['graduation_fg_alpha']
    if graduation_radius > 0 and graduation_thickness > 0 and graduation_unit_angle > 0 then
        local nb_graduation = 360 / graduation_unit_angle
        local i = 1
        while i <= nb_graduation do
            cairo_set_line_width(display, graduation_thickness)
            cairo_arc(display, x, y, graduation_radius, (((graduation_unit_angle * i)-(graduation_mark_thickness/2))*(2*math.pi/360))-(math.pi/2),(((graduation_unit_angle * i)+(graduation_mark_thickness/2))*(2*math.pi/360))-(math.pi/2))
            cairo_set_source_rgba(display,rgb_to_r_g_b(graduation_fg_colour,graduation_fg_alpha))
            cairo_stroke(display)
            cairo_set_line_width(display, graph_thickness)
            i = i + 1
        end
    end

    -- text
    local txt_radius = data['txt_radius']
    local txt_weight, txt_size = data['txt_weight'], data['txt_size']
    local txt_fg_colour, txt_fg_alpha = data['txt_fg_colour'], data['txt_fg_alpha']
    local movex = txt_radius * (math.cos((angle * 2 * math.pi / 360)-(math.pi/2)))
    local movey = txt_radius * (math.sin((angle * 2 * math.pi / 360)-(math.pi/2)))
    cairo_select_font_face (display, "Ubuntu", CAIRO_FONT_SLANT_NORMAL, txt_weight);
    cairo_set_font_size (display, txt_size);
    cairo_set_source_rgba (display, rgb_to_r_g_b(txt_fg_colour, txt_fg_alpha));
    cairo_move_to (display, x + movex - (txt_size / 2), y + movey + 3);
    cairo_show_text (display, value);
    cairo_stroke (display);
end

-------------------------------------------------------------------------------
--                                                              draw_gauge_ring
-- displays gauges
--
function draw_gauge_ring(display, data, value)
    local max_value = data['max_value']
    local x, y = data['x'], data['y']
    local graph_radius = data['graph_radius']
    local graph_thickness, graph_unit_thickness = data['graph_thickness'], data['graph_unit_thickness']
    local graph_start_angle = data['graph_start_angle']
    local graph_unit_angle = data['graph_unit_angle']
    local graph_bg_colour, graph_bg_alpha = data['graph_bg_colour'], data['graph_bg_alpha']
    local graph_fg_colour, graph_fg_alpha = data['graph_fg_colour'], data['graph_fg_alpha']
    local hand_fg_colour, hand_fg_alpha = data['hand_fg_colour'], data['hand_fg_alpha']
    local graph_end_angle = (max_value * graph_unit_angle) % 360

    -- background ring
    cairo_arc(display, x, y, graph_radius, angle_to_position(graph_start_angle, 0), angle_to_position(graph_start_angle, graph_end_angle))
    cairo_set_source_rgba(display, rgb_to_r_g_b(graph_bg_colour, graph_bg_alpha))
    cairo_set_line_width(display, graph_thickness)
    cairo_stroke(display)

    -- arc of value
    local val = value % (max_value + 1)
    local start_arc = 0
    local stop_arc = 0
    local i = 1
    while i <= val do
        start_arc = (graph_unit_angle * i) - graph_unit_thickness
        stop_arc = (graph_unit_angle * i)
        cairo_arc(display, x, y, graph_radius, angle_to_position(graph_start_angle, start_arc), angle_to_position(graph_start_angle, stop_arc))
        cairo_set_source_rgba(display, rgb_to_r_g_b(graph_fg_colour, graph_fg_alpha))
        cairo_stroke(display)
        i = i + 1
    end
    local angle = start_arc

    -- hand
    start_arc = (graph_unit_angle * val) - (graph_unit_thickness * 2)
    stop_arc = (graph_unit_angle * val)
    cairo_arc(display, x, y, graph_radius, angle_to_position(graph_start_angle, start_arc), angle_to_position(graph_start_angle, stop_arc))
    cairo_set_source_rgba(display, rgb_to_r_g_b(hand_fg_colour, hand_fg_alpha))
    cairo_stroke(display)

    -- graduations marks
    local graduation_radius = data['graduation_radius']
    local graduation_thickness, graduation_mark_thickness = data['graduation_thickness'], data['graduation_mark_thickness']
    local graduation_unit_angle = data['graduation_unit_angle']
    local graduation_fg_colour, graduation_fg_alpha = data['graduation_fg_colour'], data['graduation_fg_alpha']
    if graduation_radius > 0 and graduation_thickness > 0 and graduation_unit_angle > 0 then
        local nb_graduation = graph_end_angle / graduation_unit_angle
        local i = 0
        while i < nb_graduation do
            cairo_set_line_width(display, graduation_thickness)
            start_arc = (graduation_unit_angle * i) - (graduation_mark_thickness / 2)
            stop_arc = (graduation_unit_angle * i) + (graduation_mark_thickness / 2)
            cairo_arc(display, x, y, graduation_radius, angle_to_position(graph_start_angle, start_arc), angle_to_position(graph_start_angle, stop_arc))
            cairo_set_source_rgba(display,rgb_to_r_g_b(graduation_fg_colour,graduation_fg_alpha))
            cairo_stroke(display)
            cairo_set_line_width(display, graph_thickness)
            i = i + 1
        end
    end

    -- text
    local txt_radius = data['txt_radius']
    local txt_weight, txt_size = data['txt_weight'], data['txt_size']
    local txt_fg_colour, txt_fg_alpha = data['txt_fg_colour'], data['txt_fg_alpha']
    local movex = txt_radius * math.cos(angle_to_position(graph_start_angle, angle))
    local movey = txt_radius * math.sin(angle_to_position(graph_start_angle, angle))
    cairo_select_font_face (display, "Ubuntu", CAIRO_FONT_SLANT_NORMAL, txt_weight)
    cairo_set_font_size (display, txt_size)
    cairo_set_source_rgba (display, rgb_to_r_g_b(txt_fg_colour, txt_fg_alpha))
    cairo_move_to (display, x + movex - (txt_size / 2), y + movey + 3)
    cairo_show_text (display, value)
    cairo_stroke (display)

    -- caption
    local caption = data['caption']
    local caption_weight, caption_size = data['caption_weight'], data['caption_size']
    local caption_fg_colour, caption_fg_alpha = data['caption_fg_colour'], data['caption_fg_alpha']
    local tox = graph_radius * (math.cos((graph_start_angle * 2 * math.pi / 360)-(math.pi/2)))
    local toy = graph_radius * (math.sin((graph_start_angle * 2 * math.pi / 360)-(math.pi/2)))
    cairo_select_font_face (display, "Ubuntu", CAIRO_FONT_SLANT_NORMAL, caption_weight);
    cairo_set_font_size (display, caption_size)
    cairo_set_source_rgba (display, rgb_to_r_g_b(caption_fg_colour, caption_fg_alpha))
    cairo_move_to (display, x + tox + 5, y + toy + 1)
    -- bad hack but not enough time !
    if graph_start_angle < 105 then
        cairo_move_to (display, x + tox - 30, y + toy + 1)
    end
    cairo_show_text (display, caption)
    cairo_stroke (display)
end

-------------------------------------------------------------------------------
--                                                               go_clock_rings
-- loads data and displays clock
--
function go_clock_rings(display)
    local function load_clock_rings(display, data)
        local str, value = '', 0
        str = string.format('${%s %s}',data['name'], data['arg'])
        str = conky_parse(str)

        value = tonumber(str)
        if value == nil then value = 0 end
        draw_clock_ring(display, data, value)
    end

    for i in pairs(clock_h) do
        load_clock_rings(display, clock_h[i])
    end
    for i in pairs(clock_m) do
        load_clock_rings(display, clock_m[i])
    end
    for i in pairs(clock_s) do
        load_clock_rings(display, clock_s[i])
    end
end

-------------------------------------------------------------------------------
--                                                               go_gauge_rings
-- loads data and displays gauges
--
function go_temp_rings(display)
    local function load_temp_rings(display, data)
        local value = 0
        local f = assert(io.popen(data['arg'], 'r'))
        local str = assert(f:read('*a'))
        f:close()
        value = tonumber(str)
        if value == nil then value = 0 end
        -- value = math.floor(tonumber(str) / 0.90 + 0.5)
        draw_gauge_ring(display, data, value)
    end

    for i in pairs(temp) do
        load_temp_rings(display, temp[i])
    end
end

function go_gauge_rings(display)
    local function load_gauge_rings(display, data)
        local str, value = '', 0
        str = string.format('${%s %s}',data['name'], data['arg'])
        str = conky_parse(str)
        value = tonumber(str)
        if value == nil then value = 0 end
        draw_gauge_ring(display, data, value)
    end

    for i in pairs(gauge) do
        load_gauge_rings(display, gauge[i])
    end
end

-------------------------------------------------------------------------------
--                                                                         MAIN
function conky_main()
    if conky_window == nil then
        return
    end

    local cs = cairo_xlib_surface_create(conky_window.display, conky_window.drawable, conky_window.visual, conky_window.width, conky_window.height)
    local display = cairo_create(cs)

    local updates = conky_parse('${updates}')
    update_num = tonumber(updates)
    if update_num == nil then update_num = 0 end

    if update_num > 5 then
        go_clock_rings(display)
        go_gauge_rings(display)
        go_temp_rings(display)
    end

    cairo_surface_destroy(cs)
    cairo_destroy(display)
end

-------------------------------------------------------------------------------
--                                                                    MIN / MAX

-- max
function conky_max( list )
    return math.max(unpack(list))
end

-- min
function conky_min( list )
    return math.min(unpack(list))
end

-------------------------------------------------------------------------------
--                                                                    BANDWIDTH

-- remove trailing blanks
function conky_bandwidth( speed )
    -- return string.gsub( conky_parse( speed ), '[iB ]', '')
    return string.gsub(string.gsub( conky_parse( speed ), '[iB ]', ''), '^0$', '')
end

-------------------------------------------------------------------------------
--                                                                         SWAP

-- remove trailing blanks
function conky_swap( space )
    return string.gsub( conky_parse( space ), '0B ', '')
end

-------------------------------------------------------------------------------
--                                                                         NULL

-- blank (null)
function conky_null( desc )
    return string.gsub( conky_parse( desc ), '[(]null[)]', '')
end

-- vim: set ft=lua : --
