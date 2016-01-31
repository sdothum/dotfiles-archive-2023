-- look_simpleblue.lua drawing engine configuration file for Notion.

if not gr.select_engine("de") then return end

de.reset()

de.defstyle("*", {
  shadow_colour = "black",
  highlight_colour = "black",
  background_colour = "#6e513d",
  foreground_colour = "#afafaf",
  padding_pixels = 5,
  highlight_pixels = 0,
  shadow_pixels = 0,
  border_style = "elevated",
  font = "-*-helvetica-medium-r-normal-*-10-*-*-*-*-*-*-*",
  text_align = "center",
})

de.defstyle("frame", {
  shadow_colour = "black",
  highlight_colour = "black",
  padding_colour = "black",
  foreground_colour = "#ffffff",
  background_colour = "#293235",
  padding_pixels = 0,
  highlight_pixels = 0,
  shadow_pixels = 0,
  de.substyle("active", {
    shadow_colour = "black",
    highlight_colour = "black",
    background_colour = "black",
    foreground_colour = "#ffffff",
  }),
})

de.defstyle("tab", {
  font = "-*-helvetica-medium-r-normal-*-10-*-*-*-*-*-*-*",
  de.substyle("active-selected", {
    shadow_colour = "#f0c000",
    highlight_colour = "#f0c000",
    background_colour = "#f0c000",
    foreground_colour = "#000000",
  }),
  de.substyle("active-unselected", {
    shadow_colour = "#6e513d",
    highlight_colour = "#6e513d",
    background_colour = "#6e513d",
    foreground_colour = "#afafaf",
  }),
  de.substyle("inactive-selected", {
    shadow_colour = "#a8907f",
    highlight_colour = "#a8907f",
    background_colour = "#a8907f",
    foreground_colour = "#ffffff",
  }),
  de.substyle("inactive-unselected", {
    shadow_colour = "#6e513d",
    highlight_colour = "#6e513d",
    background_colour = "#6e513d",
    foreground_colour = "#afafaf",
  }),
  text_align = "center",
})

de.defstyle("input", {
  font = "-*-helvetica-medium-r-normal-*-12-*-*-*-*-*-*-*",
  shadow_colour = "#3f3f3f",
  highlight_colour = "#3f3f3f",
  background_colour = "#3f3f3f",
  padding_pixels = 5,
  highlight_pixels = 0,
  shadow_pixels = 0,
  foreground_colour = "#afafaf",
  text_align = "left",
  spacing = 1,

  de.substyle("*-cursor", {
    background_colour = "white",
    foreground_colour = "#3f3f3f",
  }),

  de.substyle("*-selection", {
    background_colour = "#406e63",
    foreground_colour = "#ffffff",
  }),
})

de.defstyle("input-menu", {
  padding_pixels=0,
})

dopath("lookcommon_clean")

gr.refresh()
