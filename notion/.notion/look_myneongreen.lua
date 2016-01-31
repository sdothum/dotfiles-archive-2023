--
-- look_greenlight for Notion's default drawing engine.
--

if not gr.select_engine("de") then
  return
end

-- Clear existing styles from memory.
de.reset()

-- Base style
de.defstyle("*", {
  highlight_colour = "#006600",
  shadow_colour = "#006600",
  background_colour = "#003344",
  foreground_colour = "#777777",

  shadow_pixels = 1,
  highlight_pixels = 1,
  padding_pixels = 1,
  spacing = 0,
  border_style = "elevated",
  border_sides = "tb",

  --font = "-*-helvetica-medium-r-normal-*-12-*-*-*-*-*-*-*",
  --font = "-*-monaco-*-14-*-*-*-*-*-*-*",
  --font = "-*-monospace-*-13-*-*-*-*-*-*-*",
  text_align = "center",
})

de.defstyle("frame", {
  background_colour = "#000000",
  transparent_background = false,
})

de.defstyle("tab", {
  font = "-*-helvetica-medium-r-normal-*-10-*-*-*-*-*-*-*",
  spacing = 1,

  de.substyle("active-selected", {
    highlight_colour = "#62ff00",
    shadow_colour = "#62ff00",
    foreground_colour = "#1be0af",
    --background_colour = "#222222",
  }),

  de.substyle("inactive-selected", {
    foreground_colour = "#009900",
  }),
})

de.defstyle("input", {
  text_align = "left",
  foreground_colour = "#aaaaaa",
  spacing = 1,

  de.substyle("*-selection", {
    background_colour = "#406e63",
    foreground_colour = "#ffffff",
  }),

  de.substyle("*-cursor", {
    background_colour = "#406e63",
  }),
})

dopath("lookcommon_clean")

-- Refresh objects' brushes.
gr.refresh()
