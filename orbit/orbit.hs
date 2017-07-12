-- sdothum - 2016 (c) wtfpl

-- Window Manager
-- ▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂

-- ................................................................... WM server

type Height    = Int
type Width     = Int
type Margin    = Int
type Gap       = Int
type Border    = Int
type Side      = Int
type Split     = Int
type OffsetX   = Int
type OffsetY   = Int
type FrameId   = Int
type WinId     = Int
type MonitorId = Int
type Display   = [Char]
type Label     = [Char]
type Dynamic   = Bool
type TagId     = Int
type LastId    = TagId

data Root      = Root Width Height
data Monitor   = Monitor MonitorId Display Width Height OffsetX OffsetY
data Panel     = Panel MonitorId Side Width Height
data Bar       = Bar MonitorId Side Width Height
data Screen    = Screen MonitorId Width Height [TagId] LastId
data Tag       = Tag TagId Label Split [FrameId]
data Frame     = Frame FrameId Split [WinId]
data Window    = Window WinId
