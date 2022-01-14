module ViewContext exposing (..)

import ViewMap


type ViewMode
    = ViewInfo
    | ViewMap


type ViewContext
    = MapContext ViewMap.MapContext
    | InfoContext
