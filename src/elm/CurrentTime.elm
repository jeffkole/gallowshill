module CurrentTime
    ( now
    ) where

import Native.CurrentTime

now : Int
now =
  Native.CurrentTime.now
