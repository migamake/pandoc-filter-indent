-- | Common definitions shared by Render modules.
module Render.Common where

import Data.Text(Text)
import Alignment
import Token

-- | A single colspan with its tokens.
type TokensWithColSpan = ([(MyTok,Text)], Int, Align)
