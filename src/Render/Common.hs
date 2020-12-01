module Render.Common where

import Data.Text(Text)
import Alignment
import Token

type TextWithColSpan = (Text, Int, Align)
type TokensWithColSpan = ([(MyTok,Text)], Int, Align)
