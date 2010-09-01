module Conflict where

type Conflict s = ActionTable s

conflicts :: ActionTable s -> Maybe (Conflict s)
conflicts tab = 
