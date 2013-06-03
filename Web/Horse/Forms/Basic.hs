{-#LANGUAGE ScopedTypeVariables, QuasiQuotes, OverloadedStrings #-}

module Web.Horse.Forms.Basic where

import Control.Arrow
import Data.Monoid

textField :: String -> Maybe String -> String -> String -> String
textField label err val name = 
    mconcat $ [
      maybe "" (\x -> mconcat ["<span class=\"error\">", x, "</span><br/>"]) err,
      "<label>", label, "<br/><input type=\"text\" value=\"", val, "\" name=\"", name, "\"><br/></label>"
    ]

link :: String -> String -> String
link linkName name = mconcat ["<a href=\"?", name, "=1\">", linkName, "</a><br/>"]

select :: String -> [String] -> Int -> String -> String
select label options val name = 
    mconcat [
      "<label>", label, "<br/>",
         "<select name=\"", name, "\">",
            opts,
         "</select>",
       "<br/>",
       "</label>"
    ]
    where
      opts = mconcat $ map renderOpt (zip [0..] options)
      renderOpt (n, opt) = mconcat $ [
                            "<option ", if n == val then "selected=\"selected\"" else "",
                                     " value=\"", show n, "\">",
                                     opt,
                            "</option>"
                           ]



wrapForm :: String -> String
wrapForm f = mconcat ["<form method='POST' action=''>", f, "<input type='submit'></input></form>"]
