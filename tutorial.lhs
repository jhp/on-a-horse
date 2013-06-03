Haskell on a Horse
==================

 <style>
	body{ margin-left:40px; margin-right: 15%; text-align: justify; }
	.error{ color: red; }
	h1,h2 { font-family; sans-serif; font-weight: normal; color: #222; }
	h1{ font-size: 22pt; border-bottom: double }
	h2{ font-size: 18pt; border-bottom: 3px dotted }

	.oper{ border: 1px solid; padding: 4px; }

	.literate.haskell{ background-color: #eee; border: 1px dotted #666; 
			padding: 8px; width: 100%; }
	.example{ background-color: #eee; width: 100%; 
				border-collapse: collapse; }
	.example td { border: 1px dotted #666; padding: 8px; }
	.example .sourceCode{ border: none; padding: 0; }
 </style>

Haskell on a Horse (HoH) is a combinatorial web framework for the
programming language Haskell. I developed it in 2010, but other
responsibilities prevented me from ever really finishing it - at this
point I have moved on to other projects.

The project remains interesting for various reasons - there is a
coroutine implementation in Control/Arrow/Transformer/Automaton/Monad,
the LabeledArrow and MaybeAutomaton classes are unique as far as I
know, and the compositional handling of web forms is, as far as I
know, more sophisticated than any other system.

If you'd like to browse the code, it is <a href="http://www.github.com/jhp/on-a-horse.html">available on github now</a>.

Installing and Using HoH
------------------------

    cabal install on-a-horse

> {-#LANGUAGE Arrows, QuasiQuotes, ScopedTypeVariables, NoMonomorphismRestriction #-}
> import Web.Horse 
> import Control.Applicative
> import Control.Arrow
> import Control.Monad
> import Control.Monad.Cont
> import Data.Maybe
> import Network.Wai.Handler.Warp (run)
> import Data.Monoid
> import qualified Data.Set as S
> import Data.List.Split (splitOn)
> import Control.Arrow.Transformer.All
> import Text.Pandoc

Atomic Components
-------------------

An HoH application is built up from atomic components.  A component is
a complete HoH application all by itself: it can render itself, and
respond to input.

 <table class="example"><tr><td>

> 
> ex1 = proc url -> do
>     (fo,num::Maybe Integer) <- readForm "enter a number" -< ()
>     returnA -< wrapForm fo
> 

 </td><td>EXAMPLE</td></tr></table>

run this as a web app on port 8080 using

~~~~~~~~{.haskell}
main = do
     app <- runHorse ex1
     run 8080 app
~~~~~~~~

Side-by-Side Components
--------------------------

Components can be rendered side-by-side within a page.

 <table class=example><tr><td>

> ex2 :: HoHMay Url String
> ex2 =  proc url -> do
>               (fo1, oper) <- enumForm "operation" 
>                           [("times", (*)),
>                            ("plus", (+))] -< ()
>               (fo2, x::Maybe Integer) <- readForm "x" -< ()
>               (fo3, y::Maybe Integer) <- readForm "y" -< ()
>               let result = show <$> (oper <*> x <*> y)
>               returnA -< wrapForm $ mconcat [
>                            "Calculate a number!",
>                            "<br/>",
>                            fo1, fo2, fo3,
>                            "Result:",
>                            fromMaybe "" result,
>                            "<br/>"
>                ]

 </td><td>EXAMPLE</td></tr></table>

Replacing one Component With Another
---------------------------------------

Components can be replaced.  A call to the arrow `throwAuto` will
replace the nearest enclosing `catchAuto`.  The new component will be
called immediately, with no form input.

~~~~~~{.haskell}
formSum label fs def = catchAuto $ proc _ -> do
  (fo,f) <- enumForm label fs -< ()
  case f of
    Just f' -> throwAuto -< f'
    Nothing -> returnA -< setFormOut fo def
~~~~~~

Note: `def` is a default value to be used when no form is yet
selected.

 <table class=example><tr><td>

> ex3 :: HoHMay Url String
> ex3 = formSum "example to run" [("example 1",ex1),("example 2",ex2)] mempty
>       >>> arr wrapForm

 </td><td>EXAMPLE</td></tr></table>

A More Complex Example
-------------------------

By combining the techniques above, sophisticated pages can be made
with little code.  

 <table class=example><tr><td>

> ex4 = proc url -> do
>      (fo,result) <- term "expression" -< ()
>      returnA -< wrapForm $ mconcat [fo, "Result:", maybe "" show result, "<br/>"]
>    where
>        term :: String -> HoHMay () (FormOut, Maybe Integer)
>        term label = catchMayAuto $ formSum label 
>             [("number", number label),
>              ("add",oper label "add" (+)),
>              ("multiply",oper label "multiply" (*))] (mempty, Nothing)
>
>        number :: String -> HoHErrMay (HoH () (FormOut, Maybe Integer)) 
>                   () (FormOut, Maybe Integer)
>        number termLabel = proc () -> do
>               fo1 <- linkForm "cancel" (term termLabel) -< ()
>               (fo2,x) <- readForm "number" -< ()
>               returnA -< (fo1 `mappend` fo2, x)
>
>        oper termLabel label f = proc () -> do
>             (fo1) <- linkForm "cancel" (term termLabel) -< ()
>             (fo2,x) <- liftError (term "x") -< ()
>             (fo3,y) <- liftError (term "y") -< ()
>             out <- returnA -< mconcat $ ["<div class=\"oper\">", fo1, label, "<br/>", fo2, fo3]
>             returnA -< (out, f <$> x <*> y)

 </td><td>EXAMPLE</td></tr></table>

Notes: 

* `throwAuto` works by adding an ErrorArrow to its argument.
When it is called recursively, as in the example above, `liftError`
may be required to avoid an infinite type.

* `linkForm` acts much like `throwAuto`, except that it waits to throw
its argument until the link it renders has been clicked.




Building Atomic Components
-------------------------

Atomic components should generally use the 'withInput' function.  This
will add two inputs to an arrow: the first is a unique label for the
component, and the second is the current input to the arrow, or
Nothing if there is no input.  The label should be used as a name in
any form input or query parameters.  Here is the code for `linkForm`.

~~~~~~{.haskell}
linkForm linkName f = withInput $ proc ((),nm,iname) -> do
              case iname of
                Just _ -> throwAuto -< f
                Nothing -> returnA -< (link linkName nm)
~~~~~~

(`link "name" "label"` produces `<a href="?label=1">name</a>`)


Handling urls
-------------

`runHorse` sends the URL as the sole argument to the handler.  A
function, `dispatch`, is available to construct multi-page
applications.


 
 <table class="example"><tr><td>

> ex5 = proc url -> do
>            (dispatch $ staticUrls fourOhFour $
>              [("", urls),
>               ("ex1", ex1),
>               ("ex2", ex2),
>               ("ex3", ex3),
>               ("ex4", ex4)]) -< (url,url)

 </td><td>EXAMPLE</td></tr></table>

> fourOhFour = proc url -> do
>                returnA -< "Page not found"

> urls = proc url -> do
>         returnA -< mconcat [
>                      "<a href=\"/ex1\">example 1</a><br/>",
>                      "<a href=\"/ex2\">example 2</a><br/>",
>                      "<a href=\"/ex3\">example 3</a><br/>",
>                      "<a href=\"/ex4\">example 4</a><br/>"
>                     ]
>

Running the Tutorial
--------------------

This tutorial is a sort of self-executing markdown (pandoc) file.
This is the code to run it.

> main = do
>   tut <- readFile "tutorial.lhs"
>   tmpl <- getDefaultTemplate Nothing "html"
>   let pd = readMarkdown def{readerExtensions = S.insert Ext_literate_haskell (readerExtensions def)} tut
>   let tut' = writeHtmlString def{
>               writerStandalone=True,
>               writerTemplate= either (error . show) id tmpl
>               } pd
>   let ts = splitOn ("EXA"++"MPLE") tut'
>   app <- runHorse $ proc url -> do
>          fo1 <- ex1 -< url
>          fo2 <- ex2 -< url
>          fo3 <- ex3 -< url
>          fo4 <- ex4 -< url
>          fo5 <- ex5 -< url
>          let vals = interleave ts [fo1,fo2,fo3,fo4,fo5]
>          returnA -< mconcat vals 
>   run 8000 app

> interleave (x:xs) (y:ys) = (x:y:interleave xs ys)
> interleave [] ys = ys
> interleave xs [] = xs

 <b>

> -- Jason Priestley, July 26, 2010. (jason @ this domain)

 </b>
