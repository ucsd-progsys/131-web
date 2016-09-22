--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Control.Arrow ((>>>))
import           Hakyll
--------------------------------------------------------------------------------

main :: IO ()
main = hakyll $ do
    match "static/*"     copyDir
    match "static/practice/*" copyDir
    -- match "static/hw4/*"      copyDir
    -- match "static/hw5/*"      copyDir
    -- match "static/hw6/*"      copyDir
    match "lectures/*"        copyDir

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "templates/*"   $ compile templateCompiler
    match "homeworks/*"   $ myMakeHTML
    match (fromList tops) $ myMakeHTML

tops = [ "index.markdown"
       , "grades.markdown"
       , "lectures.markdown"
       , "links.markdown"
       , "assignments.markdown"]

copyDir = do route   idRoute
             compile copyFileCompiler


myMakeHTML
  = do route   $ setExtension "html"
       compile $ pandocCompiler
         >>= loadAndApplyTemplate "templates/default.html" defaultContext
         >>= relativizeUrls
