--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import                      Data.Monoid (mappend)
import                      Data.List (intersperse)
import                      Debug.Trace
import                      Hakyll
import                      KS.Config
import                      System.Process
import                      Text.Blaze.Html (toHtml, toValue, (!))
import qualified            Text.Blaze.Html5 as H
import                      Text.Blaze.Html5.Attributes as A
import                      Text.HTML.TagSoup as TS
import                      Text.Pandoc.Options
import                      Text.Regex.TDFA hiding (match)

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    -- preprocess $
    --     callCommand "npm install"

    match "node_modules/bootstrap/dist/css/bootstrap.min.*" $ do
        route idRoute
        compile copyFileCompiler

    match "images/assets/*" $ do
        route idRoute
        compile copyFileCompiler

    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "images/assets/favicons/*" $ do
        route idRoute
        compile copyFileCompiler

    match (fromList $ lessFiles defaultEngineConfig) $ do
        route $ setExtension "css"
        compile $ getResourceString
            >>= withItemBody (unixFilter (lessCommand defaultEngineConfig) $ "-" : (lessOptions defaultEngineConfig))
            >>= return . fmap compressCss

    match (fromList ["about.md", "contact.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    -- build up tags
    tags <- buildTags "posts/*" (fromCapture "tags/*.html")

    tagsRules tags $ \tag pattern -> do
        let title = "Posts tagged \"" ++ tag ++ "\""
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll pattern
            let ctx = constField "title" title
                      `mappend` listField "posts" postCtx (return posts)
                      `mappend` defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/tag.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        let pCtx = postCtxWithTags tags
        compile $ pandocCompiler
            >>= applyFilter (withBootstrapTables . withBootstrapQuotes . withBootstrapImgs)
            >>= loadAndApplyTemplate "templates/post.html"    pCtx
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/default.html" pCtx
            >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Archives"            `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls

    create ["atom.xml"] $ do
        route idRoute
        compile $ do
            let feedCtx = postCtx `mappend` bodyField "description"

            posts <- fmap (take 10) . recentFirst =<<
                loadAllSnapshots "posts/*" "content"
            renderAtom feedConfig feedCtx posts


    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Welcome"                `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------
feedConfig :: FeedConfiguration
feedConfig = FeedConfiguration
    { feedTitle       = "keithschulze.com"
    , feedDescription = "Functional (and not so functional) Programming"
    , feedAuthorName  = "Keith Schulze"
    , feedAuthorEmail = "keith.schulze@gmail.com"
    , feedRoot        = "https://keithschulze.com"
    }

postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

postCtxWithTags :: Tags -> Context String
postCtxWithTags tags = tagsFld "tags" tags `mappend` postCtx

-- | Custom tag rendering
tagsFld :: String -> Tags -> Context a
tagsFld = tagsFieldWith getTags tagRenderLink (mconcat . intersperse "")

-- | Render one tag link
tagRenderLink :: String -> (Maybe FilePath) -> Maybe H.Html
tagRenderLink _   Nothing         = Nothing
tagRenderLink tag (Just filePath) =
  Just $ H.a ! A.href (toValue $ toUrl filePath) ! A.class_ "tag" $ toHtml tag

pandocWriterOpts :: WriterOptions
pandocWriterOpts = def
  {
    writerExtensions = enableExtension Ext_smart pandocExtensions,
    writerHighlightStyle = Nothing
  }

applyFilter :: (Monad m, Functor f) => (String -> String) -> f String -> m (f String)
applyFilter f i = return $ fmap f i

withBootstrapQuotes :: String -> String
withBootstrapQuotes = withTags tag
    where
        tag (TS.TagOpen "blockquote" attrs) = TS.TagOpen "blockquote" (attrs ++ [("class","blockquote text-right")])
        tag x = x

withBootstrapImgs :: String -> String
withBootstrapImgs = withTags tag
    where
        tag (TS.TagOpen "img" attrs) =
            if elem "class" (fst $ unzip attrs)
            then TS.TagOpen "img" attrs
            else TS.TagOpen "img" (attrs ++ [("class", "img-fluid")])
        tag x = x

withBootstrapTables :: String -> String
withBootstrapTables = withTagList $ foldl bootstrapifyTables []

bootstrapifyTables :: [TS.Tag String] -> TS.Tag String -> [TS.Tag String]
bootstrapifyTables z el =
    case el of
        TS.TagOpen "table" attrs -> z ++ [beforeDiv, TS.TagOpen "table" (attrs ++ [("class","table table-sm table-striped table-dark")])]
        _  -> if (isTagCloseName "table" el)
                  then z ++ [el, afterDiv]
                  else z ++ [el]
    where
        beforeDiv = TS.TagOpen "div" [("class", "table-responsive-lg")]
        afterDiv = TS.TagClose "div"
