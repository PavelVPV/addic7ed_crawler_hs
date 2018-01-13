{-# LANGUAGE OverloadedStrings #-}
--{-# LANGUAGE OverloadedStrings, FlexibleInstances, MultiParamTypeClasses, DeriveDataTypeable, CPP #-}

import Data.Conduit.Binary (sinkFile)
import Data.List (isInfixOf)
import Network.HTTP.Base (urlEncode)
import Network.HTTP.Conduit (http, lbsResponse, httpLbs, parseRequest, newManager, requestHeaders, Request, responseBody, responseHeaders)
import Network.HTTP.Client (defaultManagerSettings)
import Network.HTTP.Types.Header
import Text.XML (Node (NodeContent))
import qualified Data.Conduit as C
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Text.HTML.DOM as D
import qualified Data.Text.Encoding as TE
import Data.List.Split (splitOn)
import Text.XML.Cursor (Cursor, content, element, fromDocument, child, attributeIs, attribute, node, descendant, following,
                        (&/), ($//), (&|), (>=>))
import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class -- liftIO
import Control.Monad (liftM)

addictedUrl = "http://www.addic7ed.com/"
searchExt = "search.php?Submit=Search&search="

-------------------------------------------------------------------------------
-- HTTP part
-------------------------------------------------------------------------------

setUserAgent :: Request -> Request
setUserAgent req = req{requestHeaders = ("User-agent", "Mozilla/5.0 (Windows NT 10.0; WOW64; rv:44.0) Gecko/20100101 Firefox/44.0") : requestHeaders req}

setReferer :: B.ByteString -> Request -> Request
setReferer r req = req{requestHeaders = ("Referer", r) : requestHeaders req}

getContentDisposition :: ResponseHeaders -> Maybe String
getContentDisposition [] = Nothing
getContentDisposition ((a,b):xs) | a == "Content-Disposition" = Just (T.unpack $ TE.decodeUtf8 b)
                                 | otherwise = getContentDisposition xs

loadPage :: String -> IO Cursor
loadPage url = do
    putStrLn ("URL: " ++ url)

    request <- setUserAgent <$> parseRequest url
    manager <- newManager defaultManagerSettings
    response <- httpLbs request manager
    let page = responseBody response
    let cursor = fromDocument $ D.parseLBS page
    return cursor

removeColon s = let repl ':' = '_'
                    repl c = c
                 in map repl s

loadFile :: String -> String -> MaybeT IO String
loadFile r url = MaybeT $ do
    putStrLn ("File URL: " ++ url)
    putStrLn ("Referer: " ++ r)

    request <- (setReferer . TE.encodeUtf8 $ T.pack r) <$> setUserAgent <$> parseRequest url
    manager <- newManager defaultManagerSettings

    runResourceT $ do
        response <- http request manager

        let cd = getContentDisposition $ responseHeaders response
        let fileName = removeColon <$> (!! 1) <$> ((splitOn "\"") <$> cd)

        liftIO $ putStrLn $ "File name: " ++ show fileName

        case fileName of
            Nothing -> return Nothing
            Just f -> do
                responseBody response C.$$+- sinkFile f
                return $ Just f

-------------------------------------------------------------------------------
-- Movie search
-------------------------------------------------------------------------------

findMoviesHrefs :: Cursor -> [Cursor]
findMoviesHrefs = element "table" >=> attributeIs "class" "tabel" >=> descendant >=> element "a"

extractMoviesNames :: Cursor -> String
extractMoviesNames = T.unpack . T.concat . content

extractHrefs :: Cursor -> String
extractHrefs = T.unpack . T.concat . attribute "href"

getMovies :: Cursor -> [(String, String)]
getMovies c =
    let hrefs = findMoviesHrefs c
    in map (\c -> ((extractHrefs c), (extractMoviesNames ((child c) !! 0)))) hrefs

searchMovie :: String -> IO [(String, String)]
searchMovie w = do
    cursor <- loadPage $ addictedUrl ++ searchExt ++ (urlEncode w)
    return $ cursor $// (getMovies)

searchMovieFile :: FilePath -> IO [(String, String)]
searchMovieFile f = do
    file <- D.readFile f
    let cursor = fromDocument file
    return $ cursor $// (getMovies)

-------------------------------------------------------------------------------
-- Subtitle search
-------------------------------------------------------------------------------

findSubsTable :: Cursor -> [Cursor]
findSubsTable = element "table" >=> attributeIs "class" "tabel95" >=> following

findSubsLang :: Cursor -> [Cursor]
findSubsLang = element "td" >=> attributeIs "class" "language" >=> descendant

findSubsStat :: Cursor -> [Cursor]
findSubsStat = element "td" >=> attributeIs "class" "newsDate" >=> attributeIs "colspan" "2" >=> descendant

findSubsHrefs :: Cursor -> [Cursor]
findSubsHrefs = element "a" >=> attributeIs "class" "buttonDownload"

extractContent :: [Cursor] -> [String]
extractContent z =
    let b = [ x | x@NodeContent {} <- ([ node y | y <- z ]) ]
    in map (T.unpack . (\(NodeContent a) -> a)) b

getSubsLang :: Cursor -> [String]
getSubsLang c =
    let langs = (findSubsTable >=> findSubsLang) c
    -- NOTE: language contains some strange content some filter was added to remove it
    in filter (not . isInfixOf "\n\t") $ extractContent langs

getSubsHrefs :: Cursor -> [String]
getSubsHrefs c =
    let hrefs = (findSubsTable >=> findSubsHrefs) c
    in map extractHrefs hrefs

getSubsStat :: Cursor -> [String]
getSubsStat c =
    let hrefs = (findSubsTable >=> findSubsStat) c
    in extractContent hrefs

mergeResults :: [t0] -> [t1] -> [(t0, t1)]
mergeResults [] _ = []
mergeResults _ [] = []
mergeResults (a:as) (b:bs) = (a,b) : mergeResults as bs

searchSubs :: String -> MaybeT IO [(String, String)]
searchSubs url = MaybeT $ do
    cursor <- loadPage url
    return $ Just $ mergeResults
        (cursor $// (getSubsHrefs))
        (map
            (\(a,b) -> a ++ " - " ++ b)
            (mergeResults
                (cursor $// (getSubsLang))
                (cursor $// (getSubsStat))))

searchSubsFile :: FilePath -> IO [(String, [Char])]
searchSubsFile f = do
    file <- D.readFile f
    let cursor = fromDocument file
    return $ mergeResults
        (cursor $// (getSubsHrefs))
        (map
            (\(a,b) -> a ++ " - " ++ b)
            (mergeResults
                (cursor $// (getSubsLang))
                (cursor $// (getSubsStat))))

-------------------------------------------------------------------------------
-- Result selection
-------------------------------------------------------------------------------

numerate :: Integer -> [String] -> [String]
numerate _ [] = []
numerate i (x:xs) = ((show i) ++ ": " ++ x) : (numerate (i + 1) xs)

chooseLink :: [(String, String)] -> MaybeT IO String
chooseLink [] = MaybeT $ do
    putStrLn "No result"
    return Nothing
chooseLink res = MaybeT $ do
    -- Print search result
    putStrLn "Please choise the result or -1 to abort:"
    -- NOTE: Filter is added because subtitles comes with \n
    mapM_ (putStrLn) $ numerate 0 $ (map (filter (/= '\n') . snd) res)
    choise_s <- getLine
    let choise = read choise_s -- FIXME: Catch exception
    if choise == -1 then
        return Nothing
    else
        if ((choise >= 0) && (choise < (length res))) then
            return (Just (fst $ res !! choise))
        else
            return Nothing

composeLink :: String -> String
composeLink  = (addictedUrl ++)

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

getNameToSearch :: IO String
getNameToSearch = do
    putStr "Enter Serial Name: "
    x <- getLine
    return x

main :: IO ()
main = do
    what <- getNameToSearch
    searchRes <- searchMovie what
    maybeDone <- runMaybeT $ do
        movieChoise <- chooseLink searchRes
        let movie = composeLink movieChoise
        subs <- searchSubs $ movie
        subsChoise <- chooseLink subs
        let file = composeLink subsChoise
        loadFile movie file
    case maybeDone of
      Nothing -> putStrLn "Sorry, I'm failed"
      Just x -> putStrLn $ "Done! File: " ++ x

-------------------------------------------------------------------------------
-- Test functions
-------------------------------------------------------------------------------

-- Test function for movie search
movieTest :: IO (Maybe String)
movieTest = do
    searchRes <- searchMovieFile "./movie_page.html"
    maybeDone <- runMaybeT $ do
        movieChoise <- chooseLink searchRes
        return $ composeLink movieChoise
    return maybeDone

-- Test function for subtitles search
subsTest :: IO (Maybe String)
subsTest = do
    subs <- searchSubsFile "./subtitles_page.html"
    maybeDone <- runMaybeT $ do
        choise <- chooseLink subs
        return $ composeLink choise
    return maybeDone
