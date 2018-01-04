{-# LANGUAGE OverloadedStrings #-}

import Data.List (isInfixOf)
import Network.HTTP.Base (urlEncode)
import Network.HTTP.Conduit (httpLbs, parseRequest, newManager, requestHeaders, Request, responseBody)
import Network.HTTP.Client (defaultManagerSettings)
import Text.XML (Node (NodeContent))
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Text.HTML.DOM as D
import Text.XML.Cursor (Cursor, content, element, fromDocument, child, attributeIs, attribute, node, descendant, following,
                        (&/), ($//), (&|), (>=>))

addictedUrl = "http://www.addic7ed.com/"
searchExt = "search.php?Submit=Search&search="

-------------------------------------------------------------------------------
-- HTTP part
-------------------------------------------------------------------------------

setUserAgent :: Request -> Request
setUserAgent req = req{requestHeaders = ("User-agent", "Mozilla/5.0 (Windows NT 10.0; WOW64; rv:44.0) Gecko/20100101 Firefox/44.0") : requestHeaders req}

loadPage url = do
    putStrLn ("URL: " ++ url)

    request <- setUserAgent <$> parseRequest url
    manager <- newManager defaultManagerSettings
    response <- httpLbs request manager
    let page = responseBody response
    let cursor = fromDocument $ D.parseLBS page
    return cursor

-------------------------------------------------------------------------------
-- Movie search
-------------------------------------------------------------------------------

findMoviesHrefs :: Cursor -> [Cursor]
findMoviesHrefs = element "table" >=> attributeIs "class" "tabel" >=> descendant >=> element "a"

extractMoviesNames = T.unpack . T.concat . content

extractHrefs = T.unpack . T.concat . attribute "href"

getMovies c = 
    let hrefs = findMoviesHrefs c
    in map (\c -> ((extractHrefs c), (extractMoviesNames ((child c) !! 0)))) hrefs

searchMovie w = do
    cursor <- loadPage $ addictedUrl ++ searchExt ++ (urlEncode w)
    return $ cursor $// (getMovies)

searchMovieFile f = do
    file <- D.readFile f
    let cursor = fromDocument file
    return $ cursor $// (getMovies)

-------------------------------------------------------------------------------
-- Subtitle search
-------------------------------------------------------------------------------

findSubsTable = element "table" >=> attributeIs "class" "tabel95" >=> following

findSubsLang = element "td" >=> attributeIs "class" "language" >=> descendant

findSubsStat = element "td" >=> attributeIs "class" "newsDate" >=> attributeIs "colspan" "2" >=> descendant

findSubsHrefs = element "a" >=> attributeIs "class" "buttonDownload"

extractContent z = 
    let b = [ x | x@NodeContent {} <- ([ node y | y <- z ]) ]
    in map (T.unpack . (\(NodeContent a) -> a)) b

getSubsLang c =
    let langs = (findSubsTable >=> findSubsLang) c
    -- NOTE: language contains some strange content some filter was added to remove it
    in filter (not . isInfixOf "\n\t") $ extractContent langs

getSubsHrefs c = 
    let hrefs = (findSubsTable >=> findSubsHrefs) c
    in map extractHrefs hrefs

getSubsStat c = 
    let hrefs = (findSubsTable >=> findSubsStat) c
    in extractContent hrefs

mergeResults [] _ = []
mergeResults _ [] = []
mergeResults (a:as) (b:bs) = (a,b) : mergeResults as bs

searchSubs url = do
    cursor <- loadPage url
    return $ mergeResults 
        (cursor $// (getSubsHrefs))
        (map 
            (\(a,b) -> a ++ " - " ++ b) 
            (mergeResults
                (cursor $// (getSubsLang))
                (cursor $// (getSubsStat))))

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

---chooseLink :: [(String, String)] -> IO (Maybe String)
chooseLink [] = do
    putStrLn "No result"
    return Nothing
chooseLink res = do 
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

composeLink :: Maybe String -> Maybe String
composeLink Nothing = Nothing
composeLink (Just u) = Just (addictedUrl ++ u)

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

getNameToSearch = do
    putStr "Enter Serial Name: "
    x <- getLine
    return x

main = do
    what <- getNameToSearch
    searchRes <- searchMovie what
    movieChoise <- chooseLink searchRes
    let movie = composeLink movieChoise
    case movie of
        Nothing -> return Nothing
        Just x -> do
            subs <- searchSubs x
            subsChoise <- chooseLink subs
            let downloadLink = composeLink subsChoise
            return downloadLink

-------------------------------------------------------------------------------
-- Test functions
-------------------------------------------------------------------------------

-- Test function for movie search
movieTest = do
    searchRes <- searchMovieFile "./movie_page.html"
    movieChoise <- chooseLink searchRes
    return $ composeLink movieChoise

-- Test function for subtitles search
subsTest = do
    subs <- searchSubsFile "./subtitles_page.html"
    choise <- chooseLink subs
    return $ composeLink choise
