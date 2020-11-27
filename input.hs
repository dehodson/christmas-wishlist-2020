import qualified Data.ByteString.Lazy as B
import Questionnaire
import GHC.Generics
import Data.Aeson
import System.IO

sectionNames :: [String]
sectionNames = [ "Favorite Drinks"
               , "Favorite Candy"
               , "Favorite Salty Snacks"
               , "Your Obsession"
               , "Favorite Movies"
               , "Book / Magazine Wishlist"
               , "Favorite Desk Items"
               , "Favorite Car Accessories"
               , "Kitchen Gadget Wishlist"
               , "Favorite Personal Care Items"
               , "Favorite Colors"
               , "Favorite Restaurants"
               , "Ideas for Pets"
               , "Clothing Sizes"
               , "Other Wishes"
               ]

promptFor :: String -> IO String
promptFor s = putStr s >> hFlush stdout >> getLine

getSection :: String -> IO Section
getSection name = do
  putStrLn name
  let loop = do {
    item <- getLine ;
    case item of
      "" -> return []
      _ -> (item :) <$> loop
  }
  items <- loop
  return $ Section name items

getQuestionnaire :: String -> IO Questionnaire
getQuestionnaire name = do
  hFlush stdout
  sections <- sequenceA $ getSection <$> sectionNames
  return $ Questionnaire name sections

main :: IO ()
main = do
  let loop = do {
    name <- promptFor "Person's name: " ;
    case name of
      "" -> return []
      _ -> do {
        q <- getQuestionnaire name ;
        (q :) <$> loop
      }
  }
  questionnaires <- loop
  encodeFile "questionnaires.json" questionnaires
  putStrLn "Thank you! :)"
