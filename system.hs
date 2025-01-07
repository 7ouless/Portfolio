{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use isNothing" #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# HLINT ignore "Use print" #-}
{-# HLINT ignore "Redundant $" #-}
{-# HLINT ignore "Use isJust" #-}
import System.IO ()
import Data.List (intercalate, sortBy)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import Text.ParserCombinators.ReadP (char)
import Data.Char (ord, toLower)
import Data.Ord (comparing)

data User = User {userID::Maybe Int,
                    name::String}
                    deriving (Eq)
instance Show User where
  show :: User -> String
  show user = "ID: " ++ show (maybeToString (userID user)) ++ " Name: " ++ name user

data Book = Book {bookID::Maybe Int,
                    title::String,
                    author::String,
                    available::Maybe Int}
                    deriving (Eq)
instance Show Book where
  show :: Book -> String
  show book = "ID: " ++ show (maybeToString (bookID book)) ++ ", " ++ title book ++ " || " ++ author book ++ ", " ++ "Available: " ++ show (isAvailable (maybeToString (available book)))

listSelectBooks :: [Book] -> Int -> [Book]
listSelectBooks books choice = case choice of
  0 -> availableBooks books
  1 -> borrowedBooks books

availableBooks :: [Book] -> [Book]
availableBooks = filter (\book -> available book == Nothing)

borrowedBooks :: [Book] -> [Book]
borrowedBooks = filter (\book -> available book /= Nothing)

--used to properly display whether the book is available when displaying all books
isAvailable :: String -> String
isAvailable string = case string of
  "0" -> "Yes"
  _ -> "No"

--converts a maybe in to a string
maybeToString :: Maybe Int -> String
maybeToString maybeInt = show (fromMaybe 0 maybeInt)

intToMaybe :: Int -> Maybe Int
intToMaybe x
    | x > 0     = Just x
    | otherwise = Nothing

--Appends the book to a new list (immutable lists)
addBook :: String -> String -> [Book] -> [Book]
addBook title author books =
  let newBooks = books ++ [Book {bookID = nextBookID books 1, title = title, author = author, available = Nothing}]
  in sortBy (comparing bookID) newBooks

--Appends the user to a new list (immutable lists)
addUser :: String -> [User] -> [User]
addUser name users =
  let newUsers = users ++ [User {userID = nextUserID users 1, name = name}]
  in sortBy (comparing userID) newUsers

--Creates a new list with all books without the specified bookID
removeBook :: [Book] -> Int -> [Book]
removeBook books bookid = filter (\book -> bookID book /= Just bookid) books

--Creates a new list with all books without the specified bookID
removeUser :: [User] -> Int -> [User]
removeUser users userid = filter (\user -> userID user /= Just userid) users

--Rewrites the file with the column names and updated books list
saveBooks :: [Book] -> IO ()
saveBooks books = writeFile "library.csv" $ unlines $ "bookID,title,author,available" : map bookToCSV books

--Rewrites the file with the column names and updated books list
saveUsers :: [User] -> IO ()
saveUsers users = writeFile "users.csv" $ unlines $ "userID,name" : map userToCSV users

borrowBook :: [Book] -> Int -> Int -> ([Book], String) --Using a tuple as it looked interesting and useful in another program I looked at
borrowBook books bookID userID =
    let updatedBooks = map (checkAvailability bookID userID) books --creating a new list to show the borrowers ID on the book
        message = if any (\book -> bookIDMatches book bookID && available book == Just userID) updatedBooks --If the change correctly was applied
                  then "Book successfully borrowed!"
                  else "Book is not available!"
    in (updatedBooks, message) --Unsure of how necessary the tuple system is for this but it works well and simply

--Changing books availablity if it's available
checkAvailability :: Int -> Int -> Book -> Book
checkAvailability bookID userID book
    | bookIDMatches book bookID && available book == Nothing =
        book { available = Just userID }
    | otherwise = book

--Boolean value used for checking is changes have correctly taken place
bookIDMatches :: Book -> Int -> Bool
bookIDMatches book targetID = case bookID book of
    Just id -> id == targetID
    Nothing -> False

--Extremely simillar to borrowBook just the inverse
returnBook :: [Book] -> Int -> Int -> ([Book], String)
returnBook books bookID userID =
    let updatedBooks = map (checkBorrower bookID userID) books
        message = if any (\book -> bookIDMatches book bookID && available book == Just 0) updatedBooks
                  then "Book successfully returned!"
                  else "Error, Book not returned!"
    in (updatedBooks, message)

--Checks book if it is borrowed by the specified user and if so lists it as returned
checkBorrower :: Int -> Int -> Book -> Book
checkBorrower bookID userID book
    | bookIDMatches book bookID && available book == Just userID =
        book { available = Just 0 }
    | otherwise = book

--Changes the book format in to the standard format for a CSV file
bookToCSV :: Book -> String
bookToCSV book =
  intercalate "," $ --adds commas in between each 'field'
    [ show (fromMaybe 0 (bookID book)) --fromMaybe resorts to 0 if no just value is found
    , title book
    , author book
    , show (fromMaybe 0 (available book))
    ]

--Changes the user format in to the standard format for a CSV file
userToCSV :: User -> String
userToCSV user =
  intercalate "," $ --adds commas in between each 'field'
    [ show (fromMaybe 0 (userID user)) --fromMaybe resorts to 0 if no just value is found
    , name user
    ]

--reads in the library.csv file
loadBookList :: FilePath -> IO [Book]
loadBookList filepath =
  readFile filepath >>= \contents ->
  let linesOfFile = tail $ lines contents --using tail to avoid also reading in the column names (column names are added in again later)
  in pure $ map parseBook linesOfFile --using pure instead of return as it is more robust

--same principles of loadBookList but for users
loadUserList :: FilePath -> IO [User]
loadUserList filepath =
  readFile filepath >>= \contents -> --Using monadic styling instead of do blocks where possible
  let linesOfFile = tail $ lines contents
  in pure $ map parseUser linesOfFile

nextBookID :: [Book] -> Int -> Maybe Int --Int passed in will be 1 
nextBookID [] idCheckInt = intToMaybe idCheckInt
nextBookID (x:xs) idCheckInt =
    let bookid = fromMaybe 0 (bookID x)
    in if bookid == idCheckInt
       then nextBookID xs (idCheckInt + 1)
       else intToMaybe idCheckInt

nextUserID :: [User] -> Int -> Maybe Int --Int passed in will be 1 
nextUserID [] idCheckInt = intToMaybe idCheckInt
nextUserID (x:xs) idCheckInt =
    let userid = fromMaybe 0 (userID x)
    in if userid == idCheckInt
       then nextUserID xs (idCheckInt + 1)
       else intToMaybe idCheckInt

--finds the next userID in the list
maxUserID :: [User] -> Int
maxUserID users =
  let ids = [fromMaybe (-1) (userID user) | user <- users] -- Extract existing IDs, defaulting to -1 for missing IDs
  in if null ids then 1 else maximum ids + 1

--Creates a book passed from a CSV file from loadBookList 
parseBook :: String -> Book
parseBook line =
  let fields = splitOn "," line --Seperating the data at the comma points
  in Book
       { bookID = Just (read (head fields))
       , title = fields !! 1
       , author = fields !! 2
       , available = isNothing (fields !! 3)
       }

--Same as parseBook but for users, using map to be applied to all users in the list in loadUserList
parseUser :: String -> User
parseUser line =
  let fields = splitOn "," line
  in User
       { userID = Just (read (head fields))
       , name = fields !! 1
       }

--Stops the attempt of trying to turn a Nothing value in to an integer
isNothing :: String -> Maybe Int
isNothing a
  |a == "0" = Nothing
  |otherwise = strToJustInt a

--Converts a string to a Just Int
strToJustInt :: Read a => String -> Maybe a
strToJustInt a = case reads a of
  [(value, "")] -> Just value  -- Successfully parsed to an integer --The "" contains the non integer parts so if it is empty then the conversion was successful
  _             -> Nothing     -- Invalid input

loop :: [Book] -> [User] -> IO ()
loop books users = do
       putStrLn "Functions available:"
       putStrLn "'1': List All Books/Users"
       putStrLn "'2': List Available/Borrowed Books"
       putStrLn "'3': Add Book/User"
       putStrLn "'4': Remove Book/User"
       putStrLn "'5': Borrow book"
       putStrLn "'6': Return book"
       putStrLn "'7': Save"
       putStrLn "'8': Exit"
       putStrLn "Choose a function:"
       getLine >>= \chooseFunc ->
         case chooseFunc of
           "1" -> do
             putStrLn "Books 'B' or 'b', Users 'U' or 'u'"
             choice <- getLine
             let lowerChoice = map toLower choice -- Convert to lowercase for simpler UX
             case lowerChoice of
              "b" -> do
                mapM_ (putStrLn . show) books
                loop books users
              "u" -> do
                mapM_ (putStrLn . show) users
                loop books users
              _ -> do
                putStrLn "Please enter 'B' or 'b' for book or 'U' or 'u' for user"
                loop books users
           "2" -> do
             putStrLn "Borrowed 'B' or 'b', Available 'A' or 'a'"
             choice <- getLine
             let lowerChoice = map toLower choice -- Convert to lowercase for simpler UX
             case lowerChoice of
              "b" -> do
                let filteredBooks = listSelectBooks books 1
                mapM_ (putStrLn . show) filteredBooks
                loop books users
              "a" -> do
                let filteredBooks = listSelectBooks books 0
                mapM_ (putStrLn . show) filteredBooks
                loop books users
              _ -> do
                putStrLn "Please enter 'B' or 'b' for borrowed books or 'A' or 'a' for available books"
                loop books users
           "3" -> do
             putStrLn "Books 'B' or 'b', Users 'U' or 'u'"
             choice <- getLine
             let lowerChoice = map toLower choice
             case lowerChoice of
              "b" -> do
                putStrLn "Enter the book title: "
                title <- getLine
                putStrLn "Enter the book author: "
                author <- getLine
                let updatedBooks = addBook title author books
                loop updatedBooks users --always calling the loop with the new updated lists
              "u" -> do
                putStrLn "Enter users name: "
                name <- getLine
                let updatedUsers = addUser name users
                loop books updatedUsers
              _ -> do
                putStrLn "Please enter 'B' or 'b' for book or 'U' or 'u' for user"
                loop books users
           "4" -> do
             putStrLn "Books 'B' or 'b', Users 'U' or 'u'"
             choice <- getLine
             let lowerChoice = map toLower choice
             case lowerChoice of
              "b" -> do
                putStrLn "Enter the bookID to be removed: "
                bookIdStr <- getLine
                let bookid = read bookIdStr :: Int
                let updatedBooks = removeBook books bookid
                putStrLn "Book removed successfully!"
                loop updatedBooks users
              "u" -> do
                putStrLn "Enter the userID to be removed: "
                userIdStr <- getLine
                let userid = read userIdStr :: Int
                let updatedUsers = removeUser users userid
                putStrLn "User removed successfully!"
                loop books updatedUsers
              _ -> do
                putStrLn "Please enter 'B' or 'b' for book or 'U' or 'u' for user"
                loop books users
           "5" -> do
             putStrLn "Enter the book ID: "
             bookIDStr <- getLine
             putStrLn "Enter your user ID: "
             userIDStr <- getLine
             let bookID = read bookIDStr :: Int
             let userID = read userIDStr :: Int
             let (updatedBooks, message) = borrowBook books bookID userID
             putStrLn message
             loop updatedBooks users
           "6" -> do
             putStrLn "Enter the book ID: "
             bookIDStr <- getLine
             putStrLn "Enter your user ID: "
             userIDStr <- getLine
             let bookID = read bookIDStr :: Int
             let userID = read userIDStr :: Int
             let (updatedBooks, message) = returnBook books bookID userID
             putStrLn message
             loop updatedBooks users
           "7" -> do
             saveBooks books
             saveUsers users
             loop books users
           _ -> putStrLn "Exiting, type 'main' to resume library functionality"

main :: IO ()
main = do
    books <- loadBookList "library.csv"
    users <- loadUserList "users.csv"
    length books `seq` return () -- Forces evaluation of the books file
    length users `seq` return () -- Forces evaluation of the users file
    loop books users