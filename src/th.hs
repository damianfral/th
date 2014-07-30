{-# LANGUAGE FlexibleInstances
  , FlexibleContexts
  , TypeSynonymInstances
  , OverlappingInstances #-}

module Main where

import Control.Applicative
import Control.Monad
import Control.Monad.Except
import Data.List
import Data.Monoid
import Options.Applicative
import System.Directory

import qualified Data.IntMap as H

version :: String
version = "v0.4"

if' :: Bool -> a -> a -> a
if' True x _  = x
if' False _ y = y

data TaskState = NotDone | Started | Done deriving (Eq)

instance Show TaskState where
	show NotDone = "[ ]"
	show Started = "[-]"
	show Done    = "[x]"

data Task = Task {state :: TaskState, name :: String} deriving (Eq)

instance Show Task where
	show (Task taskState taskName) = show taskState ++ " " ++ taskName

type TaskList = H.IntMap Task
type TaskID   = H.Key

instance Show TaskList where
	show = H.fold (++) "" . H.map (++ "\n") . H.map show

parseTask :: String -> ExcMonad Task
parseTask ('[':' ':']':' ':taskName) = return $ Task NotDone  taskName
parseTask ('[':'-':']':' ':taskName) = return $ Task Started  taskName
parseTask ('[':'x':']':' ':taskName) = return $ Task Done     taskName
parseTask err                        = throwError $ "Error parsing: " ++ err

parseTaskList :: String -> ExcMonad TaskList
parseTaskList = mapM parseTask . filter (not . null) . lines >=> (return . H.fromList . zip [1..])

--------------------------------------------------------------------------------

findIndex' :: H.IntMap a -> H.Key
findIndex' xs = if H.null xs then 1 else (+) 1 $ fst $ H.findMax xs

createTask ::  String -> TaskList -> TaskList
createTask taskName list = H.insert (findIndex' list) (Task NotDone taskName) list

markTask :: TaskState -> H.Key -> TaskList -> TaskList
markTask s = H.update $ \ task -> return task {state = s}

startTask, finishTask, deleteTask :: H.Key -> TaskList -> TaskList
startTask  = markTask Started
finishTask = markTask Done
deleteTask = H.update $ const Nothing

--------------------------------------------------------------------------------

data Command  = Create String | Start TaskID | Finish TaskID | Delete TaskID | Print deriving Show

executeCommand :: Command -> TaskList -> TaskList
executeCommand (Create taskName) = createTask taskName
executeCommand (Start  taskId)   = startTask taskId
executeCommand (Finish taskId)   = finishTask taskId
executeCommand (Delete taskId)   = deleteTask taskId
executeCommand _                 = id

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

argParser :: Parser Int
argParser = read <$> argument str (metavar "TASKID")

parseCreate, parseStart, parseFinish, parseDelete :: Parser Command
parseCreate  = Create . mconcat . intersperse " " <$> many (argument str (metavar "TASK DESCRIPTION"))
parseStart   = Start  <$> argParser
parseFinish  = Finish <$> argParser
parseDelete  = Delete <$> argParser

abrevs :: String -> [String]
abrevs x = map (`take` x) [1..length x ]

multicommand :: String -> ParserInfo a -> Mod CommandFields a
multicommand c p = mconcat $ map (`command` p) (abrevs c)

parseCommand :: Parser Command
parseCommand = subparser (mconcat commands) <|> pure Print
    where commands = [ multicommand "create" (parseCreate `withInfo` "Create a new task")
                     , multicommand "start"  (parseStart  `withInfo` "Start a task")
                     , multicommand "finish" (parseFinish `withInfo` "Finish a task")
                     , multicommand "delete" (parseDelete `withInfo` "Delete a task") ]

data Options = Options
    { _version :: Bool
    , _file    :: String
    , _command :: Command
    } deriving Show

parseVersion :: Parser Bool
parseVersion = switch $ mconcat [long "version", short 'v', help "Show version"]

parseFile :: Parser String
parseFile = strOption $ mconcat
    [long "file", short 'f', value "todo.txt", help "Specify filename [default = todo.txt]"]


parseOptions :: Parser Options
parseOptions = Options <$> parseVersion <*> parseFile <*> parseCommand

--------------------------------------------------------------------------------

type ExcMonad = ExceptT String IO

parseIfExists ::  String -> ExcMonad TaskList
parseIfExists filename = lift (doesFileExist filename) >>= \b ->
	if b then lift (readFile filename) >>= parseTaskList
  else return H.empty

saveTaskList :: Options -> TaskList -> IO TaskList
saveTaskList options tasks = writeFile (_file options) (show tasks) >> return tasks

showTaskList :: TaskList -> String
showTaskList = H.foldWithKey (\k v xs -> "\n" ++ show k ++ " - " ++ show v ++ xs) ""

processOptions :: Options -> ExcMonad TaskList
processOptions (Options True _ _) = throwError version
processOptions options = executeCommand (_command options) <$> parseIfExists (_file options) >>= lift . saveTaskList options

reportResult :: Either String TaskList -> IO ()
reportResult (Left l)       = putStrLn l
reportResult (Right taskList) = print taskList

main :: IO ()
main = execParser parseOptions' >>= runExceptT . processOptions >>= reportResult
  where parseOptions' = parseOptions `withInfo` "A todo list manager written in Haskell"
