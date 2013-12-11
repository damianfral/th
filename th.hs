{-# LANGUAGE FlexibleInstances
  , TypeSynonymInstances
  , OverlappingInstances
  , RecordWildCards #-}

module Main where

import System.Console.GetOpt
import System.Environment
import System.Directory
import Control.Monad
-- import Control.Applicative
import Control.Monad.Trans.Class
import Control.Monad.Trans.Either
import qualified Data.IntMap as H

_version :: String
_version = "v0.2"

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
	show (xs) = H.fold (++) "" $ H.map (++ "\n") $ H.map show xs

parseTask :: String -> EitherT String IO Task
parseTask ('[':' ':']':' ':taskName) = right $ Task NotDone  taskName
parseTask ('[':'-':']':' ':taskName) = right $ Task Started  taskName
parseTask ('[':'x':']':' ':taskName) = right $ Task Done     taskName
parseTask str                        = left  $ "Error parsing: " ++ str

parseTaskList :: String -> EitherT String IO TaskList
parseTaskList = mapM parseTask . filter (not . null) . lines >=> (right . H.fromList . zip [1..])

-----------------------------------------------------------------------------------

findIndex' :: H.IntMap a -> H.Key
findIndex' xs = if H.null xs then 1 else (+) 1 $ fst $ H.findMax xs

createTask ::  String -> TaskList -> TaskList
createTask taskName list = H.insert (findIndex' list) (Task NotDone taskName) list

markTask :: TaskState -> H.Key -> TaskList -> TaskList
markTask s = H.update $ \ task -> Just task {state = s}

startTask, finishTask, deleteTask :: H.Key -> TaskList -> TaskList
startTask  = markTask Started
finishTask = markTask Done
deleteTask = H.update $ const Nothing

-----------------------------------------------------------------------------------

data Action = Create String | Start TaskID | Finish TaskID | Delete TaskID | Print deriving Show

executeAction :: Action -> TaskList -> TaskList
executeAction (Create taskName) = createTask taskName
executeAction (Start  taskId)   = startTask taskId
executeAction (Finish taskId)   = finishTask taskId
executeAction (Delete taskId)   = deleteTask taskId
executeAction _                 = id

-----------------------------------------------------------------------------------

data Options = Options
	{ help    :: Bool
	, version :: Bool
	, action  :: Action
	, file    :: String
	} deriving Show

defaultOptions :: Options
defaultOptions = Options
	{ help    = False
	, version = False
	, action  = Print
	, file    = "todo.txt"
	}

optionsDescription :: [ OptDescr (Options -> Options)]
optionsDescription =
	[ Option "vV" 	["version"]
		(NoArg $ \options -> options {version = True})
		"Show the version of th"

	, Option "hH" 	["help"]
		(NoArg $ \options -> options {help = True})
		"Show help for th"

	, Option "c"	["create"]
		(ReqArg  (\ arg options -> options {action = Create arg}) "TASKNAME")
		"Create a new task"

	, Option "s"	["start"]
		(ReqArg  (\ arg options -> options {action = Start $ read arg}) "TASKID")
		"Mark task as started"

	, Option "f"	["finish"]
		(ReqArg  (\ arg options -> options {action = Finish $ read arg}) "TASKID")
		"Mark task as finished"

	, Option "d"	["delete"]
		(ReqArg  (\ arg options -> options {action = Delete $ read arg}) "TASKID")
		"Delete task"

	, Option "l" ["list"]
		(ReqArg  (\ arg options -> options {file = arg}) "FILENAME")
		"Use this file as the task list"
	]

parseOptions :: [String] -> EitherT String IO Options
parseOptions argv = case getOpt Permute optionsDescription argv of
		(o, _, []  ) 	-> right (foldl (flip id) defaultOptions o)
		(_, _, errors) 	-> left $ (++) (concat errors) (showUsage optionsDescription)

processOptions :: Options -> EitherT String IO Options
processOptions o@(Options {..}) | help      = left $ showUsage optionsDescription
                                | version   = left _version
                                | otherwise = right o

showUsage :: [ OptDescr (Options -> Options)] -> String
showUsage = usageInfo "\n\nUsage: th [OPTION...]"

-------------------------------------------------------------------------------

parseIfExists :: String -> EitherT String IO TaskList
parseIfExists filename = do
	b <- lift $ doesFileExist filename
        fl <- lift $ readFile filename
	if' b (parseTaskList fl) (right H.empty)

processResult :: Options -> TaskList -> IO TaskList
processResult options tasks = writeFile (file options) (show tasks) >> return tasks

printTaskList :: TaskList -> IO ()
printTaskList = putStrLn . H.foldWithKey (\k v xs -> "\n" ++ show k ++ " - " ++ show v ++ xs) ""

main :: IO ()
main = do taskOpts <- runEitherT $ do
	              args <- lift getArgs
	              options <- parseOptions args >>= processOptions
                      task <- parseIfExists $ file options
                      return (task, options)
          case taskOpts of
            Left str -> putStrLn str
            Right (taskList, options) -> do
			            let taskList' = executeAction (action options) taskList
			            processResult options taskList' >>= printTaskList
