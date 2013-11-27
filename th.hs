{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, OverlappingInstances #-}

module Main where

import System.Console.GetOpt
import System.IO
import System.Environment
import System.Directory
import Control.Monad
import Control.Applicative
import Data.List

import qualified Data.IntMap as H

data TaskState = NotDone | Started | Done deriving (Eq)

instance Show TaskState where
	show NotDone = "[ ]"
	show Started = "[-]"
	show Done    = "[x]"

data Task = Task {state :: TaskState, name :: String} deriving (Eq)

instance Show Task where
	show (Task state name) = show state ++ " " ++ name

type TaskList = H.IntMap Task
type TaskID   = H.Key 

instance Show TaskList where
	show (xs) = H.fold (++) "" $ H.map (++ "\n") $ H.map show xs

parseTask :: String -> Maybe Task
parseTask ('[':' ':']':' ':name) = Just $ Task NotDone  name
parseTask ('[':'-':']':' ':name) = Just $ Task Started  name
parseTask ('[':'x':']':' ':name) = Just $ Task Done     name
parseTask _                      = Nothing

parseTaskList ::  String -> Maybe TaskList
parseTaskList = mapM parseTask . filter (not . null) . lines >=> (Just . H.fromList . zip [1..])

-----------------------------------------------------------------------------------

findIndex' :: H.IntMap a -> H.Key
findIndex' xs = if H.null xs then 1 else (+) 1 $ fst $ H.findMax xs

createTask ::  String -> TaskList -> TaskList
createTask name list = H.insert (findIndex' list) (Task NotDone name) list

markTask :: TaskState -> H.Key -> TaskList -> TaskList
markTask s = H.update $ \ task -> Just task {state = s}

startTask, finishTask, deleteTask :: H.Key -> TaskList -> TaskList
startTask  = markTask Started
finishTask = markTask Done
deleteTask = H.update $ const Nothing

-----------------------------------------------------------------------------------

data Action = Create String | Start TaskID | Finish TaskID | Delete TaskID | Print deriving Show

executeAction :: Action -> TaskList -> TaskList
executeAction (Create name) = createTask name
executeAction (Start id)    = startTask id
executeAction (Finish id)   = finishTask id
executeAction (Delete id)   = deleteTask id
executeAction _             = id

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

options :: [ OptDescr (Options -> Options)]
options =
	[ Option "vV" 	["version"] 
		(NoArg $ \opts -> opts {version = True})
		"Show the version of th"

	, Option "hH" 	["help"] 
		(NoArg $ \opts -> opts {help = True})
		"Show help for th"

	, Option "c"	["create"] 	
		(ReqArg  (\ arg opts -> opts {action = Create arg}) "TASKNAME")
		"Create a new task"
	
	, Option "s"	["start"] 	
		(ReqArg  (\ arg opts -> opts {action = Start $ read arg}) "TASKID")
		"Mark task as started"

	, Option "f"	["finish"] 	
		(ReqArg  (\ arg opts -> opts {action = Finish $ read arg}) "TASKID")
		"Mark task as finished"

	, Option "d"	["delete"] 	
		(ReqArg  (\ arg opts -> opts {action = Delete $ read arg}) "TASKID")
		"Delete task"

	, Option "l" ["list"]
		(ReqArg  (\ arg opts -> opts {file = arg}) "FILENAME")
		"Use this file as the task list"
	]

parseOptions :: [String] -> Either String Options
parseOptions argv = case getOpt Permute options argv of
		(o, n, []  ) 	-> Right (foldl (flip id) defaultOptions o)
		(_, _, errors) 	-> Left $ concat errors ++ usageInfo header options
	where header = "\n\nUsage: th [OPTION...]"

-------------------------------------------------------------------------------

processResult :: Options -> Maybe TaskList -> IO ()
processResult options Nothing      = putStrLn $ "Error parsing" ++ file options
processResult options (Just tasks) = print tasks >> writeFile (file options) (show tasks)

parseIfExists :: String -> IO (Maybe TaskList)
parseIfExists filename = doesFileExist filename >> parseTaskList <$> readFile filename


main :: IO ()
main = do
	args <- getArgs
	case parseOptions args of 
		Left str -> putStrLn str
		Right options -> do
			mTaskList <- parseIfExists $ file options
			processResult options $ mTaskList >>= Just . executeAction (action options)