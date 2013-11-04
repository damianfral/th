{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, OverlappingInstances #-}

module Main where

import System.Console.GetOpt
import System.IO
import Control.Monad
import Data.List
import System.Environment
import System.Directory
import Control.Monad.Error

data TaskState = NotDone | Started | Done deriving (Eq)

instance Show TaskState where
	show NotDone = "[ ]"
	show Started = "[-]"
	show Done    = "[x]"

data Task = Task {state :: TaskState, name :: String} deriving (Eq)

instance Show Task where
	show (Task state name) = show state ++ " " ++ name

type TaskList = [Task]
type TaskID = Int

instance Show TaskList where
	show (xs) = intercalate "\n" $ map show xs

parseTask :: String -> Either String Task
parseTask ('[':' ':']':' ':name) = Right $ Task NotDone name
parseTask ('[':'-':']':' ':name) = Right $ Task Started name
parseTask ('[':'x':']':' ':name) = Right $ Task Done name
parseTask _                      = Left "Bad format"

parseTaskList' :: [String] -> TaskList -> Either String TaskList
parseTaskList' [] x = Right x
parseTaskList' (x:xs) tasks = parseTask x >>= parseTaskList' xs . (append tasks)
	where append xs x = xs ++ [x]

parseTaskList :: String -> Either String TaskList
parseTaskList str = parseTaskList' (lines str) []

---------------------------------------------------------------------------------

createTask ::  String -> TaskList -> TaskList
createTask name list = Task NotDone name :list

markTask :: TaskState -> TaskID -> TaskList -> TaskList
markTask st 1 (x:xs)  = x { state = st } : xs
markTask st i (x:xs) 	| i > 1     = x : markTask st (i-1) xs 
						| otherwise = x:xs
markTask _ _ list = list

startTask, finishTask, deleteTask ::  TaskID -> TaskList -> TaskList
startTask  = markTask Started
finishTask = markTask Done

deleteTask 1 (x:xs) = xs
deleteTask i (x:xs)	| i > 1     = x : deleteTask (i-1) xs
					| otherwise = x:xs
deleteTask _ list = list

---------------------------------------------------------------------------------

data Action = Create String | Start TaskID | Finish TaskID | Delete TaskID | Print deriving Show

executeAction :: Action -> TaskList -> TaskList
executeAction (Create name) = createTask name
executeAction (Start id)    = startTask id
executeAction (Finish id)   = finishTask id
executeAction (Delete id)   = deleteTask id
executeAction _             = id

---------------------------------------------------------------------------------

printList :: TaskList -> IO ()
printList list = mapM_ printListItem $ zip [1..] list

printListItem :: (Int,Task) -> IO()
printListItem (n, t) = putStrLn $ show n ++ " - " ++ show t

---------------------------------------------------------------------------------

processResult :: Options -> Either String TaskList -> IO ()
processResult _ (Left str)          = putStrLn str
processResult options (Right tasks) = printList tasks >> (writeFile  (file options) (show tasks))

parseIfExists' :: String -> Bool -> IO (Either String TaskList)
parseIfExists' _ False       = return (Right [])
parseIfExists' filename True = fmap parseTaskList $ readFile filename

parseIfExists :: String -> IO (Either String TaskList)
parseIfExists filename = doesFileExist filename >>= parseIfExists' filename

---------------------------------------------------------------------------------

data Options = Options
	{ help :: Bool
	, version :: Bool
	, action :: Action
	, file :: String
	} deriving Show

defaultOptions :: Options
defaultOptions = Options 
	{ help = False
	, version = False
	, action = Print
	, file = "todo.txt"
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

---------------------------------------------------------------------------------

main :: IO ()
main = do
	args <- getArgs
	case parseOptions args of 
		Left str -> putStrLn str
		Right options -> do
			taskList <- parseIfExists $ file options
			processResult options $ taskList >>= (Right . executeAction (action options))