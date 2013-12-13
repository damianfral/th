{-# LANGUAGE FlexibleInstances
  , FlexibleContexts
  , TypeSynonymInstances
  , OverlappingInstances
  , RecordWildCards #-}

module Main where

import System.Console.GetOpt
import System.Environment
import System.Directory
import Control.Monad
import Control.Eff
import Control.Eff.Lift
import Control.Eff.Exception
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

parseTask :: (Member (Exc String) r) => String -> Eff r Task
parseTask ('[':' ':']':' ':taskName) = return $ Task NotDone  taskName
parseTask ('[':'-':']':' ':taskName) = return $ Task Started  taskName
parseTask ('[':'x':']':' ':taskName) = return $ Task Done     taskName
parseTask str                        = throwExc  $ "Error parsing: " ++ str

parseTaskList :: (Member (Exc String) r) => String -> Eff r TaskList
parseTaskList = mapM parseTask . filter (not . null) . lines >=> (return . H.fromList . zip [1..])

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

parseOptions :: (Member (Exc String) r) => [String] -> Eff r Options
parseOptions argv = case getOpt Permute optionsDescription argv of
		(o, _, []  ) 	-> return (foldl (flip id) defaultOptions o)
		(_, _, errors) 	-> throwExc $ (++) (concat errors) (showUsage optionsDescription)

processOptions :: (Member (Exc String) r) => Options -> Eff r Options
processOptions o@(Options {..}) | help      = throwExc $ showUsage optionsDescription
                                | version   = throwExc _version
                                | otherwise = return o

showUsage :: [ OptDescr (Options -> Options)] -> String
showUsage = usageInfo "\n\nUsage: th [OPTION...]"

-------------------------------------------------------------------------------

parseIfExists :: (SetMember Lift (Lift IO) r, Member (Exc String) r) => String -> Eff r TaskList
parseIfExists filename = do
	b <- lift $ doesFileExist filename
        fl <- lift $ readFile filename
	if' b (parseTaskList fl) (return H.empty)

processResult :: Options -> TaskList -> IO TaskList
processResult options tasks = writeFile (file options) (show tasks) >> return tasks

printTaskList :: TaskList -> IO ()
printTaskList = putStrLn . H.foldWithKey (\k v xs -> "\n" ++ show k ++ " - " ++ show v ++ xs) ""


helper :: (SetMember Lift (Lift IO) r, Member (Exc String) r) => Eff r ()
helper = do
  args <- lift getArgs
  options <- parseOptions args >>= processOptions
  task <- parseIfExists $ file options
  let taskList' = executeAction (action options) task
  lift $ processResult options taskList' >>= printTaskList


main :: IO ()
main = void $ runLift ((runExc $ catchExc helper (lift . putStrLn)):: (SetMember Lift (Lift IO) r) => Eff r (Either String ()))
