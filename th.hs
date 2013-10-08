{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, OverlappingInstances #-}

module Main where

import System.Random
import System.IO
import Control.Monad
import Data.List
--import Text.Peggy

data TaskState = NotDone | Started | Done 
	deriving (Eq)

instance Show TaskState where
	show NotDone = "[ ]"
	show Started = "[~]"
	show Done    = "[X]"

instance Read TaskState where
	readsPrec d ['[', x, ']'] =
		case x of 
			' ' -> [(NotDone, "")]
			'+' -> [(Started, "")]
			'X' -> [(Done, "")]

data Task = Task {identifier :: Int, name :: String, state :: TaskState}
	deriving (Read)

instance Show Task where
	show (Task _ name state) = show state ++ " " ++ name 

instance Eq Task where
	(==) (Task identifier1 _ _) (Task identifier2 _ _) = identifier1 == identifier2


type TaskList = [Task]
instance Show TaskList where
	show (xs) = concat $ intersperse ('\n':"") $ map show xs

-------------------------------------------------------------------------------

rand :: IO Int
rand = do
	newStdGen
	gen <- getStdGen
	return $ fst $ randomR (0::Int, 16777216::Int) gen


createTask :: String -> IO Task
createTask name = do
		identifier <- rand
		return $ Task identifier name NotDone


finishTask :: Task -> Task
finishTask (Task identifier name state) = Task identifier name Done

deleteTask :: Task -> [Task] -> [Task]
deleteTask task = filter (/= task)

-------------------------------------------------------------------------------

data Command = Create | Finish | Delete | Print | Exit | Unrecognized 
	deriving (Read, Show)

readCommand :: String -> Command

readCommand "create" = Create
readCommand "c"      = Create

readCommand "finish" = Finish
readCommand "f"      = Finish

readCommand "delete" = Finish
readCommand "d"      = Finish

readCommand "exit"   = Exit
readCommand "e"      = Exit


readCommand "print"  = Print
readCommand "p"      = Print

readCommand _        = Unrecognized


executeCommand :: Command -> [Task] -> IO [Task]

executeCommand Unrecognized list = do
	putStrLn "Unrecognized Command"
	return list

executeCommand Create list = do
	name <- prompt "Task name: "
	task <- createTask name
	return $ task:list


--executeCommand Finish list = do
--	putStr "Task number: "
--	number <- getLine
--	task   <- createTask name
--	return $ task:list

executeCommand Print list = do
	printList list
	return list

executeCommand _ list = return list


printList :: [Task] -> IO ()
printList list = mapM_ printListItem $ zip [1..] list


printListItem :: (Int,Task) -> IO()
printListItem (n, t) = do
	putStrLn $ show n ++ " - " ++ show t

-------------------------------------------------------------------------------

prompt :: String -> IO String
prompt text = do
	putStr text
	hFlush stdout
	getLine

repl :: [Task] -> IO [Task]
repl list = do
	input <- prompt "> "
	putStrLn ""
	let command = readCommand input
	case command of 
		Exit 	-> return list
		_ 		-> do
			list' <- executeCommand command list
			list'' <- repl list'
			return list''

main :: IO ()
main = do
	putStrLn "## Task list ##"
	repl []
	putStrLn ""