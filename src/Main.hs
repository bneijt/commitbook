{-# LANGUAGE DeriveDataTypeable #-}

module Main where
import Rendering
import System.Console.CmdArgs


data CommitBook = CommitBook {
        width :: Int,
        height :: Int,
        files :: [String]
    } deriving (Show, Data, Typeable)

options = CommitBook {
        width = 3180,
        height = 2382,
        files = [] &= typFile &= args
    }

main = do
    arguments <- cmdArgs options
    renderImages (width arguments) (height arguments) (files arguments)


