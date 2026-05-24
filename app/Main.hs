{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

    import Database.PostgreSQL.Simple
    import Connection
    import IOfunctions
    import Recipes
    import qualified Graphics.UI.Threepenny as UI
    import Graphics.UI.Threepenny.Core
    import GUIfunctions
    import Control.Monad
    import System.Process (callCommand)
    import Control.Concurrent (forkIO, threadDelay)
    import System.Directory (getCurrentDirectory)


    main :: IO ()
    main = do

        currentDir <- getCurrentDirectory
        let staticPath = Just "../wwwroot"

        conn <- startConnection

        startGUI defaultConfig
            { jsPort = Just 8023
            , jsStatic = Just "../wwwroot"
            } setup

    setup :: Window -> UI ()
    setup window = do
        conn <- liftIO startConnection
        return window # set UI.title "recipers"
        showMainView window conn
    
    openBrowser :: IO ()
    openBrowser = callCommand "open http://127.0.0.1:8023"

