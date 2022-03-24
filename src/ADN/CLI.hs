-- |interractive CLI powered by "System.Console.Haskeline" for ADN.
module ADN.CLI (
  CLIConf(..),
  runCLI,
  defaultCLIhistory,defaultCLILogLevel,
  ) where

import Control.Monad.Trans
import Data.List.Split
import Data.List
import System.Console.Haskeline

import ADN.Core


-- | Configuration of the Command Line Interface (CLI)
data CLIConf = CLIConf {
    -- |file to store the cli history between executions. 'Nothing' for no history, default is .adn_history
    cliHistFile :: Maybe String,
    -- |minimum level to display logs. Default is 'Info'
    cliLogLevel :: LogLevel
}

-- | run a given Pipeline, and manage it with the CLI. This function does not return until the CLI is exited. 
runCLI :: MonadIO m => ADNConf -> CLIConf -> ADN () -> m ()
runCLI adn cli =  runADN adn . runHaskelineCLI cli


-- | > defaultCLIhistory = ".adn_history" 
defaultCLIhistory = ".adn_history" 
-- | > defaultCLILogLevel = Info
defaultCLILogLevel = Info


-- ##### INTERNAL #####

type Logger = (String -> ADN ()) -> Log -> ADN ()

makeLogger :: CLIConf -> Logger
makeLogger cli printLog l@(Log lvl blk msg) = if lvl >= cliLogLevel cli
                              then printLog (show l) else pure ()


runHaskelineCLI :: CLIConf -> ADN () -> ADN ()
runHaskelineCLI conf p = runInputT haskelineSettings $ do
    printLog <- (liftIO .) <$> getExternalPrint 
    lift $ registerLogger ( logger printLog ) >> p           -- setting log through the CLI
    lift $ debug $ logCLI conf
    cliIntro >> cli                                         -- running CLI
    lift $ registerLogger ( logger $ liftIO . putStrLn )    -- putting back logs in std output (to display closing logs)
    where 
        cli :: InputT ADN ()
        cli = do 
            sM <- getLine
            case sM of 
                    Nothing -> pure ()
                    Just "exit" -> pure ()
                    Just "help" -> cliIntro >> cli
                    Just s -> dispatch s >> cli
        dispatch s = lift (dispatchCLI s) >>= outputStrLn 
        cliIntro = outputStrLn "\n\t#### ADN Haskeline CLI ####\n" >> dispatch "help"
        getLine = withInterrupt $ handleInterrupt (pure $ Just "") $ getInputLine "ADN % "
        haskelineSettings = Settings completion (cliHistFile conf) True 
        logger = makeLogger conf 

logCLI (CLIConf h l) = "starting Haskeline CLI with log-level: " ++ show l ++ case h of
                        Nothing -> "\tno history will be saved"
                        Just f -> "\thistory will be saved to file: " ++ f

dispatchCLI :: String -> ADN String
dispatchCLI [] = pure ""
dispatchCLI s = dispatch dest
    where 
        dest = wordsBy (=='/') s 
        dispatch :: [String] -> ADN String
        dispatch [] = pure ""
        dispatch (order:[]) = runBlockCLI order
        dispatch ds = maybeInSubBlock (head ds) notFound $ dispatch (tail ds) 
            where notFound = pure $ "Block not found: " ++ show ds


completion :: (String, String) -> ADN (String, [Completion])
completion = completeWord Nothing " \t" $ searchFunc
    where
        searchFunc :: String -> ADN [Completion]
        searchFunc str = map simpleCompletion . filter (str `isPrefixOf`) <$> getOrders

