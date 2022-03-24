-- |This module is designed to externalize every priviledged action in bash scripts (or C code),
-- in order to execute the main programm as simple user. 
--
-- This insure that sensibles actions (such as configuring network interface)
-- are clearly identified, and can be quickly verified by a cautious user without having to read the whole project.
--
-- Prviledged action are devided in 3 categories:
--
--  * configuration commands, executed before the program, are written to the script /before.sh/
--
--  * cleanup commands, executed after the program, are written to the script /after.sh/
--
--  * runtime programs can be called by the main program, and execute priviledged action. They are compiled C code, owned by the root user, with the suid bit set.
--
module ADN.Core.RootAccess (
    -- * Defining priviledged actions
    RootCall(..),
    RootAccess(..),
        
    -- * Generating scripts
    generateScript,

    -- * runtime execution
    callRuntime,
    getTunFD,
  ) where

import Control.Monad
import Data.List.Split
import Data.List
import System.Process
import Control.Monad.IO.Class
import Text.Read

import ADN.Core.Block
import ADN.Core.Conf


-- |a priviledged action declared by a module
data RootCall = 
    -- |included in before.sh
    RootBefore { 
        -- |description of the action executed (commented in the script)
        rcDesc :: String,
        -- |shell commands to execute
        rcCalls :: [String]} |
    -- |included in after.sh
    RootAfter {
        rcDesc :: String,
        rcCalls :: [String]} |
    -- |copy a model C code (replacing parameters by their values), and compile the result. Set the binary's ownership to root, and set the suid bit
    RootRuntime {
        rcDesc :: String,
        -- |Name of the code without the path: the same name is used for the model, the target and the binary (without .c)
        rcName :: String,
        -- |List of replacement to apply to the model
        rcRecplace :: [(String,String)]} |
    -- | add a commented keyword to the script before.sh. If present, run_adn.sh will execute the C launcher.
    RootRequestTun {
        rcTunName :: String}

-- |Each module requiring priviledged actions should implement the class 'RootAccess', and declare in 'root' the list of 'RootCall' needed.
class RootAccess a where root :: a -> [RootCall]


-- |Generates the scripts before.sh and after.sh, as well as the runtime C code.
generateScript :: ScriptLocation -> [RootCall] -> IO ()
generateScript _ [] = pure ()
generateScript loc calls = do
    mapM_ (copyRuntime loc) calls   -- generate the runtime programs' source codes.
    writeScript slBefore (bash_header "CONFIGURE") before   -- generate before script
    writeScript slAfter (bash_header "CLOSE") after     -- generate before script
    writeScript slTUN "" tuns
        where 
        before = concat $ map showBefore calls
        after  = concat $ map showAfter calls
        tuns   = intercalate "\n" . filter (not . null) $ map showTun calls
 
        showBefore (RootBefore desc call) = showCall desc call          -- appends 'RootBefore' calls 
        showBefore (RootRuntime desc name _) = showRuntime desc name   
        showBefore (RootRequestTun name) = "\t# ADN will create the TUN interface " ++ name
        showBefore _ = []

        showAfter (RootAfter desc call) = showCall desc call            -- appends 'RootAfter' calls    
        showAfter _ = []

        showTun (RootRequestTun name) = name ++ " "
        showTun _ = []


        showCall desc calls = showDesc desc ++ intercalate "\n" calls ++ "\n" 
        showDesc desc = ("\n"++) . unlines $ map ("\t# "++) $ lines desc     -- comment and indent description lines

        showRuntime desc name = showDesc desc ++ "\t# ADN will call this script at runtime, asking for sudo: " ++ name ++ ".sh\n"

        writeScript _ _ [] = pure ()
        writeScript acc h s = do
            putStrLn $ "writting file: " ++ acc loc
            writeFile (acc loc) $ h ++ s 

-- |call a runtime scripts, with a list of arguments. 
callRuntime :: String -> [String] -> ADN ()
callRuntime name args = do
    loc <- adnScriptLocation <$> readADNConf
    liftIO $ callProcess (slRuntime loc ++ name ++ ".sh") args

-- | retreive the file descriptor associated with a given interface
getTunFD :: String -> ADN (Maybe Int)
getTunFD name = do
        filename <- slTUN . adnScriptLocation <$> readADNConf 
        lines <- lines <$> liftIO (readFile filename)
        findName lines
    where 
        findName :: [String] -> ADN (Maybe Int)
        findName [] = return Nothing
        findName (h:t) = 
            let w = words h 
            in if length w < 2 then do
                warning $ "error parsing TUN file, line format incorrect : " ++ h
                findName t
            else if head w == name then return $ readMaybe $ head $ tail w
            else findName t



-- ##### INTERNAL #####


-- header of the script before and after
bash_header name = "#!/usr/bin/env sh\n#CONFIGURATION SCRIPT TO " ++ name ++ " THE ADN NETWORK - generated automatically \nset -x\n\n"

-- copy a runtime program from a model, applying the necessary replacements
copyRuntime :: ScriptLocation -> RootCall -> IO ()
copyRuntime loc (RootRuntime _ name rs) = do
    putStrLn $ "creating runtime script : " ++ target
    callCommand cmd     -- most of it is done in bash
        where 
            cmd = "cat " ++ model ++ " | " ++ sedCalls ++ " > " ++ target 
            sedCalls = intercalate " | " $ map sedCall rs
            sedCall (k,v) = "sed 's/" ++ k ++ "/" ++ v ++ "/g'"     -- best command ever
            model = slModels loc ++ name ++ ".sh"        -- name of the model file
            target = slRuntime loc ++ name ++ ".sh"      -- name of the target file
copyRuntime _ _ = pure ()



