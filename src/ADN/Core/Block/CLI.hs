module ADN.Core.Block.CLI (
  addOrderCLI, addOrderCLI_,
  runBlockCLI,
  getOrders,

  registerBlockCLI,
  ) where

import Prelude hiding (error)
import Control.Lens
import Data.Char
import qualified Data.Map as M

import ADN.Core.Block.ADN
import ADN.Core.Block.Tools
import ADN.Core.Block.Close
import ADN.Core.Block.Log



-- | Same as `addOrderCli`, but where the handler ignores its parameter
addOrderCLI_ :: String -> String -> ADN String -> ADN ()
addOrderCLI_ o h a = addOrderCLI o h (pure a)

-- | Register a handler for a command
addOrderCLI :: String -- ^ The name of the command to register ('help', 'send', ...)
            -> String -- ^ A small help message for that command
            -> (String -> ADN String)-- ^ A handler, called on the arguments
            -> ADN ()
addOrderCLI order helpMsg act = do
        m <- cmdCLI  <$> readBlockCmd
        case M.lookup order m of
            Just _ -> error $ "order already defined: " ++ order
            Nothing -> modifyCmd $ \cmd -> cmd{cmdCLI = M.insert order (Order helpMsg act) $ cmdCLI cmd}


-- | Run the block CLI, the parameter is "order arguments_string"
runBlockCLI :: String -> ADN String
runBlockCLI s = (cmdCLI  <$> readBlockCmd) >>= runCLI
    where
        (cmd,args) = over _2 (dropWhile isSpace) $ break isSpace s
        runCLI m 
            | cmd == "help" = pure $ helpIntro ++ concat (showOrder <$> M.assocs m)
            | otherwise = case M.lookup cmd m of
                                Nothing -> pure $ "command " ++ cmd ++ " not found"
                                Just o -> (++"\n") <$> (orderCall o) args
        showOrder (o,order) = showHelp (o, orderDescription order)
        showHelp (o,h) = "\t\t - " ++ o ++"\t" ++ h ++ "\n"
        helpIntro :: String
        helpIntro = "\tSyntax: [PATH]CMD\n" ++
                    "\t[PATH] (optionnal): list of nested blocks, separated by /\n"++
                    "\tCMD: command to send to addressed block\n"++
                        "\t\t This block recognize the commands:\n"++ 
                    showHelp ("help", "print this help, and block-specific commands") ++
                    showHelp ("exit", "exit the CLI")


-- | The currently registered orders for the block
getOrders :: ADN [String]
getOrders = (++) <$> cmds <*> subs
    where
        cmds = M.keys .cmdCLI <$> readBlockCmd
        subs :: ADN [String]
        subs = getSubBlocks >>= (\sb -> concat <$> mapM subsOrder sb)
        subsOrder (name,blk) = map ((name++"/")++) <$> runBlock blk getOrders
        getSubBlocks = M.assocs <$> readBlockSubs


    -- ## INTERNAL ##

registerBlockCLI :: ADN ()
registerBlockCLI = do
    addOrderCLI_ "close" "close the block, and its sub-blocks"           $ getBlock >>= closeBlock >> pure ""
    addOrderCLI_ "ls"  "\tshow the whole sub-block tree"                   $ getSubBlocks >>= findBlockCLI ""

        where 
            getSubBlocks = M.assocs <$> readBlockSubs
            findBlockCLI :: String -> [(BlockName, Block)] -> ADN String
            findBlockCLI p [] = pure ""
            findBlockCLI p ((n,b):bs) = do
                let p' = p ++ "/" ++ n
                resB <- runBlock b getSubBlocks >>= findBlockCLI p'
                ((p'++"\n"++resB)++) <$> findBlockCLI p bs

           
                
              
                    

