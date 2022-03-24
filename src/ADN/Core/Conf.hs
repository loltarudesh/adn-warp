module ADN.Core.Conf where



-- | Static configuration for the ADN monad 
data ADNConf = ADNConf {
    adnChanImplem :: ChanImplem,            -- ^ Implementation of (unidirectionnal) Chan used to link 'Sink' and 'Source'
    adnScriptLocation :: ScriptLocation     -- ^ location of priviledged scripts
    }

-- |Path to the scripts used for priviledged action (used in "ADN.Core.RootAccess")
data ScriptLocation = ScriptLocation {
    -- | root directory: scripts/
    slPath :: String,
    -- | before script: scripts/before.hs
    slBefore :: String,
    -- | after script: scripts/after.hs
    slAfter :: String,
    -- | TUN file: contain the name and file descriptor to TUN interfaces
    slTUN :: String,
    -- | models directory: scripts\/models/
    slModels :: String,
    -- | runtime directory:  scripts\/runtime/
    slRuntime :: String}
    deriving Show


data ChanImplem = BaseChan | TQueue | TBQueue Int | UnagiBChan Int 
defaultChanImplem = UnagiBChan 4


