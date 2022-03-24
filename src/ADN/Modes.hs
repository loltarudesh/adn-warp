-- | This module provide simple structure to define different modes for ADN
--
-- it defines the 2 mains commands: "genconf" and "run",
-- and the data 'ADNMode' used to define modes of execution for ADN. 
--
-- It is mainly based on the arguments parsing provided by "Options.Applicative". Each mode correspond to a command, with parameters passed as options.
module ADN.Modes where


import ADN.Core hiding(info)
import ADN.CLI
import Options.Applicative 

-- | A complete mode of execution for ADN: the execution value is the full pipeline.
type ADNMode = Mode (ADN ())

-- | Generic type for a optionnal part of ADN.
--
-- In addition to a name and a description, user must define the 2 functions:
--
--   * 'modeGenConf': generates the list of 'RootAccess' needed by the mode when the command /genconf/ is called, parsing relevant parameters from the arguments.
--  
--   * 'modeRunConf': generate the run value of type a, parsing relevant parameters from the arguments.
--
data Mode a = Mode {
    -- |name of the mode (used as the command/option keyword)
    modeName :: String,
    -- |description of the mode, shown in the help
    modeDesc :: String,
    -- |return the list of 'RootCall' used to generate the configuration files, parsing the parameters from the arguments.
    modeGenConf :: Parser [RootCall],
    -- |return the execution value
    modeRunConf :: Parser a
}
instance Functor Mode where fmap f (Mode n d g r) = Mode n d g $ f <$> r

-- | looks much nicer for a 'Mode' not needing priviledges than (pure [])
noRootCalls = pure [] :: Parser [RootCall]


-- |top-level function, intended to be directly called by the main.
-- allows to choose one mode of execution from the provided list.
-- 
-- the syntax for calling the programm is:
--
-- > adn-exe genconf mode_name [--option VALUE]
-- 
-- /generates the configurations scripts for the mode mode_name with/ "ADN.Core.RootAccess"
--
-- > adn-exe run mode_name [--option VALUE]
--
-- /run the mode mode_name with/ "ADN.CLI"
--
runADNModes :: [ADNMode] -> IO ()
runADNModes cmds = customExecParser pref opts >>= \a -> a
    where 
        pref = prefs $ mempty -- subparserInline
        opts = info (parseCmd <**> helper)
                    (fullDesc <> header "An IP tunnel over a ADN network"
                              <> progDesc "Allows to route IP traffic through a mesh ad-hoc network. This is still experimental")

        parseCmd = hsubparser (command "genconf"  (info parseGen  $ progDesc "generates the scripts which will perform privilleged actions") <>
                              (command "run"      (info parseRun $ progDesc "runs the ADN daemon")))


        parseGen = generateScript <$> parseScript <*> subs modeGenConf
        parseRun = runCLI <$> parseADNConf <*> parseCLI <*> subs modeRunConf 
        subs f = subModes $ zip cmds (map f cmds)

        parseADNConf = ADNConf defaultChanImplem <$> parseScript
        parseScript = ScriptLocation <$> opt "script-path" <*> opt "script-before" <*> opt "script-after" <*> opt "script-tun" <*> opt "script-models" <*> opt "script-runtime"
            where opt n = strOption (long n <> internal)
        parseCLI = CLIConf <$> parseHistfile <*> parseLogLevel
        parseHistfile = (Just <$> strOption (long "history" <> metavar "FILENAME"  <> value defaultCLIhistory <> showDefault <> help "file to save the CLI history"))
                    <|> flag' Nothing (long "no-history" <> help "don't save the CLI history")
        parseLogLevel = toEnum . read <$> strOption (long "log" <> metavar "LEVEL"  <> value (show $ fromEnum defaultCLILogLevel) <> showDefault <> help "minimum level to display logs")
        

        subModes :: [(Mode a, Parser r)] -> Parser r
        subModes cmds = hsubparser . mconcat $ mkCommand <$> cmds
            where mkCommand ((Mode name desc _ _),p) = command name (info p $ progDesc desc)



manyModes :: [Mode a] -> Mode s -> Mode (s,[a])
manyModes mds (Mode name desc gen run) = Mode name desc genRoots genRuns
    where
        genRoots = (++) <$> gen <*> fmap concat modesRoot
        modesRoot = manyParser modeGenConf 
        genRuns = (,) <$> run <*> runList
        runList = manyParser $ modeRunConf
        manyParser f = many $ foldr1 (<|>) $ map oneMode mds
            where 
            oneMode md = parseModeOpt md *> f md
            parseModeOpt (Mode name desc gen run) = flag' () (long name <> help desc)


parseModeParameter :: Mode a -> Parser s -> Mode (a,s)
parseModeParameter (Mode n d g r) p = Mode n d g $ (,) <$> r <*> p


