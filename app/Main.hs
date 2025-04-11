module Main (main) where

import Control.Monad (when)
import Data.Char (toLower)
import Data.Time (UTCTime)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Options.Applicative
import System.Environment (lookupEnv)
import System.Exit (die)

import RiskScore (riskScore)
import RiskStatus (RiskStatus(..), parseRiskStatus, renderRiskStatus)
import RiskTypes (RiskLevel, parseRiskLevel, renderRiskLevel)

data Command
  = Add AddOptions
  | List ListOptions
  | Summary SummaryOptions
  | Update UpdateOptions
  | Attention ListOptions
  | Queue QueueOptions

data ListOptions = ListOptions
  { listLimit :: Int
  , listStatus :: Maybe String
  }

newtype SummaryOptions = SummaryOptions
  { summaryDays :: Int
  }

data UpdateOptions = UpdateOptions
  { updateEntryId :: Int
  , updateStatus :: Maybe String
  , updateOwner :: Maybe String
  }

data QueueOptions = QueueOptions
  { queueStatus :: String
  , queueLimit :: Int
  }

data AddOptions = AddOptions
  { optScholarId :: String
  , optScholarName :: String
  , optRiskLevel :: String
  , optCategory :: String
  , optNote :: Maybe String
  , optReportedBy :: String
  }


main :: IO ()
main = do
  command <- execParser opts
  conn <- connectFromEnv
  case command of
    Add options -> runAdd conn options
    List options -> runList conn options
    Summary options -> runSummary conn options
    Update options -> runUpdate conn options
    Attention options -> runAttention conn options

opts :: ParserInfo Command
opts = info (commandParser <**> helper)
  ( fullDesc
 <> progDesc "Log enrollment risk signals and summarize trends."
  )

commandParser :: Parser Command
commandParser = hsubparser
  ( command "add" (info (Add <$> addParser) (progDesc "Add a risk entry"))
 <> command "list" (info (List <$> listParser) (progDesc "List recent entries"))
 <> command "summary" (info (Summary <$> summaryParser) (progDesc "Summarize risks"))
 <> command "update" (info (Update <$> updateParser) (progDesc "Update status/owner"))
 <> command "attention" (info (Attention <$> listParser) (progDesc "List unresolved high/critical risks"))
  )

addParser :: Parser AddOptions
addParser = AddOptions
  <$> strOption (long "scholar-id" <> metavar "ID" <> help "Scholar identifier")
  <*> strOption (long "scholar-name" <> metavar "NAME" <> help "Scholar name")
  <*> strOption (long "risk-level" <> metavar "LEVEL" <> help "low | medium | high | critical")
  <*> strOption (long "category" <> metavar "CATEGORY" <> help "Risk category (financial, academic, etc.)")
  <*> optional (strOption (long "note" <> metavar "NOTE" <> help "Optional notes"))
  <*> strOption (long "reported-by" <> metavar "REPORTER" <> help "Reporter email or name")

listParser :: Parser ListOptions
listParser = ListOptions
  <$> option auto (long "limit" <> metavar "N" <> value 20 <> help "Number of entries")

summaryParser :: Parser SummaryOptions
summaryParser = SummaryOptions
  <$> option auto (long "days" <> metavar "DAYS" <> value 30 <> help "Lookback window in days")

updateParser :: Parser UpdateOptions
updateParser = UpdateOptions
  <$> option auto (long "entry-id" <> metavar "ID" <> help "Entry id to update")
  <*> optional (strOption (long "status" <> metavar "STATUS" <> help "open | acknowledged | resolved"))
  <*> optional (strOption (long "owner" <> metavar "OWNER" <> help "Assigned owner"))


connectFromEnv :: IO Connection
connectFromEnv = do
  host <- requireEnv "PGHOST"
  portStr <- requireEnv "PGPORT"
  user <- requireEnv "PGUSER"
  password <- requireEnv "PGPASSWORD"
  database <- requireEnv "PGDATABASE"
  let port = read portStr
  connect defaultConnectInfo
    { connectHost = host
    , connectPort = port
    , connectUser = user
    , connectPassword = password
    , connectDatabase = database
    }

requireEnv :: String -> IO String
requireEnv key = do
  value <- lookupEnv key
  case value of
    Just v | not (null v) -> pure v
    _ -> die ("Missing required environment variable: " <> key)


runAdd :: Connection -> AddOptions -> IO ()
runAdd conn options = do
  level <- parseLevel (optRiskLevel options)
  let score = riskScore level
      levelText = renderRiskLevel level
  _ <- execute conn
    "INSERT INTO groupscholar_enrollment_risk_log.risk_entries\
    \ (scholar_id, scholar_name, risk_level, category, note, reported_by, risk_score, status, owner)\
    \ VALUES (?,?,?,?,?,?,?,?,?)"
    ( optScholarId options
    , optScholarName options
    , levelText
    , optCategory options
    , optNote options
    , optReportedBy options
    , score
    , renderRiskStatus Open
    , Nothing :: Maybe String
    )
  putStrLn "Risk entry saved."

parseLevel :: String -> IO RiskLevel
parseLevel raw =
  case parseRiskLevel raw of
    Right level -> pure level
    Left err -> die err

parseStatus :: String -> IO RiskStatus
parseStatus raw =
  case parseRiskStatus raw of
    Right status -> pure status
    Left err -> die err

data EntryRow = EntryRow
  { entryScholarId :: String
  , entryScholarName :: String
  , entryRiskLevel :: String
  , entryCategory :: String
  , entryReporter :: String
  , entryReportedAt :: UTCTime
  , entryRiskScore :: Int
  , entryStatus :: String
  , entryOwner :: Maybe String
  }

instance FromRow EntryRow where
  fromRow =
    EntryRow <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

runList :: Connection -> ListOptions -> IO ()
runList conn options = do
  rows <- query conn
    "SELECT scholar_id, scholar_name, risk_level, category, reported_by, reported_at, risk_score, status, owner\
    \ FROM groupscholar_enrollment_risk_log.risk_entries\
    \ ORDER BY reported_at DESC\
    \ LIMIT ?"
    (Only (listLimit options))
  when (null rows) $ putStrLn "No entries found."
  mapM_ renderEntry rows

renderEntry :: EntryRow -> IO ()
renderEntry row = do
  putStrLn $ entryScholarId row
    <> " | " <> entryScholarName row
    <> " | " <> entryRiskLevel row
    <> " | " <> entryCategory row
    <> " | score " <> show (entryRiskScore row)
    <> " | " <> entryReporter row
    <> " | " <> entryStatus row
    <> " | owner " <> maybe "unassigned" id (entryOwner row)
    <> " | " <> show (entryReportedAt row)

runSummary :: Connection -> SummaryOptions -> IO ()
runSummary conn options = do
  let days = summaryDays options
  levelCounts <- query conn
    "SELECT risk_level, count(*)\
    \ FROM groupscholar_enrollment_risk_log.risk_entries\
    \ WHERE reported_at >= now() - ($1 || ' days')::interval\
    \ GROUP BY risk_level\
    \ ORDER BY count(*) DESC"
    (Only days)
  categoryCounts <- query conn
    "SELECT category, count(*)\
    \ FROM groupscholar_enrollment_risk_log.risk_entries\
    \ WHERE reported_at >= now() - ($1 || ' days')::interval\
    \ GROUP BY category\
    \ ORDER BY count(*) DESC"
    (Only days)
  statusCounts <- query conn
    "SELECT status, count(*)\
    \ FROM groupscholar_enrollment_risk_log.risk_entries\
    \ WHERE reported_at >= now() - ($1 || ' days')::interval\
    \ GROUP BY status\
    \ ORDER BY count(*) DESC"
    (Only days)
  putStrLn ("Summary for last " <> show days <> " days")
  putStrLn "By risk level:"
  mapM_ renderCount levelCounts
  putStrLn "By category:"
  mapM_ renderCount categoryCounts
  putStrLn "By status:"
  mapM_ renderCount statusCounts

renderCount :: (String, Int) -> IO ()
renderCount (label, count) =
  putStrLn ("- " <> map toLower label <> ": " <> show count)

runUpdate :: Connection -> UpdateOptions -> IO ()
runUpdate conn options = do
  let entryId = updateEntryId options
  existing <- query conn
    "SELECT id FROM groupscholar_enrollment_risk_log.risk_entries WHERE id = ?"
    (Only entryId)
  when (null (existing :: [Only Int])) $ die "No entry found for that id."
  case updateStatus options of
    Nothing -> pure ()
    Just rawStatus -> do
      status <- parseStatus rawStatus
      _ <- execute conn
        "UPDATE groupscholar_enrollment_risk_log.risk_entries\
        \ SET status = ?, status_updated_at = now(),\
        \ resolved_at = CASE WHEN ? = 'resolved' THEN now() ELSE NULL END\
        \ WHERE id = ?"
        (renderRiskStatus status, renderRiskStatus status, entryId)
      pure ()
  case updateOwner options of
    Nothing -> pure ()
    Just owner -> do
      _ <- execute conn
        "UPDATE groupscholar_enrollment_risk_log.risk_entries\
        \ SET owner = ?, status_updated_at = now()\
        \ WHERE id = ?"
        (owner, entryId)
      pure ()
  when (updateStatus options == Nothing && updateOwner options == Nothing) $
    die "Provide at least one of --status or --owner."
  putStrLn "Entry updated."

runAttention :: Connection -> ListOptions -> IO ()
runAttention conn options = do
  rows <- query conn
    "SELECT scholar_id, scholar_name, risk_level, category, reported_by, reported_at, risk_score, status, owner\
    \ FROM groupscholar_enrollment_risk_log.risk_entries\
    \ WHERE status <> 'resolved'\
    \ AND risk_level IN ('high','critical')\
    \ ORDER BY reported_at DESC\
    \ LIMIT ?"
    (Only (listLimit options))
  when (null rows) $ putStrLn "No urgent entries found."
  mapM_ renderEntry rows
