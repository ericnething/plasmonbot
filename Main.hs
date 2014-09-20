import Network     (connectTo, PortID(..))
import System.IO
import System.Exit (exitWith, ExitCode(..))

import Text.Printf (hPrintf, printf)

import Control.Exception (bracket)
import Control.Monad     (forever)
import Control.Monad.Reader

import Data.List (isPrefixOf)

server  = "chat.freenode.net"
port    = 6667
channel = "#plasmonbot-testing"
nick    = "plasmonbot"

-- We are using ReaderT to thread the socket handle throughout the
-- program to avoid explicity passing it around.
type Net = ReaderT Bot IO

-- A wrapper for our socket
data Bot = Bot { socket :: Handle }


main :: IO ()
main = bracket connect disconnect loop
  where loop = runReaderT run

-- Lift a computation from IO into Net
io :: IO a -> Net a
io = liftIO


-- | Connect to the server
connect :: IO Bot
connect = do
  handle <- connectTo server (PortNumber $ fromIntegral port)
  hSetBuffering handle NoBuffering
  return (Bot handle)


-- | Disconnect from the server
disconnect :: Bot -> IO ()
disconnect = hClose . socket


-- | Set the user options for the server and listen on the socket
run :: Net ()
run = do

  -- set our user options on the server and join a channel
  write "NICK" nick
  write "USER" $ nick ++ " 0 * :plasmon bot"
  write "JOIN" channel

  -- get the socket handle from Net and listen for activity
  asks socket >>= listen


-- | Write to the socket
write :: String -> String -> Net ()
write cmd input = do
  handle <- asks socket
  io $ hPrintf handle "%s %s\r\n" cmd input
  io $ printf "> %s %s\r\n" cmd input


-- | Listen to the socket
listen :: Handle -> Net ()
listen handle = forever $

                -- capture input from the server
                io (hGetLine handle) >>=

                -- drop the last item from the input
                (\t -> let s = init t in

                  -- route the data to the proper function
                  listenHelper handle s >>
                  
                  -- output the result to the console
                  io (putStrLn s))


-- | Route the incoming data to the proper utility
listenHelper :: Handle -> String -> Net ()
listenHelper handle input

  -- handle server PING events by responding with a PONG event
  | ping input = pong input

  -- evaluate all other events
  | otherwise = eval (clean input)
                
  where ping x = "PING :" `isPrefixOf` x
        pong x = write "PONG" (':' : drop 6 x)
        clean = drop 1 . dropWhile (/= ':') . drop 1


-- | Evaluate messages from other users
eval :: String -> Net ()

-- allow the bot to quit on command !quit
eval "!quit" = write "QUIT" ":Exiting" >> io (exitWith ExitSuccess)

-- echo back any message prefixed with "!id "
eval x | "!id" `isPrefixOf` x = privmsg $ drop 4 x

-- ignore everything else
eval _ = return ()


-- | Private message utility
privmsg :: String -> Net ()
privmsg s = write "PRIVMSG" (channel ++ " :" ++ s)
