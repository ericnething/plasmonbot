import Network (connectTo, PortID(..))
import System.IO
import System.Exit (exitWith, ExitCode(..))
import Text.Printf (hPrintf, printf)
import Control.Monad (forever)
import Data.List (isPrefixOf)

server  = "chat.freenode.net"
port    = 6667
channel = "#plasmonbot-testing"
nick    = "plasmonbot"

main :: IO ()
main = do
  handle <- connectTo server (PortNumber $ fromIntegral port)
  hSetBuffering handle NoBuffering
  write handle "NICK" nick
  write handle "USER" $ nick ++ " 0 * :plasmon bot"
  write handle "JOIN" channel
  listen handle


-- | Write to the server
write :: Handle -> String -> String -> IO ()
write handle cmd input = do
  hPrintf handle "%s %s\r\n" cmd input
  printf "> %s %s\r\n" cmd input


-- | Listen to the server
listen :: Handle -> IO ()
listen handle = forever $

                -- capture input from the server
                hGetLine handle >>=

                -- drop the last item from the input
                (\t -> let s = init t in

                  -- route the data to the proper function
                  listenHelper handle s >>
                  
                  -- output the result to the console
                  putStrLn s)


-- | Route the incoming data to the proper utility
listenHelper :: Handle -> String -> IO ()
listenHelper handle input

  -- handle server PING events by responding with a PONG event
  | ping input = pong input

  -- evaluate all other events
  | otherwise = eval handle (clean input)
                
  where ping x = "PING :" `isPrefixOf` x
        pong x = write handle "PONG" (':' : drop 6 x)
        clean = drop 1 . dropWhile (/= ':') . drop 1


-- | Evaluate messages from other users
eval :: Handle -> String -> IO ()

-- allow the bot to quit on command !quit
eval h "!quit"           = write h "QUIT" ":Exiting" >> exitWith ExitSuccess

-- echo back any message prefixed with "!id "
eval h x
  | "!id" `isPrefixOf` x = privmsg h $ drop 4 x

-- ignore everything else
eval _ _                 = return ()


-- | Private message utility
privmsg :: Handle -> String -> IO ()
privmsg h s = write h "PRIVMSG" (channel ++ " :" ++ s)
