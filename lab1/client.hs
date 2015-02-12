import Network.Socket

main = do
  sock <- socket AF_INET Stream defaultProtocol
  address <- inet_addr "127.0.0.1"
  connect sock $ SockAddrInet 4321 address
  repl sock
  sClose sock


repl sock = do
  s <- getLine
  send sock s
  response <- recv sock 256
  putStrLn response
  repl sock
