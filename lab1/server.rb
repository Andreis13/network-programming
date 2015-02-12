require 'socket'

server = Socket.new :INET, :STREAM
server.bind Addrinfo.tcp('127.0.0.1', 4321)
server.listen(5)

loop do
  client, client_addr = server.accept
  while data = client.gets
    puts data
  end
  client.close
end

