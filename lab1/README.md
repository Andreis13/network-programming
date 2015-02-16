# Laboratory work nr. 1

*A simple client-server application written in Haskell.*

### General Program Flow

### Server

- Create and bind a listening socket
- Init the so called 'MVar's for inter-thread communication
- Fork a thread for accepting connections (it is necessary for being able to shutdown the server, otherwise it would block in `accept` and wouldn't see the shutdown flag)
  - Wait for a connection
  - When a connection is accepted, store it in the MVar
  - Repeat
- Run server
  - Check if the shutdown flag MVar was set, and shutdown server and kill all other threads if true
  - Check if there is a new conection in the MVar
  - Fork a new thread for an incoming connection and store the thread ID
    - Process the connection
    - Split by spaces the incoming message
    - Find and execute the appropriate handler
    - Send back the result
    - Repeat
  - Repeat

### Client

- Create a socket
- Connect through it to the server
- Run a simple REPL on this socket
  - Read the command from keyboard
  - Send it to the server and receive a response (Eval)
  - Print the processed response (the `close` and `showtime` commands are processed a little different from the others)
  - Loop


### List of supported commands

- `time` - gets the time from server
- `hastalavista` - shuts down the server
- `showtime` - downloads a picture of Arnie
- `close` - closes the connection from the client
- `ping` - returns "pong"
- `reverse` - returns the inverted string that was given as an argument
- `add` - takes a list of space-separated integers and returns their sum
- `mul` - the same as `add`, but multiplies the numbers instead
- `tellmeastory` - waits for input on the server and returns the string that was typed (if there was anyone to type it of course)


### Comments and issues

The protocol described above is an ugly and dirty one. In order not to be
ashamed of the written code it is necessary to redesign it. The problem with the one above
is the fact that it is very likely to have a situation when the server and the client
are out of sync. If something happens on either end, the other one is left without
any explanation, as a result we have hanging `send` and `recv` calls.


The server is multi-threaded, which introduces some complications appart from giving the
possibility for multiple clients to connect. This can be illustrated on the example
of the `hastalavista` command, it surely shuts down the server, however the clients
are left hanging and the connections are still up (even though the processing
threads on the server are killed, similarly to what I've described before).
This leads to the situation when the resources are not freed and it is impossible
to start another server on the same port, not until we shutdown all the clients or try to
send a couple commands to the server to make the client realize that nobody is listening to them.


As a conclusion I can say that for such programs it is important to put some thoughts in
designing the way in which the client and the server will be communicating.
The programmer should be able to predict different kinds of scenarios that can happen, and should design
strategies for handling these scenarios.


