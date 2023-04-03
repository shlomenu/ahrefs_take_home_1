# Ahrefs Take-Home Test

## Description 

Simple one on one chat.


Application should start in two modes:

- as a server, waiting for one client to connect or;
- as a client, taking an IP address (or hostname) of server to connect to.

After connection is established, user on either side (server and client) can send messages to the other side. After connection is terminated by the client, server continues waiting for another client. The receiving side should acknowledge every incoming message (automatically send back a "message received" indication), sending side should show the roundtrip time for acknowledgment. Wire protocol shouldn't make any assumptions on the message contents (e.g. allowed byte values, character encoding, etc).
 

UI is your choice - can be just a console.
 

Requirements:

- Application is to be compiled and run on Linux
- Implementation language: OCaml
- You may use any 3rd-party general-purpose libraries (extlib, containers, lwt, etc)
- Primary objectives: robustness, code simplicity and maintainability

## Implementation

This repo provides a command line utility for a two-way chat.  On a UNIX system with opam installed within an OCaml 5.0.0 switch, the following command should succeed: 

```
$ dune exec ahrefs_take_home_1 -- -help 
```

This should display the full set of options.  The chat application utilizes standard input/output for message input and emits received messages, message acknowledgements and other logs on standard error.  It is therefore recommended, when testing, to redirect standard error to a file (it is easy to see the contents of this file as they appear by opening the file in VSCode, for example).  A full fledged command to launch a chat server might then look like this:

```
$ dune exec ahrefs_take_home_1 -- -s --decode-text 2> server.log
```

A chat client can then be launched by running this command from a separate process:

```
$ dune exec ahrefs_take_home_1 -- -c -a YOUR_IPV4_ADDR_HERE --decode-text 2> client.log
```

If you don't know your computer's current IP addresses, the server will emit logs containing all of the host's non-loopback network interfaces and their current IP addresses as soon as it begins listening for connection requests.