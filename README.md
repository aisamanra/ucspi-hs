# ucspi-hs

[`UCSPI`](http://cr.yp.to/proto/ucspi.txt), or the Unix Client-Server
Program Interface, is a general interface designed for writing clients
and servers over some network interface. For a given protocol `xproto`,
there should exist `xprotoserver` and `xprotoclient` applications which
handle accepting and connecting to hosts, respectively, before executing
some Unix application. This is a (very small!) library to make writing
such applications simpler in Haskell.

One advantage to writing applications to the `UCSPI` interface is that
it becomes trivial to change the underlying network protocol being used
without modifying the application itself, by switching (for example)
from `tcpserver` to `sslserver`. Another is that very small, clean
network services can be written using these interfaces, because the
network-handling code is abstracted away.

## Sample Client

A client which sends a single line to the server, receives a line
back, prints it to stdout, and exits looks like

~~~~{.haskell}
import Network.UCSPI (ucspiClient)
import System.IO (hGetLine, hPutStrLn)

main :: IO ()
main = ucspiClient $ \ _ rdH wrH -> do
  hPutStrLn wrH "hello"
  ln <- hGetLine rdH
  putStrLn ln
~~~~

## Sample Server

A server which receives a line from the client and sends back the
same line before closing the connection looks like

~~~~{.haskell}
import Network.UCSPI (ucspiServer)

main :: IO ()
main = ucspiServer $ \ _ -> getLine >>= putstrLn
~~~~
