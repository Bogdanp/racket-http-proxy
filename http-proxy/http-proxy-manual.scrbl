#lang scribble/manual

@(require (for-label net/http-proxy
                     openssl
                     racket/base
                     racket/contract))

@title{HTTP @tt{CONNECT} Proxy}
@author[(author+email "Bogdan Popa" "bogdan@defn.io")]
@defmodule[net/http-proxy]

This package provides a simple HTTP @tt{CONNECT} proxy implementation.

@defproc[(start-http-proxy [#:host host (or/c #f string?) "127.0.0.1"]
                           [#:port port (integer-in 0 65535) 1080]
                           [#:ssl-ctx ssl-ctx (or/c #f ssl-server-context?) #f]
                           [#:port-ch port-ch (or/c #f (channel/c (integer-in 1 65535))) #f]
                           [handler (-> string? (integer-in 1 65535)
                                        string? (integer-in 1 65535)
                                        (listof bytes?)
                                        bytes?)
                                    (lambda (src-host src-port dst-host dst-port headers) #"200 OK")]) (-> void?)]{

  Starts an HTTP proxy bound to the interface associated with
  @racket[host] and listening on @racket[port]. Returns a procedure that
  stops the server when called. The returned stop procedure blocks while
  there are active connections and breaking while the procedure blocks
  immediately stops the server and closes all open connections.

  When @racket[port-ch] is a channel, the server sends the port it's
  bound to over it once ready.

  The @racket[handler] procedure determines which clients are allowed to
  connect to which destination servers. A return value of @racket[#"200 OK"]
  from the handler signals to the server that the client is allowed to
  connect to the destination.
}
