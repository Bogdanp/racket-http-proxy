#lang racket/base

(require openssl
         racket/contract/base
         racket/match
         racket/tcp)

(provide
 (contract-out
  [start-http-proxy
   (->* []
        [#:host (or/c #f string?)
         #:port (integer-in 0 65535)
         #:ssl-ctx (or/c #f ssl-server-context?)
         #:port-ch (channel/c port/c)
         (-> string? port/c
             string? port/c
             (listof bytes?)
             bytes?)]
        (-> void?))]))

(define port/c
  (integer-in 1 65535))

(define-logger http-proxy)

(define (start-http-proxy #:host [host "127.0.0.1"]
                          #:port [port 1080]
                          #:ssl-ctx [ssl-ctx #f]
                          #:port-ch [port-ch #f]
                          [handler (λ (_src-host _src-port _dst-host _dst-port _headers) #"200 OK")])
  (define cust
    (make-custodian))
  (define stop-sema
    (make-semaphore))
  (define listen-thd
    (parameterize-break #f
      (parameterize ([current-custodian cust])
        (define-values (listener accept close addresses)
          (if ssl-ctx
              (values (ssl-listen port 512 #t host ssl-ctx) ssl-accept ssl-close ssl-addresses)
              (values (tcp-listen port 512 #t host) tcp-accept tcp-close tcp-addresses)))
        (define-values (local-host local-port _remote-host _remote-port)
          (addresses listener #t))
        (log-http-proxy-debug "proxy listening on ~a:~a" local-host local-port)
        (thread
         (lambda ()
           (define buf
             (make-bytes (* 16 1024)))
           (define (read-http-line in)
             (define len (read-http-line! buf in))
             (and len (subbytes buf 0 len)))
           (define (read-http-line* in)
             (define line (read-http-line in))
             (and line (bytes->string/utf-8 line #\uFFD)))
           (when port-ch
             (channel-put port-ch local-port))
           (let loop ([accepting? #t]
                      [in-progress null])
             (when (or accepting? (not (null? in-progress)))
               (apply
                sync
                (handle-evt
                 stop-sema
                 (lambda (_)
                   (cond
                     [accepting?
                      (log-http-proxy-debug "stop received; draining ~s connections" (length in-progress))
                      (loop #f in-progress)]
                     [else
                      (log-http-proxy-debug "stop received; stopping immediately")])))
                (handle-evt
                 (if accepting? listener never-evt)
                 (lambda (l)
                   (define handler-cust (make-custodian))
                   (define handler-thd
                     (parameterize ([current-custodian handler-cust])
                       (define-values (src-in src-out)
                         (accept l))
                       (define-values (_local-host _local-port src-host src-port)
                         (addresses src-in #t))
                       (log-http-proxy-debug "accepted connection from ~a:~a" src-host src-port)
                       (thread
                        (lambda ()
                          (match (read-http-line* src-in)
                            [(regexp #rx"CONNECT ([^:]+):([^ ]+) HTTP/1.1"
                                     (list _ dst-host (app string->number dst-port)))
                             #:when (and dst-port (> dst-port 0))
                             (log-http-proxy-debug
                              "CONNECT ~a:~a -> ~a:~a"
                              src-host src-port dst-host dst-port)
                             (define headers
                               (let header-loop ([headers null])
                                 (define line (read-http-line src-in))
                                 (if (or (not line)
                                         (bytes=? line #""))
                                     (reverse headers)
                                     (header-loop (cons line headers)))))
                             (define status-message
                               (handler src-host src-port dst-host dst-port headers))
                             (log-http-proxy-debug
                              "CONNECT ~a:~a -> ~a:~a ~a"
                              src-host src-port dst-host dst-port status-message)
                             (cond
                               [(bytes=? status-message #"200 OK")
                                (with-handlers ([exn:fail?
                                                 (lambda (e)
                                                   ((error-display-handler) (exn-message e) e)
                                                   (fprintf* src-out "HTTP/1.1 502 Bad Gateway\r\n\r\n"))])
                                  (define-values (dst-in dst-out)
                                    (tcp-connect dst-host dst-port))
                                  (fprintf* src-out "HTTP/1.1 ~a\r\n\r\n" status-message)
                                  (define src-thd (thread (λ () (pipe src-in dst-out))))
                                  (define dst-thd (thread (λ () (pipe dst-in src-out))))
                                  (sync src-thd)
                                  (sync dst-thd))]
                               [else
                                (fprintf* src-out "HTTP/1.1 ~a\r\n\r\n" status-message)])]
                            [_
                             (log-http-proxy-debug "bad request from ~a:~a" src-host src-host)
                             (fprintf* src-out "HTTP/1.1 400 Bad Request\r\n")])))))
                   (thread
                    (lambda ()
                      (thread-wait handler-thd)
                      (custodian-shutdown-all handler-cust)))
                   (loop accepting? (cons handler-thd in-progress))))
                (for/list ([thd (in-list in-progress)])
                  (handle-evt thd (λ (t) (loop accepting? (remq t in-progress))))))))
           (close listener)
           (log-http-proxy-debug "proxy stopped"))))))
  (lambda ()
    (parameterize-break #f
      (let loop ()
        (semaphore-post stop-sema)
        (with-handlers* ([exn:break? (λ (_) (loop))])
          (parameterize-break #t
            (thread-wait listen-thd))))
      (custodian-shutdown-all cust))))

(define (fprintf* out fmt . args)
  (apply fprintf out fmt args)
  (flush-output out))

(define (read-http-line! bs in)
  (let loop ([len 0])
    (define n-peeked
      (peek-bytes-avail! bs len #f in len))
    (cond
      [(eof-object? n-peeked) #f]
      [(zero? n-peeked) #f]
      [else
       (define next-len
         (+ len n-peeked))
       (and (<= next-len (bytes-length bs))
            (match (regexp-match-positions #rx"\r\n" bs 0 next-len)
              [#f (loop next-len)]
              [`((,crlf-pos . ,end-pos))
               (begin0 crlf-pos
                 (read-bytes! bs in 0 end-pos))]))])))

(define (pipe in out)
  (define buf
    (make-bytes (* 16 1024)))
  (let loop ()
    (sync
     (handle-evt (port-closed-evt out) void)
     (handle-evt
      in
      (lambda (inp)
        (define n-read (read-bytes-avail! buf inp))
        (unless (eof-object? n-read)
          (write-bytes buf out 0 n-read)
          (flush-output out)
          (loop))))))
  (close-input-port in)
  (close-output-port out))
