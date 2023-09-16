#lang racket/base

(require net/http-easy
         net/http-proxy
         rackunit)

(define (call-with-proxy #:handler [handler (λ _ #"200 OK")] proc)
  (define port-ch (make-channel))
  (define stop
    (start-http-proxy
     handler
     #:port 0
     #:port-ch port-ch))
  (dynamic-wind
    void
    (lambda ()
      (define addr
        (format "127.0.0.1:~a" (channel-get port-ch)))
      (define session
        (make-session
         #:proxies (list
                    (make-http-proxy addr)
                    (make-https-proxy addr))))
      (parameterize ([current-session session])
        (proc addr)))
    (lambda ()
      (stop))))

(define suite
  (test-suite
   "http-proxy"

   (test-case "basics"
     (call-with-proxy
      (lambda (_addr)
        (define resp
          (get #:close? #t "http://example.com"))
        (check-equal? (response-status-code resp) 200))))

   (test-case "host filter"
     (call-with-proxy
      #:handler (lambda (_src-host _src-port dst-host dst-port _headers)
                  (if (and
                       (equal? dst-host "example.com")
                       (eqv? dst-port 80))
                      #"200 OK"
                      #"403 Forbidden"))
      (lambda (_addr)
        (check-equal?
         (response-status-code
          (get #:close? #t "http://example.com"))
         200)
        (check-exn
         #rx"HTTP CONNECT failed: HTTP/1.1 403 Forbidden"
         (lambda ()
           (get #:close? #t "https://defn.io"))))))))

(module+ test
  (require rackunit/text-ui)
  (run-tests suite))
