#lang racket
; HoLy is HTTP Library
; (c) 2017 Alexander Sharikhin
;
; Some source based on raf library

(require web-server/servlet
           web-server/servlet-env)
  
  (provide http/get
           http/post
           http/delete
           http/put
           server/set-port
           request/param
           request/cookie
           response/make
           response/404
           server/run)

  ; Request handlers
  (define handlers (make-hash))
  ; Listening port
  (define env/port 8000)
  ; HTTP-params hash(for GET/POST etc. vars)
  (define params (make-hash))
  ; Cookies hash
  (define cookies (make-hash))

  ; Creating empty hash-maps for request handlers
  (map
   (λ (element)
     (hash-set! handlers element (make-hash)))
   '(get post put delete))

  ; Remember request handler
  ; Params are - request method(get/post/put/delete), path("/", "/hello/:id/") and callback
  (define (http/handler method path proc)
    (hash-set! (hash-ref handlers method) path proc))

  ; GET request handler
  (define (http/get path proc)
    (http/handler 'get path proc))

  ; POST request handler
  (define (http/post path proc)
    (http/handler 'post path proc))

  ; PUT request handler
  (define (http/put path proc)
    (http/handler 'put path proc))

  ; DELETE request handler
  (define (http/delete path proc)
    (http/handler 'delete path proc))

  ; Http path template to regexp translation
  (define (path->regexp path)
    (regexp
     (string-append
      "^/"
      (string-trim
       (regexp-replace* #rx":[^\\/]+" path "([^/?]+)") "/")
      "(?:\\?|$)")))

  ; Set listen port
  (define (server/set-port [port 8000])
    (set! env/port port))

  ; Get request parameter
  (define (request/param name)
    (hash-ref params name #f))

  ; Parse cookies to hash-map
  (define (server/parse-cookies req)
    (map (λ (cookie)
           (hash-set! cookies
                      (client-cookie-name cookie)
                      (client-cookie-value cookie)))
         (request-cookies req)))

  ; Get cookie by name
  (define (request/cookie name)
    (hash-ref cookies name))

  ; Make response
  (define (response/make #:code [code 200]
                         #:message [message #"OK"]
                         #:seconds [seconds (current-seconds)]
                         #:mime-type [mime-type TEXT/HTML-MIME-TYPE]
                         #:headers [headers (list (make-header #"Cache-Control" #"no-cache"))]
                         content)
    (response/full code
                   message
                   seconds
                   mime-type
                   headers
                   (list (string->bytes/utf-8 content))))

  ; 404 Response
  (define (response/404)
    (response/make #:code 404 "Page not found"))

  ; Find handler 
  (define (server/find-handler req)
    (let* ((path (regexp-replace* #rx"\\?.*" (url->string (request-uri req)) ""))
           (method
            (case (request-method req)
              [(#"GET") 'get]
              [(#"POST") 'post]
              [(#"PUT") 'put]
              [(#"DELETE") 'delete]))
           (handler-key
            (findf (λ (key)  (regexp-match (path->regexp key) path))
                   (hash-keys (hash-ref handlers method)))))
      (list path handler-key method)))

  ; Makes response
  (define (server/response path-handler-method req)
    (let* ((path (car path-handler-method))
           (handler-key (cadr path-handler-method))
           (method (caddr path-handler-method)))
      (case handler-key
        [(#f) (response/404)]
        [else
         (let* ((keys (map
                       (λ (match) (string->symbol (substring match 2)))
                       (regexp-match* #rx"/:([^\\/]+)" handler-key)))
                (pairs
                 (for/list ([key keys]
                            [val (cdr (regexp-match (path->regexp handler-key) path))])
                   (cons key val))))
           (set! params (make-hash (append pairs (url-query (request-uri req)))))
           (server/parse-cookies req)
           (let* ((handler (hash-ref handlers method))
                  (response ((hash-ref handler handler-key #f) req)))
             (if (response? response)
                 response
                 (response/make ((hash-ref handler handler-key #f) req)))))])))
  ; Runs server
  (define (server/run)
    (serve/servlet
     (λ (req)
       (server/response
        (server/find-handler req)
        req))
     #:launch-browser? #f
     #:servlet-path "/"
     #:port env/port
     #:listen-ip #f
     #:servlet-regexp #rx""))
