#lang racket
; HoLy is HTTP Library
; (c) 2017 Alexander Sharikhin
;
; Some source based on raf library

(require web-server/servlet
           web-server/servlet-env)
  
(provide (all-defined-out))

; Request handlers
(define handlers (make-hash))
; Listening port
(define env/port 8000)
; HTTP-params hash(for GET/POST etc. vars)
(define params (make-hash))

; Creating empty hash-maps for request handlers
(map
 (λ (element)
   (hash-set! handlers element (make-hash)))
 '(get post put delete))

; Remember request handler
; Params are - request method(get/post/put/delete), path("/", "/hello/:id/") and callback
(define (http/handler method path proc)
  (hash-set! (hash-ref handlers method) path proc))

; Little shugar 
(define-syntax push!
  (syntax-rules ()
   [(push! to what)
    (set! to (append to (list what)))]))

(define-syntax http
  (syntax-rules ()
    [(http method url handler)
       ((λ()
           (define headers (list (make-header #"Cache-Control" #"no-cache")))
           
           (define (response/add-header header)
             (push! headers header))
       
           (define (response/add-cookie name value)
             (response/add-header
              (cookie->header (make-cookie name value))))
        
           (http/handler
            (quote method)
            url
            (λ (req)
              (let ([response (handler req)])
                (cond
                  [(response? response)
                   response]
                  [else
                   (response/make response #:headers headers)]))))))]))
  

; GET request handler
;(define (http/get path proc)
;  (http-handler 'get path proc))

; POST request handler
;(define (http/post path proc)
;  (http-handler 'post path proc))

; PUT request handler
;(define (http/put path proc)
;  (http-handler 'put path proc))

; DELETE request handler
;(define (http/delete path proc)
;  (http-handler 'delete path proc))

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
(define (request-get-param name)
  (hash-ref params name #f))


(define  (find-cookie request cookie-name)
  (findf
    (λ (cookie)
      (string=? cookie-name (client-cookie-name cookie)))
    (request-cookies request)))

; Get cookie value from request
(define (request/cookie request cookie-name)
  (let ([cookie (find-cookie request cookie-name)])
    (cond
      [cookie (client-cookie-value cookie)]
      [else #f])))

; Get parameter from request
(define (request/param request form-field)
  (let ([param (request-get-param form-field)])
   (cond
     [param param]
     [else
      (let ([form-val (bindings-assq
                            (string->bytes/utf-8 form-field)
                            (request-bindings/raw request))])
        (cond
         [form-val (bytes->string/utf-8
                    (binding:form-value form-val))]
         [else #f]))])))
 

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
                     (λ (match) (substring match 2))
                     (regexp-match* #rx"/:([^\\/]+)" handler-key)))
              (pairs
               (for/list ([key keys]
                          [val (cdr (regexp-match (path->regexp handler-key) path))])
                 (cons key val))))
         (set! params (make-hash (append pairs (url-query (request-uri req)))))
         (let* ((handler (hash-ref handlers method))
                (response ((hash-ref handler handler-key #f) req)))
           response))])))

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
