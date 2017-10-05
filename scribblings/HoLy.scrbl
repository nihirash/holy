#lang scribble/manual
@require[@for-label[HoLy
                    racket]]

@title{HoLy}
@author{nihirash}

@defmodule[HoLy]

HoLy is simple a HTTP-server Library for Racket.

Example of usage:

@racketblock[
 (require HoLy)
             
 (http/get "/"
           (λ (req) "Welcome"))

 (http/get "/article/:id"
           (λ (req)
             (let* ((id (request/param 'id)))
               (string-append "This is article #" id))))

 (server/set-port 8080)

 (server/run)
 ]

@table-of-contents[]

@section{http/get}

Defines request handler for GET-request

@racketblock[
 (define
   (http/get path proc))
 ]

Where path is a path to resource(placeholder's can be used, eg. "/" or "/posts/:category/:id").

@section{http/post}

Defines request hanlder for POST-request.

@racketblock[
 (define
   (http/post path proc))
 ]

Where path is a path to resource(placeholder's can be used, eg. "/" or "/posts/:category/:id").

@section{http/delete}

Defines request hanlder for DELETE-request.

@racketblock[
 (define
   (http/delete path proc))
 ]

Where path is a path to resource(placeholder's can be used, eg. "/" or "/posts/:category/:id").

@section{http/put}

Defines request hanlder for PUT-request.

@racketblock[
 (define
   (http/put path proc))
 ]

Where path is a path to resource(placeholder's can be used, eg. "/" or "/posts/:category/:id").

@section{request/param}

Gets request parameter. It might be GET, POST or placeholder parameter

@racketblock[
 (define (request/param name))
 ]
@section{request/cookie}

Gets request cookie value by name.

@racketblock[
 (define (request/cookie name))
 ]

@section{response/make}

Prepares response(for defining custom http response code or setting headers).

@racketblock[
 (define (response/make #:code [code 200]
                        #:message [message #"OK"]
                        #:seconds [seconds (current-seconds)]
                        #:mime-type [mime-type TEXT/HTML-MIME-TYPE]
                        #:headers [headers (list (make-header #"Cache-Control" #"no-cache"))]
                        content))
 ]

@section{response/404}

Responses with 404 status code

@section{server/set-port}

Defines port for server listening(by default it is 8000).

@racketblock[
 (define (server/set-port [port 8000]))
 ]

@section{server/run}

Runs server's event loop.