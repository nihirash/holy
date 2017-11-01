# HoLy

## What is it?

HoLy is simple a HTTP-server Library for Racket.

**WARNING** Project in alpha state. API is unstable. Sorry.

Pull requests are welcome!

## Installation

You can install it with using console utillity **raco**:

```
raco pkg install https://github.com/nihirash/holy.git
```

Or with using DrRacket IDE.

## Usage

```racket
#lang racket
(require HoLy)

(http/get "/"
    (λ (req) "Welcome"))

(http/get "/article/:id"
    (λ (req)
	(let* ((id (request/param 'id)))
	    (string-append "This is article #" id))))

(server/set-port 8080)

(server/run)
```

## API

### http/get, http/post, http/delete, http/put

Defines request handler for specified request type.

Parameters are url as string(with possibility usage of placeholders) and callback function.

Example:

```racket
(http/get "/article/:id"
    (λ (req)
	(let* ((id (request/param 'id)))
	    (string-append "This is article #" id))))
```

### request/param

Gets request parameter's value(from get/post or placeholder parameter).

Parameter is name of parameter.

### request/cookie

Gets cookie value.

Parameters are request and name of cookie.

### response/make

Creates response. It have required parameter content and several rest parameters. 

Default values are: 
```
#:code [code 200]
#:message [message #"OK"]
#:seconds [seconds (current-seconds)]
#:mime-type [mime-type TEXT/HTML-MIME-TYPE]
#:headers [headers (list (make-header #"Cache-Control" #"no-cache"))]
```

Can be used for creating custom response statuses/placing new headers etc.

### response/404

Response with 404 status. 


### response/add-header

Add's header to response.

```racket
(define (response/add-header header))
```

Header must be racket web-server's header.

### response/add-cookie

Add's cookie to response.

```racket
(define (response/add-cookie name value))
```

### server/set-port

Defines port that's be listened with http server

### server/run

Run's server event loop

## Credits

 * [z-song](https://github.com/z-song/raf/) is author Raf framework(HoLy is created by reading Raf sources).
 
 * [nihirash](https://github.com/nihirash/) is author and maintainer of HoLy library
