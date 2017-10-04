# HoLy

## What is it?

HoLy is simple a HTTP-server Library for Racket.

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

## Credits

 * [z-song](https://github.com/z-song/raf/) is author Raf framework(HoLy is created by reading Raf sources).
 
 * [nihirash](https://github.com/nihirash/) is author and maintainer of HoLy library
