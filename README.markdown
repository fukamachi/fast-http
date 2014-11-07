# fast-http

This is a fast HTTP request/response protocol parser for Common Lisp.

## How fast?

![Benchmark graph when parsing an HTTP request header 100000 times.](images/benchmark.png)

- [http-parser](https://github.com/joyent/http-parser): An HTTP Parser for [Node.js](http://github.com/joyent/node)

NOTE: @y2q-actionman pointed out Node.js's [http-parser](https://github.com/joyent/http-parser) is designed to use less memory. fast-http consumes more memory spaces than http-parser. (See [#8](https://github.com/fukamachi/fast-http/issues/8), the information is based on the old implementation, though)

See [Benchmark](#benchmark) for the detail.

## Features

* Parses both HTTP requests and responses.
* Handles persistent streams (keep-alive).
* Decodes chunked encoding.
* Exports low-level APIs which don't any memory allocations during parsing.

## API differences from http-parse

The API is quite similar to [http-parse](https://github.com/orthecreedence/http-parse), although there's some differences.

* `http`, `http-request` and `http-response` are structure classes, not standard classes.
* `http` doesn't have `:force-stream` option. (always streaming)
* `http` doesn't have `:store-body` option because it can consume much memory.
* `body-callback` for `make-parser` and `make-multipart-parser` doesn't take a flag `body-complete-p`.
  * Use `finish-callback` to know if the parsing is finished.
* `multipart-callback` for `make-parser` has been deleted.
  * Use `make-multipart-parser` and `body-callback` by yourself.
* `:callback` of `make-multipart-parser` takes a stream, not a body octet vector at the 4th argument.
* Raises errors aggressively while parsing.
  * Handle `fast-http-error` as you needed.
* Doesn't use a property list as a representation of HTTP headers. (See [issue #1](https://github.com/fukamachi/fast-http/issues/1))

## APIs

### \[Structure] http

Base structure class extended by `http-request` and `http-response`.

**NOTE**: Don't use this class directly unless you're intended to use low-level APIs of fast-http.

```common-lisp
(make-http)
;=> #S(FAST-HTTP.HTTP:HTTP
      :METHOD NIL
      :MAJOR-VERSION 0
      :MINOR-VERSION 9
      :STATUS 0
      :CONTENT-LENGTH NIL
      :CHUNKED-P NIL
      :UPGRADE-P NIL
      :HEADERS NIL
      :HEADER-READ 0
      :MARK -1
      :STATE 0)
```

#### Methods

- `http-method`: Returns a HTTP request method in a keyword (such like `:GET`, `:POST` or `:HEAD`).
- `http-major-version`: Returns a HTTP protocol major version in an integer (such like `1` or `0`).
- `http-minor-version`: Returns a HTTP protocol minor version in an integer (such like `1` or `0`).
- `http-version`: Returns a HTTP protocol version in a float (such like `1.0` or `1.1`).
- `http-status`: Returns a HTTP response status code in an integer (such like `200` or `302`).
- `http-content-length`: Returns a value of `Content-Length` header in an integer. If the header doesn't exist, it returns `NIL`.
- `http-chunked-p`: Returns `T` if the value of `Transfer-Encoding` header is `chunked`. If the header doesn't exist, it returns `NIL`.
- `http-upgrade-p`: Returns `T` if `Upgrade` header exists.
- `http-headers`: Returns a hash-table which represents HTTP headers. Note all hash keys are lower-case. (`Content-Length` -> `"content-length"`).

### \[Structure] http-request (extends http)

Structure class holds values specific to an HTTP request.

```common-lisp
(make-http-request)
;=> #S(FAST-HTTP.HTTP:HTTP-REQUEST
;     :METHOD NIL
;     :MAJOR-VERSION 0
;     :MINOR-VERSION 9
;     :STATUS 0
;     :CONTENT-LENGTH NIL
;     :CHUNKED-P NIL
;     :UPGRADE-P NIL
;     :HEADERS NIL
;     :HEADER-READ 0
;     :MARK -1
;     :STATE 0
;     :RESOURCE NIL)
```

#### Methods

- `http-resource`: Returns an URI string.

### \[Structure] http-response (extends http)

Structure class holds values specific to an HTTP response.

```common-lisp
(make-http-response)
;=> #S(FAST-HTTP.HTTP:HTTP-RESPONSE
;     :METHOD NIL
;     :MAJOR-VERSION 0
;     :MINOR-VERSION 9
;     :STATUS 0
;     :CONTENT-LENGTH NIL
;     :CHUNKED-P NIL
;     :UPGRADE-P NIL
;     :HEADERS NIL
;     :HEADER-READ 0
;     :MARK -1
;     :STATE 0
;     :STATUS-TEXT NIL)
```

#### Methods

- `http-status-text`: Returns an response status text (such like `Continue`, `OK` or `Bad Request`).

### \[Function] make-parser (http &key first-line-callback header-callback body-callback finish-callback)

Makes a parser closure and returns it.

```common-lisp
(let ((http (make-http-request)))
  (make-parser http
               :body-callback
               (lambda (data)
                 (write-to-buffer data))
               :finish-callback
               (lambda ()
                 (handle-response http))))
;=> #<CLOSURE (LAMBDA (DATA &KEY (START 0) END)
;              :IN
;              FAST-HTTP.PARSER:MAKE-PARSER) {10090BDD0B}>
```

The closure takes one required argument `data`, that is a simple byte vector and two keyword arguments `start` and `end`.

#### Callbacks

- `first-line-callback` (): This callback function will be called when the first line is parsed.
- `header-callback` (headers-hash-table): This callback function will be called when the header lines are parsed. This function is the same object to the `http` object holds.
- `body-callback` (data-byte-vector): This callback function will be called whenever it gets a chunk of HTTP body. Which means this can be called multiple times.
- `finish-callback` (): This callback function will be called when the HTTP message ends.

NOTE: If the HTTP request/response has multiple messages (like HTTP/1.1 pipelining), all these functions can be called multiple times.

### \[Function] make-multipart-parser (content-type callback)

Makes a multipart/form-data parser closure and returns it.

This takes 2 arguments, `content-type` (such like `"multipart/form-data; boundary=--AsB03x"`) and `callback`. The `callback` is a function which takes exact 4 arguments -- a field name, field headers, field meta data and body bytes.

## Low-level APIs

The following functions are intended to be used for internally. These APIs are likely to change in the future.

Most of functions are declared as `(optimize (speed 3) (safety 0))` which means it won't check the type of arguments.

### \[Structure] callbacks

Structure class holds callback functions. The callbacks are similar to `make-parser`'s, but don't correspond to them directly.

#### Slots

- `message-begin` (http): This will be called when a new HTTP message begins.
- `url` (http data start end): This will be called when an URL part of the HTTP request parsed.
- `first-line` (http): This will be called when the first line of the HTTP request/response parsed.
- `status` (http data start end): This will be called when the status text (not code) of the HTTP response parsed.
- `header-field` (http data start end): This will be called when a header field parsed.
- `header-value` (http data start end): This will be called when a header value parsed. This function can be called multiple times when the header value is folded onto multiple lines.
- `headers-complete` (http): This will be called when all headers parsed.
- `body` (http data start end): This will be called whenever the parser gets a chunk of HTTP body.
- `message-complete` (http): This will be called when the HTTP message ends.

### \[Function] parse-request (http callbacks data &key (start 0) end)

Parses `data` as an HTTP request, sets values to `http` and invokes callbacks in `callbacks`.

This takes a `http` object, a `callbacks` object, and a simple byte vector `data` and two pointers -- `start` and `end`. If `end` is `nil`, the length of `data` will be used.

### \[Function] parse-response (http callbacks data &key (start 0) end)

Parses `data` as an HTTP response, sets values to `http` and invokes callbacks in `callbacks`.

Takes a `http` object, a `callbacks` object, and a simple byte vector `data` and two pointers -- `start` and `end`. If `end` is `nil`, the length of `data` will be used.

### \[Condition] eof

Will be raised when the `data` ends in the middle of parsing.

## Installation

```common-lisp
(ql:quickload :fast-http)
```

## Running tests

```common-lisp
(asdf:test-system :fast-http)
```

## Benchmark

- Parsing an HTTP request header 100000 times.

In this benchmark, fast-http is **5.6 times faster** than [http-parser](https://github.com/joyent/http-parser), a C equivalent.

| http-parser (C) | fast-http |
| ---------------:| ---------:|
|      0.284s     |   0.050s  |

### Environment

* MacBook Pro OSX Mavericks (CPU: 3GHz Intel Core i7, Memory: 8GB)
* SBCL 1.2.5
* GCC version 6.0 (clang-600.0.51)

### fast-http (Common Lisp)

```common-lisp
(syntax:use-syntax :interpol)

(defun run-benchmark ()
  (let ((http (make-http-request))
        (callbacks (make-callbacks))
        (data (babel:string-to-octets #?"GET /cookies HTTP/1.1\r\nHost: 127.0.0.1:8090\r\nConnection: keep-alive\r\nCache-Control: max-age=0\r\nAccept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8\r\nUser-Agent: Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.17 (KHTML, like Gecko) Chrome/24.0.1312.56 Safari/537.17\r\nAccept-Encoding: gzip,deflate,sdch\r\nAccept-Language: en-US,en;q=0.8\r\nAccept-Charset: ISO-8859-1,utf-8;q=0.7,*;q=0.3\r\nCookie: name=wookie\r\n\r\n")))
    (time
     (loop repeat 100000 do
       (parse-request http callbacks data)))))

(run-benchmark)
```

```
Evaluation took:
  0.050 seconds of real time
  0.050187 seconds of total run time (0.050089 user, 0.000098 system)
  100.00% CPU
  151,546,149 processor cycles
  0 bytes consed
```

### http-parser (C)


```c
#include "http_parser.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <time.h>

static http_parser *parser;

static http_parser_settings settings_null =
  {.on_message_begin = 0
  ,.on_header_field = 0
  ,.on_header_value = 0
  ,.on_url = 0
  ,.on_status = 0
  ,.on_body = 0
  ,.on_headers_complete = 0
  ,.on_message_complete = 0
  };

int
main (void)
{
  const char *buf;
  int i;
  float start, end;
  size_t parsed;

  parser = malloc(sizeof(http_parser));

  buf = "GET /cookies HTTP/1.1\r\nHost: 127.0.0.1:8090\r\nConnection: keep-alive\r\nCache-Control: max-age=0\r\nAccept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8\r\nUser-Agent: Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.17 (KHTML, like Gecko) Chrome/24.0.1312.56 Safari/537.17\r\nAccept-Encoding: gzip,deflate,sdch\r\nAccept-Language: en-US,en;q=0.8\r\nAccept-Charset: ISO-8859-1,utf-8;q=0.7,*;q=0.3\r\nCookie: name=wookie\r\n\r\n";

  start = (float)clock()/CLOCKS_PER_SEC;
  for (i = 0; i < 100000; i++) {
    http_parser_init(parser, HTTP_REQUEST);
    parsed = http_parser_execute(parser, &settings_null, buf, strlen(buf));
  }
  end = (float)clock()/CLOCKS_PER_SEC;

  free(parser);
  parser = NULL;

  printf("Elapsed %f seconds.\n", (end - start));

  return 0;
}
```

```
$ make
$ gcc -Wall -Wextra -Werror -O3 http_parser.o bench.c -o bench
$ bench
Elapsed 0.284098 seconds.
```

## Author

* Eitaro Fukamachi (e.arrows@gmail.com)

## Copyright

Copyright (c) 2014 Eitaro Fukamachi

## License

Licensed under the MIT License.
