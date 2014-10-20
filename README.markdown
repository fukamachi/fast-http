# fast-http

This is a fast HTTP request/response protocol parser for Common Lisp.

## Usage

The API is quite similar to [http-parse](https://github.com/orthecreedence/http-parse).

```common-lisp
(let* ((http (make-http-request))
       (parser (make-parser http
                            :header-callback (lambda (headers)
                                               (my-app:got-headers!!! headers))
                            :body-callback (lambda (bytes)
                                             (my-app:got-body-piece bytes)))))
  (loop for http-data = (my-app:get-http-data-from-request-i-sent-out-earlier) do
    (multiple-value-bind (http headers-finished-p body-finished-p)
        (funcall parser http-data)
      (when body-finished-p
        (my-app:close-http-stream))
      ...)))
```

## API differences from http-parse

* `http`, `http-request` and `http-response` are structure classes, not standard classes.
* `http` doesn't have `:force-stream` option. (always streaming)
* `http` doesn't have `:store-body` option because it can consume much memory.
* `body-callback` for `make-parser` and `make-multipart-parser` doesn't take a flag `body-complete-p`.
  * Use `finish-callback` to know if the parsing is finished.
* `:multipart-callback` of `make-parser` and `:callback` of `make-multipart-parser` takes a stream, not a body octet vector at the 4th argument.
* Raises errors aggressively while parsing.
  * Handle `fast-http-error` as you needed.
* Doesn't use a property list as a representation of HTTP headers. (See [issue #1](https://github.com/fukamachi/fast-http/issues/1))

## Installation

Until this library will be available on [Quicklisp](http://www.quicklisp.org/beta/), download this from GitHub.

```
$ cd ~/common-lisp/
$ git clone git@github.com:fukamachi/fast-http
```

```common-lisp
(ql:quickload :fast-http)
```

## Running tests

```common-lisp
(asdf:test-system :fast-http)
```

## Benchmark

### Summary

* Parsing a HTTP request header 100000 times.

In this benchmark, fast-http is **5.5% faster** than [http-parser](https://github.com/joyent/http-parser), a C library.

| fast-http | http-parser (C) |
| ---------:| ---------------:|
|   0.431s  |      0.456s     |

### Environment

* MacBook Pro OSX Mavericks (CPU: 3GHz Intel Core i7, Memory: 8GB)
* SBCL 1.2.4
* GCC version 6.0 (clang-600.0.51)

### fast-http (Common Lisp)

```common-lisp
(syntax:use-syntax :interpol)

(defun run-benchmark ()
  (let ((parser (make-ll-parser))
        (callbacks (make-parser-callbacks))
        (data (babel:string-to-octets #?"GET /cookies HTTP/1.1\r\nHost: 127.0.0.1:8090\r\nConnection: keep-alive\r\nCache-Control: max-age=0\r\nAccept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8\r\nUser-Agent: Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.17 (KHTML, like Gecko) Chrome/24.0.1312.56 Safari/537.17\r\nAccept-Encoding: gzip,deflate,sdch\r\nAccept-Language: en-US,en;q=0.8\r\nAccept-Charset: ISO-8859-1,utf-8;q=0.7,*;q=0.3\r\nCookie: name=wookie\r\n\r\n")))
    (time
     (loop repeat 100000 do
       (http-parse parser callbacks data)))))

(run-benchmark)
```

```
Evaluation took:
  0.431 seconds of real time
  0.431906 seconds of total run time (0.430379 user, 0.001527 system)
  100.23% CPU
  1,288,960,305 processor cycles
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
    assert(parsed == strlen(buf));
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
$ gcc -Wall -Wextra -Werror -O3 http_parser_g.o bench.c -o bench
$ bench
Elapsed 0.456435 seconds.
```

## Author

* Eitaro Fukamachi (e.arrows@gmail.com)

## Copyright

Copyright (c) 2014 Eitaro Fukamachi

## License

Licensed under the MIT License.
