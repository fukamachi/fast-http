# Changelog

## 0.3.1

This release fixes three security vulnerabilities. Update from 0.3.0 or any earlier version.

### Security

- [GHSA-xf42-8f28-vr3r](https://github.com/fukamachi/fast-http/security/advisories/GHSA-xf42-8f28-vr3r) (high): The multipart parser can signal the internal condition `fast-http.parser::eof` out of the public API. This condition is not an `error`, so a server that catches only `error` can stop. The parse result of a multipart body also depends on how the body is split into chunks. Reported and fixed by Satoshi Imai (@masatoi).
- [GHSA-vcr9-q5m4-wrw9](https://github.com/fukamachi/fast-http/security/advisories/GHSA-vcr9-q5m4-wrw9) (high): `make-parser` keeps an unterminated header line, request line or status line without a limit. A peer can use all of the heap with one connection.
- [GHSA-h9hc-rhwj-6cgf](https://github.com/fukamachi/fast-http/security/advisories/GHSA-h9hc-rhwj-6cgf) (medium): After a chunked body, `parse-request` and `parse-response` drop the bytes that follow it in the same call. The requests that the parser sees then depend on how the stream is split.

### Changes in behavior

HTTP messages:

- A request header or trailer with obsolete line folding (RFC 9112, section 5.2) now causes the new condition `fast-http:obsolete-line-folding`. This condition is a subclass of `invalid-header-token`, so existing handlers catch it. Woo replies with 400 Bad Request. Before this release, the parser accepted the folded line.
- In a response, the parser replaces each fold with one SP. Before this release, it kept the whitespace at the start of the continuation line.
- A plain `http` object (not `http-request` or `http-response`) accepts folded lines, as before.
- The new variable `fast-http:*max-header-line-bytes*` (default 65536) sets the maximum length of one incomplete line that `make-parser` keeps. Past this limit, `make-parser` signals `header-overflow`.
- `make-parser` parses a kept line again only when a LF arrives. An invalid byte in an unterminated line therefore causes an error only when the line ends.
- After a chunked body, the parser continues with the next pipelined message, as it does after a `Content-Length` body.
- A `Transfer-Encoding` line that is split across two calls no longer causes `duplicate-transfer-encoding`.

Multipart bodies:

- A boundary must have 1 to 70 characters (RFC 2046). `make-ll-multipart-parser` signals `invalid-boundary` for an empty boundary or a boundary longer than 70 characters. `make-multipart-parser` signals `invalid-boundary` for a boundary longer than 70 characters. Before this release, they accepted a boundary of any length.
- The headers of one part can use at most 8192 bytes. Past this limit, the parser signals `header-overflow`.
- The parser parses the headers of a part only when the blank line after them arrives. An error in the part headers therefore occurs only when the blank line arrives.
- When `http-multipart-parse` keeps bytes for the next call, it returns `end`. Before this release, it returned the position where it stopped, and a caller could send the kept bytes again.
- With `:start` greater than 0, the body callback no longer gets the bytes before `start`.
- The parser makes a new header parser for each part. The parser now accepts a form with more than 1024 header lines in total. It also accepts two parts that each have a `Content-Length` or `Transfer-Encoding` header.
- The parser accepts part headers that use RFC 5322 folding, as before.
