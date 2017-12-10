# Elm Server

## Usage

Run the server:

```
$ (cd bin/ && ./build && ./run)
```

Send requests:

```
$ curl localhost:8080/echo
<Sun Dec 10 2017 12:31:14 GMT-0500 (EST)> GET /echo%

$ curl "localhost:8080/greet?name=Brad"
{"message":"Hello, Brad!"}%

$ curl -v localhost:8080/boof
* ...
> ...
< HTTP/1.1 404 Not Found
< Content-Type: text/plain
< ...
* ...
Not found:  GET /boof%
```
