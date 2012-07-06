## logtail: tail -f logfiles in the browser with websockets

*NB: incomplete hack!*

### How to build and run
```
$ rebar get-deps
$ ./start-dev.sh
```

Now visit http://localhost:9090/index.html 

(still need to make cowboy treat / as /index.html)

### Configuration

Add entries to the sources section in app.config:

*Single file*
```
{file, "/path/to/file.log", []}
```

*Multiple files, wildcard expansion*
```
{files, "/var/log/*", []}
```

