rebar compile && erl -pa ebin/ -pa deps/*/ebin -boot start_sasl -config ./app.config -s logtail
