# Feel free to use, reuse and abuse the code in this file.

rel: app
	@./rebar generate

app: get-deps
	@./rebar compile

get-deps:
	@./rebar get-deps

clean:
	@./rebar clean
	rm -f erl_crash.dump

dist-clean: clean
