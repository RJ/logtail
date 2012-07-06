-module(logtail_web).
-export([init/3]).
-export([websocket_init/3, websocket_handle/3,
         websocket_info/3, websocket_terminate/3]).

init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_http_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
    self() ! send_init,
    logtail_mgr:subscribe({file, "/tmp/logtail.txt"}),
    {ok, Req, undefined_state}.

websocket_handle({text, Msg}, Req, State) ->
    {J} = ejson:decode(Msg),
    handle_ws_request(proplists:get_value(<<"type">>, J), J, Req, State);

websocket_handle(_Data, Req, State) ->
    {ok, Req, State}.

%% Send list of available sources
websocket_info(send_init, Req, State) ->
    Sources = logtail_mgr:which_sources(),
    Json = {[{<<"type">>, <<"sources">>},
            {<<"list">>, 
                { [ {list_to_binary(Path), true} || {{file, Path, _Opts},_Pid} <- Sources] }
            }]},
    {reply, {text, ejson:encode(Json)}, Req, State};

websocket_info({{subscription, {file, Path}}, Line}, Req, State) ->
    J = {[  {<<"type">>, <<"line">>},
            {<<"source">>, list_to_binary(Path)},
            {<<"line">>, Line}
        ]},
    {reply, {text, ejson:encode(J)}, Req, State};

websocket_info({timeout, _Ref, Msg}, Req, State) ->
    erlang:start_timer(1000, self(), <<"How' you doin'?">>),
    {reply, {text, Msg}, Req, State};
websocket_info(_Info, Req, State) ->
    {ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
    ok.

handle_ws_request(<<"subscribe">>, Props, Req, State) ->
    Path = binary_to_list(proplists:get_value(<<"path">>, Props)),
    logtail_mgr:subscribe({file, Path}),
    Resp = {[{<<"type">>, <<"subscribed">>}, {<<"path">>, list_to_binary(Path)}]},
    {reply, {text, ejson:encode(Resp)}, Req, State};

handle_ws_request(<<"unsubscribe">>, Props, Req, State) ->
    Path = binary_to_list(proplists:get_value(<<"path">>, Props)),
    logtail_mgr:unsubscribe({file, Path}),
    Resp = {[{<<"type">>, <<"unsubscribed">>}, {<<"path">>, list_to_binary(Path)}]},
    {reply, {text, ejson:encode(Resp)}, Req, State}.



