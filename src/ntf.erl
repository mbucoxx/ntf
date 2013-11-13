-module(ntf).

-export([main/1]).


main(_Args) ->
    application:start(ntf).
