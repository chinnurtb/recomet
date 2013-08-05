-module(crypto_util).

-export([md5/1]).

md5(S) ->
  list_to_binary([io_lib:format("~2.16.0b", [N])||N <- binary_to_list(erlang:md5(S))]).

