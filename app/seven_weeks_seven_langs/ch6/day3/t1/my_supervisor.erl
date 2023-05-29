-module(my_supervisor).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, {{one_for_one, 5, 60}, [{my_process, {my_process, start_link, []}, permanent, 5000, worker, [my_process]}]}}.
