-module(jsonschema_steps).

-include("sedate_info.hrl").


-record(world,{schema=undefined}).

-export([init/0, step/3]).

init() ->
    #world{}.


step([the, schema, is, Schema], World=#world{}, _Info) ->
    {ok, World#world{schema=mochijson2:decode(Schema)}};

step([the, schema, 'is:'], World=#world{}, _Info=#sedate_info{string=Schema}) ->
    {ok, World#world{schema=mochijson2:decode(Schema)}};

step([Document, is, valid, json], World=#world{schema=Schema}, _Info) ->
    ok = jsonschema:validate(mochijson2:decode(Document), Schema),
    {ok, World};

step([Document, is, 'not', valid, json], World=#world{schema=Schema}, _Info) ->
    {error, _} = jsonschema:validate(mochijson2:decode(Document), Schema),
    {ok, World};

step([this, is, valid, 'json:'], World=#world{schema=Schema}, _Info=#sedate_info{string=Document}) ->
    ok = jsonschema:validate(mochijson2:decode(Document), Schema),
    {ok, World};

step([these, are, valid, 'json:'], World=#world{schema=Schema}, _Info=#sedate_info{table=DocumentsTable}) ->
    lists:map(fun ([Document]) ->
                      ok = jsonschema:validate(mochijson2:decode(Document), Schema)
              end,
              DocumentsTable),
    {ok, World};

step([these, are, 'not', valid, 'json:'], World=#world{schema=Schema}, _Info=#sedate_info{table=DocumentsTable}) ->
    lists:map(fun ([Document]) ->
                      {error, _} = jsonschema:validate(mochijson2:decode(Document), Schema)
              end,
              DocumentsTable),
    {ok, World};
step(StepSpec, _World, _Info) ->
    {error, {undefined_step, StepSpec}}.

