-module(jsonschema).

-export([validate/2]).


validate(Document, {struct, SchemaProps}) ->
    io:format("Validating ~p using ~p~n", [Document, SchemaProps]),
    %% проверяем все подряд херни, которые могут быть указаны в схеме
    Type = proplists:get_value(<<"type">>, SchemaProps, <<"any">>),
    ok = validate(Document, <<"type">>, Type),
    %% {struct, Properties} = proplists:get_value(<<"properties">>, SchemaProps, []),
    %% ok = validate(Document, <<"properties">>, Properties),
    Minimum = proplists:get_value(<<"minimum">>, SchemaProps),
    ok = validate(Document, <<"minimum">>, Minimum),
    Maximum = proplists:get_value(<<"maximum">>, SchemaProps),
    ok = validate(Document, <<"maximum">>, Maximum);

validate(_Document, _Schema) ->
    {error, "Schema must be an object"}.


validate(Document, <<"type">>, <<"string">>) when is_binary(Document) ->
    ok;
validate(Document, <<"type">>, <<"number">>) when is_number(Document) ->
    ok;
validate(Document, <<"type">>, <<"integer">>) when is_integer(Document) ->
    ok;
validate(true, <<"type">>, <<"boolean">>) ->
    ok;
validate(false, <<"type">>, <<"boolean">>) ->
    ok;
validate({struct, _Document}, <<"type">>, <<"object">>) ->
    ok;
validate(Document, <<"type">>, <<"array">>) when is_list(Document) ->
    ok;
validate(null, <<"type">>, <<"null">>) ->
    ok;
validate(_Document, <<"type">>, <<"any">>) ->
    ok;

validate(Document, <<"type">>, Union) when is_list(Union) ->
    case lists:any(fun (ok) ->
                      true;
                  ({error, _}) ->
                           false
                   end,
                   lists:map(fun (Schema={struct, _}) ->
                                     validate(Document, Schema);
                                 (Type) ->
                                     validate(Document, <<"type">>, Type)
                             end,
                             Union)) of
        true -> ok;
        false -> {error, {type, Document, Union}}
    end;

validate(Document, <<"type">>, Type) ->
    {error, {type, Document, Type}};

validate(_Document, <<"minimum">>, undefined) ->
    ok;
validate(Document, <<"minimum">>, Minimum) when is_number(Document), Document >= Minimum ->
    ok;
validate(Document, <<"minimum">>, Minimum) when is_number(Document) ->
    {error, {minimum, Document, Minimum}};
validate(_Document, <<"minimum">>, _Minimum) ->
    ok;

validate(_Document, <<"maximum">>, undefined) ->
    ok;
validate(Document, <<"maximum">>, Maximum) when is_number(Document), Document =< Maximum ->
    ok;
validate(Document, <<"maximum">>, Maximum) when is_number(Document) ->
    {error, {maximum, Document, Maximum}};
validate(_Document, <<"maximum">>, _Maximum) ->
    ok.
