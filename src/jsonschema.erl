-module(jsonschema).

-export([validate/2]).


%% Возвращает {ok, Document}, если документ `Document` валидируется
%% схемой `Schema` либо {error, /описание ошибки/} в случае, если это не так

validate(Document, {struct, SchemaProps}) ->
    io:format("Validating ~p using ~p~n", [Document, SchemaProps]),
    %% проверяем все подряд херни, которые могут быть указаны в схеме
    Type = proplists:get_value(<<"type">>, SchemaProps, <<"any">>),
    validate(Document, <<"type">>, Type);

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
        false -> {error, {type_error, Document, Union}}
    end;

validate(Document, <<"type">>, Type) ->
    {error, {type_error, Document, Type}}.
