-module(jsonschema).

-export([validate/2]).




validate(Document, Schema) ->
    case validate1(Document, Schema) of
        [] -> ok;
        Errors -> {error, Errors}
    end.


validate1(Document, Schema={struct, SchemaProps}) ->
    Attrs = [{<<"type">>, <<"any">>},
             {<<"minimum">>, undefined},
             {<<"maximum">>, undefined}
            ],
    lists:filter(fun ([]) -> false;
                     (ok) -> false;
                     (_) -> true
                 end,
                 
                 lists:map(fun ({Attr, Default}) ->  
                                   validate(Document, Schema, Attr, proplists:get_value(Attr, SchemaProps, Default))
                           end,
                           Attrs));

validate1(_Document, _Schema) ->
    {error, "Schema must be an object"}.


validate(Document, _Schema, <<"type">>, <<"string">>) when is_binary(Document) ->
    ok;
validate(Document, _Schema, <<"type">>, <<"number">>) when is_number(Document) ->
    ok;
validate(Document, _Schema, <<"type">>, <<"integer">>) when is_integer(Document) ->
    ok;
validate(true, _Schema, <<"type">>, <<"boolean">>) ->
    ok;
validate(false, _Schema, <<"type">>, <<"boolean">>) ->
    ok;
validate({struct, _Document}, _Schema, <<"type">>, <<"object">>) ->
    ok;
validate(Document, _Schema, <<"type">>, <<"array">>) when is_list(Document) ->
    ok;
validate(null, _Schema, <<"type">>, <<"null">>) ->
    ok;
validate(_Document, _Schema, <<"type">>, <<"any">>) ->
    ok;

validate(Document, Schema, <<"type">>, Union) when is_list(Union) ->
    Types = lists:map(fun (Schema={struct, _}) ->
                              validate1(Document, Schema);
                          (Type) ->
                              validate(Document, Schema, <<"type">>, Type)
                      end,
                      Union),
    Any = lists:any(fun (ok) ->
                           true;
                        ([]) ->
                            true;
                        (_) ->
                            false
                    end,
                    Types),
    case Any of
        true -> ok;
        false -> {type, Document, Union}
    end;


validate(Document, _Schema, <<"type">>, Type) ->
    {type, Document, Type};


validate(_Document, _Schema, <<"minimum">>, _Minimum) ->
    ok;
validate(Document, _Schema={struct, SchemaProps}, <<"minimum">>, Minimum) when is_number(Document), is_number(Minimum) ->
    case proplists:get_value(<<"exclusiveMinimum">>, SchemaProps, false) of
        true when Document > Minimum -> ok;
        _ when Document >= Minimum -> ok;
        _ -> {minimum, Document, Minimum}
    end;
validate(_Document, _Schema, <<"minimum">>, _Minimum) ->
    ok;

validate(Document, _Schema={struct, SchemaProps}, <<"maximum">>, Maximum) when is_number(Document), is_number(Maximum) ->
    case proplists:get_value(<<"exclusiveMaximum">>, SchemaProps, false) of
        true when Document < Maximum -> ok;
        _ when Document =< Maximum -> ok;
        _ -> {maximum, Document, Maximum}
    end;
validate(_Document, _Schema, <<"maximum">>, _Maximum) ->
    ok.
