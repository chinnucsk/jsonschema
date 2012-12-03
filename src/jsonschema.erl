-module(jsonschema).

-export([validate/2]).



%% TODO: maybe I should use proplists module to unpack object's
%% properties 


validate(Document, Schema) ->
    case validate1(Document, Schema) of
        [] -> ok;
        Errors -> {error, Errors}
    end.


validate1(Document, Schema={struct, SchemaProps}) ->
    Attrs = [{<<"type">>, <<"any">>},
             {<<"minimum">>, undefined},
             {<<"maximum">>, undefined},
             {<<"enum">>, undefined},
             {<<"properties">>,undefined}
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
    ok;

validate(Document, _Schema, <<"enum">>, Enum) when is_list(Enum) ->
    case lists:any(fun (Value) when Value =:= Document -> true;
                 (_) -> false
              end,
              Enum) of
        true -> ok;
        false -> {enum, Document, Enum}
    end;
validate(_Document, _Schema, <<"enum">>, _) ->
    ok;

validate({struct, DocumentProperties}, _Schema, <<"properties">>, {struct, Properties}) ->
    %% каждый проперти из объявленных в Properties достаём из
    %% документа и валидируем
    Result = lists:map(fun ({PropertyName, PropertySchema}) -> 
                      PropertyValue = proplists:get_value(PropertyName, DocumentProperties),
                      validate1(PropertyValue, PropertySchema)
              end,
                       Properties),
    lists:filter(fun ([]) -> false;
                     (ok) -> false;
                     (_) -> true
                 end,
                 Result);
validate(_Document, _Schema, <<"properties">>, undefined) ->
    ok;

validate(Document={struct, _DocumentProperties}, 
         Schema={struct, _SchemaProperties}, 
         <<"additionalProperties">>, false) ->
    case additional_properties(Document, Schema) of
        [] -> ok;
        AdditionalProperties -> {additional_properties, Document, AdditionalProperties}
    end;
validate(Document={struct, DocumentProperties},
         Schema={struct, _SchemaProperties},
         <<"additionalProperties">>, 
         AdditionalPropertiesSchema) ->
    lists:filter(fun (ok) -> false;
                     ([]) -> false;
                     (_) -> true
                 end,
                 lists:map(fun(PropertyName) ->
                                   validate1(
                                     proplists:get_value(PropertyName, DocumentProperties),
                                     AdditionalPropertiesSchema
                                    )
                           end,
                           additional_properties(Document, Schema)));
%% special case for empty additional properties
validate(_Document, _Schema, <<"additional properties">>, {struct, []}) ->
    ok.

    

%% internal functions
additional_properties({struct, DocumentProperties}, {struct, SchemaProperties}) ->
    %% вынимаем все проперти документа и проверяем, определены ли они
    %% в списке пропертей
    DefinedPropertiesNames = 
        case proplists:get_value(<<"properties">>, SchemaProperties) of
            {struct, Properties} ->
                proplists:get_keys(Properties);
            undefined -> []
        end,
    DocumentPropertiesNames = proplists:get_keys(DocumentProperties),
    %% NOTE: The complexity of lists:subtract(A, B) is proportional to
    %% length(A)*length(B), meaning that it will be very slow if both
    %% A and B are long lists. (Using ordered lists and
    %% ordsets:subtract/2 is a much better choice if both lists are
    %% long.)
    lists:subtract(DocumentPropertiesNames, DefinedPropertiesNames).

