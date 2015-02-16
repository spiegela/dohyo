-module(dohyo_validations_test).

-compile(export_all).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-include("test_helpers.hrl").

%%% Property Tests

property_test_() ->
  [ ?_assertEqual(
       ?_proper_passes(missing_field_fails_presence()), true
    ),
    ?_assertEqual(
       ?_proper_passes(present_field_passes_presence()), true
    ),
    ?_assertEqual(
       ?_proper_passes(invalid_field_fails_inclusion()), true
    ),
    ?_assertEqual(
       ?_proper_passes(missing_field_passes_inclusion()), true
    ),
    ?_assertEqual(
       ?_proper_passes(valid_field_passes_inclusion()), true
    ),
    ?_assertEqual(
       ?_proper_passes(invalid_field_fails_exclusion()), true
    ),
    ?_assertEqual(
       ?_proper_passes(missing_field_passes_exclusion()), true
    ),
    ?_assertEqual(
       ?_proper_passes(valid_field_passes_exclusion()), true
    ),
    ?_assertEqual(
       ?_proper_passes(invalid_field_fails_format()), true
    ),
    ?_assertEqual(
       ?_proper_passes(missing_field_passes_format()), true
    ),
    ?_assertEqual(
       ?_proper_passes(valid_field_passes_format()), true
    ),
    ?_assertEqual(
       ?_proper_passes(missing_field_passes_length()), true
    ),
    ?_assertEqual(
       ?_proper_passes(invalid_field_fails_length()), true
    ),
    ?_assertEqual(
      ?_proper_passes(valid_field_passes_length()), true
    )

  ].

%%% Properties

missing_field_fails_presence() ->
  ?FORALL({Field, Plist}, missing_field(),
          begin
            dohyo_validations:presence(Plist, Field) =:=
              {true, {Field, not_present}}
          end).

present_field_passes_presence() ->
  ?FORALL({Field, Plist}, present_field(),
          begin
            dohyo_validations:presence(Plist, Field) =:= false
          end).

invalid_field_fails_inclusion() ->
  ?FORALL({{Field, Plist}, InList}, excluded_field(),
          begin
            dohyo_validations:inclusion(Plist, Field, InList) =:=
            {true, {Field, not_included}}
          end).

missing_field_passes_inclusion() ->
  ?FORALL({{Field, Plist}, InList}, {missing_field(), list()},
          begin
            dohyo_validations:inclusion(Plist, Field, InList) =:= false
          end).

valid_field_passes_inclusion() ->
  ?FORALL({{Field, Plist}, InList}, included_field(),
          begin
            dohyo_validations:inclusion(Plist, Field, InList) =:= false
          end).

invalid_field_fails_exclusion() ->
  ?FORALL({{Field, Plist}, InList}, included_field(),
          begin
            dohyo_validations:exclusion(Plist, Field, InList) =:=
            {true, {Field, not_excluded}}
          end).

missing_field_passes_exclusion() ->
  ?FORALL({{Field, Plist}, InList}, {missing_field(), list()},
          begin
            dohyo_validations:exclusion(Plist, Field, InList) =:= false
          end).

valid_field_passes_exclusion() ->
  ?FORALL({{Field, Plist}, InList}, excluded_field(),
          begin
            dohyo_validations:exclusion(Plist, Field, InList) =:= false
          end).

invalid_field_fails_format() ->
  ?FORALL({{Field, Plist}, Regex}, unmatching_field(),
          begin
            dohyo_validations:format(Plist, Field, Regex) =:=
              {true, {Field, bad_format}}
          end).

missing_field_passes_format() ->
  ?FORALL({{Field, Plist}, Regex}, {missing_field(), regex_string()},
          begin
            dohyo_validations:format(Plist, Field, Regex) =:= false
          end).

valid_field_passes_format() ->
  ?FORALL({{Field, Plist}, Regex}, matching_field(),
          begin
            dohyo_validations:format(Plist, Field, Regex) =:= false
          end).

invalid_field_fails_length() ->
  ?FORALL({{Field, Plist}, Length}, field_with_bad_length(),
          begin
            dohyo_validations:length(Plist, Field, Length) =:=
              {true, {Field, bad_length}}
          end).

missing_field_passes_length() ->
  ?FORALL({{Field, Plist}, Length}, {missing_field(), pos_integer()},
          begin
            dohyo_validations:length(Plist, Field, Length) =:= false
          end).

valid_field_passes_length() ->
  ?FORALL({{Field, Plist}, Length}, field_with_good_length(),
          begin
            dohyo_validations:length(Plist, Field, Length) =:= false
          end).

%%% Generators

property() ->
  ?LET(Prop, {atom(), term()}, Prop).

proplist() ->
  ?LET(Plist, list(property()), Plist).

present_field() ->
  ?LET( {Field, Value, {Plist1, Plist2}},
        {atom(), term(), split_proplist()},
        {Field, splice_into({Field, Value}, Plist1, Plist2)}
      ).

present_field(Field, {value, Value}) ->
  ?LET( {Plist1, Plist2},
        split_proplist(Field),
        {Field, splice_into({Field, Value}, Plist1, Plist2)}
      ).

split_proplist() ->
  ?LET(Plist, proplist(), split_at_random(Plist)).

split_proplist(Field) ->
  ?LET(Plist, missing_field(Field), split_at_random(Plist)).

missing_field(Field) ->
  ?SUCHTHAT(Plist, proplist(), is_not_a_key(Field, Plist)).

missing_field() ->
  ?SUCHTHAT({Field, Plist}, field_lookup(), is_not_a_key(Field, Plist)).

field_lookup() -> {atom(), proplist()}.

excluded_field() ->
  ?SUCHTHAT( {{Field, Plist}, InList},
             {present_field(), list()},
             value_is_not_included(Field, Plist, InList)
           ).

included_field() ->
  ?LET( {{Field, Plist}, {InList1, InList2}},
        {present_field(), split_list()},
        {{Field, Plist}, splice_value_into(Field, Plist, InList1, InList2)}
      ).

unmatching_field() ->
  ?LET( {Field, {Binary, Regex}},
        {atom(), unmatching_regex()},
        {present_field(Field, {value, Binary}), Regex}
      ).

matching_field() ->
  ?LET( {Field, {Binary, Regex}},
        {atom(), matching_regex()},
        {present_field(Field, {value, Binary}), Regex}
      ).

split_list() -> ?LET(List, list(), split_at_random(List)).

unmatching_regex() ->
  ?SUCHTHAT( {Binary, Regex},
             {escaped_utf8_bin(), regex_string()},
             regex_does_not_match(Binary, Regex)
           ).

matching_regex() ->
  ?LET( {String1, String2, Regex},
        {regex_string(), regex_string(), regex_string()},
        {list_to_binary(splice_into(Regex, String1, String2)), Regex}
      ).

regex_string() ->
  default("0",list(union([range($a, $z), range($A, $Z), range($0, $9)]))).

escaped_utf8_bin() ->
  ?SUCHTHAT( Bin,
             ?LET(S, ?SUCHTHAT(L, list(escaped_char()), L /= []),
                  unicode:characters_to_binary(S, unicode, utf8)),
             is_binary(Bin)
           ).


escaped_char() ->
  ?LET( C, char(),
        case C of
          $" -> "\\\"";
          C when C == 65534 -> 65533;
          C when C == 65535 -> 65533;
          C when C > 1114111 -> 1114111;
          C -> C
        end
      ).

field_with_bad_length() ->
  ?LET( {Field, {String, Length}},
        {atom(), string_with_bad_length()},
        {present_field(Field, {value, String}), Length}
      ).

string_with_bad_length() ->
  ?SUCHTHAT( {String, Length},
             {string(), non_neg_integer()},
             length(String) =/= Length
           ).

field_with_good_length() ->
  ?LET( {Field, String},
        {atom(), string()},
        {present_field(Field, {value, String}), length(String)}
      ).

%%% Utility Functions

splice_into(Elem, List1, List2) -> lists:concat([List1, [Elem], List2]).

split_at_random([]) -> {[], []};
split_at_random(Plist) -> lists:split(random_index(Plist), Plist).

random_index(Plist) -> random:uniform(length(Plist)).

is_not_a_key(Field, Plist) -> not lists:keymember(Field, 1, Plist).

splice_value_into(Field, Plist, InList1, InList2) ->
  {_, Value} = lists:keyfind(Field, 1, Plist),
  splice_into(Value, InList1, InList2).

value_is_not_included(Field, Plist, InList) ->
  {_, Value} = lists:keyfind(Field, 1, Plist),
  not lists:member(Value, InList).

regex_does_not_match(Binary, Regex) -> not regex_matches(Binary, Regex).

regex_matches(Binary, Regex) ->
  case re:run(Binary, Regex) of
    {match, _Captured} -> true;
    nomatch -> false
  end.
