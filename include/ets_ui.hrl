-define(DEFAULT_RESP_HEAD, #{<<"content-type">> => <<"application/json">>}).

-define(DUMMY_VERSION_1, 1).
-define(DUMMY_VERSION_2, 2).
-type version_1() :: ?DUMMY_VERSION_1.
-type version_2() :: ?DUMMY_VERSION_2.
% -type version() :: version_1() |
%                    version_2().
-type user_id() :: pos_integer().
%% Association :: Type := Type  %% denotes a mandatory association
%%              | Type => Type  %% denotes an optional association
-type versioned_map_1() :: #{
        vsn := ?DUMMY_VERSION_1,
        is_even := boolean(),
        datetime := calendar:datetime()
    }.
-type versioned_map_2() :: #{
        vsn := ?DUMMY_VERSION_2,
        is_even := boolean(),
        datetime := calendar:datetime(),
        created_by_user_id => user_id(),
        approved => boolean()
    }.
-type versioned_map() :: versioned_map_1() |
                         versioned_map_2().
-type dummy_key() :: pos_integer().
-record(dummy, {
    key :: dummy_key(),
    map :: versioned_map()
}).



-record(example_record, {col1, is_even}).
