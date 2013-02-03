% Types
-type datetime() :: {date(), time()}.
-type date() :: {pos_integer(), pos_integer(), pos_integer()}.
-type time() :: {pos_integer(), pos_integer(), pos_integer()}.

% Schema
-record(memory, {uuid    :: binary(),
                 plugin  :: atom(),
                 key     :: binary(),
                 value   :: any(),
                 created :: datetime()
                }).
