% Types
-type date() :: {pos_integer(), pos_integer(), pos_integer()}.
-type time() :: {pos_integer(), pos_integer(), pos_integer()}.
-type datetime() :: {date(), time()}.

% Schema
-record(memory, {uuid    :: binary(),
                 plugin  :: atom(),
                 key     :: binary(),
                 value   :: any(),
                 created :: datetime()
                }).
