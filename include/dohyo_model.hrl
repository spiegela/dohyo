
-export([sumo_schema/0, sumo_sleep/1, sumo_wakeup/1]).

sumo_sleep(Plist) -> maps:from_list(Plist4).

sumo_wakeup(Map) -> maps:to_list(Map).

sumo_schema() -> dohyo_model:sumo_schema(?MODULE).
