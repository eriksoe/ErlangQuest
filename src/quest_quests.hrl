-ifndef(quest_quests_hrl).
-define(quest_quests_hrl, included).

-record(quest, {generate :: fun(()-> _ | {'$save',_,_}),
                verify :: fun((_,_)->boolean())}).

-endif.

