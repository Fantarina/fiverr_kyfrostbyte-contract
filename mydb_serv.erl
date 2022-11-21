-module(mydb_serv).

-export([start/0,write/2,delete/1,read/1,update/2,clear/0]).

-behaviour(gen_server).
%% API
-export([start_link/1, stop/1]).

-export([code_change/3, handle_call/3, handle_cast/2,
	 handle_info/2, init/1, terminate/2]).

stop(Name) -> gen_server:call(Name, stop).
start()->
    start_link([]).
start_link(_Map) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [_Map],
			  []).

init(_Map) -> {ok, #{}}.


handle_call({write,Key,Value},_From,State)->
    case maps:is_key(Key,State) of
        true->
            {reply,{ error , "Already existing key" },State};
        false->
            NewState= State#{Key=>Value},
            {reply,ok,NewState} end;

handle_call({delete, Key}, _From, State) ->
    case maps:is_key(Key,State) of
    true->
        NewState = maps:remove(Key,State),
        {reply, ok, NewState};
    false->
        {reply,{ error , "Key not here" },State} end;

handle_call({read, Key}, _From, State) ->
   case maps:get ( Key , State, none ) of 
         none -> { reply , { error , "Key not here" } , State } ;
         Value-> { reply , { ok , Value } , State } end ;

handle_call ( {update , Key,Value } , _From , State ) ->
    case maps:is_key(Key,State) of
        true->
            NewState= State#{Key:=Value},
            {reply,ok,NewState};
        false->
            {reply,{ error , "Key not here" },State} end.


handle_cast({clear}, _State) -> {noreply, #{}}.

handle_info(_Info, State) ->
    {reply, "Unexcepted Message", State}.

terminate(_Reason, _State)-> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%write value into the database
write(Key, Value) ->    
    case gen_server:call(?MODULE, {write, Key, Value}) of
        ok->"Success";
        { error , "Already existing key"}-> "Already existing key"end.

%delete value in the database
delete(Key) ->
    case gen_server:call(?MODULE, {delete, Key}) of
        ok-> "Done";
        { error , "Key not here" }->"Key not here" end.

%read value in the database
read(Key) ->    
    case gen_server:call(?MODULE, {read, Key}) of
        {ok,Value}->Value;
        {error,"Key not here" }-> "Key not here"
    end.

%update key in the database
update(Key,Value) ->
    case gen_server:call(?MODULE,{update,Key, Value}) of
        ok-> "Success";
        { error , "Already existing key" }-> "Already existing key"
    end.

%eclear database
clear()->
    gen_server:cast(?MODULE,{clear}).