%% emplog.erl
%%
%% @copyright 2011-2012 The EMP Group <http://www.emp-d.com/>
%% @end
%%
%% This library is free software; you can redistribute it and/or
%% modify it under the terms of the GNU Lesser General Public
%% License as published by the Free Software Foundation; either 
%% version 2.1 of the License, or (at your option) any later version.
%% 
%% This library is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%% Lesser General Public License for more details.
%% 
%% You should have received a copy of the GNU Lesser General Public
%% License along with this library.  If not, see <http://www.gnu.org/licenses/>.
%% ----------------------------------------------------------------------------
%% @doc 
%%	The API for the syslog/disklog wrapper used by EMP and related 
%%  applicaitons.
%% @end
%%
-module(emplog).
%%
%% EMP Logger: 
%%      Its a wrapper for erlang's disk_log module or *nix's Syslog. 
%%  Both of which are highly distributable and FAST. EMP wraps these to make 
%%  logging easier for plugins and emp developers. Configurations can be 
%%  passed in through startup. By default, emp will use syslog on *nix systems
%%  and disk_log for all others. This can be overridden by passing in a 
%%  configuration value.
%%

-define(REG_NAME, ?MODULE).
-define(LOGGER, emplogd).

% Names of the type of logging types. Just changes how the log entry is formatted.
-define(SYS_LOG, emplog_system).
-define(EVENT_LOG, emplog_events).

% App API
-export([start/0,stop/0]).

% Server API
-export([start_link/1, terminate/0]).
-export([log_monitor/2]).

% Logging API
-export([info/1,debug/1,warn/1,error/2,event/1]).
-export([info/2,debug/2,warn/2,error/3,event/2]).

%%
%% =================================================
%% EmpLog Application Functions
%% =================================================
%%

start() ->
    application:start(emplog).
stop() ->
    application:stop(emplog).

%%
%% =================================================
%% EmpLog Server API
%% =================================================
%%
start_link( LogSettings ) ->
    {{SysName, SystemFiles},EventLoggingSettings} = verifyInstallation(),
    SOver = keymerge(SystemFiles, LogSettings),

    case lists:keyfind(usesyslog, 1, SOver) of
        {usesyslog, true} ->
            syslog:start(),
            syslog:open("empd", [cons,pid,ndelay], local5),
            Pid = spawn(fun logsender_syslog/0),
            register(?LOGGER,Pid),
            {ok, Pid};
        _ -> 
            % not using syslog, so we are starting our own logger with disk_log.
            % our logging process will be different this time.
            Pid = spawn(fun logsender_disk/0),
            register(?LOGGER,Pid),
            
            % Get the configs for the disk_log
            {EventName,EventFiles} = EventLoggingSettings,
            EOverR = keymerge(EventFiles, cleanargs(LogSettings,[])),
            SOverR = cleanargs(SOver,[]), %purge the syslog setting (disk_log will complain).
            
            % Start up our personal monitor for our log files. It assures that we never drop any logs.
            MonPid = spawn(?MODULE, log_monitor, [{SysName,SOverR},{EventName,EOverR}]),    
            
            % Open our system log first.
            case open_log( SOverR, MonPid ) of
                {ok, SysName} ->
                    % Successful, so lets try opening our event log.
                    case open_log(EOverR, MonPid) of
                        {ok, EventName} -> 
                            % Both logs have started successfully, register the log monitor.
                            global:register_name(?REG_NAME, MonPid),
                            {ok,Pid};
                        Er -> 
                            % Event log crashed, close the System log and record the crash.
                            error_logger:error_msg("Event Log crashed!",[Er]),
                            ?LOGGER ! shutdown,
                            MonPid ! shutdown,
                            Er
                    end;
                Er -> %System log died, nothing we can do, report the message to the caller.
                    ?LOGGER ! shutdown,
                    MonPid ! shutdown, 
                    Er
            end
    end.

terminate() -> global:send(?REG_NAME, shutdown).

%%
%% =================================================
%% Public Logging API:
%% =================================================
%%

info(Msg)  -> log(?SYS_LOG, info, Msg).
debug(Msg) -> log(?SYS_LOG, debug, Msg).
warn(Msg)  -> log(?SYS_LOG, warning, Msg).
event(Msg) -> log(?EVENT_LOG, notice, Msg).
error(Msg, Trace) -> log(?SYS_LOG, err, Msg, Trace).

% Format message before logging. %
info(FMsg, Vars)  -> log(?SYS_LOG, info, io_lib:format(FMsg, Vars)).
debug(FMsg, Vars) -> log(?SYS_LOG, debug, io_lib:format(FMsg, Vars)).
warn(FMsg, Vars)  -> log(?SYS_LOG, warning, io_lib:format(FMsg, Vars)).
event(FMsg, Vars) -> log(?EVENT_LOG, notice, io_lib:format(FMsg, Vars)).
error(FMsg, Vars, Trace) -> log(?SYS_LOG, err, io_lib:format(FMsg, Vars), Trace).

%%
%% ==================================================
%% Private implementation of logging.
%% ==================================================
%%

log(Log,Level,Msg)-> 
    Packet = {Log, Level, Msg, []},
    try ?LOGGER ! Packet catch _:_ -> pass end, Msg.
log(Log,Level,Msg,Extra)-> 
    Packet = {Log,Level,Msg, Extra},
    try ?LOGGER ! Packet catch _:_ -> pass end, Msg.
    
% Runs one of these two servers based on user configuration, these are 
% linked with the application's supervisor. 
logsender_syslog() ->
    receive 
        shutdown -> ok;
        {_, Level, Message, Extra} ->
            syslog:log(Level, [Message|Extra]),
            logsender_syslog();
        _Unknown -> logsender_syslog() %discard unknown.
    end.
logsender_disk()->
    receive 
        shutdown -> ok;
        {Log, Level, Message, Extra} ->
            case disk_log:blog( Log, io_lib:format("~p:[~p]-~p~n",[Level,timestamp(),Message]) ) 
            of
                {error, no_such_log} -> fix( Log, Level, Message, Extra );
                ok -> 
                    % Successful logging, lets indent and print the extra data included.
                    disk_log:blog_terms( Log, 
                                         lists:map( fun(X) -> 
                                                           io_lib:format("      ~p~n",[X]) 
                                                    end, Extra))
            end,
            logsender_disk();
        _Unknown -> logsender_disk()
    end.
%%
%% ===================================================================
%% Private Utility functions
%% ===================================================================
%%

% Quick unix timestamp calculation.
timestamp()-> {Mega, Secs, _} = now(), Mega*1000000 + Secs.
% Override keymerge to instead overwrite duplicates.
keymerge(Defaults, [])-> lists:keysort(1, Defaults);
keymerge(Defaults, [H={X,_}|R])-> keymerge(lists:keystore(X,1,Defaults,H),R).
% Clean all arguments that disk_log doesnt want.
cleanargs([],Accum)-> Accum;
cleanargs([V={Key,_}|Rest], Accum)->
    case lists:member(Key,
                    [file,name,linkto,repair,type,format,size,
                     distributed,notify,head,head_func,mode]) 
    of
       true  -> cleanargs(Rest,[V|Accum]);
       false -> cleanargs(Rest,Accum)
    end.

% Log is down, restart it and log the message we received.
fix( Log, Level, Message, Extra ) ->
    % Ask the monitor to restart the logger and then pass it the log msg.
    global:send(?REG_NAME, {fix, Log, Level, Message, Extra}), ok.

% Open a log with a set of overrides to the default settings.
open_log(Over, LinkPid) when is_list(Over) ->
    case disk_log:open( [{linkto, LinkPid}|Over] ) of
        %TODO: cases will change if we become distributed logging.
        {repaired, SysName, _Recovered, _Badbytes} -> {ok, SysName};
        {ok, SysName} -> {ok, SysName}; 
        Error -> Error
    end. 

% The server process, it listens for messages missing the log.
log_monitor( SStat, EStat ) ->
    receive
        {fix, Log, Level, Msg, Extra} ->
            case restart(SStat, EStat, Log) of
                {ok, Log} -> 
                    log(Log, Level, Msg, Extra);
                {error, Err} ->
                    % We were unsuccessful in our attempts to restart our log file handle.
                    % So we will alert the user via erlang's error_logger and then print to IO
                    % the recieved log message.
                    error_logger:error_msg("EMP LOG ERROR!! CANNOT RESTART LOG HANDLER: ~p", Err),
                    error_logger:info_msg("Log recieved while disk_log was down: ~p",
                                          io_lib:format("~p:[~p]-~p~n",[Level,timestamp(),Msg]))
            end,
            log_monitor(SStat, EStat);
        
        %SHUTDOWN CASES:
        shutdown -> 
            %TODO: save state?
            disk_log:close(?SYS_LOG),
            disk_log:close(?EVENT_LOG), ok;
        Err -> 
            io:format("emplog:monitor received unknown message, Shutting down!: ~p", Err),
            disk_log:close(?SYS_LOG),
            disk_log:close(?EVENT_LOG)
    end.

% Restart a log file handle based on what log was given.
restart({Log, Over}, _, Log) ->
    case disk_log:info(Log) of
        {error,_} ->
            open_log(Over, self());
        _ -> % Log is already running, we don't need to restart. 
            {ok, Log}
    end;
restart(_,{Log,Over}, Log) ->
    case disk_log:info(Log) of
        {error,_} ->
            open_log(Over, self());
        _ -> % Log is already running, we don't need to restart. 
            {ok, Log}
    end;
restart(_,_,Log) ->
    {error, io_lib:format("ERR: No log named: ~p",Log)}.

%% Verify the installation of syslog, and also ensure backup disk_log
%% paths are created. Even if syslog is the chosen output source.
verifyInstallation() ->
    SyslogDefault = case os:type() of 
                        {unix,_} -> true; 
                        _ -> false
                    end,
    DEFAULT_DIR = getDefaultLogDir(),
    SysFile   = filename:join([DEFAULT_DIR, "system.log"]),
    EventFile = filename:join([DEFAULT_DIR, "events.log"]),
    % ensure the dir structure is valid and created.
    filelib:ensure_dir(SysFile),
    {
     {?SYS_LOG,   [{file,SysFile},  {name,?SYS_LOG}, {usesyslog, SyslogDefault}]},
     {?EVENT_LOG, [{file,EventFile},{name,?EVENT_LOG} ]}
    }.

getDefaultLogDir() ->
    case os:type() of
        vxworks -> "";
        {win32, _} -> filename:join([os:getenv("HOME"), ".emp", "logs"]);
        {unix,linux}-> filename:join([os:getenv("HOME"), ".emp", "logs"]);
        {unix,_}-> ""
    end.
