%% emplog_app.erl
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
%%  Primary logging application, its a wrapper to syslog or disklog.
%% @end
%%
-module(emplog_app).
-behaviour(application).

%% Application callbacks
-export([start/2, pre_stop/1, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start( normal, StartArgs ) ->
    emplog_sup:start_link( normal, StartArgs );

start( _StartType, StartArgs ) ->
    emplog_sup:start_link( distributed, StartArgs ).

pre_stop( _State ) ->
    emplog:debug("~EMP LOG SHUTDOWN~").

stop(_State) ->
    ok. % there is no application state
