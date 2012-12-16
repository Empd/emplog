%% emplog_sup.erl
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
%%  Supervisor for the logging server used by EMP and EMPDB.
%% @end
%%
-module(emplog_sup).
-behaviour(supervisor).

%% Supervisor API
-export([start_link/2]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(DistCheck, StartArgs) ->
    %process_flag(trap_exit, true),
    Args = [{distribution, DistCheck}|StartArgs],
    supervisor:start_link( 
                           {local, ?MODULE}, 
                           ?MODULE,
                           [ Args ] 
                         ).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(Args) ->
    {ok, { {one_for_one, 5, 10}, [
        {emplog, {emplog, start_link, Args}, 
            permanent, 5000, worker, [emplog]}
    ]} }.

