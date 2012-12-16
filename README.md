## EMP Logger ##

The EMP logging system has been abstracted and pushed into its own repo. It 
uses the Syslog protocol and can be routed to a local or external logging 
server. It also can use Erlang's built in disk-logger if you have no syslog 
server avaliable (but it normally comes standard on any linux distribution).

NOTE: There is no need to clone this repo directly, Empd/emp will pull this 
repo as a dependency using rebar.

