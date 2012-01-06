Erlang splice() linked-in driver
==================================

**splice** is a linked-in driver for the splice(2) Linux syscall.

Based on sendfile driver from [sendfile](https://github.com/tuncer/sendfile).

Building and Installing
-----------------------

splice is built with [rebar](http://bitbucket.org/basho/rebar/) and
we do expect `rebar` to be in the search `PATH`.  
If `rebar` can not be found in the search `PATH` it will be
automatically downloaded to `support/rebar` for local usage.
