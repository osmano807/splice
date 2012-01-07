Erlang splice() linked-in driver
==================================

***NOT WORKING!**: The process seems, to be stopped in `filemap_fault` / `__mutex_lock_slowpath`*

*See: https://lkml.org/lkml/2006/7/7/34 and https://lkml.org/lkml/2006/7/7/141*


**splice** is a linked-in driver for the splice(2) Linux syscall.

Based on sendfile driver from [sendfile](https://github.com/tuncer/sendfile).

Building and Installing
-----------------------

splice is built with [rebar](http://bitbucket.org/basho/rebar/) and
we do expect `rebar` to be in the search `PATH`.  
If `rebar` can not be found in the search `PATH` it will be
automatically downloaded to `support/rebar` for local usage.
