savannadb_agent
===============

An original agent of `SavannaDB`, which is able to easily retrieve metrics and statistics for Erlang's apllications.


## Usage

```erlang

sample() ->
    %% Launch an agent
    ok = savanna_agent:start(ram_copies),
    ok = savanna_agent:sync_schemas(['savanna_manager_0@127.0.0.1',
                                     'savanna_manager_1@127.0.0.1']),

    %% Create metric-servers
    Schema = 'sample',
    MetricGroup = 'test',
    Window = 30, %% 30sec
    ok = savanna_agent:create_metrics(Schema, MetricGroup, Window),

    %% Notify events
    ok = savanna_agent:notify(MetricGroup, 'req_get',     1),
    ok = savanna_agent:notify(MetricGroup, 'req_put',     1),
    ok = savanna_agent:notify(MetricGroup, 'obj_len_get', 1024),
    ok = savanna_agent:notify(MetricGroup, 'obj_len_put', 2048),
    ok.

```


## License

savanna_agent's license is [Apache License Version 2.0](http://www.apache.org/licenses/LICENSE-2.0.html)
