savannadb_agent
===============

An original agent of `SavannaDB`, which is able to easily retrieve metrics and statistics for Erlang's apllications.

## Usage

```erlang

sample() ->
    %% ----------------------------------------------------------
    %% Launch an agent
    %% ----------------------------------------------------------
    ok = savanna_agent:start(ram_copies),
    ok = savanna_agent:sync_schemas(['savanna_manager_0@127.0.0.1',
                                     'savanna_manager_1@127.0.0.1']),

    %% ----------------------------------------------------------
    %% Create a schema, columns and a metric-group
    %% ----------------------------------------------------------
    Schema      = 'sample',
    MetricGroup = 'bucket',
    Window      = 30,
    Col_1 = 'counter_get',
    Col_2 = 'length_get',

    %% Create schema and columns
    ok = svc_tbl_schema:insert(#sv_schema{name = SchemaName,
                                          created_at = leo_date:now()}),
    ok = svc_tbl_column:insert(#?SV_COLUMN{id = {SchemaName, Col_1},
                                           schema_name = SchemaName,
                                           name = Col_1,
                                           type = ?COL_TYPE_COUNTER,
                                           created_at = leo_date:now()}),
    ok = svc_tbl_column:insert(#?SV_COLUMN{id = {SchemaName, Col_2},
                                           schema_name = SchemaName,
                                           name = Col_2,
                                           type = ?COL_TYPE_H_UNIFORM,
                                           constraint = [{?HISTOGRAM_CONS_SAMPLE, 3000}],
                                           created_at = leo_date:now()}),
    %% Create a metric-group
    ok = savanna_agent:create_metrics(Schema, MetricGroup, Window),

    %% ----------------------------------------------------------
    %% Notify events
    %% ----------------------------------------------------------
    ok = savanna_agent:notify(MetricGroup, Col_1, 1),
    ok = savanna_agent:notify(MetricGroup, Col_2, 16384),
    ok = savanna_agent:notify(MetricGroup, Col_2, 262144),
    ok = savanna_agent:notify(MetricGroup, Col_2, 524288),
    ok.

```

## License

savanna_agent's license is [Apache License Version 2.0](http://www.apache.org/licenses/LICENSE-2.0.html)

## Sponsors

LeoProject/LeoFS is sponsored by [Rakuten, Inc.](http://global.rakuten.com/corp/) and supported by [Rakuten Institute of Technology](http://rit.rakuten.co.jp/).
