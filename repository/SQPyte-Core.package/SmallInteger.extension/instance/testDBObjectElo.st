*SQPyte-Core
testDBObjectElo
    | benchmark time |
    
    DBObject Mode: 2.
    "DBObject DBFileName: 'objects.db'."
    benchmark := EloBenchmark new.
    benchmark setUp.
    time := [benchmark benchElo] timeToRun.
    "DBObject close."
    ^ time.