# simulator

Build:
stack build

Run:
stack exec simulator-exe programs/<program> <prediction_method> <caches> <cache_policy> <rob_size> <pipeline_width>

Parameters:
prediction_method: two_bit, two_level, local
caches: no_cache, l1, l1l2
cache_policy: lru, mru, fifo
rob_size: Any integer > 1
pipeline_width: Any integer > 0

