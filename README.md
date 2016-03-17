kct
===

Helper to run common test from the Erlang shell

Usage examples
--------------
```erlang

   %% Run all ct suites belonging to the applicaiton
   kct:run().

   %% Run suite1_SUITE
   kct:run(suite1).

   %% Run suite1_SUITE and suite2_SUITE
   kct:run([suite1, suite2]).

   %% Run testcase1 in suite1_SUITE
   kct:run(suite1, testcase1).

   %% Run testcase1 and testcase2 in suite1_SUITE
   kct:run(suite1, [testcase1, testcase2]).
```
