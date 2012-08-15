This is an implementation of a decompositional partial-order causal-link planner. Most of the implementation adheres to this paper:

Young, R.M., Pollack, M.E. 1994. Decomposition and causality in partial-order planning. In Proceedings of the 2nd International Conference on Artificial Intelligence and Planning Systems.

In addition, the planner keeps a simple ontology which checks legality of variable binding.

You need Scala 2.9.0 or higher to compile and run the code.

Run /src/test/TestDecompPlanning to run the planner.

See some sample planning problems in the planfiles folder, including:
dpocl1.act, dpocl1.prob, dpocl1.decomp
game1.act, game1.prob, game1.decomp
game2.act, game2.prob, game2.decomp