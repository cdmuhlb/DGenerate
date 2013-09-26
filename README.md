DGenerate
=========

DGenerate is a parallel Discontinuous Galerkin solver implemented using actors.
It currently solves 1D flux-conservative problems, including advection and wave
equations.  Inter-node parallelism is achieved using Akka clustering.

Dependencies
------------
DGenerate is written in Scala and requires an SBT launcher compatible with
version 0.13.0 and a Java SE 7 JVM.

Usage
-----
Currently, some configuration is specified in `application.conf`, while other
options are hard-coded in `Main.scala`.  Nodes are started by invoking

    sbt run <hostname> <port>

For a single-node run, use `127.0.0.1` for `<hostname>` and `2552` for `<port>`.
Additionally, the directory `/tmp/harvest` must exist (this restriction will
obviously be addressed in the future).
