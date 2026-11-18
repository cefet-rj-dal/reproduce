Chiron: A Parallel Engine for Algebraic Scientific Workflows

This folder provides context and pointers for reproducing experiments and learning resources related to the Chiron workflow engine.

Paper
- Ogasawara, E., Dias, J., Silva, V., Chirigati, F., De Oliveira, D., Porto, F., Valduriez, P., Mattoso, M. Chiron: A parallel engine for algebraic scientific workflows. Concurrency and Computation: Practice and Experience, 2013. DOI: 10.1002/cpe.3032.

Objectives
- Present Chiron’s approach to parallel workflow execution in HPC environments backed by a workflow algebra for optimization, dynamic scheduling, and runtime steering.
- Enable data-parallel execution automatically from workflow specifications (parameter sweeps or fragmented input datasets) with runtime query provenance support.
- Provide references to code, documentation, and demonstration materials to guide replication and use.

Folder Contents
- Documentation pointers for Chiron’s paper, project site, and demo video.
- There are no experiment scripts or notebooks in this directory; for source code and full documentation, please use the links below.

Project Links
- Paper (DOI): https://dx.doi.org/10.1002/cpe.3032
- Project homepage: http://chironengine.sourceforge.net/index.php/home
- Video demonstration: https://youtu.be/iWiuqe9HoGc?si=V-PhkqSO1C09jrrn

Reproducing Experiments (high-level guidance)
- Prerequisites: Java (JDK), PostgreSQL (for provenance), Hadoop/MapReduce-style environment or MPJ (for parallelism), JDBC driver for PostgreSQL.
- Obtain Chiron’s source/binaries from the project homepage and follow its installation instructions.
- Configure database connectivity (JDBC) and HPC cluster (or MPJ runtime) per your environment.
- Load example workflows (from the official distribution) and execute them to validate setup; provenance should be recorded in PostgreSQL.

Notes
- This folder has no Jupyter notebooks or additional Markdown documents beyond this README. If Markdown guides are added later, they will be referenced here.

