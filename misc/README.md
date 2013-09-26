Miscellaneous utilities
=======================
This directory houses scripts and support programs for DGenerate.

ConcatYgraph
------------
ConcatYgraph concatenates individual ygraph files for the elements of a 1D
domain into a single ygraph file representing the whole domain.

Usage:

    java ConcatYgraph <nElems> > domain.yg

Choose `nElems` to be the number of elements in your domain.  Output is written
to standard out.
