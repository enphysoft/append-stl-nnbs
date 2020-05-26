# append stl nearest neighbors (nnbs)
## Objective
This package aims to append the three nearest neighbors to each facet of a stereolithography (STL) file.

## File list
- Source code
  - append-stl-nnbs.f90 - the main f90 program 
- Library files
  - fufs.f90 - includes frequently used format styles
  - math.f90 - pure math functions including operator routines
  - mathstl.f90 - basic math functions for STL format and data structures
- Make Utility 
  - makefile - the _makefile_ used to compile the source codes and generate an executable file, e.g., append-stl-nnbs.x
- Input STL files
  - hinge.stl - input stl-file used as the first argument, i.e., $ append-stl-nnbs.x hinge.stl, to generate hinge_nnbs.stl.
  - STL_INPUT.stl - default input stl-file if there is no argument, i.e., $ append-stl-nnbs.x, which will generate STL_INPUT_NNBS.stl.
- Output STL files: as explained above
  - hinge_nnbs.stl
  - STL_INPUT_NNBS.stl
- Output Analysis files
  - NNBcheck.dat - to compare two vertices, common to two edges of two contacting neighbors
  - NNBfacet.dat - to list three contacting facets (i.e., nearest neighbors) to a specific facet.
  - NNBindex.dat - to list the full nearest neighbors with vertices paired. 
  - NNBlists.dat - to list the three nearest neighbors to a specific fact (with its vertex coordinates).
  
## Compile and run
- To clean 
```bash
$ make clean 
``` 
- To compile
```bash
$ make 
```
- To run
```bash
$ make run 
```
