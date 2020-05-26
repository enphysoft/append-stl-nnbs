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
  - hinge.stl
  - STL_INPUT.stl
- Output STL files
  - hinge_nnbs.stl
  - STL_INPUT_NNBS.stl
- Output Analysis files
  - NNBcheck.dat
  - NNBfacet.dat
  - NNBindex.dat
  - NNBlists.dat
 
