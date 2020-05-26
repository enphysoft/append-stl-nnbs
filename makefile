
FC=ifort
# FC=gfortran # gfortran complins some grammatical difference.

srcroot=append-stl-nnbs
srcfile=$(srcroot)$(version).f90
exefile=$(srcroot).x
OBJ=fufs.o math.o   mathstl.o

%.o: %.f90
	$(FC) $(FCOPT) -c $?
%.o: %.f
	$(FC) $(FCOPT) -c $?

cmp: $(OBJ)
	@echo
	@echo "#! Compiling $(srcfile) using $(FC) ... "
	@echo
	$(FC) $(srcfile)  $(OBJ) -o   $(exefile)
	@echo
	@echo "#! Compiling $(srcfile) done! \n"

run:
	./$(exefile)

clean:
	rm -rf *.o *.mod *.x *.exc a.out 

all: clean cmp run 
