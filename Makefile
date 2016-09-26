#-*-makefile-*-
COMP = gfortran
FFLAGS = -O2 -w 

SOLVR = ./DASPK

OBJS = $(SOLVR)/solver/ddaspk.o $(SOLVR)/solver/daux.o $(SOLVR)/solver/dlinpk.o $(SOLVR)/preconds/dbanpre.o 

MOV6 = movcol6.o  movcol6mod.o $(OBJS) 

MOV6 : $(MOV6)
	$(COMP) $(FFLAGS) -o run.exe $(MOV6)

movcol6.o: movcol6.f90 movcol6mod.mod 
	$(COMP) $(FFLAGS) -c movcol6.f90

movcol6mod.mod movcol6mod.o: movcol6mod.f90  
	$(COMP) $(FFLAGS) -c movcol6mod.f90	


.PHONY : clean
clean :
	-rm run.exe $(OBJS) movcol6mod.mod


.f.o: ; $(COMP) $(FFLAGS) -c $*.f -o $*.o
.f90.o: ; $(COMP) $(FFLAGS) -c $*.f90 -o $*.o
