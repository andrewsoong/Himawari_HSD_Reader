#******************************************************************************%
# *
# *    Copyright (C) 2016 Simon Proud <simon.proud@physics.ox.ac.uk>
# *
# *    This source code is licensed under the GNU General Public License (GPL),
# *    Version 3.  See the file COPYING for more details.
# *
# ******************************************************************************

.SUFFIXES: .c .f90

# Change this to point to your local libraries for HDF5 and NetCDF
LIBDIR=/home/proud/Desktop/ORAC/Libraries

INCDIRS	+= -I$(LIBDIR)/hdf5/include -I$(LIBDIR)/ncdff/include -I$(LIBDIR)/ncdf4/include
LIBDIRS 	+= -L$(LIBDIR)/hdf5/lib     -L$(LIBDIR)/ncdff/lib -L$(LIBDIR)/ncdf4/lib
LINKS+= -lhdf5 -lhdf5_fortran -lhdf5hl_fortran -lnetcdf -lnetcdff -lm -fopenmp

OBJECTS = himawari.o \
		himawari_headerinfo.o \
		himawari_readheader.o \
		himawari_utils.o \
		himawari_nav.o \
		himawari_readwrite.o

SOLOBS = solpos.o \

CC=gcc
F90=gfortran

#CC=icc
#F90=ifort

CFLAGS=-g -O2 -ffree-line-length-0 -cpp -fopenmp -I. #-DVERBOSE
F90FLAGS=-g -O2 -I. -ffree-line-length-0 -fopenmp -cpp # -DVERBOSE

#CFLAGS=-g -O2 -fpp -align rec1byte -warn noalign  -I.#-DVERBOSE
#F90FLAGS=-g -O2 -I. -align rec1byte -warn noalign  -fpp #-DVERBOSE


#%.o: %.c $(DEPS)
#	$(CC) $(CFLAGS) -c -o $@ $<

all: solar.a \
	AHI \
	libhimawari_util.a

solar.a: $(SOLOBS)
	ar -rs solar_util.a $(SOLOBS)


libhimawari_util.a: $(OBJECTS) $(SOLOBS)
	ar -rs libhimawari_util.a $(OBJECTS) $(SOLOBS)

AHI: himawari.o himawari_headerinfo.o himawari_readheader.o himawari_utils.o himawari_nav.o himawari_readwrite.o AHI_Example.o libhimawari_util.a solar_util.a
	$(F90) -o AHI himawari.o $(F90FLAGS) himawari_headerinfo.o himawari_readheader.o himawari_utils.o himawari_nav.o himawari_readwrite.o  AHI_Example.o libhimawari_util.a solar_util.a  $(INCDIRS) $(LIBDIRS) $(LINKS) #-DVERBOSE

clean:
	rm -f *.a *.o *.mod AHI



.c.o:
	$(CC) $(CCFLAGS) $(INCDIRS) -c -o $*.o $<

%.o : %.mod

.f90.o:
	$(F90) -o $*.o -c $(F90FLAGS) $(INCDIRS) $<

