!******************************************************************************%
! *
! *    Copyright (C) 2016 Simon Proud <simon.proud@physics.ox.ac.uk>
! *
! *    This source code is licensed under the GNU General Public License (GPL),
! *    Version 3.  See the file COPYING for more details.
! *
! ******************************************************************************/


!*******************************************************************************
! Module containing the Fortran interface to himawari_native_util.
!*******************************************************************************

module himawari

	implicit none

	private

	public ::	himawari_t_data, &
			      himawari_t_navdata, &
			      himawari_t_info, &
			      himawari_t_struct

   	! Type kind value
   	integer, parameter, public	::	ahi_byte		=	1
   	integer, parameter, public	::	ahi_sint		=	2
   	integer, parameter, public	::	ahi_lint		=	4
   	integer, parameter, public	::	ahi_sreal	=	4
   	integer, parameter, public	::	ahi_dreal	=	8


	! Fill values, equivalent to those in ORAC
	integer(kind=ahi_byte),  parameter, public	::	him_byte_fill_value		=	-127
	integer(kind=ahi_sint),  parameter, public	::	him_sint_fill_value		=	-32767
	integer(kind=ahi_lint),  parameter, public	::	him_lint_fill_value		=	-32767
	real(kind=ahi_sreal),    parameter, public	::	him_sreal_fill_value		=	-999.0
	real(kind=ahi_dreal),    parameter, public	::	him_dreal_fill_value		=	-999.0

	!V arious useful parameters for Himawari processing
	integer(kind=ahi_sint), parameter, public 	::	HIMAWARI_BOUNDS_FULL_DISK		=	0
	integer(kind=ahi_sint), parameter, public 	::	HIMAWARI_BOUNDS_ACTUAL_IMAGE	=	1
	integer(kind=ahi_sint), parameter, public 	::	HIMAWARI_BOUNDS_LINE_COLUMN	=	2
	integer(kind=ahi_sint), parameter, public 	::	HIMAWARI_BOUNDS_LAT_LON			=	3

	integer(kind=ahi_sint), parameter, public 	::	HIMAWARI_UNIT_CNT		=	0
	integer(kind=ahi_sint), parameter, public 	::	HIMAWARI_UNIT_RAD		=	1
	integer(kind=ahi_sint), parameter, public 	::	HIMAWARI_UNIT_RBT		=	2
	integer(kind=ahi_sint), parameter, public 	::	HIMAWARI_UNIT_BRT		=	3

	integer(kind=ahi_sint), parameter, public 	::	HIMAWARI_MAX_CHANS	=	16
	integer(kind=ahi_sint), parameter, public 	::	HIMAWARI_CHARLEN		=	512

	integer(kind=ahi_sint), parameter, public 	::	HIMAWARI_SUCCESS		=	0
	integer(kind=ahi_sint), parameter, public 	::	HIMAWARI_FAILURE		=	-1

	integer(kind=ahi_sint), parameter, public 	::	HIMAWARI_TRUE 			=	1
	integer(kind=ahi_sint), parameter, public 	::	HIMAWARI_FALSE 		=	0


	! Image sizes, assumed to be full disk
	integer(kind=ahi_sint), parameter, public 	::	HIMAWARI_IR_NLINES	=	5500
	integer(kind=ahi_sint), parameter, public 	::	HIMAWARI_IR_NCOLS		=	5500

	integer(kind=ahi_sint), parameter, public 	::	HIMAWARI_VIS_NLINES	=	11000
	integer(kind=ahi_sint), parameter, public 	::	HIMAWARI_VIS_NCOLS	=	11000

	integer(kind=ahi_sint), parameter, public 	::	HIMAWARI_HVI_NLINES	=	22000
	integer(kind=ahi_sint), parameter, public 	::	HIMAWARI_HVI_NCOLS	=	22000

	integer(kind=ahi_sint), parameter, public 	::	HIMAWARI_NCHANS		=	16

	! Definitions of physical constants. Used for calculating EBT from RAD
   ! In most cases we will use the values from the header instead
	real(kind=ahi_sreal), parameter, public 	:: HIMAWARI_lgtspd	=	299792458
	real(kind=ahi_sreal), parameter, public 	:: HIMAWARI_plkcons	=	6.62606957E-034
	real(kind=ahi_sreal), parameter, public 	:: HIMAWARI_bltcons	=	1.3806488E-023


!	Definitions of navigation parameters. Used for transforming line/col to lat/lon
	real(kind=ahi_dreal), parameter, public 	:: HIMAWARI_DEGTORAD	=	(4*atan (1.0))/180.0
	real(kind=ahi_dreal), parameter, public 	:: HIMAWARI_RADTODEG	=	180.0/(4*atan (1.0))
	real(kind=ahi_dreal), parameter, public 	:: HIMAWARI_SCLUNIT	=	1.525878906250000e-05


   ! Main data arrays for the image, geoloc etc
	type	::	himawari_t_data
		integer(kind=ahi_sint)				                     ::	memory_alloc_d
		integer(kind=ahi_sint)				                     ::	n_bands
		integer(kind=ahi_sint)				                     ::	n_lines
		integer(kind=ahi_sint)				                     ::	n_cols
		real(kind=ahi_dreal), DIMENSION(:,:), ALLOCATABLE	   ::	time(:, :)
		real(kind=ahi_sreal), DIMENSION(:,:), ALLOCATABLE	   ::	lat(:, :)
		real(kind=ahi_sreal), DIMENSION(:,:), ALLOCATABLE	   ::	lon(:, :)
		real(kind=ahi_sreal), DIMENSION(:,:), ALLOCATABLE	   ::	sza(:, :)
		real(kind=ahi_sreal), DIMENSION(:,:), ALLOCATABLE	   ::	saa(:, :)
		real(kind=ahi_sreal), DIMENSION(:,:), ALLOCATABLE	   ::	vza(:, :)
		real(kind=ahi_sreal), DIMENSION(:,:), ALLOCATABLE	   ::	vaa(:, :)
		real(kind=ahi_sreal), DIMENSION(:,:,:), ALLOCATABLE	::	indata(:, :, :)
	end type	himawari_t_data

	! Navigation parameters derived from the header file
	type ::	himawari_t_navdata
		real(kind=ahi_dreal)		::	subLon
		real(kind=ahi_dreal)		::	cfac
		real(kind=ahi_dreal)		::	lfac
		real(kind=ahi_dreal)		::	coff
		real(kind=ahi_dreal)		::	loff
		real(kind=ahi_dreal)		::	satDis
		real(kind=ahi_dreal)		::	eqtrRadius
		real(kind=ahi_dreal)		::	polrRadius
		real(kind=ahi_dreal)		::	projParam1
		real(kind=ahi_dreal)		::	projParam2
		real	(kind=ahi_dreal)	::	projParam3
		real(kind=ahi_dreal)		::	projParamSd
	end type	himawari_t_navdata

   ! Useful bits and bobs for determining the segment filenames
	type ::	himawari_t_info
		character(len=HIMAWARI_CHARLEN)	::	indir
		character(len=HIMAWARI_CHARLEN)	::	timeslot
		character(len=HIMAWARI_CHARLEN)	::	satname
	end type	himawari_t_info

   ! Main struct
	type	::	himawari_t_struct
		type(himawari_t_data)	::	ahi_data
		type(himawari_t_navdata)::	ahi_navdata
		type(himawari_t_info)	::	ahi_info
		integer(kind=ahi_sint)	::	inchans(HIMAWARI_MAX_CHANS)
		integer(kind=ahi_sint)	::	convert(HIMAWARI_MAX_CHANS)
	end type himawari_t_struct

end module himawari
