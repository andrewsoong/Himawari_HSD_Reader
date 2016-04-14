!******************************************************************************%
! *
! *    Copyright (C) 2016 Simon Proud <simon.proud@physics.ox.ac.uk>
! *
! *    This source code is licensed under the GNU General Public License (GPL),
! *    Version 3.  See the file COPYING for more details.
! *
! ******************************************************************************/


!*******************************************************************************
! This stores all the information that can be read from the HSD header struct
!*******************************************************************************


module himawari_headerinfo

	use himawari
	use iso_c_binding

	implicit none

	private

	public ::	himawari_t_Basic_Info, \
			himawari_t_Data_Info, \
			himawari_t_Proj_Info, \
			himawari_t_Navi_Info, \
			himawari_t_Calib_Info, \
			himawari_t_IR_Calib_Info, \
			himawari_t_VIS_Calib_Info, \
			himawari_t_InterCalib_Info, \
			himawari_t_Segm_Info, \
			himawari_t_NaviCorr_Info, \
			himawari_t_ObsTime_Info, \
			himawari_t_Error_Info, \
			himawari_t_Spare, \
			himawari_t_Correct_Table, \
			himawari_t_IR_Header, \
			himawari_t_VIS_Header


   ! Basic info regarding the file and processing
type	::	himawari_t_Basic_Info
	integer(kind=ahi_byte)	::	HeaderNum
	integer(kind=ahi_sint)	::	BlockLen
	integer(kind=ahi_sint)	::	header_Num
	integer(kind=ahi_byte)	::	byteOrder
	character*16		::	satName
	character*16		::	proName
	character*4		::	ObsType1
	character*2		::	ObsType2
	integer(kind=ahi_sint)	::	TimeLine
	real(kind=ahi_dreal)	::	ObsStartTime
	real(kind=ahi_dreal)	::	ObsEndTime
	real(kind=ahi_dreal)	::	fileCreationMjd
	integer(kind=ahi_lint)	::	totalHeaderLen
	integer(kind=ahi_lint)	::	dataLen
	integer(kind=ahi_byte)	::	qflag1
	integer(kind=ahi_byte)	::	qflag2
	integer(kind=ahi_byte)	::	qflag3
	integer(kind=ahi_byte)	::	qflag4
	character*32		::	verName
	character*128		::	fileName
	character*40		::	spare
	
end type	himawari_t_Basic_Info


	! Some more basic info, mostly unused by us
type	::	himawari_t_Data_Info
	integer(kind=ahi_byte)	::	HeaderNum
	integer(kind=ahi_sint)	::	BlockLen
	integer(kind=ahi_sint)	::	bitPix
	integer(kind=ahi_sint)	::	nPix
	integer(kind=ahi_sint)	::	nLin
	integer(kind=ahi_byte)	::	comp
	character*40		::	spare
end type	himawari_t_Data_Info

   ! Projection info, very useful for geoprocessing
type	::	himawari_t_Proj_Info
	integer(kind=ahi_byte) 	::	HeaderNum
	integer(kind=ahi_sint)	::	BlockLen
	real(kind=ahi_dreal)	::	subLon
	integer(kind=ahi_lint)	::	cfac
	integer(kind=ahi_lint)	::	lfac
	real(kind=ahi_sreal)	::	coff
	real(kind=ahi_sreal)	::	loff
	real(kind=ahi_dreal)	::	satDis
	real(kind=ahi_dreal)	::	eqtrRadius
	real(kind=ahi_dreal)	::	polrRadius
	real(kind=ahi_dreal)	::	projParam1
	real(kind=ahi_dreal)	::	projParam2
	real(kind=ahi_dreal)	::	projParam3
	real(kind=ahi_dreal)	::	projParamSd
	integer(kind=ahi_sint)  ::	resampleKind
	integer(kind=ahi_sint)	::	resampleSize
	character*40		::	spare
end type himawari_t_Proj_Info

   ! Could also be used for geoproc, but we don't bother.
   ! Most of this is unnecessary when using the previous.
type	::	himawari_t_Navi_Info
	integer(kind=ahi_byte) 	::	HeaderNum
	integer(kind=ahi_sint)	::	BlockLen
	real(kind=ahi_dreal)	::	navMjd
	real(kind=ahi_dreal)	::	sspLon
	real(kind=ahi_dreal)	::	sspLat
	real(kind=ahi_dreal)	::	satDis
	real(kind=ahi_dreal)	::	nadirLon
	real(kind=ahi_dreal)	::	nadirLat
	real(kind=ahi_dreal)	::	sunPos_x
	real(kind=ahi_dreal)	::	sunPos_y
	real(kind=ahi_dreal)	::	sunPos_z
	real(kind=ahi_dreal)	::	moonPos_x
	real(kind=ahi_dreal)	::	moonPos_y
	real(kind=ahi_dreal)	::	moonPos_z
	character*40		::	spare
end type himawari_t_Navi_Info

   ! Main calibration structure for each band
type	::	himawari_t_Calib_Info
	integer(kind=ahi_byte) 	::	HeaderNum
	integer(kind=ahi_sint)	::	BlockLen
	integer(kind=ahi_sint)	::	bandNo
	real(kind=ahi_dreal)	::	waveLen
	integer(kind=ahi_sint)	::	bitPix
	integer(kind=ahi_sint)	::	errorCount
	integer(kind=ahi_sint)	::	outCount
!	/* count-radiance conversion equation */
	real(kind=ahi_dreal)	::	gain_cnt2rad
	real(kind=ahi_dreal)	::	cnst_cnt2rad
end type himawari_t_Calib_Info

   ! Specific calibration info for the IR bands
type	::	himawari_t_IR_Calib_Info
	real(kind=ahi_dreal)	::	rad2btp_c0
	real(kind=ahi_dreal)	::	rad2btp_c1
	real(kind=ahi_dreal)	::	rad2btp_c2
	real(kind=ahi_dreal)	::	btp2rad_c0
	real(kind=ahi_dreal)	::	btp2rad_c1
	real(kind=ahi_dreal)	::	btp2rad_c2
	real(kind=ahi_dreal)	::	lightSpeed
	real(kind=ahi_dreal)	::	planckConst
	real(kind=ahi_dreal)	::	bolzConst
	character*40		::	spare
end type himawari_t_IR_Calib_Info

   ! Specific valib info for the VIS/NIR bands
type	::	himawari_t_VIS_Calib_Info
	real(kind=ahi_dreal)	::	rad2albedo
	character*104		::	spare
end type himawari_t_VIS_Calib_Info

!	/*  --------------------------------------
!	version 1.0
!		11 Ancillary text
!		12 Spare
!    version 1.1
!		11 Radiance valid range of GSICS Calibration Coefficients
!			(upper limit)
!		12 Radiance valid range of GSICS Calibration Coefficients
!			(lower limit)
!		13 File name of GSICS Correction
!		14 Spare
!	version 1.2
!		3 GSICS calibration coefficient (Intercept)
!		4 GSICS calibration coefficient (Slope)
!		5 GSICS calibration coefficient (Quadratic term)
!		6 Radiance bias for standard scene
!		7 Uncertainty of radiance bias for standard scene
!		8 Radiance for standard scene
!	-------------------------------------- */

   !We don't use this
type	::	himawari_t_InterCalib_Info
	integer(kind=ahi_byte) 	::	HeaderNum
	integer(kind=ahi_sint)	::	BlockLen
	real(kind=ahi_dreal)	::	gsicsCorr_C
	real(kind=ahi_dreal)	::	gsicsCorr_C_er
	real(kind=ahi_dreal)	::	gsicsCorr_1
	real(kind=ahi_dreal)	::	gsicsCorr_1_er
	real(kind=ahi_dreal)	::	gsicsCorr_2
	real(kind=ahi_dreal)	::	gsicsCorr_2_er
	real(kind=ahi_dreal)	::	gsicsBias
	real(kind=ahi_dreal)	::	gsicsUncert
	
	character*64		::	gsicsCorrInfo
	character*128		::	spare
end type himawari_t_InterCalib_Info

   ! Per-segment info. Is constant so we ignore it
type	::	himawari_t_Segm_Info
	integer(kind=ahi_byte) 	::	HeaderNum
	integer(kind=ahi_sint)	::	BlockLen
	integer(kind=ahi_byte)	::	totalSegNum
	integer(kind=ahi_byte)	::	segSeqNo
	integer(kind=ahi_sint)	::	strLineNo
	character*40		::	spare
end type himawari_t_Segm_Info

   ! Not used by us, relates to L1.0 -> L1.5 proc
type	::	himawari_t_NaviCorr_Info
	integer(kind=ahi_byte) 	::	HeaderNum
	integer(kind=ahi_sint)	::	BlockLen
	real(kind=ahi_sreal)	::	RoCenterColumn
	real(kind=ahi_sreal)	::	RoCenterLine
	real(kind=ahi_dreal)	::	RoCorrection
	integer(kind=ahi_sint)	::	correctNum
	integer(kind=ahi_sint)	::	lineNo
	real(kind=ahi_sreal)	::	columnShift
	real(kind=ahi_sreal)	::	lineShift
	character*50		::	spare
end type himawari_t_NaviCorr_Info

   ! Observation info, useful for computing SZA/SAA
type	::	himawari_t_ObsTime_Info
	integer(kind=ahi_byte) 	::	HeaderNum
	integer(kind=ahi_sint)	::	BlockLen
	integer(kind=ahi_sint)	::	obsNum
	integer(kind=ahi_sint)	::	lineNo
	real(kind=ahi_dreal)	::	obsMJD
	character*70		::	spare
end type himawari_t_ObsTime_Info

   ! We ignore this
type	::	himawari_t_Error_Info
	integer(kind=ahi_byte)	::	HeaderNum
	integer(kind=ahi_sint)	::	BlockLen
	integer(kind=ahi_sint)	::	errorNum
	integer(kind=ahi_sint)	::	lineNo
	integer(kind=ahi_sint)  ::	errPixNum
	character*40		::	spare
end type himawari_t_Error_Info

   ! For future use
type	::	himawari_t_Spare
	integer(kind=ahi_byte) 	::	HeaderNum
	integer(kind=ahi_sint)	::	BlockLen
	character*40		::	spare
end type himawari_t_Spare

!/* navigation correction information table */
type	::	himawari_t_Correct_Table
	character		::	flag
	integer(kind=ahi_sint)	::	startLineNo
	integer(kind=ahi_sint)	::	lineNum
	real(kind=ahi_sreal)	::	cmpCoff
	real(kind=ahi_sreal)	::	cmpLoff
end type himawari_t_Correct_Table


   ! Header structure for the IR channels
type	::	himawari_t_IR_Header
	type(himawari_t_Basic_Info)		::	him_basic
	type(himawari_t_Data_Info)			::	him_data
	type(himawari_t_Proj_Info)			::	him_proj
	type(himawari_t_Navi_Info)			::	him_nav
	type(himawari_t_Calib_Info)		::	him_calib
	type(himawari_t_IR_Calib_Info)	::	him_chan_calib
	type(himawari_t_InterCalib_Info)	::	him_interCalib
	type(himawari_t_Segm_Info)			::	him_seg
	type(himawari_t_NaviCorr_Info)	::	him_navcorr
	type(himawari_t_ObsTime_Info)		::	him_obstime
	type(himawari_t_Error_Info)		::	him_error
	type(himawari_t_Spare)				::	him_spare
	type(himawari_t_Correct_Table)	::	him_correct_table
end type	himawari_t_IR_Header

   ! Header structure for the VIS channels
type	::	himawari_t_VIS_Header
	type(himawari_t_Basic_Info)		::	him_basic
	type(himawari_t_Data_Info)			::	him_data
	type(himawari_t_Proj_Info)			::	him_proj
	type(himawari_t_Navi_Info)			::	him_nav
	type(himawari_t_Calib_Info)		::	him_calib
	type(himawari_t_VIS_Calib_Info)	::	him_chan_calib
	type(himawari_t_InterCalib_Info)	::	him_interCalib
	type(himawari_t_Segm_Info)			::	him_seg
	type(himawari_t_NaviCorr_Info)	::	him_navcorr
	type(himawari_t_ObsTime_Info)		::	him_obstime
	type(himawari_t_Error_Info)		::	him_error
	type(himawari_t_Spare)				::	him_spare
	type(himawari_t_Correct_Table)	::	him_correct_table
end type	himawari_t_VIS_Header

end module himawari_headerinfo
			
