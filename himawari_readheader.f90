!******************************************************************************%
! *
! *    Copyright (C) 2016 Simon Proud <simon.proud@physics.ox.ac.uk>
! *
! *    This source code is licensed under the GNU General Public License (GPL),
! *    Version 3.  See the file COPYING for more details.
! *
! ******************************************************************************/


!*******************************************************************************
! Module containing the Fortran interface to himawari_native_util.  Please see 
! the comments in the C code for descriptions of the various routines including
! their arguments and return values.
!*******************************************************************************


module himawari_readheader

	use himawari
	use himawari_headerinfo
	use iso_c_binding

	implicit none

	private

	public ::	AHI_readhdr_IR, \
			AHI_readhdr_VIS
	
	
contains
integer function AHI_readhdr_IR(filelun,ahi_hdr)result(status)
	type(himawari_t_IR_Header),intent(inout)	::	ahi_hdr
	integer,intent(in)						::	filelun
	
	integer ::	retval,fpos
	
	type(himawari_t_Basic_Info)		::	him_basic
	type(himawari_t_Data_Info)		::	him_data
	type(himawari_t_Proj_Info)		::	him_proj
	type(himawari_t_Navi_Info)		::	him_nav
	type(himawari_t_Calib_Info)		::	him_calib
	type(himawari_t_IR_Calib_Info)	::	him_chan_calib	
	type(himawari_t_InterCalib_Info)	::	him_interCalib
	type(himawari_t_Segm_Info)		::	him_seg
	type(himawari_t_NaviCorr_Info)	::	him_navcorr
	type(himawari_t_ObsTime_Info)		::	him_obstime
	type(himawari_t_Error_Info)		::	him_error
	type(himawari_t_Spare)			::	him_spare
	type(himawari_t_Correct_Table)	::	him_correct_table
	
	fpos	=	0
	
	call	fseek(filelun,fpos,0,retval)
	read(filelun)him_basic
	fpos	=	fpos + him_basic%BlockLen
	call	fseek(filelun,fpos,0,retval)
	
	read(filelun)him_data
	fpos	=	fpos + him_data%BlockLen
	call	fseek(filelun,fpos,0,retval)
	
	read(filelun)him_proj
	fpos	=	fpos + him_proj%BlockLen
	call	fseek(filelun,fpos,0,retval)
	
	read(filelun)him_nav
	fpos	=	fpos + him_nav%BlockLen
	call	fseek(filelun,fpos,0,retval)
	
	read(filelun)him_calib	
	read(filelun)him_chan_calib
	fpos	=	fpos + him_calib%BlockLen
	call	fseek(filelun,fpos,0,retval)
	
	read(filelun)him_interCalib
	fpos	=	fpos + him_interCalib%BlockLen
	call	fseek(filelun,fpos,0,retval)
	
	read(filelun)him_seg
	fpos	=	fpos + him_seg%BlockLen
	call	fseek(filelun,fpos,0,retval)
	
	read(filelun)him_navcorr
	fpos	=	fpos + him_navcorr%BlockLen
	call	fseek(filelun,fpos,0,retval)
	
	read(filelun)him_obstime
	fpos	=	fpos + him_obstime%BlockLen
	call	fseek(filelun,fpos,0,retval)
	
	read(filelun)him_error
	fpos	=	fpos + him_error%BlockLen
	call	fseek(filelun,fpos,0,retval)
	
	call	fseek(filelun,him_basic%totalHeaderLen,0,retval)
		
	ahi_hdr%him_basic		=	him_basic
	ahi_hdr%him_proj		=	him_proj
	ahi_hdr%him_nav		=	him_nav
	ahi_hdr%him_calib		=	him_calib
	ahi_hdr%him_chan_calib	=	him_chan_calib
	ahi_hdr%him_interCalib	=	him_interCalib
	ahi_hdr%him_seg		=	him_seg
	ahi_hdr%him_navcorr		=	him_navcorr
	ahi_hdr%him_obstime		=	him_obstime
	ahi_hdr%him_error		=	him_error
		
	status	=	HIMAWARI_SUCCESS
	return 
	
end function AHI_readhdr_IR
		
integer function AHI_readhdr_VIS(filelun,ahi_hdr)result(status)
	type(himawari_t_VIS_Header),intent(inout)	::	ahi_hdr
	integer,intent(in)						::	filelun
	
	integer ::	retval,fpos
	
	type(himawari_t_Basic_Info)		::	him_basic
	type(himawari_t_Data_Info)		::	him_data
	type(himawari_t_Proj_Info)		::	him_proj
	type(himawari_t_Navi_Info)		::	him_nav
	type(himawari_t_Calib_Info)		::	him_calib
	type(himawari_t_VIS_Calib_Info)	::	him_chan_calib	
	type(himawari_t_InterCalib_Info)	::	him_interCalib
	type(himawari_t_Segm_Info)		::	him_seg
	type(himawari_t_NaviCorr_Info)	::	him_navcorr
	type(himawari_t_ObsTime_Info)		::	him_obstime
	type(himawari_t_Error_Info)		::	him_error
	type(himawari_t_Spare)			::	him_spare
	type(himawari_t_Correct_Table)	::	him_correct_table
	
	call	fseek(filelun,0,0,retval)
	read(filelun)him_basic
	fpos	=	fpos + him_basic%BlockLen
	call	fseek(filelun,fpos,0,retval)
	
	read(filelun)him_data
	fpos	=	fpos + him_data%BlockLen
	call	fseek(filelun,fpos,0,retval)
	
	read(filelun)him_proj
	fpos	=	fpos + him_proj%BlockLen
	call	fseek(filelun,fpos,0,retval)
	
	read(filelun)him_nav
	fpos	=	fpos + him_nav%BlockLen
	call	fseek(filelun,fpos,0,retval)
	
	read(filelun)him_calib	
	read(filelun)him_chan_calib
	fpos	=	fpos + him_calib%BlockLen
	call	fseek(filelun,fpos,0,retval)
	
	read(filelun)him_interCalib
	fpos	=	fpos + him_interCalib%BlockLen
	call	fseek(filelun,fpos,0,retval)
	
	read(filelun)him_seg
	fpos	=	fpos + him_seg%BlockLen
	call	fseek(filelun,fpos,0,retval)
	
	read(filelun)him_navcorr
	fpos	=	fpos + him_navcorr%BlockLen
	call	fseek(filelun,fpos,0,retval)
	
	read(filelun)him_obstime
	fpos	=	fpos + him_obstime%BlockLen
	call	fseek(filelun,fpos,0,retval)
	
	read(filelun)him_error
	fpos	=	fpos + him_error%BlockLen
	call	fseek(filelun,fpos,0,retval)
	
	call	fseek(filelun,him_basic%totalHeaderLen,0,retval)
	
	ahi_hdr%him_basic		=	him_basic
	ahi_hdr%him_proj		=	him_proj
	ahi_hdr%him_nav		=	him_nav
	ahi_hdr%him_calib		=	him_calib
	ahi_hdr%him_chan_calib	=	him_chan_calib
	ahi_hdr%him_interCalib	=	him_interCalib
	ahi_hdr%him_seg		=	him_seg
	ahi_hdr%him_navcorr		=	him_navcorr
	ahi_hdr%him_obstime		=	him_obstime
	ahi_hdr%him_error		=	him_error
	
	status	=	HIMAWARI_SUCCESS
	return 
	
end function AHI_readhdr_VIS

end module himawari_readheader
