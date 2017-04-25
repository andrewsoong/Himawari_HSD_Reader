!******************************************************************************%
! *
! *    Copyright (C) 2016 Simon Proud <simon.proud@physics.ox.ac.uk>
! *
! *    This source code is licensed under the GNU General Public License (GPL),
! *    Version 3.  See the file COPYING for more details.
! *
! ******************************************************************************/


module himawari_utils

	use himawari
	implicit none

	public	::	AHI_get_timeslot, &
				AHI_get_indir, &
				AHI_free_vals, &
				AHI_alloc_vals, &
				AHI_alloc_vals_data, &
				AHI_get_file_name, &
				AHI_file_exists

contains

integer function AHI_get_timeslot(filename,timeslot) result(status)

	character(len=*), intent(in)	::	filename
	character(len=*), intent(out)	::	timeslot

	integer pos

	pos		=	index(trim(filename),".DAT")
	if (pos .le. 0) then
		status	=	HIMAWARI_FAILURE
		return
	endif

	timeslot(1:8)	=	filename(pos-32:pos-32+8)
	timeslot(9:12)	=	filename(pos-23:pos-23+4)

	status	=	HIMAWARI_SUCCESS
	return

end function 		AHI_get_timeslot


integer function  AHI_get_indir(filename,indir) result(status)

	character(len=*), intent(in)	::	filename
	character(len=*), intent(out)	::	indir

	integer pos

	pos		=	index(trim(filename),"HS_H08_")
	if (pos .le. 0) then
		pos		=	index(trim(filename),"HS_H09_")
	endif
	if (pos .le. 0) then
		status	=	HIMAWARI_FAILURE
		return
	endif

	indir	=	filename(1:pos-1)

	status	=	HIMAWARI_SUCCESS
	return

end function 		AHI_get_indir

integer function	AHI_free_vals(ahi_main) result(status)

	type(himawari_t_struct), intent(inout)	::	ahi_main

	deallocate(ahi_main%ahi_data%lat)
	deallocate(ahi_main%ahi_data%lon)
	deallocate(ahi_main%ahi_data%vza)
	deallocate(ahi_main%ahi_data%vaa)
	deallocate(ahi_main%ahi_data%sza)
	deallocate(ahi_main%ahi_data%saa)
	deallocate(ahi_main%ahi_data%time)
	deallocate(ahi_main%ahi_data%indata)

	status	=	HIMAWARI_SUCCESS
	return

end function		AHI_free_vals


integer function	AHI_free_vals_data(ahi_data,verbose) result(status)

	type(himawari_t_data), intent(inout)	::	ahi_data
	logical, intent(in)							:: verbose

	deallocate(ahi_data%lat)
	deallocate(ahi_data%lon)
	deallocate(ahi_data%vza)
	deallocate(ahi_data%vaa)
	deallocate(ahi_data%sza)
	deallocate(ahi_data%saa)
	deallocate(ahi_data%time)
	deallocate(ahi_data%indata)

	status	=	HIMAWARI_SUCCESS
	return

end function		AHI_free_vals_data

integer function	AHI_alloc_vals_data(ahi_data,nchans,verbose) result(status)

	type(himawari_t_data), intent(inout)	::	ahi_data
	integer, intent(in)							::	nchans
	integer											::	i,length
	logical, intent(in)							:: verbose


	if (verbose) then
		write(*,*)"Number of bands to read:",nchans
	endif

	allocate(ahi_data%lat(HIMAWARI_IR_NLINES,HIMAWARI_IR_NCOLS))
	allocate(ahi_data%lon(HIMAWARI_IR_NLINES,HIMAWARI_IR_NCOLS))
	allocate(ahi_data%vza(HIMAWARI_IR_NLINES,HIMAWARI_IR_NCOLS))
	allocate(ahi_data%vaa(HIMAWARI_IR_NLINES,HIMAWARI_IR_NCOLS))
	allocate(ahi_data%sza(HIMAWARI_IR_NLINES,HIMAWARI_IR_NCOLS))
	allocate(ahi_data%saa(HIMAWARI_IR_NLINES,HIMAWARI_IR_NCOLS))
	allocate(ahi_data%time(HIMAWARI_IR_NLINES,HIMAWARI_IR_NCOLS))

	allocate(ahi_data%indata(HIMAWARI_IR_NLINES,HIMAWARI_IR_NCOLS,nchans))

	status	=	HIMAWARI_SUCCESS
	return

end function		AHI_alloc_vals_data

integer function	AHI_alloc_vals(ahi_main,verbose) result(status)

	type(himawari_t_struct), intent(inout)	::	ahi_main
	integer											::	i,length
	logical, intent(in)							:: verbose

!	ahi_main%ahi_data%n_bands	=	0
!	do i=1,16
!		if (ahi_main%inchans(i) .eq. 1) then
!			ahi_main%ahi_data%n_bands = ahi_main%ahi_data%n_bands+1
!		endif
!	end do

	if (ahi_main%ahi_data%n_bands <= 0) then
		write(*,*)"No bands are selected!"
		status	=	HIMAWARI_FAILURE
		return
	endif

	if (verbose) then
		write(*,*)"Number of bands to read:",ahi_main%ahi_data%n_bands
	endif

	allocate(ahi_main%ahi_data%lat(HIMAWARI_IR_NLINES,HIMAWARI_IR_NCOLS))
	allocate(ahi_main%ahi_data%lon(HIMAWARI_IR_NLINES,HIMAWARI_IR_NCOLS))
	allocate(ahi_main%ahi_data%vza(HIMAWARI_IR_NLINES,HIMAWARI_IR_NCOLS))
	allocate(ahi_main%ahi_data%vaa(HIMAWARI_IR_NLINES,HIMAWARI_IR_NCOLS))
	allocate(ahi_main%ahi_data%sza(HIMAWARI_IR_NLINES,HIMAWARI_IR_NCOLS))
	allocate(ahi_main%ahi_data%saa(HIMAWARI_IR_NLINES,HIMAWARI_IR_NCOLS))
	allocate(ahi_main%ahi_data%time(HIMAWARI_IR_NLINES,HIMAWARI_IR_NCOLS))

	allocate(ahi_main%ahi_data%indata(HIMAWARI_IR_NLINES,HIMAWARI_IR_NCOLS,ahi_main%ahi_data%n_bands))

	status	=	HIMAWARI_SUCCESS
	return

end function		AHI_alloc_vals

integer function	AHI_get_file_name(cnum, timeslot, satname, indir, outfile,verbose) result(status)

	integer, intent(in)			::	cnum
	character(len=*), intent(in)	::	timeslot
	character(len=*), intent(in)	::	satname
	character(len=*), intent(in)	::	indir
	character(len=*), intent(out)	::	outfile
	logical, intent(in)				:: verbose

	outfile=indir
	outfile	=	trim(outfile)//trim(satname)
	outfile	=	trim(outfile)//trim("_ahi_le1b_")
	select case(cnum)
		case(1)
			outfile	=	trim(outfile)//trim("b01")
		case(2)
			outfile	=	trim(outfile)//trim("b02")
		case(3)
			outfile	=	trim(outfile)//trim("b03")
		case(4)
			outfile	=	trim(outfile)//trim("b04")
		case(5)
			outfile	=	trim(outfile)//trim("b05")
		case(6)
			outfile	=	trim(outfile)//trim("b06")
		case(7)
			outfile	=	trim(outfile)//trim("b07")
		case(8)
			outfile	=	trim(outfile)//trim("b08")
		case(9)
			outfile	=	trim(outfile)//trim("b09")
		case(10)
			outfile	=	trim(outfile)//trim("b10")
		case(11)
			outfile	=	trim(outfile)//trim("b11")
		case(12)
			outfile	=	trim(outfile)//trim("b12")
		case(13)
			outfile	=	trim(outfile)//trim("b13")
		case(14)
			outfile	=	trim(outfile)//trim("b14")
		case(15)
			outfile	=	trim(outfile)//trim("b15")
		case(16)
			outfile	=	trim(outfile)//trim("b16")
		case default
			status	=	HIMAWARI_FAILURE
			return
	end select
	outfile	=	trim(outfile)//trim("_org_f_")
	outfile	=	trim(outfile)//trim(timeslot)
	outfile	=	trim(outfile)//trim(".bin")

	status	=	HIMAWARI_SUCCESS
	return

end function	 AHI_get_file_name



integer function	AHI_get_file_name_seg(cnum, seg, timeslot, satname, indir, outfile,verbose) result(status)

	integer, intent(in)				::	cnum
	integer, intent(in)				::	seg
	character(len=*), intent(in)	::	timeslot
	character(len=*), intent(in)	::	satname
	character(len=*), intent(in)	::	indir

	character(len=*), intent(out)	::	outfile
	logical, intent(in)				:: verbose
	character(len=3)					::	tstr

	outfile=indir
	outfile	=	trim(outfile)//trim("HS_H08_")
	outfile	=	trim(outfile)//trim(timeslot(1:8))
	outfile	=	trim(outfile)//trim("_")
	outfile	=	trim(outfile)//trim(timeslot(9:12))
	outfile	=	trim(outfile)//trim("_")

	select case(cnum)
		case(1)
			outfile	=	trim(outfile)//trim("B01")
		case(2)
			outfile	=	trim(outfile)//trim("B02")
		case(3)
			outfile	=	trim(outfile)//trim("B03")
		case(4)
			outfile	=	trim(outfile)//trim("B04")
		case(5)
			outfile	=	trim(outfile)//trim("B05")
		case(6)
			outfile	=	trim(outfile)//trim("B06")
		case(7)
			outfile	=	trim(outfile)//trim("B07")
		case(8)
			outfile	=	trim(outfile)//trim("B08")
		case(9)
			outfile	=	trim(outfile)//trim("B09")
		case(10)
			outfile	=	trim(outfile)//trim("B10")
		case(11)
			outfile	=	trim(outfile)//trim("B11")
		case(12)
			outfile	=	trim(outfile)//trim("B12")
		case(13)
			outfile	=	trim(outfile)//trim("B13")
		case(14)
			outfile	=	trim(outfile)//trim("B14")
		case(15)
			outfile	=	trim(outfile)//trim("B15")
		case(16)
			outfile	=	trim(outfile)//trim("B16")
		case default
			status	=	HIMAWARI_FAILURE
			return
	end select
	outfile	=	trim(outfile)//trim("_FLDK_R")
	if (cnum.eq.1.or.cnum.eq.2.or.cnum.eq.4) then
		outfile	=	trim(outfile)//trim("10_S")
	else if (cnum.eq.3) then
		outfile	=	trim(outfile)//trim("05_S")
	else
		outfile	=	trim(outfile)//trim("20_S")
	endif
	write(tstr,"(I2.2)")seg
	outfile	=	trim(outfile)//trim(tstr)
	outfile	=	trim(outfile)//trim("10.DAT")

	status	=	HIMAWARI_SUCCESS
	return

end function	 AHI_get_file_name_seg

integer function	AHI_file_exists(filename,verbose) result(status)

	character(len=*), intent(in)	::	filename
	logical, intent(in)				:: verbose
	logical exists

	inquire(file=filename, exist=exists)
	if (exists .eqv. .true.) then
  		status	=	HIMAWARI_SUCCESS;
	else
  		status	=	HIMAWARI_FAILURE;
  	endif
  	return
end function	 AHI_file_exists

end module himawari_utils
