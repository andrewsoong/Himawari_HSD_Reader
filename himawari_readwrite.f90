!******************************************************************************%
! *
! *    Copyright (C) 2016 Simon Proud <simon.proud@physics.ox.ac.uk>
! *
! *    This source code is licensed under the GNU General Public License (GPL),
! *    Version 3.  See the file COPYING for more details.
! *
! ******************************************************************************/


module himawari_readwrite
	use himawari
	use himawari_utils
	use himawari_navigation
	use himawari_headerinfo
	use himawari_readheader
	use netcdf
	use omp_lib

	implicit none

	public	::	AHI_Main_Read, \
				AHI_Setup_Read_Chans, \
				AHI_readchan, \
				AHI_resample_hres, \
				AHI_SavetoNCDF, \
				AHI_NCDF_check

contains


integer function AHI_Main_Read(filename,geofile,ahi_data2,n_bands,band_ids,do_not_alloc,do_geo,predef_geo,verbose) result(status)

	character(len=*), intent(in)			::	filename
	character(len=*), intent(in)			::	geofile
	type(himawari_t_data), intent(inout)::	ahi_data2
	integer,intent(in)						::	n_bands
	integer,intent(in),dimension(16)		::	band_ids
	integer,intent(in)						::	do_not_alloc
	integer,intent(in)						::	do_geo
	logical,intent(in)						:: predef_geo
	logical,intent(in)						:: verbose

	character(len=HIMAWARI_CHARLEN)		::	satname
	character(len=HIMAWARI_CHARLEN)		::	timeslot
	character(len=HIMAWARI_CHARLEN)		::	indir
	integer,dimension(HIMAWARI_NCHANS)	::	inbands
	type(himawari_t_struct)					::	ahi_main

	integer		::	i,retval

	satname	=	"himawari8"
	ahi_main%ahi_data%memory_alloc_d	=	do_not_alloc
	ahi_main%ahi_data%n_bands		=	n_bands

	retval	=	AHI_get_timeslot(filename,timeslot)
	retval	=	AHI_get_indir(filename,indir)

	ahi_main%ahi_info%indir		=	indir
	ahi_main%ahi_info%timeslot	=	timeslot
	ahi_main%ahi_info%satname	=	satname

	write(*,*)"Reading AHI data for ",trim(ahi_main%ahi_info%timeslot(1:12))

	ahi_main%inchans(:)	=	0

	do i=1,n_bands
		if(band_ids(i)>0 .and. band_ids(i).le.HIMAWARI_NCHANS)ahi_main%inchans(band_ids(i))	=	1
	enddo
	ahi_main%convert=HIMAWARI_UNIT_RBT

	if (verbose) then
		write(*,*)"	-	Will process bands: ",ahi_main%inchans
!		do i=1,HIMAWARI_NCHANS+1
!			if (ahi_main%inchans(i).eq.1)write(*,*)i,"	"
!		enddo
	endif

	if (ahi_main%ahi_data%memory_alloc_d.ne.1) then
		retval	=	AHI_alloc_vals(ahi_main,verbose)
!		retval	=	AHI_alloc_vals_data(ahi_main%ahi_data,n_bands)
		if (retval.ne.HIMAWARI_SUCCESS) then
			status	=	HIMAWARI_FAILURE
			return
		endif
	endif

	retval	=	AHI_Setup_Read_Chans(ahi_main,verbose)
	if (retval.ne.HIMAWARI_SUCCESS) then
		status	=	HIMAWARI_FAILURE
		return
	endif
	if (do_geo.eq.1) then
		if (.not. predef_geo) then
			if(verbose)print*,"Computing lat/lon and satellite angles"
			retval	=	AHI_Pix2Geo(ahi_main,verbose)
			if (retval.ne.HIMAWARI_SUCCESS) then
				status	=	HIMAWARI_FAILURE
				return
			endif
			retval	=	AHI_calc_satangs(ahi_main,verbose)
			if (retval.ne.HIMAWARI_SUCCESS) then
				status	=	HIMAWARI_FAILURE
				return
			endif
		else
			if(verbose)print*,"Retrieving lat/lon and satellite angles from file"
			retval	=	AHI_Retrieve_Predef_Geo(ahi_main,geofile,verbose)
			if (retval.ne.HIMAWARI_SUCCESS) then
				status	=	HIMAWARI_FAILURE
				return
			endif
		endif
		retval	=	AHI_Calctime(ahi_main,verbose)
		if (retval.ne.HIMAWARI_SUCCESS) then
			status	=	HIMAWARI_FAILURE
			return
		endif
	endif

	ahi_data2	=	ahi_main%ahi_data

	if (ahi_main%ahi_data%memory_alloc_d.ne.1) then
		retval	=	AHI_free_vals(ahi_main)
		if (retval.ne.HIMAWARI_SUCCESS) then
			status	=	HIMAWARI_FAILURE
			return
		endif
	endif

	status	=	HIMAWARI_SUCCESS
	return

end function AHI_Main_Read

integer function AHI_Retrieve_Predef_Geo(ahi_main,geofile,verbose) result(status)
	use netcdf
	implicit none

	type(himawari_t_struct), intent(inout)		::	ahi_main
	character(len=*), intent(in)			      ::	geofile
	logical,intent(in)								:: verbose

	integer :: ncid, varid
	call AHI_NCDF_check( nf90_open(geofile, NF90_NOWRITE, ncid) )
	call AHI_NCDF_check( nf90_inq_varid(ncid, "Lat", varid) )
	call AHI_NCDF_check( nf90_get_var(ncid, varid, ahi_main%ahi_data%lat) )
	call AHI_NCDF_check( nf90_inq_varid(ncid, "Lon", varid) )
	call AHI_NCDF_check( nf90_get_var(ncid, varid, ahi_main%ahi_data%lon) )
	call AHI_NCDF_check( nf90_inq_varid(ncid, "VZA", varid) )
	call AHI_NCDF_check( nf90_get_var(ncid, varid, ahi_main%ahi_data%vza) )
	call AHI_NCDF_check( nf90_inq_varid(ncid, "VAA", varid) )
	call AHI_NCDF_check( nf90_get_var(ncid, varid, ahi_main%ahi_data%vaa) )

end function AHI_Retrieve_Predef_Geo


integer function AHI_Setup_Read_Chans(ahi_main,verbose) result(status)

	type(himawari_t_struct), intent(inout)		::	ahi_main
	logical,intent(in)								:: verbose

	character(HIMAWARI_CHARLEN)				::	fname
	real(kind=ahi_sreal), DIMENSION(:,:), ALLOCATABLE::	tdata2

	integer,dimension(10)					::	segpos_ir
	integer,dimension(10)					::	segpos_vi
	integer,dimension(10)					::	segpos_hv
	integer,dimension(10)					::	segpos

	integer	::	i,j,minseg,maxseg
	integer	::	retval,bandpos
	integer	::	segdel,segdel_ir,segdel_vi,segdel_hv
	integer	::	startl,endl
	integer 	::	xsize,ysize

	segdel_ir	=	550
	segpos_ir	=	(/1, 551,1101,1651,2201,2751, 3301, 3851, 4401, 4951/)
	segdel_vi	=	1100
	segpos_vi	=	(/1,1101,2201,3301,4401,5501, 6601, 7701, 8801, 9901/)
	segdel_hv	=	2200
	segpos_hv	=	(/1,2201,4401,6601,8801,11001,13201,15401,17601,19801/)

	if (verbose) then
		write(*,*)"Reading image data"
	endif

	bandpos	=	1
	minseg	=	1
	maxseg	=	10

	do i=1,HIMAWARI_NCHANS+1
		if (ahi_main%inchans(i)==1) then
			if (verbose) then
				write(*,*)"Reading data for channel",i
			endif
			if (i.eq.1.or.i.eq.2.or.i.eq.4) then
				allocate(tdata2(HIMAWARI_VIS_NLINES,HIMAWARI_VIS_NCOLS))
				xsize	=	HIMAWARI_VIS_NLINES
				ysize	=	HIMAWARI_VIS_NCOLS
				segdel	=	segdel_vi
				segpos	=	segpos_vi
			else if (i.eq.3) then
				allocate(tdata2(HIMAWARI_HVI_NLINES,HIMAWARI_HVI_NCOLS))
				xsize	=	HIMAWARI_HVI_NLINES
				ysize	=	HIMAWARI_HVI_NCOLS
				segdel	=	segdel_hv
				segpos	=	segpos_hv
			else
				allocate(tdata2(HIMAWARI_IR_NLINES,HIMAWARI_IR_NCOLS))
				xsize	=	HIMAWARI_IR_NLINES
				ysize	=	HIMAWARI_IR_NCOLS
				segdel	=	segdel_ir
				segpos	=	segpos_ir
			endif
			do j=minseg,maxseg
				if (verbose) then
					write(*,*)"	-	Reading segment number",j
				endif
				if (j.eq.0) then
					retval	=	AHI_get_file_name(i,ahi_main%ahi_info%timeslot,ahi_main%ahi_info%satname,ahi_main%ahi_info%indir,fname,verbose)
				else
					retval	=	AHI_get_file_name_seg(i,j,ahi_main%ahi_info%timeslot,ahi_main%ahi_info%satname,ahi_main%ahi_info%indir,fname,verbose)
				endif
				if (retval.ne.HIMAWARI_SUCCESS) then
					write(*,*)"Cannot get filename for band: ",i
					status	=	HIMAWARI_FAILURE
					return
				endif
				retval	=	ahi_file_exists(fname,verbose)
				if (retval.ne.HIMAWARI_SUCCESS) then
					write(*,*)"File does not exist: ",fname
					status	=	HIMAWARI_FAILURE
					return
				endif
				startl	=	segpos(j)
				endl		=	segpos(j)+segdel-1
				retval	=	AHI_readchan(fname,tdata2(:,:),i,ahi_main%convert(i),ahi_main%ahi_navdata,j,segpos(j),verbose)

				if (retval.ne.HIMAWARI_SUCCESS) then
					write(*,*)"Failed to read the channel: ",i
					status	=	HIMAWARI_FAILURE
					return
				endif
			enddo
!			if (verbose) then
!				write(*,*)"Image size is: ",xsize,ysize
!			endif
			retval	=	AHI_resample_hres(tdata2, ahi_main%ahi_data%indata(:,:,bandpos),xsize,ysize,verbose)
			if (retval.ne.HIMAWARI_SUCCESS) then
				write(*,*)"Cannot resample data for channel: ",i
				deallocate(tdata2);
				status	=	HIMAWARI_FAILURE
				return
			endif
			deallocate(tdata2)

			bandpos	=	bandpos + 1
		endif
	enddo

	status	=	HIMAWARI_SUCCESS
	return

end function AHI_Setup_Read_Chans

integer function AHI_readchan(fname, indata,band,convert,ahi_nav,seg,sline,verbose)result(status)

	character(len=*), intent(in)					::	fname
	real(kind=ahi_sreal), DIMENSION(:,:), intent(inout)	::	indata
	integer, intent(in)							::	band
	integer, intent(in)							::	seg
	integer(kind=ahi_sint), intent(in)		::	convert
	integer, intent(in)							::	sline
	type(himawari_t_navdata), intent(inout)			::	ahi_nav
	logical,intent(in)						:: verbose

	integer(2), DIMENSION(:,:), ALLOCATABLE			::	tdata
	real(kind=ahi_sreal), DIMENSION(:,:), ALLOCATABLE	::	tdata2
	real(kind=ahi_sreal), DIMENSION(:,:), ALLOCATABLE	::	outdata
	real(kind=ahi_sreal), DIMENSION(:,:), ALLOCATABLE	::	temp3

	type(himawari_t_VIS_Header)				::	ahi_hdrvis
	type(himawari_t_IR_Header)					::	ahi_hdrir

	integer			::	reclen,xsize,ysize,i,x,y,bval,retval
	real 			::	temp,temp2
	integer			::	arrxs,arrys,filelun,flen
	real(kind=ahi_dreal)	::	gain,offset,c0,c1,c2,lspd,plnk,bolz,clamb

	bval	=	band

	allocate(outdata(HIMAWARI_IR_NLINES,HIMAWARI_IR_NCOLS))
	if (band.eq.1.or.band.eq.2.or.band.eq.4) then
		open(newunit=filelun, file=fname,form='unformatted',action='read',status='old',access='stream',convert='little_endian')
		arrxs	=	HIMAWARI_VIS_NLINES/10
		arrys	=	HIMAWARI_VIS_NCOLS
	else if (band.eq.3) then
		open(newunit=filelun, file=fname,form='unformatted',action='read',status='old',access='stream',convert='little_endian')
		arrxs	=	HIMAWARI_HVI_NLINES/10
		arrys	=	HIMAWARI_HVI_NCOLS
	else
		open(newunit=filelun, file=fname,form='unformatted',action='read',status='old',access='stream',convert='little_endian')
		arrxs	=	HIMAWARI_IR_NLINES/10
		arrys	=	HIMAWARI_IR_NCOLS
	endif
	allocate(tdata(arrys,arrxs))
	allocate(tdata2(arrys,arrxs))
	allocate(temp3(arrys,arrxs))

	if (band.lt.7) then
		retval					=	AHI_readhdr_VIS(filelun,ahi_hdrvis,verbose)
		gain						=	ahi_hdrvis%him_calib%gain_cnt2rad
		offset					=	ahi_hdrvis%him_calib%cnst_cnt2rad
		clamb						=	ahi_hdrvis%him_calib%waveLen
		c0							=	ahi_hdrvis%him_chan_calib%rad2albedo
		ahi_nav%subLon			=	ahi_hdrvis%him_proj%subLon
		ahi_nav%cfac			=	ahi_hdrvis%him_proj%cfac
		ahi_nav%lfac			=	ahi_hdrvis%him_proj%lfac
		ahi_nav%coff			=	ahi_hdrvis%him_proj%coff
		ahi_nav%loff			=	ahi_hdrvis%him_proj%loff

		ahi_nav%satdis			=	ahi_hdrvis%him_proj%satdis
		ahi_nav%eqtrRadius	=	ahi_hdrvis%him_proj%eqtrRadius
		ahi_nav%polrRadius	=	ahi_hdrvis%him_proj%polrRadius
		ahi_nav%projParam1	=	ahi_hdrvis%him_proj%projParam1
		ahi_nav%projParam2	=	ahi_hdrvis%him_proj%projParam2
		ahi_nav%projParam3	=	ahi_hdrvis%him_proj%projParam3
		ahi_nav%projParamSd	=	ahi_hdrvis%him_proj%projParamSd

		ahi_nav%cfac			=	ceiling((ahi_nav%cfac)/2)
		ahi_nav%lfac			=	ceiling((ahi_nav%lfac)/2)
		ahi_nav%coff			=	(ahi_nav%coff-0.5)/2
		ahi_nav%loff			=	(ahi_nav%loff-0.5)/2
		if (band.eq.3) then
			ahi_nav%cfac		=	ceiling(ahi_nav%cfac/2)
			ahi_nav%lfac		=	ceiling(ahi_nav%lfac/2)
			ahi_nav%coff		=	ahi_nav%coff/2
			ahi_nav%loff		=	ahi_nav%loff/2
		endif

		ahi_nav%coff			=	ahi_nav%coff+0.5
		ahi_nav%loff			=	ahi_nav%loff+0.5

	else
		retval					=	AHI_readhdr_IR(filelun,ahi_hdrir,verbose)
		gain						=	ahi_hdrir%him_calib%gain_cnt2rad
		offset					=	ahi_hdrir%him_calib%cnst_cnt2rad
		clamb						=	ahi_hdrir%him_calib%waveLen
		c0							=	ahi_hdrir%him_chan_calib%btp2rad_c0
		c1							=	ahi_hdrir%him_chan_calib%btp2rad_c1
		c2							=	ahi_hdrir%him_chan_calib%btp2rad_c2
		lspd						=	ahi_hdrir%him_chan_calib%lightSpeed
		plnk						=	ahi_hdrir%him_chan_calib%planckConst
		bolz						=	ahi_hdrir%him_chan_calib%bolzConst

		ahi_nav%subLon			=	ahi_hdrir%him_proj%subLon
		ahi_nav%cfac			=	ahi_hdrir%him_proj%cfac
		ahi_nav%lfac			=	ahi_hdrir%him_proj%lfac
		ahi_nav%coff			=	ahi_hdrir%him_proj%coff
		ahi_nav%loff			=	ahi_hdrir%him_proj%loff
		ahi_nav%satdis			=	ahi_hdrir%him_proj%satdis
		ahi_nav%eqtrRadius	=	ahi_hdrir%him_proj%eqtrRadius
		ahi_nav%polrRadius	=	ahi_hdrir%him_proj%polrRadius
		ahi_nav%projParam1	=	ahi_hdrir%him_proj%projParam1
		ahi_nav%projParam2	=	ahi_hdrir%him_proj%projParam2
		ahi_nav%projParam3	=	ahi_hdrir%him_proj%projParam3
		ahi_nav%projParamSd	=	ahi_hdrir%him_proj%projParamSd
	endif

	ahi_nav%cfac				=	ahi_nav%cfac/HIMAWARI_DEGTORAD
	ahi_nav%lfac				=	ahi_nav%lfac/HIMAWARI_DEGTORAD

!	if (verbose) then
!		write(*,*)"Filename is:",trim(fname)
!		write(*,*)"Imsize is:",arrxs,arrys
!	endif
	if (seg.ne.0) then
		INQUIRE(FILE=fname, SIZE=flen)
		flen	=	flen-(arrxs*arrys*2)
!		if (verbose) then
!			write(*,*)"Offset is:",flen
!		endif
		call	fseek(filelun,flen,0,retval)
	endif
	read(filelun)tdata(:,:)

	if (convert.eq.HIMAWARI_UNIT_RAD .or. convert.eq.HIMAWARI_UNIT_RBT) then
		tdata2	=	float(tdata)*gain+offset

		if (convert == HIMAWARI_UNIT_RBT) then
			if (band<7) then
				tdata2	=	tdata2*c0
				where(tdata2.lt. -10.0) tdata2=him_sreal_fill_value
				where(tdata2.gt. 10.0) tdata2=him_sreal_fill_value
			else
				temp		=	(lspd*plnk)/(bolz*clamb/1e6);
				temp2	=	(2.0*lspd*lspd*plnk)/((clamb/1e6)**5)
				temp3	=	log(temp2/(tdata2*1e6)+1)

				tdata2	=	temp / temp3;
				tdata2	=	c0 + c1*tdata2 + c2*tdata2*tdata2
				where(tdata2.le. 0.0) tdata2=him_sreal_fill_value
				where(tdata2.gt. 400.0) tdata2=him_sreal_fill_value
			endif
		endif
	else
		tdata2	=	float(tdata)
	endif
	where(tdata.le.0)tdata2=him_sreal_fill_value
	indata(:,sline:sline+arrxs-1)	=	tdata2

	close(filelun)
	deallocate(tdata)
	deallocate(tdata2)
	deallocate(temp3)

	status	=	HIMAWARI_SUCCESS
	return

end function AHI_readchan


integer function AHI_resample_hres(indata, outdata,xsize,ysize,verbose) result(status)

	use omp_lib

	real(kind=ahi_sreal), DIMENSION(:,:), intent(in)	::	indata
	real(kind=ahi_sreal), DIMENSION(:,:), intent(out)	::	outdata
	integer, intent(in)							::	xsize
	integer, intent(in)							::	ysize
	logical,intent(in)							:: verbose

	real,dimension(HIMAWARI_IR_NLINES,HIMAWARI_IR_NCOLS) :: temparr

	integer	::	x,y
	integer	::	outx,outy
	integer	::	i,j
	integer	::	inposvar
	integer	::	outposvar
	integer	::	sizerx,sizery
	real 		::	val
	integer 	::	inpix
	integer	::	n_threads

	if (HIMAWARI_IR_NLINES.eq.xsize.or.HIMAWARI_IR_NCOLS.eq.ysize) then
		outdata	=	indata
		status	=	HIMAWARI_SUCCESS
		return
	endif

	temparr(:,:)=0

	sizerx	=	xsize / HIMAWARI_IR_NLINES
	sizery	=	ysize / HIMAWARI_IR_NCOLS

#ifdef _OPENMP
	if (verbose) then
		n_threads	=	omp_get_max_threads()
		write(*,*) 'Resampling VIS grid to IR grid using',n_threads,'threads'
	endif
!$omp parallel DO PRIVATE(x,y,outx,outy,val,inpix)
#else
	if (verbose)write(*,*) 'Resampling VIS grid to IR grid using without threading'
#endif
!	do y=1,ysize-sizery
!		outy	=	int(y/sizery)+1
!		do x=1,xsize-sizerx
!			outy	=	int(x/sizerx)+1
!			val	=	0
!			inpix=	0
!			do j=1,sizery
!				do i=1,sizerx
!					if (indata(x+i,y+j).gt.-100) then
!						val	=	val + indata(x+i,y+j)
!						inpix=	inpix + 1
!					endif
!				enddo
!			enddo
!			val	=	val/inpix
!			if (outx .le. 0 .or. outx .gt. HIMAWARI_IR_NLINES .or. &
!			    outy .le. 0 .or. outy .gt. HIMAWARI_IR_NCOLS) then
!			    	continue
!			endif
!!$OMP CRITICAL
!			if (temparr(outx,outy).le. 0) temparr(outx,outy)=val
!!$OMP END CRITICAL
!		enddo
!	enddo
!
	do x=1,xsize-sizerx
		outx	=	int(x/sizerx)+1
		do y=1,ysize-sizery
			outy	=	int(y/sizery)+1
			val	=	0
			inpix=	0
			do i=1,sizerx
				do j=1,sizery
					if (indata(x+i,y+j).gt.-100) then
						val	=	val + indata(x+i,y+j)
						inpix=	inpix + 1
					endif
				enddo
			enddo
			val	=	val/inpix
			if (outx .le. 0 .or. outx .ge. HIMAWARI_IR_NLINES .or. &
			    outy .le. 0 .or. outy .ge. HIMAWARI_IR_NCOLS) then
			    	continue
			endif

			if (temparr(outx,outy).le. 0) then
				temparr(outx,outy)=val
			endif

		enddo
	enddo

#ifdef _OPENMP
!$omp end parallel do
#endif
	outdata	=	temparr
	status	=	HIMAWARI_SUCCESS
	return

end function AHI_resample_hres

integer function AHI_SavetoNCDF(outdata,fname,bname,newfile,verbose) result(status)

	character(len=*), intent(in)					::	fname
	character(len=*), intent(in)					::	bname
	real(kind=ahi_sreal), DIMENSION(:,:), intent(in)		::	outdata
	integer, intent(in)								::	newfile
	logical,intent(in)								:: verbose

	integer, parameter :: NDIMS = 2
	integer, parameter :: NX = 5500, NY = 5500
	integer :: ncid, varid, dimids(NDIMS)
	integer :: x_dimid, y_dimid
	integer :: data_out(NY, NX)
	integer :: x, y

	do x = 1, NX
	do y = 1, NY
		data_out(y, x) = (x - 1) * NY + (y - 1)
	end do
	end do

	if (newfile.eq.1) then
		call AHI_NCDF_check(nf90_create(fname, NF90_CLOBBER, ncid))
		call AHI_NCDF_check(nf90_def_dim(ncid, "x", NX, x_dimid))
		call AHI_NCDF_check(nf90_def_dim(ncid, "y", NY, y_dimid))
	else
		call AHI_NCDF_check(nf90_open(fname, NF90_WRITE, ncid))
		call AHI_NCDF_check(nf90_redef(ncid))
	endif

	dimids =  (/ y_dimid, x_dimid /)
	call AHI_NCDF_check(nf90_def_var(ncid,bname, NF90_FLOAT, dimids, varid))

	call AHI_NCDF_check(nf90_enddef(ncid))
	call AHI_NCDF_check(nf90_put_var(ncid, varid, outdata))
	call AHI_NCDF_check(nf90_close(ncid))

	status	= HIMAWARI_SUCCESS
	return

end function AHI_SavetoNCDF

subroutine AHI_NCDF_check(status)
    integer, intent ( in) :: status

    if(status /= nf90_noerr) then
      print *, trim(nf90_strerror(status))
      stop "Stopped"
    end if
end subroutine AHI_NCDF_check

end module himawari_readwrite
