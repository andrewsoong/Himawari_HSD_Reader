! Example program to read a Himawari-8 AHI image from HSD format
! the output is saved to a netcdf file. The lines below can
! be used to read/save specific bands and ancillary data

! Command line arguments are:
! Input filename
! Output filename
! Channel numbers (each separate)
! For example:
!	./AHI HS_H08_20150711_0040_B01_FLDK_R10_S0310.DAT OUT.nc 1 2 3 8 14
!    Will load all segments for timeslot 0040 on 20150711
!    Will save to the 'OUT.nc' file in current directory
!    Will process data for channels 1, 2, 3, 8 and 14

program AHI_example_f90

	use himawari
	use himawari_utils
	use himawari_readwrite
	use omp_lib

	implicit none

	! Filename of the file to be read.
	character(HIMAWARI_CHARLEN)		::	filename
	! Name of NetCDF file to use for output
	character(HIMAWARI_CHARLEN)		::	outname

	! Housekeeping vars
	integer retval, nchans, ncmd, ios, iostat, fpos
	CHARACTER(len=1024)				:: 	inarg

	! Data type that stores the AHI data and geolocs
	type(himawari_t_data)			::	ahi_data
	! Bands to read/write
	integer,dimension(:), allocatable	::	band_ids

	logical	:: verbose = .true.


	! Loop over the command line arguments to extract files / chan numbers
	ncmd		=	1
	nchans	=	0
	do
		call get_command_argument(ncmd,inarg)
		if (len_trim(inarg).eq. 0) exit
		if (ncmd==1 .and. trim(inarg)=="h") then
			write(*,*)"Instructions:"
			stop
		endif
		if (ncmd==1) filename	=	trim(inarg)
		if (ncmd==2) outname	=	trim(inarg)
		if (ncmd.ge. 3) nchans	=	nchans + 1
		read( inarg, '(i10)',iostat=ios )retval
		if (ncmd.ge. 3 .and. ios .ne. 0) nchans	=	nchans -1
		ncmd	=	ncmd + 1
	enddo
	allocate(band_ids(nchans))
	ncmd		=	1

	! Loop again to extract the actual channels from the argument
	! This is inefficient, but it works!
	do
		call get_command_argument(ncmd+2,inarg)
		if (len_trim(inarg).eq. 0) exit
		read( inarg, '(i10)',iostat=ios )retval
		if (ios .ne. 0) then
			write(*,*) "Incorrect channel specification:",trim(inarg)
			stop
		endif
		band_ids(ncmd)	=	retval

		if (ncmd .gt. nchans) exit
		ncmd	=	ncmd + 1
	enddo

	! Allocate space for all the output data
	retval	=	AHI_alloc_vals_data(ahi_data,nchans,verbose)
	if (retval .ne. HIMAWARI_SUCCESS) then
		write(*,*)"Error encountered in data allocation. Quitting."
		stop
	endif

	! Call the main reader function
	retval	=	AHI_Main_Read(filename,"/network/aopp/apres/users/proud/ORAC_Data/GEO_FILES/AHI_ANGLES_PAC.nc",ahi_data,nchans,band_ids,0,1,.true.,verbose)

	print*,"Band: ",ahi_data%indata(2500,2500,:)
	print*,"Lon: ",ahi_data%lon(2500,2500)
	print*,"Lat: ",ahi_data%lat(2500,2500)
	print*,"Out: ",trim(outname)

	stop

	if (retval .ne. HIMAWARI_SUCCESS) then
		write(*,*)"Error encountered in data reading. Quitting."
		stop
	endif

	! Calls for saving the data. Uncomment as necessary.
	! These are the bands.
	! Note: Final var controls if a new file. Pass 1 only for first call to function, otherwise 0.
	! Note 2: Final dimension of %indata is the channel.
	!         They are sequential, not based on channel IDs!
	retval	=	AHI_SavetoNCDF(ahi_data%indata(:,:,1),outname,"Band_01",1,verbose)
	retval	=	AHI_SavetoNCDF(ahi_data%indata(:,:,2),outname,"Band_02",0,verbose)
	retval	=	AHI_SavetoNCDF(ahi_data%indata(:,:,3),outname,"Band_03",0,verbose)
	retval	=	AHI_SavetoNCDF(ahi_data%indata(:,:,4),outname,"Band_09",0,verbose)
!	retval	=	AHI_SavetoNCDF(ahi_data%indata(:,:,5),outname,"Band_05",0,verbose)
!	retval	=	AHI_SavetoNCDF(ahi_data%indata(:,:,6),outname,"Band_06",0,verbose)
!	retval	=	AHI_SavetoNCDF(ahi_data%indata(:,:,7),outname,"Band_07",0,verbose)
!	retval	=	AHI_SavetoNCDF(ahi_data%indata(:,:,8),outname,"Band_08",0,verbose)
!	retval	=	AHI_SavetoNCDF(ahi_data%indata(:,:,9),outname,"Band_09",0,verbose)
!	retval	=	AHI_SavetoNCDF(ahi_data%indata(:,:,10),outname,"Band_10",0,verbose)
!	retval	=	AHI_SavetoNCDF(ahi_data%indata(:,:,11),outname,"Band_11",0,verbose)
!	retval	=	AHI_SavetoNCDF(ahi_data%indata(:,:,12),outname,"Band_12",0,verbose)
!	retval	=	AHI_SavetoNCDF(ahi_data%indata(:,:,13),outname,"Band_13",0,verbose)
!	retval	=	AHI_SavetoNCDF(ahi_data%indata(:,:,14),outname,"Band_14",0,verbose)
!	retval	=	AHI_SavetoNCDF(ahi_data%indata(:,:,15),outname,"Band_15",0,verbose)
	retval	=	AHI_SavetoNCDF(ahi_data%indata(:,:,16),outname,"Band_16",0,verbose)

	! These are geolocation, uncomment to save
!	retval	=	AHI_SavetoNCDF(ahi_data%lon,outname,"Lon",0,verbose)
!	retval	=	AHI_SavetoNCDF(ahi_data%lat,outname,"Lat",0,verbose)

	! These are solar angles, uncomment to save
!	retval	=	AHI_SavetoNCDF(ahi_data%sza,outname,"SZA",0,verbose)
!	retval	=	AHI_SavetoNCDF(ahi_data%saa,outname,"SAA",0,verbose)

	! These are viewing angles, uncomment to save
!	retval	=	AHI_SavetoNCDF(ahi_data%vza,outname,"VZA",0,verbose)
!	retval	=	AHI_SavetoNCDF(ahi_data%vaa,outname,"VAA",0,verbose)

	! Clear up all the variables
	retval	=	AHI_free_vals_data(ahi_data,verbose)
	deallocate(band_ids)

end program AHI_example_f90
