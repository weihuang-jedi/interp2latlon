!----------------------------------------------------------------------------------------
subroutine generate_header(k, tile, latlon, gridtype, flnm, last)
  
   use netcdf
   use tile_module
   use latlon_module

   implicit none

   integer, intent(in)                         :: k
   type(tilegrid), dimension(6), intent(inout) :: tile
   type(latlongrid), intent(inout)             :: latlon
   character(len=*), intent(in)                :: gridtype, flnm
   logical, intent(in)                         :: last

   integer :: rc

  !print *, 'Enter generate_header'
  !print *, 'k = ', k
  !print *, 'gridtype = ', trim(gridtype)
  !print *, 'flnm = ', trim(flnm)
  
   if(k == 1) then
      call create_coord(tile(1)%nt, tile(1)%time(1:tile(1)%nt), latlon, flnm)
   end if

   if('fv_core.res.tile' == trim(gridtype)) then
      call create_fv_core_var_attr(tile, latlon)
   else if('sfc_data.tile' == trim(gridtype)) then
      call create_sfc_data_var_attr(tile, latlon)
   else if('fv_tracer.res.tile' == trim(gridtype)) then
      call create_fv_tracer_var_attr(tile, latlon)
   else if('fv_srf_wnd.res.tile' == trim(gridtype)) then
      call create_fv_srf_wnd_var_attr(tile, latlon)
   else if('phy_data.tile' == trim(gridtype)) then
      call create_phy_data_var_attr(tile, latlon)
   end if

   if(last) then
     !End define mode.
      rc = nf90_enddef(latlon%ncid)
      if(rc /= nf90_noerr) then
         write(unit=0, fmt='(a,i6,a)') "Problem to enddef ncid: <", latlon%ncid, ">."
         write(unit=0, fmt='(2a)') "Error status: ", trim(nf90_strerror(rc))
         write(unit=0, fmt='(3a, i4)') &
              "Stop in file: <", __FILE__, ">, line: ", __LINE__
         stop
      end if
   end if

  !print *, 'Leave generate_header'

end subroutine generate_header

!----------------------------------------------------------------------------------------
subroutine closefile(latlon)

   use netcdf
   use latlon_module
   use tile_module, only : check_status

   implicit none

   type(latlongrid), intent(inout)             :: latlon

   integer :: i, j, n, rc

   rc =  nf90_close(latlon%ncid)
   call check_status(rc)
   print *, 'Finished Write to file: ', trim(latlon%filename)

end subroutine closefile

!----------------------------------------------------------------------------------------
subroutine interp2latlongrid(gridtype, tile, latlon)

   use netcdf
   use tile_module
   use latlon_module

   implicit none

   character(len=*), intent(in)                :: gridtype
   type(tilegrid), dimension(6), intent(inout) :: tile
   type(latlongrid), intent(inout)             :: latlon

  !print *, 'Enter interp2latlongrid'
  !print *, 'gridtype = ', trim(gridtype)

   if('fv_core.res.tile' == trim(gridtype)) then
      call process_fv_core(tile, latlon)
   else if('sfc_data.tile' == trim(gridtype)) then
      call process_sfc_data(tile, latlon)
   else if('fv_tracer.res.tile' == trim(gridtype)) then
      call process_fv_tracer(tile, latlon)
   else if('fv_srf_wnd.res.tile' == trim(gridtype)) then
      call process_fv_srf_wnd(tile, latlon)
   else if('phy_data.tile' == trim(gridtype)) then
      call process_phy_data(tile, latlon)
   end if

end subroutine interp2latlongrid

!---------------------------------------------------------------------------
subroutine create_global_attr(ncid, filename, title, type)

   implicit none

   integer, intent(in) :: ncid
   character(len = *), intent(in) :: filename, title, type

  !print *, 'Enter create_global_attr'

 ! ----put global attributes----
   call nc_putGlobalCharAttr(ncid, 'filename', trim(filename))
   call nc_putGlobalCharAttr(ncid, 'title', trim(title))
   call nc_putGlobalCharAttr(ncid, 'grid_type', trim(type))

  !call nc_putGlobalIntAttr(ncid, 'WRF_for_first_guess', iwrf)

  !call nc_putGlobalRealAttr(ncid, 'top_height',bdytop)

end subroutine create_global_attr

!----------------------------------------------------------------------------------------
subroutine create_coord(nt, time, latlon, flnm)

   use netcdf
   use latlon_module
   use tile_module, only : check_status

   implicit none

   integer,          intent(in)              :: nt
   real(kind=8), dimension(1:nt), intent(in) :: time
   type(latlongrid), intent(inout)           :: latlon
   character(len=*), intent(in)              :: flnm

   integer :: i, nd, rc, ncid

   integer, dimension(2) :: dimids

   real(kind=8), dimension(latlon%nlev) :: lev

   logical :: fileExists

  !print *, 'Enter create_coord'
  !print *, 'flnm = ', trim(flnm)
  !print *, 'nt = ', nt
  !print *, 'time(1:nt) = ', time(1:nt)

  !print *, 'latlon%nlon = ',  latlon%nlon
  !print *, 'latlon%nlat = ',  latlon%nlat
   print *, 'latlon%nlev = ',  latlon%nlev

   latlon%filename = trim(flnm)

   allocate(latlon%lev(latlon%nlev))

   do i = 1, latlon%nlev
      latlon%lev(i) = dble(i-1)
      lev(i) = dble(i-1)
   end do

  !print *, 'latlon%lon = ',  latlon%lon
  !print *, 'latlon%lat = ',  latlon%lat
   print *, 'latlon%lev = ',  latlon%lev

   rc = nf90_noerr

   latlon%dimidt = -1

  !Create the file. 
   inquire(file=trim(flnm), exist=fileExists)
   if (fileExists) then
     !call nf90_open(filePath, NF90_WRITE, ncid)
      open(unit=1234, iostat=rc, file=trim(flnm), status='old')
      if(rc == 0) close(1234, status='delete')
   end if

  !rc = nf90_create(trim(flnm), NF90_CLOBBER, ncid)
   rc = nf90_create(trim(flnm), NF90_NETCDF4, ncid)
   call check_status(rc)

   latlon%ncid = ncid
  !print *, 'ncid = ', ncid

   rc = nf90_def_dim(ncid, 'longitude', latlon%nlon, latlon%dimidx)
   call check_status(rc)
   rc = nf90_def_dim(ncid, 'latitude', latlon%nlat, latlon%dimidy)
   call check_status(rc)
   rc = nf90_def_dim(ncid, 'level', latlon%nlev, latlon%dimidz)
   call check_status(rc)
   rc = nf90_def_dim(ncid, 'Time', NF90_UNLIMITED, latlon%dimidt)
   call check_status(rc)

   call create_global_attr(ncid, flnm, 'FV3 to Lat-Lon Grid', 'Lat-Lon Grid')

   dimids(1) = latlon%dimidx
   nd = 1
!--Field lon
   call nc_putAxisAttr(ncid, nd, dimids, NF90_REAL8, &
                      'longitude', &
                      "Lontitude Coordinate", &
                      "degree_east", &
                      "Longitude" )

   dimids(1) = latlon%dimidy
!--Field lat
   call nc_putAxisAttr(ncid, nd, dimids, NF90_REAL8, &
                      'latitude', &
                      "Latitude Coordinate", &
                      "degree_north", &
                      "Latitude" )

   dimids(1) = latlon%dimidz
!--Field lev
   call nc_putAxisAttr(ncid, nd, dimids, NF90_REAL8, &
                      'level', &
                      "Altitude Coordinate", &
                      "top_down", &
                      "Altitude" )

   dimids(1) = latlon%dimidt
!--Field time
   call nc_putAxisAttr(ncid, nd, dimids, NF90_REAL8, &
                      "Time", &
                      "Time Coordinate", &
                      "time level", &
                      "Time" )

   !write lon
   call nc_put1Dvar0(ncid, 'longitude', latlon%lon, 1, latlon%nlon)

   !write lat
   call nc_put1Dvar0(ncid, 'latitude', latlon%lat, 1, latlon%nlat)

   !write lev
  !call nc_put1Dvar0(ncid, 'level', latlon%lev, 1, latlon%nlev)
   call nc_put1Dvar0(ncid, 'level', lev, 1, latlon%nlev)

   !write time
   call nc_put1Ddbl0(ncid, 'Time', time, 1, nt)

  !print *, 'Leave create_coord'
end subroutine create_coord

!-------------------------------------------------------------------------------------
subroutine create_fv_core_var_attr(tile, latlon)

   use netcdf
   use tile_module
   use latlon_module

   implicit none

   type(tilegrid), dimension(6), intent(inout) :: tile
   type(latlongrid), intent(inout)             :: latlon

   integer, dimension(4) :: dimids
   integer :: rc, nd, i
   integer :: missing_int
   real(kind=8) :: missing_value
   character(len=80) :: long_name, units, coordinates

  !print *, 'Enter create_fv_core_var_attr'

   missing_value = -1.0e38
   missing_int = -999999

   do i = 1, tile(1)%nVars
      if((trim(tile(1)%vars(i)%name) == 'xaxis_1') .or. &
         (trim(tile(1)%vars(i)%name) == 'xaxis_2') .or. &
         (trim(tile(1)%vars(i)%name) == 'yaxis_1') .or. &
         (trim(tile(1)%vars(i)%name) == 'yaxis_2') .or. &
         (trim(tile(1)%vars(i)%name) == 'zaxis_1') .or. &
         (trim(tile(1)%vars(i)%name) == 'u') .or. &
         (trim(tile(1)%vars(i)%name) == 'v') .or. &
         (trim(tile(1)%vars(i)%name) == 'Time')) then
         cycle
      end if

      long_name = 'unknown'
      units = 'unknown'
      coordinates = 'Time level latitude longitude'
      dimids(1) = latlon%dimidx
      dimids(2) = latlon%dimidy
      dimids(3) = latlon%dimidz
      dimids(4) = latlon%dimidt
      nd = 4

      long_name = trim(tile(1)%vars(i)%name)
      if((trim(tile(1)%vars(i)%name) == 'ps') .or. &
         (trim(tile(1)%vars(i)%name) == 'phis')) then
         dimids(3) = latlon%dimidt
         nd = 3
         coordinates = 'Time latitude longitude'
         if(trim(tile(1)%vars(i)%name) == 'ps') then
            long_name = 'surface_pressure'
            units = 'Pa'
         else if(trim(tile(1)%vars(i)%name) == 'phis') then
            long_name = 'surface_geopotential_height'
            units = 'm'
         end if
      else if((trim(tile(1)%vars(i)%name) == 'ua') .or. &
              (trim(tile(1)%vars(i)%name) == 'va') .or. &
              (trim(tile(1)%vars(i)%name) == 'W') .or. &
              (trim(tile(1)%vars(i)%name) == 'delp') .or. &
              (trim(tile(1)%vars(i)%name) == 'DZ') .or. &
              (trim(tile(1)%vars(i)%name) == 'T')) then
         if(trim(tile(1)%vars(i)%name) == 'ua') then
            long_name = 'eastward_wind'
            units = 'm/s'
         else if(trim(tile(1)%vars(i)%name) == 'va') then
            long_name = 'northward_wind'
            units = 'm/s'
         else if(trim(tile(1)%vars(i)%name) == 'T') then
            long_name = 'air_temperature'
            units = 'K'
         end if
      end if

      call nc_putAttr(latlon%ncid, nd, dimids, NF90_REAL8, &
                      trim(tile(1)%vars(i)%name), &
                      trim(long_name), trim(units), &
                      trim(coordinates), missing_value)
   end do

end subroutine create_fv_core_var_attr

!-------------------------------------------------------------------------------------
subroutine create_sfc_data_var_attr(tile, latlon)

   use netcdf
   use tile_module
   use latlon_module

   implicit none

   type(tilegrid), dimension(6), intent(inout) :: tile
   type(latlongrid), intent(inout)             :: latlon

   integer, dimension(4) :: dimids
   integer :: rc, nd, i, j
   integer :: missing_int
   real(kind=8) :: missing_value
   character(len=80) :: long_name, units, coordinates

  !print *, 'Enter create_sfc_data_var_attr'

   missing_value = -1.0e38
   missing_int = -999999

   do i = 1, tile(1)%nVars
      j = tile(1)%vars(i)%ndims

     !print *, 'Var ', i, ' name: <', trim(tile(1)%vars(i)%dimnames(j)), '>, ndims = ', &
     !         tile(1)%vars(i)%ndims

      if(tile(1)%vars(i)%ndims < 3) then
         cycle
      end if

      if('Time' /= trim(tile(1)%vars(i)%dimnames(j))) then
         cycle
      end if

      long_name = 'unknown'
      units = 'unknown'
      dimids(1) = latlon%dimidx
      dimids(2) = latlon%dimidy

      if(3 == tile(1)%vars(i)%ndims) then
        coordinates = 'Time latitude longitude'
        dimids(3) = latlon%dimidt
        nd = 3
      else if(4 == tile(1)%vars(i)%ndims) then
        coordinates = 'Time level latitude longitude'
        dimids(3) = latlon%dimidz
        dimids(4) = latlon%dimidt
        nd = 4
      else
        print *, 'Var ', i, ' name: <', trim(tile(1)%vars(i)%dimnames(1)), &
                 '>, ndims = ', tile(1)%vars(i)%ndims

        print *, 'Problem in File: ', __FILE__, ', at line: ', __LINE__
      end if

      long_name = trim(tile(1)%vars(i)%name)

      call nc_putAttr(latlon%ncid, nd, dimids, NF90_REAL8, &
                      trim(tile(1)%vars(i)%name), &
                      trim(long_name), trim(units), &
                      trim(coordinates), missing_value)
   end do

end subroutine create_sfc_data_var_attr

!-------------------------------------------------------------------------------------
subroutine create_fv_tracer_var_attr(tile, latlon)

   use netcdf
   use tile_module
   use latlon_module

   implicit none

   type(tilegrid), dimension(6), intent(inout) :: tile
   type(latlongrid), intent(inout)             :: latlon

   integer, dimension(4) :: dimids
   integer :: rc, nd, i, j
   integer :: missing_int
   real(kind=8) :: missing_value
   character(len=80) :: long_name, units, coordinates

  !print *, 'Enter create_fv_tracer_var_attr'

   missing_value = -1.0e38
   missing_int = -999999

   do i = 1, tile(1)%nVars
      j = tile(1)%vars(i)%ndims

     !print *, 'Var ', i, ' name: <', trim(tile(1)%vars(i)%dimnames(j)), '>, ndims = ', &
     !         tile(1)%vars(i)%ndims

      if(tile(1)%vars(i)%ndims < 3) then
         cycle
      end if

      if('Time' /= trim(tile(1)%vars(i)%dimnames(j))) then
         cycle
      end if

      long_name = trim(tile(1)%vars(i)%name)
      units = 'none'
      coordinates = 'Time level latitude longitude'

      dimids(1) = latlon%dimidx
      dimids(2) = latlon%dimidy
      dimids(3) = latlon%dimidz
      dimids(4) = latlon%dimidt
      nd = 4

      if(4 /= tile(1)%vars(i)%ndims) then
        print *, 'Var ', i, ' name: <', trim(tile(1)%vars(i)%dimnames(1)), &
                 '>, ndims = ', tile(1)%vars(i)%ndims

        print *, 'Problem in File: ', __FILE__, ', at line: ', __LINE__
      end if

      call nc_putAttr(latlon%ncid, nd, dimids, NF90_REAL8, &
                      trim(tile(1)%vars(i)%name), &
                      trim(long_name), trim(units), &
                      trim(coordinates), missing_value)
   end do

end subroutine create_fv_tracer_var_attr

!-------------------------------------------------------------------------------------
subroutine create_fv_srf_wnd_var_attr(tile, latlon)

   use netcdf
   use tile_module
   use latlon_module

   implicit none

   type(tilegrid), dimension(6), intent(inout) :: tile
   type(latlongrid), intent(inout)             :: latlon

   integer, dimension(4) :: dimids
   integer :: rc, nd, i, j
   integer :: missing_int
   real(kind=8) :: missing_value
   character(len=80) :: long_name, units, coordinates

  !print *, 'Enter create_fv_srf_wnd_var_attr'

   missing_value = -1.0e38
   missing_int = -999999

   do i = 1, tile(1)%nVars
      j = tile(1)%vars(i)%ndims

     !print *, 'Var ', i, ' name: <', trim(tile(1)%vars(i)%dimnames(j)), '>, ndims = ', &
     !         tile(1)%vars(i)%ndims

      if(tile(1)%vars(i)%ndims < 3) then
         cycle
      end if

      if('Time' /= trim(tile(1)%vars(i)%dimnames(j))) then
         cycle
      end if

      long_name = trim(tile(1)%vars(i)%name)
      units = 'none'
      coordinates = 'Time latitude longitude'

      dimids(1) = latlon%dimidx
      dimids(2) = latlon%dimidy
      dimids(3) = latlon%dimidt
      nd = 3

      if(3 /= tile(1)%vars(i)%ndims) then
        print *, 'Var ', i, ' name: <', trim(tile(1)%vars(i)%dimnames(1)), &
                 '>, ndims = ', tile(1)%vars(i)%ndims

        print *, 'Problem in File: ', __FILE__, ', at line: ', __LINE__
      end if

      call nc_putAttr(latlon%ncid, nd, dimids, NF90_REAL8, &
                      trim(tile(1)%vars(i)%name), &
                      trim(long_name), trim(units), &
                      trim(coordinates), missing_value)
   end do

end subroutine create_fv_srf_wnd_var_attr

!-------------------------------------------------------------------------------------
subroutine create_phy_data_var_attr(tile, latlon)

   use netcdf
   use tile_module
   use latlon_module

   implicit none

   type(tilegrid), dimension(6), intent(inout) :: tile
   type(latlongrid), intent(inout)             :: latlon

   integer, dimension(4) :: dimids
   integer :: rc, nd, i, j
   integer :: missing_int
   real(kind=8) :: missing_value
   character(len=80) :: long_name, units, coordinates

  !print *, 'Enter create_phy_data_var_attr'

   missing_value = -1.0e38
   missing_int = -999999

   do i = 1, tile(1)%nVars
      j = tile(1)%vars(i)%ndims

     !print *, 'Var ', i, ' name: <', trim(tile(1)%vars(i)%dimnames(j)), '>, ndims = ', &
     !         tile(1)%vars(i)%ndims

      if(tile(1)%vars(i)%ndims < 3) then
         cycle
      end if

      if('Time' /= trim(tile(1)%vars(i)%dimnames(j))) then
         cycle
      end if

      long_name = 'unknown'
      units = 'unknown'
      dimids(1) = latlon%dimidx
      dimids(2) = latlon%dimidy

      if(3 == tile(1)%vars(i)%ndims) then
        coordinates = 'Time latitude longitude'
        dimids(3) = latlon%dimidt
        nd = 3
      else if(4 == tile(1)%vars(i)%ndims) then
        coordinates = 'Time level latitude longitude'
        dimids(3) = latlon%dimidz
        dimids(4) = latlon%dimidt
        nd = 4
      else
        print *, 'Var ', i, ' name: <', trim(tile(1)%vars(i)%dimnames(1)), &
                 '>, ndims = ', tile(1)%vars(i)%ndims

        print *, 'Problem in File: ', __FILE__, ', at line: ', __LINE__
      end if

      long_name = trim(tile(1)%vars(i)%name)

      call nc_putAttr(latlon%ncid, nd, dimids, NF90_REAL8, &
                      trim(tile(1)%vars(i)%name), &
                      trim(long_name), trim(units), &
                      trim(coordinates), missing_value)
   end do

end subroutine create_phy_data_var_attr

!----------------------------------------------------------------------------------------
subroutine process_fv_core(tile, latlon)

   use netcdf
   use tile_module
   use latlon_module

   implicit none

   type(tilegrid), dimension(6), intent(inout) :: tile
   type(latlongrid), intent(inout)             :: latlon

   integer :: i, n, rc

   real(kind=8), dimension(:,:,:), allocatable :: var2d
   real(kind=8), dimension(:,:,:), allocatable :: var3d

  !print *, 'Enter process_fv_core'

   allocate(var2d(latlon%nlon, latlon%nlat, 1))
   allocate(var3d(latlon%nlon, latlon%nlat, latlon%nlev))

   do i = 1, tile(1)%nVars
      rc = nf90_inquire_variable(tile(1)%fileid, tile(1)%varids(i), &
               ndims=tile(1)%vars(i)%nDims, natts=tile(1)%vars(i)%nAtts)
      call check_status(rc)

     !print *, 'Var No. ', i, ': ndims = ', tile(1)%vars(i)%nDims

     !rc = nf90_inquire_variable(tile(1)%fileid, tile(1)%varids(i), &
     !         dimids=tile(1)%vars(i)%dimids)
     !call check_status(rc)

      rc = nf90_inquire_variable(tile(1)%fileid, tile(1)%varids(i), &
               name=tile(1)%vars(i)%name)
      call check_status(rc)

     !print *, 'Var No. ', i, ': name: ', trim(tile(1)%vars(i)%name)
     !print *, 'Var No. ', i, ': varid: ', tile(1)%varids(i)

      if(tile(1)%vars(i)%nDims < 2) cycle

      do n = 1, 6
         rc = nf90_inquire_variable(tile(n)%fileid, tile(n)%varids(i), &
                  name=tile(n)%vars(i)%name)
         call check_status(rc)

        !print *, 'Tile ', n, ', Var No. ', i, ': varid: ', tile(n)%varids(i)
        !print *, 'Tile ', n, ', Var ', i, ': ', trim(tile(n)%vars(i)%name)

         if((trim(tile(n)%vars(i)%name) == 'ps') .or. &
            (trim(tile(n)%vars(i)%name) == 'phis')) then
            rc = nf90_get_var(tile(n)%fileid, tile(n)%varids(i), tile(n)%var2d)
            call check_status(rc)
         else if((trim(tile(n)%vars(i)%name) == 'ua') .or. &
                 (trim(tile(n)%vars(i)%name) == 'va') .or. &
                 (trim(tile(n)%vars(i)%name) == 'W') .or. &
                 (trim(tile(n)%vars(i)%name) == 'delp') .or. &
                 (trim(tile(n)%vars(i)%name) == 'DZ') .or. &
                 (trim(tile(n)%vars(i)%name) == 'T')) then
            rc = nf90_get_var(tile(n)%fileid, tile(n)%varids(i), tile(n)%var3d)
            call check_status(rc)
         end if
      end do

      if((trim(tile(1)%vars(i)%name) == 'ps') .or. &
         (trim(tile(1)%vars(i)%name) == 'phis')) then
         call interp2dvar(tile, latlon, var2d)
         call nc_put3Dvar(latlon%ncid, trim(tile(1)%vars(i)%name), &
              var2d, 1, 1, latlon%nlon, 1, latlon%nlat, 1, 1)
      else if((trim(tile(1)%vars(i)%name) == 'ua') .or. &
              (trim(tile(1)%vars(i)%name) == 'va') .or. &
              (trim(tile(1)%vars(i)%name) == 'W') .or. &
              (trim(tile(1)%vars(i)%name) == 'delp') .or. &
              (trim(tile(1)%vars(i)%name) == 'DZ') .or. &
              (trim(tile(1)%vars(i)%name) == 'T')) then
         call interp3dvar(tile, latlon, var3d)
         call nc_put3Dvar(latlon%ncid, trim(tile(1)%vars(i)%name), &
              var3d, 1, 1, latlon%nlon, 1, latlon%nlat, 1, latlon%nlev)
      end if
   end do

   deallocate(var2d)
   deallocate(var3d)

end subroutine process_fv_core

!----------------------------------------------------------------------------------------
subroutine process_sfc_data(tile, latlon)

   use netcdf
   use tile_module
   use latlon_module

   implicit none

   type(tilegrid), dimension(6), intent(inout) :: tile
   type(latlongrid), intent(inout)             :: latlon

   integer :: i, j, n, rc

   real(kind=8), dimension(:,:), allocatable :: var2d
   real(kind=8), dimension(:,:,:), allocatable :: var3d

  !print *, 'Enter process_sfc_data'

   allocate(var2d(latlon%nlon, latlon%nlat))
   allocate(var3d(latlon%nlon, latlon%nlat, latlon%nlev))

   do i = 1, tile(1)%nVars
      j = tile(1)%vars(i)%ndims

     !print *, 'Var ', i, ' name: <', trim(tile(1)%vars(i)%dimnames(j)), &
     !         '>, ndims = ', tile(1)%vars(i)%ndims

      if(tile(1)%vars(i)%ndims < 3) then
         cycle
      end if

      if('Time' /= trim(tile(1)%vars(i)%dimnames(j))) then
         cycle
      end if

     !rc = nf90_inquire_variable(tile(1)%fileid, tile(1)%varids(i), &
     !         ndims=tile(1)%vars(i)%nDims, natts=tile(1)%vars(i)%nAtts)
     !call check_status(rc)

     !rc = nf90_inquire_variable(tile(1)%fileid, tile(1)%varids(i), &
     !         dimids=tile(1)%vars(i)%dimids)
     !call check_status(rc)

     !rc = nf90_inquire_variable(tile(1)%fileid, tile(1)%varids(i), &
     !         name=tile(1)%vars(i)%name)
     !call check_status(rc)

     !print *, 'Var No. ', i, ' name: ', trim(tile(1)%vars(i)%name)
     !print *, 'Var No. ', i, ': varid: ', tile(1)%varids(i)
     !print *, 'Var No. ', i, ', ndims = ', tile(1)%vars(i)%ndims

      do n = 1, 6
         rc = nf90_inquire_variable(tile(n)%fileid, tile(n)%varids(i), &
                  name=tile(n)%vars(i)%name)
         call check_status(rc)

        !print *, 'Tile ', n, ', Var No. ', i, ': varid: ', tile(n)%varids(i)
        !print *, 'Tile ', n, ', Var ', i, ': ', trim(tile(n)%vars(i)%name)

         if(3 == tile(1)%vars(i)%ndims) then
            rc = nf90_get_var(tile(n)%fileid, tile(n)%varids(i), tile(n)%var2d)
            call check_status(rc)
         else if(4 == tile(1)%vars(i)%ndims) then
            rc = nf90_get_var(tile(n)%fileid, tile(n)%varids(i), tile(n)%var3d)
            call check_status(rc)
         else
           print *, 'Var ', i, ' name: <', trim(tile(1)%vars(i)%dimnames(1)), &
                    '>, ndims = ', tile(1)%vars(i)%ndims

           print *, 'Problem in File: ', __FILE__, ', at line: ', __LINE__
         end if
      end do

     !print *, 'Var No. ', i, ' name: ', trim(tile(1)%vars(i)%name)
     !print *, 'Var No. ', i, ': varid: ', tile(1)%varids(i)
     !print *, 'Var No. ', i, ', ndims = ', tile(1)%vars(i)%ndims

     !do j = 1, tile(1)%vars(i)%ndims
        !print *, 'Dim ', j, ' name: <', trim(tile(1)%vars(i)%dimnames(j)), &
        !         '>, len = ', tile(1)%vars(i)%dimlen(j)
     !end do

      if(3 == tile(1)%vars(i)%ndims) then
         call interp2dvar4sfc(tile, latlon, var2d)
         call nc_put3Dvar1(latlon%ncid, trim(tile(1)%vars(i)%name), &
              var2d, 1, 1, latlon%nlon, 1, latlon%nlat)
      else if(4 == tile(1)%vars(i)%ndims) then
         call interp3dvar4sfc(tile, latlon, var3d)
         call nc_put3Dvar(latlon%ncid, trim(tile(1)%vars(i)%name), &
              var3d, 1, 1, latlon%nlon, 1, latlon%nlat, 1, latlon%nlev)
      else
         print *, 'Var ', i, ' name: <', trim(tile(1)%vars(i)%dimnames(1)), &
                  '>, ndims = ', tile(1)%vars(i)%ndims

         print *, 'Problem in File: ', __FILE__, ', at line: ', __LINE__
      end if
   end do

   deallocate(var2d)
   deallocate(var3d)

end subroutine process_sfc_data

!----------------------------------------------------------------------------------------
subroutine process_fv_tracer(tile, latlon)

   use netcdf
   use tile_module
   use latlon_module

   implicit none

   type(tilegrid), dimension(6), intent(inout) :: tile
   type(latlongrid), intent(inout)             :: latlon

   integer :: i, j, n, rc

   real(kind=8), dimension(:,:,:), allocatable :: var3d

  !print *, 'Enter process_fv_tracer'

   allocate(var3d(latlon%nlon, latlon%nlat, latlon%nlev))

   do i = 1, tile(1)%nVars
      j = tile(1)%vars(i)%ndims

     !print *, 'Var ', i, ' name: <', trim(tile(1)%vars(i)%dimnames(j)), &
     !         '>, ndims = ', tile(1)%vars(i)%ndims

      if(tile(1)%vars(i)%ndims < 3) then
         cycle
      end if

      if('Time' /= trim(tile(1)%vars(i)%dimnames(j))) then
         cycle
      end if

     !rc = nf90_inquire_variable(tile(1)%fileid, tile(1)%varids(i), &
     !         ndims=tile(1)%vars(i)%nDims, natts=tile(1)%vars(i)%nAtts)
     !call check_status(rc)

     !rc = nf90_inquire_variable(tile(1)%fileid, tile(1)%varids(i), &
     !         dimids=tile(1)%vars(i)%dimids)
     !call check_status(rc)

     !rc = nf90_inquire_variable(tile(1)%fileid, tile(1)%varids(i), &
     !         name=tile(1)%vars(i)%name)
     !call check_status(rc)

     !print *, 'Var No. ', i, ' name: ', trim(tile(1)%vars(i)%name)
     !print *, 'Var No. ', i, ': varid: ', tile(1)%varids(i)
     !print *, 'Var No. ', i, ', ndims = ', tile(1)%vars(i)%ndims

      do n = 1, 6
         rc = nf90_inquire_variable(tile(n)%fileid, tile(n)%varids(i), &
                  name=tile(n)%vars(i)%name)
         call check_status(rc)

        !print *, 'Tile ', n, ', Var No. ', i, ': varid: ', tile(n)%varids(i)
        !print *, 'Tile ', n, ', Var ', i, ': ', trim(tile(n)%vars(i)%name)

         if(4 == tile(1)%vars(i)%ndims) then
            rc = nf90_get_var(tile(n)%fileid, tile(n)%varids(i), tile(n)%var3d)
            call check_status(rc)
         else
           print *, 'Var ', i, ' name: <', trim(tile(1)%vars(i)%dimnames(1)), &
                    '>, ndims = ', tile(1)%vars(i)%ndims

           print *, 'Problem in File: ', __FILE__, ', at line: ', __LINE__
         end if
      end do

     !print *, 'Var No. ', i, ' name: ', trim(tile(1)%vars(i)%name)
     !print *, 'Var No. ', i, ': varid: ', tile(1)%varids(i)
     !print *, 'Var No. ', i, ', ndims = ', tile(1)%vars(i)%ndims

     !do j = 1, tile(1)%vars(i)%ndims
        !print *, 'Dim ', j, ' name: <', trim(tile(1)%vars(i)%dimnames(j)), &
        !         '>, len = ', tile(1)%vars(i)%dimlen(j)
     !end do

      if(4 == tile(1)%vars(i)%ndims) then
         call interp3dvar(tile, latlon, var3d)
         call nc_put3Dvar(latlon%ncid, trim(tile(1)%vars(i)%name), &
              var3d, 1, 1, latlon%nlon, 1, latlon%nlat, 1, latlon%nlev)
      else
         print *, 'Var ', i, ' name: <', trim(tile(1)%vars(i)%dimnames(1)), &
                  '>, ndims = ', tile(1)%vars(i)%ndims

         print *, 'Problem in File: ', __FILE__, ', at line: ', __LINE__
      end if
   end do

   deallocate(var3d)

end subroutine process_fv_tracer

!----------------------------------------------------------------------------------------
subroutine process_fv_srf_wnd(tile, latlon)

   use netcdf
   use tile_module
   use latlon_module

   implicit none

   type(tilegrid), dimension(6), intent(inout) :: tile
   type(latlongrid), intent(inout)             :: latlon

   integer :: i, j, n, rc

   real(kind=8), dimension(:,:), allocatable :: var2d

  !print *, 'Enter process_fv_srf_wnd'

   allocate(var2d(latlon%nlon, latlon%nlat))

   do i = 1, tile(1)%nVars
      j = tile(1)%vars(i)%ndims

     !print *, 'Var ', i, ' name: <', trim(tile(1)%vars(i)%dimnames(j)), &
     !         '>, ndims = ', tile(1)%vars(i)%ndims

      if(tile(1)%vars(i)%ndims < 3) then
         cycle
      end if

      if('Time' /= trim(tile(1)%vars(i)%dimnames(j))) then
         cycle
      end if

     !rc = nf90_inquire_variable(tile(1)%fileid, tile(1)%varids(i), &
     !         ndims=tile(1)%vars(i)%nDims, natts=tile(1)%vars(i)%nAtts)
     !call check_status(rc)

     !rc = nf90_inquire_variable(tile(1)%fileid, tile(1)%varids(i), &
     !         dimids=tile(1)%vars(i)%dimids)
     !call check_status(rc)

     !rc = nf90_inquire_variable(tile(1)%fileid, tile(1)%varids(i), &
     !         name=tile(1)%vars(i)%name)
     !call check_status(rc)

     !print *, 'Var No. ', i, ' name: ', trim(tile(1)%vars(i)%name)
     !print *, 'Var No. ', i, ': varid: ', tile(1)%varids(i)
     !print *, 'Var No. ', i, ', ndims = ', tile(1)%vars(i)%ndims

      do n = 1, 6
         rc = nf90_inquire_variable(tile(n)%fileid, tile(n)%varids(i), &
                  name=tile(n)%vars(i)%name)
         call check_status(rc)

        !print *, 'Tile ', n, ', Var No. ', i, ': varid: ', tile(n)%varids(i)
        !print *, 'Tile ', n, ', Var ', i, ': ', trim(tile(n)%vars(i)%name)

         if(3 == tile(1)%vars(i)%ndims) then
            rc = nf90_get_var(tile(n)%fileid, tile(n)%varids(i), tile(n)%var2d)
            call check_status(rc)
         else
           print *, 'Var ', i, ' name: <', trim(tile(1)%vars(i)%dimnames(1)), &
                    '>, ndims = ', tile(1)%vars(i)%ndims

           print *, 'Problem in File: ', __FILE__, ', at line: ', __LINE__
         end if
      end do

     !print *, 'Var No. ', i, ' name: ', trim(tile(1)%vars(i)%name)
     !print *, 'Var No. ', i, ': varid: ', tile(1)%varids(i)
     !print *, 'Var No. ', i, ', ndims = ', tile(1)%vars(i)%ndims

     !do j = 1, tile(1)%vars(i)%ndims
        !print *, 'Dim ', j, ' name: <', trim(tile(1)%vars(i)%dimnames(j)), &
        !         '>, len = ', tile(1)%vars(i)%dimlen(j)
     !end do

      if(3 == tile(1)%vars(i)%ndims) then
         call interp2dvar4sfc(tile, latlon, var2d)
         call nc_put3Dvar1(latlon%ncid, trim(tile(1)%vars(i)%name), &
              var2d, 1, 1, latlon%nlon, 1, latlon%nlat)
      else
         print *, 'Var ', i, ' name: <', trim(tile(1)%vars(i)%dimnames(1)), &
                  '>, ndims = ', tile(1)%vars(i)%ndims

         print *, 'Problem in File: ', __FILE__, ', at line: ', __LINE__
      end if
   end do

   deallocate(var2d)

end subroutine process_fv_srf_wnd

!----------------------------------------------------------------------------------------
subroutine process_phy_data(tile, latlon)

   use netcdf
   use tile_module
   use latlon_module

   implicit none

   type(tilegrid), dimension(6), intent(inout) :: tile
   type(latlongrid), intent(inout)             :: latlon

   integer :: i, j, n, rc

   real(kind=8), dimension(:,:), allocatable :: var2d
   real(kind=8), dimension(:,:,:), allocatable :: var3d

  !print *, 'Enter process_phy_data'

   allocate(var2d(latlon%nlon, latlon%nlat))
   allocate(var3d(latlon%nlon, latlon%nlat, latlon%nlev))

   do i = 1, tile(1)%nVars
      j = tile(1)%vars(i)%ndims

     !print *, 'Var ', i, ' name: <', trim(tile(1)%vars(i)%dimnames(j)), &
     !         '>, ndims = ', tile(1)%vars(i)%ndims

      if(tile(1)%vars(i)%ndims < 3) then
         cycle
      end if

      if('Time' /= trim(tile(1)%vars(i)%dimnames(j))) then
         cycle
      end if

     !rc = nf90_inquire_variable(tile(1)%fileid, tile(1)%varids(i), &
     !         ndims=tile(1)%vars(i)%nDims, natts=tile(1)%vars(i)%nAtts)
     !call check_status(rc)

     !rc = nf90_inquire_variable(tile(1)%fileid, tile(1)%varids(i), &
     !         dimids=tile(1)%vars(i)%dimids)
     !call check_status(rc)

     !rc = nf90_inquire_variable(tile(1)%fileid, tile(1)%varids(i), &
     !         name=tile(1)%vars(i)%name)
     !call check_status(rc)

     !print *, 'Var No. ', i, ' name: ', trim(tile(1)%vars(i)%name)
     !print *, 'Var No. ', i, ': varid: ', tile(1)%varids(i)
     !print *, 'Var No. ', i, ', ndims = ', tile(1)%vars(i)%ndims

      do n = 1, 6
         rc = nf90_inquire_variable(tile(n)%fileid, tile(n)%varids(i), &
                  name=tile(n)%vars(i)%name)
         call check_status(rc)

        !print *, 'Tile ', n, ', Var No. ', i, ': varid: ', tile(n)%varids(i)
        !print *, 'Tile ', n, ', Var ', i, ': ', trim(tile(n)%vars(i)%name)

         if(3 == tile(1)%vars(i)%ndims) then
            rc = nf90_get_var(tile(n)%fileid, tile(n)%varids(i), tile(n)%var2d)
            call check_status(rc)
         else if(4 == tile(1)%vars(i)%ndims) then
            rc = nf90_get_var(tile(n)%fileid, tile(n)%varids(i), tile(n)%var3d)
            call check_status(rc)
         else
           print *, 'Var ', i, ' name: <', trim(tile(1)%vars(i)%dimnames(1)), &
                    '>, ndims = ', tile(1)%vars(i)%ndims

           print *, 'Problem in File: ', __FILE__, ', at line: ', __LINE__
         end if
      end do

     !print *, 'Var No. ', i, ' name: ', trim(tile(1)%vars(i)%name)
     !print *, 'Var No. ', i, ': varid: ', tile(1)%varids(i)
     !print *, 'Var No. ', i, ', ndims = ', tile(1)%vars(i)%ndims

     !do j = 1, tile(1)%vars(i)%ndims
        !print *, 'Dim ', j, ' name: <', trim(tile(1)%vars(i)%dimnames(j)), &
        !         '>, len = ', tile(1)%vars(i)%dimlen(j)
     !end do

      if(3 == tile(1)%vars(i)%ndims) then
         call interp2dvar4sfc(tile, latlon, var2d)
         call nc_put3Dvar1(latlon%ncid, trim(tile(1)%vars(i)%name), &
              var2d, 1, 1, latlon%nlon, 1, latlon%nlat)
      else if(4 == tile(1)%vars(i)%ndims) then
         call interp3dvar(tile, latlon, var3d)
         call nc_put3Dvar(latlon%ncid, trim(tile(1)%vars(i)%name), &
              var3d, 1, 1, latlon%nlon, 1, latlon%nlat, 1, latlon%nlev)
      else
         print *, 'Var ', i, ' name: <', trim(tile(1)%vars(i)%dimnames(1)), &
                  '>, ndims = ', tile(1)%vars(i)%ndims

         print *, 'Problem in File: ', __FILE__, ', at line: ', __LINE__
      end if
   end do

   deallocate(var2d)
   deallocate(var3d)

end subroutine process_phy_data

!----------------------------------------------------------------------
subroutine interp2dvar(tile, latlon, var2d)

  use tile_module
  use latlon_module

  implicit none

  type(tilegrid), dimension(6), intent(in) :: tile
  type(latlongrid), intent(in) :: latlon
  real(kind=8), dimension(latlon%nlon, latlon%nlat, 1), intent(out) :: var2d

  integer :: i, j, n, ik, jk, m
  real(kind=8) :: w

  do jk = 1, latlon%nlat
  do ik = 1, latlon%nlon
     var2d(ik, jk, 1) = 0.0

     do m = 1, latlon%npnt
        n = latlon%tile(ik, jk, m)
        i = latlon%ilon(ik, jk, m)
        j = latlon%jlat(ik, jk, m)
        w = latlon%wgt(ik, jk, m)

        var2d(ik, jk, 1) = var2d(ik, jk, 1) + w*tile(n)%var2d(i, j)
     end do
  end do
  end do

end subroutine interp2dvar

!----------------------------------------------------------------------
subroutine interp3dvar(tile, latlon, var3d)

  use tile_module
  use latlon_module

  implicit none

  type(tilegrid), dimension(6), intent(in) :: tile
  type(latlongrid), intent(in) :: latlon
  real(kind=8), dimension(latlon%nlon, latlon%nlat, latlon%nlev), intent(out) :: var3d

  integer :: i, j, k, n, ik, jk, m
  real(kind=8) :: w

  do jk = 1, latlon%nlat
  do ik = 1, latlon%nlon
     do k = 1, latlon%nlev
        var3d(ik, jk, k) = 0.0
     end do

     do m = 1, latlon%npnt
        n = latlon%tile(ik, jk, m)
        i = latlon%ilon(ik, jk, m)
        j = latlon%jlat(ik, jk, m)
        w = latlon%wgt(ik, jk, m)

        do k = 1, latlon%nlev
           var3d(ik, jk, k) = var3d(ik, jk, k) + w*tile(n)%var3d(i, j, k)
        end do
     end do
  end do
  end do

end subroutine interp3dvar

!----------------------------------------------------------------------
subroutine interp2dvar4sfc(tile, latlon, var2d)

  use tile_module
  use latlon_module

  implicit none

  type(tilegrid), dimension(6),              intent(in)  :: tile
  type(latlongrid),                          intent(in)  :: latlon
  real(kind=8), dimension(latlon%nlon, latlon%nlat), intent(out) :: var2d

  integer :: i, j, n, ik, jk, m
  real(kind=8) :: w

  do jk = 1, latlon%nlat
  do ik = 1, latlon%nlon
     var2d(ik, jk) = 0.0

     do m = 1, latlon%npnt
        n = latlon%tile(ik, jk, m)
        i = latlon%ilon(ik, jk, m)
        j = latlon%jlat(ik, jk, m)
        w = latlon%wgt(ik, jk, m)

        var2d(ik, jk) = var2d(ik, jk) + w*tile(n)%var2d(i, j)
     end do
  end do
  end do

end subroutine interp2dvar4sfc

!----------------------------------------------------------------------
subroutine interp3dvar4sfc(tile, latlon, var3d)

  use tile_module
  use latlon_module

  implicit none

  type(tilegrid), dimension(6), intent(in) :: tile
  type(latlongrid), intent(in) :: latlon
  real(kind=8), dimension(latlon%nlon, latlon%nlat, latlon%nlev), intent(out) :: var3d

  integer :: i, j, k, n, ik, jk, m
  real(kind=8) :: w

  do jk = 1, latlon%nlat
  do ik = 1, latlon%nlon
     do k = 1, latlon%nlev
        var3d(ik, jk, k) = 0.0
     end do

     do m = 1, latlon%npnt
        n = latlon%tile(ik, jk, m)
        i = latlon%ilon(ik, jk, m)
        j = latlon%jlat(ik, jk, m)
        w = latlon%wgt(ik, jk, m)

        do k = 1, latlon%nlev
           var3d(ik, jk, k) = var3d(ik, jk, k) + w*tile(n)%var3d(i, j, k)
        end do
     end do
  end do
  end do

end subroutine interp3dvar4sfc

