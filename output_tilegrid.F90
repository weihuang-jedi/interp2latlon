!----------------------------------------------------------------------------------------

subroutine output_tilegrid(tile)

   use netcdf
   use tile_module

   implicit none

   type(tilegrid), dimension(6), intent(in) :: tile
   character(len=1024)          :: output_flnm

   real(kind=8), dimension(:,:), allocatable :: tilenumb

   integer, dimension(6) :: ncid, dimidx, dimidy, dimidz

   integer :: n, status
   status = nf90_noerr

   allocate(tilenumb(tile(1)%nx, tile(1)%ny))

   do n = 1, 6
      tilenumb = dble(n)

      write(output_flnm, fmt='(a, i1, a)') 'grid.tile', n, '.nc'

      print *, 'Write tile ', n, ' to file: ', trim(output_flnm)

      !Create the file. 
      status = nf90_create(trim(output_flnm), NF90_CLOBBER, ncid(n))
      call check_status(status)
   
      status = nf90_def_dim(ncid(n), 'nx', tile(n)%nx, dimidx(n))
      call check_status(status)
      status = nf90_def_dim(ncid(n), 'ny', tile(n)%ny, dimidy(n))
      call check_status(status)
      status = nf90_def_dim(ncid(n), 'nz', tile(n)%nz, dimidy(n))
      call check_status(status)

      print *, 'output global attr'

      call output_global_attr(ncid(n), output_flnm, 'Test Output Grid', 'FV3')

      print *, 'output var attr'

      call output_var_attr(ncid(n), dimidx(n), dimidy(n))

      print *, 'output nx'

      !--write nx
      call nc_put1Dvar0(ncid(n), 'nx', tile(n)%x, 1, tile(n)%nx)

      !--write ny
      call nc_put1Dvar0(ncid(n), 'ny', tile(n)%y, 1, tile(n)%ny)

      !--write nz
      call nc_put1Dvar0(ncid(n), 'nz', tile(n)%z, 1, tile(n)%nz)

      !--write lon
      call nc_put2Dvar0(ncid(n), 'lon', tile(n)%lon, 1, tile(n)%nx, 1, tile(n)%ny)

      !--write lat
      call nc_put2Dvar0(ncid(n), 'lat', tile(n)%lat, 1, tile(n)%nx, 1, tile(n)%ny)

      !--write tilenumb
      call nc_put2Dvar0(ncid(n), 'tilenumb', tilenumb, 1, tile(n)%nx, 1, tile(n)%ny)

      status =  nf90_close(ncid(n))
      call check_status(status)
      print *, 'Finished Write tile ', n, ' to file: ', trim(output_flnm)
   end do

end subroutine output_tilegrid

!-------------------------------------------------------------------------------------
subroutine output_var_attr(ncid, dimid_nx, dimid_ny)

   use netcdf

   implicit none

   integer, intent(in) :: ncid
   integer, intent(in) :: dimid_nx, dimid_ny

   integer, dimension(2) :: dimids
   integer :: missing_int
   integer :: status, nd
   real(kind=8) :: missing_double

   missing_double = -1.0e38
   missing_int = -999999

   dimids(1) = dimid_nx
   nd = 1
!--Field lon
   call nc_putAxisAttr(ncid, nd, dimids, NF90_DOUBLE, &
                      "nx", &
                      "Lontitude Coordinate", &
                      "degree_east", &
                      "Longitude" )

   dimids(1) = dimid_ny
   nd = 1
!--Field lat
   call nc_putAxisAttr(ncid, nd, dimids, NF90_DOUBLE, &
                      "ny", &
                      "Latitude Coordinate", &
                      "degree_north", &
                      "Latitude" )

   dimids(1) = dimid_nx
   dimids(2) = dimid_ny
   nd = 2

!--Field 1, lon  
   call nc_putAttr(ncid, nd, dimids, NF90_DOUBLE, &
                   "lon", &
                   "Lontitude Coordinate", NF90_DOUBLE, &
                   "degree_east", &
                   "ny nx", &
                   missing_double)

!--Field 2, lat
   call nc_putAttr(ncid, nd, dimids, NF90_DOUBLE, &
                   "lat", &
                   "Latitude Coordinate", &
                   "degree_north", &
                   "ny nx", &
                   missing_double)

!--Field 3, tilenumb
   call nc_putAttr(ncid, nd, dimids, NF90_DOUBLE, &
                   "tilenumb", &
                   "Tile Number", &
                   "unitless", &
                   "ny nx", &
                   missing_double)

!--End define mode.
   status = nf90_enddef(ncid)
   if(status /= nf90_noerr) then
      write(unit=0, fmt='(a,i6,a)') "Problem to enddef ncid: <", ncid, ">."
      write(unit=0, fmt='(2a)') "Error status: ", trim(nf90_strerror(status))
      write(unit=0, fmt='(3a, i4)') &
           "Stop in file: <", __FILE__, ">, line: ", __LINE__
      stop
   end if

end subroutine output_var_attr

!---------------------------------------------------------------------------
subroutine output_global_attr(ncid, filename, title, type)

   implicit none

   integer, intent(in) :: ncid
   character(len = *), intent(in) :: filename, title, type

 ! ----put global attributes----
   call nc_putGlobalCharAttr(ncid, 'filename', trim(filename))
   call nc_putGlobalCharAttr(ncid, 'title', trim(title))
   call nc_putGlobalCharAttr(ncid, 'grid_type', trim(type))

  !call nc_putGlobalIntAttr(ncid, 'WRF_for_first_guess', iwrf)

  !call nc_putGlobalRealAttr(ncid, 'top_height',bdytop)

end subroutine output_global_attr

