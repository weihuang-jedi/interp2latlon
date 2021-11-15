!----------------------------------------------------------------------------------------

subroutine read_weights(latlon, wgt_flnm)

   use netcdf
   use latlon_module
   use tile_module, only : check_status

   implicit none

   type(latlongrid), intent(in) :: latlon
   character(len=*), intent(in) :: wgt_flnm

   integer :: ncid, dimidx, dimidy, dimidp

   integer :: i, j, n, status

   status = nf90_noerr

   n = 0

   !Open the file. 
   status = nf90_open(trim(wgt_flnm), nf90_nowrite, ncid)
   call check_status(status)

   !read lon
   call nc_get1Dvar0(ncid, 'lon', latlon%lon, 1, latlon%nlon)

   !read lat
   call nc_get1Dvar0(ncid, 'lat', latlon%lat, 1, latlon%nlat)

   !read pnt
   call nc_get1Dvar0(ncid, 'pnt', latlon%pnt, 1, latlon%npnt)

   !--read tile
   call nc_get3Dint0(ncid, 'tile', latlon%tile, 1, latlon%nlon, &
                     1, latlon%nlat, 1, latlon%npnt)

   !--read ilon
   call nc_get3Dint0(ncid, 'ilon', latlon%ilon, 1, latlon%nlon, &
                     1, latlon%nlat, 1, latlon%npnt)

   !--read jlat
   call nc_get3Dint0(ncid, 'jlat', latlon%jlat, 1, latlon%nlon, &
                     1, latlon%nlat, 1, latlon%npnt)

   !--read wgt
   call nc_get3Dvar0(ncid, 'wgt', latlon%wgt, 1, latlon%nlon, &
                     1, latlon%nlat, 1, latlon%npnt)

   status =  nf90_close(ncid)
   call check_status(status)

  !print *, 'Finished Read weights from file: ', trim(wgt_flnm)

end subroutine read_weights

