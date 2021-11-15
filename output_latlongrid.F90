!----------------------------------------------------------------------------------------

subroutine write_latlongrid(latlon, flnm)

   use netcdf
   use latlon_module
   use tile_module, only : check_status

   implicit none

   type(latlongrid), intent(in) :: latlon
   character(len=*), intent(in) :: flnm

   integer :: ncid, dimidx, dimidy, dimidp

   real(kind=8), dimension(1:latlon%nlon) :: lon
   real(kind=8), dimension(1:latlon%nlat) :: lat
   real(kind=8), dimension(1:latlon%npnt) :: pnt

   integer :: i, j, n, rc

   lon = latlon%lon
   lat = latlon%lat
   pnt = latlon%pnt

   rc = nf90_noerr

   !Create the file. 
   rc = nf90_create(trim(flnm), NF90_CLOBBER, ncid)
   call check_status(rc)

   print *, 'ncid = ', ncid

   rc = nf90_def_dim(ncid, 'lon', latlon%nlon, dimidx)
   call check_status(rc)
   rc = nf90_def_dim(ncid, 'lat', latlon%nlat, dimidy)
   call check_status(rc)
   rc = nf90_def_dim(ncid, 'pnt', latlon%npnt, dimidp)
   call check_status(rc)

   print *, 'dimidx = ', dimidx
   print *, 'dimidy = ', dimidy
   print *, 'dimidp = ', dimidp

   call write_global_attr(ncid, flnm, 'Weight of Grid', 'Lat-Lon')

   call write_var_attr(ncid, dimidx, dimidy, dimidp)

  !End define mode.
   rc = nf90_enddef(ncid)
   if(rc /= nf90_noerr) then
      write(unit=0, fmt='(a,i6,a)') "Problem to enddef ncid: <", ncid, ">."
      write(unit=0, fmt='(2a)') "Error status: ", trim(nf90_strerror(rc))
      write(unit=0, fmt='(3a, i4)') &
           "Stop in file: <", __FILE__, ">, line: ", __LINE__
      stop
   end if

   !write lon
   call nc_put1Dvar0(ncid, 'lon', lon, 1, latlon%nlon)

   !write lat
   call nc_put1Dvar0(ncid, 'lat', lat, 1, latlon%nlat)

   !write pnt
   call nc_put1Dvar0(ncid, 'pnt', pnt, 1, latlon%npnt)

   !--write pos
   call nc_put2Dvar0(ncid, 'pos', latlon%pos, 1, latlon%nlon, 1, latlon%nlat)

   !--write tile
   call nc_put3Dint0(ncid, 'tile', latlon%tile, 1, latlon%nlon, &
                     1, latlon%nlat, 1, latlon%npnt)

   !--write ilon
   call nc_put3Dint0(ncid, 'ilon', latlon%ilon, 1, latlon%nlon, &
                     1, latlon%nlat, 1, latlon%npnt)

   !--write jlat
   call nc_put3Dint0(ncid, 'jlat', latlon%jlat, 1, latlon%nlon, &
                     1, latlon%nlat, 1, latlon%npnt)

   !--write wgt
   call nc_put3Dvar0(ncid, 'wgt', latlon%wgt, 1, latlon%nlon, &
                     1, latlon%nlat, 1, latlon%npnt)

   rc =  nf90_close(ncid)

   print *, 'nf90_close rc = ', rc
   print *, 'nf90_noerr = ', nf90_noerr

   if(rc /= nf90_noerr) then
      write(unit=0, fmt='(a,i6,a)') "Problem to close ncid: <", ncid, ">."
      write(unit=0, fmt='(2a)') "Error status: ", trim(nf90_strerror(rc))
      write(unit=0, fmt='(3a, i4)') &
           "Stop in file: <", __FILE__, ">, line: ", __LINE__
      stop
   end if

   print *, 'Finished Write to file: ', trim(flnm)

end subroutine write_latlongrid

!-------------------------------------------------------------------------------------
subroutine write_var_attr(ncid, dimidx, dimidy, dimidp)

   use netcdf

   implicit none

   integer, intent(in) :: ncid
   integer, intent(in) :: dimidx, dimidy, dimidp

   integer, dimension(6) :: dimids
   integer :: rc, nd
   integer :: missing_int
   real(kind=8) :: missing_value

   missing_value = -1.0e38
   missing_int = -999999

   dimids(1) = dimidx
   nd = 1
!--Field lon
   call nc_putAxisAttr(ncid, nd, dimids, NF90_REAL, &
                      "lon", &
                      "Lontitude Coordinate", &
                      "degree_east", &
                      "Longitude" )

   dimids(1) = dimidy
   nd = 1
!--Field lat
   call nc_putAxisAttr(ncid, nd, dimids, NF90_REAL, &
                      "lat", &
                      "Latitude Coordinate", &
                      "degree_north", &
                      "Latitude" )

   dimids(1) = dimidp
   nd = 1
!--Field pnt
   call nc_putAxisAttr(ncid, nd, dimids, NF90_REAL, &
                      "pnt", &
                      "Points for Weighting", &
                      "unitless", &
                      "Point" )

   dimids(1) = dimidx
   dimids(2) = dimidy
   nd = 2

!--Field 1, pos
   call nc_putAttr(ncid, nd, dimids, NF90_REAL, &
                   "pos", &
                   "Postion in Tile", &
                   "unitless", &
                   "lat lon", &
                   missing_value)

   dimids(1) = dimidx
   dimids(2) = dimidy
   dimids(3) = dimidp
   nd = 3

!--Field 2, tile
   call nc_putAttrInt(ncid, nd, dimids, NF90_INT, &
                   "tile", &
                   "Tile Number of Grid", &
                   "unitless", &
                   "pnt lat lon", &
                   missing_int)

!--Field 3, ilon
   call nc_putAttrInt(ncid, nd, dimids, NF90_INT, &
                   "ilon", &
                   "Index of Longitude", &
                   "unitless", &
                   "pnt lat lon", &
                   missing_int)

!--Field 4, jlat
   call nc_putAttrInt(ncid, nd, dimids, NF90_INT, &
                   "jlat", &
                   "Index of Latitude", &
                   "unitless", &
                   "pnt lat lon", &
                   missing_int)

!--Field 5, wgt
   call nc_putAttr(ncid, nd, dimids, NF90_REAL, &
                   "wgt", &
                   "Weight of Grids", &
                   "unitless", &
                   "pnt lat lon", &
                   missing_value)

end subroutine write_var_attr

!---------------------------------------------------------------------------
subroutine write_global_attr(ncid, filename, title, type)

   implicit none

   integer, intent(in) :: ncid
   character(len = *), intent(in) :: filename, title, type

  !output global attributes
   call nc_putGlobalCharAttr(ncid, 'filename', trim(filename))
   call nc_putGlobalCharAttr(ncid, 'title', trim(title))
   call nc_putGlobalCharAttr(ncid, 'grid_type', trim(type))

end subroutine write_global_attr

