!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! svn propset svn:keywords "URL Rev Author Date Id"
! $URL: file:///neem/users/huangwei/.vdras_source_code/SVN_REPOSITORY/trunk/vdras/io/netcdf4/nc_putAttr.F90 $
! $Rev: 144 $
! $Author: huangwei $
! $Date: 2010-11-15 10:33:52 -0700 (Mon, 15 Nov 2010) $
! $Id: nc_putAttr.F90 144 2010-11-15 17:33:52Z huangwei $
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine nc_putAxisAttr(ncid, nd, dimid, xtype, var_name, &
                      desc, units, coordinates)

   use netcdf

   implicit none
 
   integer, intent(in) :: ncid, nd, xtype
   integer, dimension(6), intent(in) :: dimid
   character(len=*), intent(in) :: var_name, desc, units, coordinates

!  real(kind=8),    intent(in) :: missing_value
!  real(kind=8),    intent(in), optional :: missing_value
!  integer, intent(in), optional :: missing_int

!--Variable id
   integer :: varid, md

!--Return status
   integer :: status

   md = nd

   if(md > 6) then
       write(unit=0, fmt='(a, i6)') "We can only handle data up to 5d. but here nd = ", nd
   endif

!--Always set the extra dimension unlimited.
!  dimid(nd+1) = nf90_unlimited

   status = nf90_def_var(ncid, trim(var_name), xtype, dimid(1:md), varid)
   if(status /= nf90_noerr) then 
      write(unit=0, fmt='(a, i6)') "ncid: ", ncid
      write(unit=0, fmt='(a, 6i6)') "dimid: ", dimid(1:md)
      write(unit=0, fmt='(3a)') "Problem to def varid for: <", trim(var_name), ">.", &
                                "Error status: ", trim(nf90_strerror(status))
      write(unit=0, fmt='(3a, i4)') &
           "Stop in file: <", __FILE__, ">, line: ", __LINE__
      stop
   endif

!  status = nf90_put_att(ncid, varid, "description", trim(desc))
   status = nf90_put_att(ncid, varid, "long_name", trim(desc))
   if(status /= nf90_noerr) then
      write(unit=0, fmt='(3a)') "Problem to write attribute description: <", trim(desc), ">.", &
                                "Error status: ", trim(nf90_strerror(status))
      write(unit=0, fmt='(3a, i4)') &
           "Stop in file: <", __FILE__, ">, line: ", __LINE__
      stop
   endif

!    status = nf90_put_att(ncid, varid, "long_name", trim(var_name))
!    if(status /= nf90_noerr) then
!       write(unit=0, fmt='(3a)') "Problem to write attribute long_name: <", trim(var_name), ">.", & 
!                                 "Error status: ", trim(nf90_strerror(status))
!       write(unit=0, fmt='(3a, i4)') &
!           "Stop in file: <", __FILE__, ">, line: ", __LINE__
!       stop
!    endif

   status = nf90_put_att(ncid, varid, "units", trim(units))
   if(status /= nf90_noerr) then
      write(unit=0, fmt='(3a)') "Problem to write attribute units: <", trim(units), ">.", &
                                "Error status: ", trim(nf90_strerror(status))
      write(unit=0, fmt='(3a, i4)') &
           "Stop in file: <", __FILE__, ">, line: ", __LINE__
      stop
   endif

   status = nf90_put_att(ncid, varid, "_CoordinateAxisType", trim(coordinates))
   if(status /= nf90_noerr) then
      write(unit=0, fmt='(3a)') "Problem to write attribute coordinates: <", trim(coordinates), ">.", & 
                                "Error status: ", trim(nf90_strerror(status))
      write(unit=0, fmt='(3a, i4)') &
          "Stop in file: <", __FILE__, ">, line: ", __LINE__
      stop
   endif

!  if(present(missing_value)) then
!      status = nf90_put_att(ncid, varid, "_FillValue", missing_value)
!      if(status /= nf90_noerr) then
!         write(unit=0, fmt='(a, f12.2)') "Problem to write attribute missing_value: ", missing_value
!         write(unit=0, fmt='(3a)') "Error status: ", trim(nf90_strerror(status))
!         write(unit=0, fmt='(3a, i4)') &
!              "Stop in file: <", __FILE__, ">, line: ", __LINE__
!         stop
!      endif
!  endif

#if 0
!   if(present(missing_int)) then
!      status = nf90_put_att(ncid, varid, "_FillValue", missing_int)
!      if(status /= nf90_noerr) then
!         write(unit=0, fmt='(a, f12.2)') "Problem to write attribute missing_int: ", missing_int
!         write(unit=0, fmt='(3a)') "Error status: ", trim(nf90_strerror(status))
!         write(unit=0, fmt='(3a, i4)') &
!              "Stop in file: <", __FILE__, ">, line: ", __LINE__
!         stop
!      endif
!   endif
#endif

end subroutine nc_putAxisAttr

