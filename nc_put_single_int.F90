!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine nc_put_single_int(ncid, var_name, iv, nrec)

   use netcdf

   implicit none
 
   integer,          intent(in) :: ncid, nrec, iv
   character(len=*), intent(in) :: var_name

!--Variable id
   integer :: varid

!--Return status
   integer :: status

   integer, dimension(1) :: start, count, nv

   nv(1) = iv

   start(1) = nrec
   count(1) = 1

   status = nf90_inq_varid(ncid, trim(var_name), varid)
   if(status /= nf90_noerr) then
      write(unit=0, fmt='(3a)') "Problem to get varid for: <", trim(var_name), ">.", &
                                "Error status: ", trim(nf90_strerror(status))
      write(unit=0, fmt='(3a, i4)') &
           "Stop in file: <", __FILE__, ">, line: ", __LINE__
      stop
   end if

   status = nf90_put_var(ncid, varid, nv, start=start, count=count)
   if(status /= nf90_noerr) then
      write(unit=0, fmt='(3a)') "Problem to write variable: <", trim(var_name), ">.", &
                                "Error status: ", trim(nf90_strerror(status))
      write(unit=0, fmt='(3a, i4)') &
           "Stop in file: <", __FILE__, ">, line: ", __LINE__
      stop
   end if

end subroutine nc_put_single_int

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine nc_put_single_double(ncid, var_name, dv, nrec)

   use netcdf

   implicit none
 
   integer,          intent(in) :: ncid, nrec
   real(kind=8),     intent(in) :: dv
   character(len=*), intent(in) :: var_name

!--Variable id
   integer :: varid

!--Return status
   integer :: status

   integer, dimension(1) :: start, count
   real(kind=8), dimension(1) :: nv

   nv(1) = dv

   start(1) = nrec
   count(1) = 1

   status = nf90_inq_varid(ncid, trim(var_name), varid)
   if(status /= nf90_noerr) then
      write(unit=0, fmt='(3a)') "Problem to get varid for: <", trim(var_name), ">.", &
                                "Error status: ", trim(nf90_strerror(status))
      write(unit=0, fmt='(3a, i4)') &
           "Stop in file: <", __FILE__, ">, line: ", __LINE__
      stop
   end if

   status = nf90_put_var(ncid, varid, nv, start=start, count=count)
   if(status /= nf90_noerr) then
      write(unit=0, fmt='(3a)') "Problem to write variable: <", trim(var_name), ">.", &
                                "Error status: ", trim(nf90_strerror(status))
      write(unit=0, fmt='(3a, i4)') &
           "Stop in file: <", __FILE__, ">, line: ", __LINE__
      stop
   end if

end subroutine nc_put_single_double

