!----------------------------------------------------------------------------------

subroutine nc_put1Dvar(ncid, var_name, v1d, nrec, &
                       m1s, m1e)

   use netcdf

   implicit none
 
   integer, intent(in) :: ncid, nrec
   integer, intent(in) :: m1s, m1e
   character(len=*), intent(in) :: var_name
   real(kind=8), dimension(m1s:m1e), intent(in) :: v1d

!--Variable id
   integer :: varid

!--Return Code
   integer :: rc

   integer :: start(2), length(2)

   start(1) = m1s
   start(2) = nrec

   length(1) = m1e - m1s + 1
   length(2) = 1

   rc = nf90_inq_varid(ncid, trim(var_name), varid)
   if(rc /= nf90_noerr) then
      write(unit=0, fmt='(3a)') "Problem to get varid for: <", trim(var_name), ">.", &
                                "Error status: ", trim(nf90_strerror(rc))
      write(unit=0, fmt='(3a, i4)') &
           "Stop in file: <", __FILE__, ">, line: ", __LINE__
      stop
   end if

   rc = nf90_put_var(ncid,varid,v1d,start=start,count=length)
   if(rc /= nf90_noerr) then
      write(unit=0, fmt='(3a)') "Problem to write variable: <", trim(var_name), ">.", &
                                "Error status: ", trim(nf90_strerror(rc))
      write(unit=0, fmt='(3a, i4)') &
           "Stop in file: <", __FILE__, ">, line: ", __LINE__
      stop
   end if

end subroutine nc_put1Dvar

!----------------------------------------------------------------------------------

subroutine nc_put1Dvar0(ncid, var_name, v1d, m1s, m1e)

   use netcdf

   implicit none
 
   integer, intent(in) :: ncid, m1s, m1e
   character(len=*), intent(in) :: var_name
   real(kind=8), dimension(m1s:m1e), intent(in) :: v1d

!--Variable id
   integer :: varid

!--Return Code
   integer :: rc

   integer :: start(1), length(1)

   start(1) = m1s
   length(1) = m1e - m1s + 1

   rc = nf90_inq_varid(ncid, trim(var_name), varid)

  !print *, 'var_name=', trim(var_name)
  !print *, 'ncid,varid=', ncid,varid
  !print *, 'rc=', rc
  !print *, 'v1d=', v1d

   if(rc /= nf90_noerr) then
      write(unit=0, fmt='(3a)') "Problem to get varid for: <", trim(var_name), ">.", &
                                "Error status: ", trim(nf90_strerror(rc))
      write(unit=0, fmt='(3a, i4)') &
           "Stop in file: <", __FILE__, ">, line: ", __LINE__
      stop
   end if

   rc = nf90_put_var(ncid,varid,v1d,start=start,count=length)
  !rc = nf90_put_var(ncid,varid,v1d)
   if(rc /= nf90_noerr) then
      write(unit=0, fmt='(3a)') "Problem to write variable: <", trim(var_name), ">.", &
                                "Error status: ", trim(nf90_strerror(rc))
      write(unit=0, fmt='(3a, i4)') &
           "Stop in file: <", __FILE__, ">, line: ", __LINE__
      stop
   end if

end subroutine nc_put1Dvar0

!----------------------------------------------------------------------------------

subroutine nc_put1Ddbl0(ncid, var_name, v1d, m1s, m1e)

   use netcdf

   implicit none

   integer, intent(in) :: ncid, m1s, m1e
   character(len=*), intent(in) :: var_name
   real(kind=8), dimension(m1s:m1e), intent(in) :: v1d

!--Variable id
   integer :: varid

!--Return Code
   integer :: rc

   integer :: start(1), length(1)

   start(1) = m1s

   length(1) = m1e - m1s + 1

   rc = nf90_inq_varid(ncid, trim(var_name), varid)
   if(rc /= nf90_noerr) then
      write(unit=0, fmt='(3a)') "Problem to get varid for: <", trim(var_name), ">.", &
                                "Error status: ", trim(nf90_strerror(rc))
      write(unit=0, fmt='(3a, i4)') &
           "Stop in file: <", __FILE__, ">, line: ", __LINE__
      stop
   end if

  !print *, 'var_name=', trim(var_name)
  !print *, 'ncid,varid=', ncid,varid
  !print *, 'v1d=', v1d

   rc = nf90_put_var(ncid,varid,v1d,start=start,count=length)
  !rc = nf90_put_var(ncid,varid,v1d)
   if(rc /= nf90_noerr) then
      write(unit=0, fmt='(3a)') "Problem to write variable: <", trim(var_name), ">.", &
                                "Error status: ", trim(nf90_strerror(rc))
      write(unit=0, fmt='(3a, i4)') &
           "Stop in file: <", __FILE__, ">, line: ", __LINE__
      stop
   end if

end subroutine nc_put1Ddbl0

