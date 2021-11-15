!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! svn propset svn:keywords "URL Rev Author Date Id"
! $URL: file:///data/zhuming/.vdras_source_code/SVN_REPOSITORY/VDRAS/trunk/vdras/io/netcdf4/nc_put1Dint.F90 $
! $Rev: 145 $
! $Author: huangwei $
! $Date: 2010-11-15 10:34:40 -0700 (Mon, 15 Nov 2010) $
! $Id: nc_put1Dint.F90 145 2010-11-15 17:34:40Z huangwei $
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine nc_put1Dint(ncid, var_name, v1d, nrec, &
                       m1s, m1e)

   use netcdf

   implicit none
 
   integer, intent(in) :: ncid, nrec
   integer, intent(in) :: m1s, m1e
   character(len=*), intent(in) :: var_name
   integer, dimension(m1s:m1e), intent(in) :: v1d

 ! Variable id
   integer :: varid

 ! Return status
   integer :: status

   integer, dimension(2) :: start, count

   start(1) = m1s
   start(2) = nrec

   count(1) = m1e - m1s + 1
   count(2) = 1

   status = nf90_inq_varid(ncid, var_name, varid)
   if(status /= nf90_noerr) then 
      write(unit=0, fmt='(3a)') "Problem to get id for: <", trim(var_name), ">.", &
                                "Error status: ", trim(nf90_strerror(status))
      write(unit=0, fmt='(3a, i4)') &
          "Stop in file: <", __FILE__, ">, line: ", __LINE__
      stop
   end if

   status = nf90_put_var(ncid,varid,v1d,start=start,count=count)
   if(status /= nf90_noerr) then
          write(unit=0, fmt='(3a)') "Problem to write variable: <", trim(var_name), ">.", &
                                 "Error status: ", trim(nf90_strerror(status))
      write(unit=0, fmt='(3a, i4)') &
          "Stop in file: <", __FILE__, ">, line: ", __LINE__
      stop
   end if

end subroutine nc_put1Dint

