!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! svn propset svn:keywords "URL Rev Author Date Id"
! $URL: file:///data/zhuming/.vdras_source_code/SVN_REPOSITORY/VDRAS/trunk/vdras/io/netcdf4/nc_put4Dvar.F90 $
! $Rev: 144 $
! $Author: huangwei $
! $Date: 2010-11-15 10:33:52 -0700 (Mon, 15 Nov 2010) $
! $Id: nc_put4Dvar.F90 144 2010-11-15 17:33:52Z huangwei $
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine nc_put4Dvar(ncid, var_name, v4d, nrec, &
                      m1s, m1e, m2s, m2e, m3s, m3e, m4s, m4e)

   use netcdf

   implicit none
 
   integer, intent(in) :: ncid, nrec
   integer, intent(in) :: m1s, m1e, m2s, m2e, m3s, m3e, m4s, m4e
   character(len=*), intent(in) :: var_name
   real(kind=8), dimension(m1s:m1e, m2s:m2e, m3s:m3e, m4s:m4e), intent(in) :: v4d

 ! Variable id
   integer :: varid

 ! Return status
   integer :: status

   integer, dimension(5) :: start, count

   start(1) = m1s
   start(2) = m2s
   start(3) = m3s
   start(4) = m4s
   start(5) = nrec

   count(1) = m1e - m1s + 1
   count(2) = m2e - m2s + 1
   count(3) = m3e - m3s + 1
   count(4) = m4e - m4s + 1
   count(5) = 1

   status = nf90_inq_varid(ncid, var_name, varid)
   if(status /= nf90_noerr) then 
      write(unit=0, fmt='(3a)') "Problem to get id for: <", trim(var_name), ">.", &
                                "Error status: ", trim(nf90_strerror(status))
      write(unit=0, fmt='(3a, i4)') &
          "Stop in file: <", __FILE__, ">, line: ", __LINE__
      stop
   end if

   status = nf90_put_var(ncid,varid,v4d,start=start,count=count)
   if(status /= nf90_noerr) then
          write(unit=0, fmt='(3a)') "Problem to write variable: <", trim(var_name), ">.", &
                                 "Error status: ", trim(nf90_strerror(status))
      write(unit=0, fmt='(3a, i4)') &
          "Stop in file: <", __FILE__, ">, line: ", __LINE__
      stop
   end if

end subroutine nc_put4Dvar

