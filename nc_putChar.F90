!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! svn propset svn:keywords "URL Rev Author Date Id"
! $URL: file:///data/zhuming/.vdras_source_code/SVN_REPOSITORY/VDRAS/trunk/vdras/io/netcdf4/nc_putChar.F90 $
! $Rev: 144 $
! $Author: huangwei $
! $Date: 2010-11-15 10:33:52 -0700 (Mon, 15 Nov 2010) $
! $Id: nc_putChar.F90 144 2010-11-15 17:33:52Z huangwei $
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine nc_putChar(ncid,var_name,str,nrec)

   use netcdf

   implicit none
 
   integer, intent(in) :: ncid, nrec
   character(len=*), intent(in) :: str
   character(len=*), intent(in) :: var_name

!--Variable id
   integer :: varid

!--Return status
   integer :: status

   integer :: start(2), count(2)

   start(1) = 1
   start(2) = nrec

   count(1) = len_trim(str)
   count(2) = 1

   status = nf90_inq_varid(ncid, trim(var_name), varid)
   if(status /= nf90_noerr) then
      write(unit=0, fmt='(3a)') "Problem to get varid for: <", trim(var_name), ">.", &
                                "Error status: ", trim(nf90_strerror(status))
      write(unit=0, fmt='(3a, i4)') &
           "Stop in file: <", __FILE__, ">, line: ", __LINE__
      stop
   end if

   status = nf90_put_var(ncid,varid,str,start=start,count=count)
   if(status /= nf90_noerr) then
      write(unit=0, fmt='(3a)') "Problem to write variable: <", trim(var_name), ">.", &
                                "Error status: ", trim(nf90_strerror(status))
      write(unit=0, fmt='(3a, i4)') &
           "Stop in file: <", __FILE__, ">, line: ", __LINE__
      stop
   end if

end subroutine nc_putChar

