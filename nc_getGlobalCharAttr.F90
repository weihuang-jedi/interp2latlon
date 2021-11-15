!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! svn propset svn:keywords "URL Rev Author Date Id"
! $URL: file:///data/zhuming/.vdras_source_code/SVN_REPOSITORY/VDRAS/trunk/vdras/io/netcdf4/nc_getGlobalCharAttr.F90 $
! $Rev: 144 $
! $Author: huangwei $
! $Date: 2010-11-15 10:33:52 -0700 (Mon, 15 Nov 2010) $
! $Id: nc_getGlobalCharAttr.F90 144 2010-11-15 17:33:52Z huangwei $
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine nc_getGlobalCharAttr(ncid,desc,var)

   use netcdf

   implicit none
 
   integer, intent(in) :: ncid
   character(len=*), intent(in) :: desc
   character(len=*), intent(out) :: var
   integer :: status

!  write(unit=0, fmt='(3a,i6)') "file: ", __FILE__, ", line: ", __LINE__
!  write(unit=0, fmt='(a,i6)') "ncid=", ncid
!  write(unit=0, fmt='(2a)') "desc=", desc

   status = nf90_get_att(ncid, NF90_GLOBAL, desc, var)

   if(status /= nf90_noerr) then 
       write(unit=0, fmt='(3a)') "Problem to get att: <", trim(desc), ">.", &
                                 "Error status: ", trim(nf90_strerror(status))
       write(unit=0, fmt='(3a, i4)') &
            "Stop in file: <", __FILE__, ">, line: ", __LINE__
       stop
   end if

!  write(unit=0, fmt='(2a)') "var=", var

end subroutine nc_getGlobalCharAttr

