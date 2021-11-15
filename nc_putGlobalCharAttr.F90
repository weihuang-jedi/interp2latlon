!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! svn propset svn:keywords "URL Rev Author Date Id"
! $URL: file:///data/zhuming/.vdras_source_code/SVN_REPOSITORY/VDRAS/trunk/vdras/io/netcdf4/nc_putGlobalCharAttr.F90 $
! $Rev: 144 $
! $Author: huangwei $
! $Date: 2010-11-15 10:33:52 -0700 (Mon, 15 Nov 2010) $
! $Id: nc_putGlobalCharAttr.F90 144 2010-11-15 17:33:52Z huangwei $
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine nc_putGlobalCharAttr(ncid,desc,var)

   use netcdf

   implicit none
 
   integer, intent(in) :: ncid
   character(len = *), intent(in) :: var
   character(len = *), intent(in) :: desc
   integer :: status

   status = nf90_put_att(ncid, NF90_GLOBAL, desc, var)

   if(status /= nf90_noerr) then 
       write(unit=0, fmt='(3a)') "Problem to put att: <", trim(desc), ">.", &
                                 "Error status: ", trim(nf90_strerror(status))

       write(unit=0, fmt='(3a,i6)') "file: ", __FILE__, ", line: ", __LINE__
       write(unit=0, fmt='(a,i6)') "ncid=", ncid
       write(unit=0, fmt='(2a)') "desc=", desc
       write(unit=0, fmt='(2a)') "var=", var
       stop
   end if

end subroutine nc_putGlobalCharAttr

