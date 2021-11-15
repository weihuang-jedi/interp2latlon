!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! svn propset svn:keywords "URL Rev Author Date Id"
! $URL: file:///data/zhuming/.vdras_source_code/SVN_REPOSITORY/VDRAS/trunk/vdras/io/netcdf4/nc_write_dimInfo.F90 $
! $Rev: 144 $
! $Author: huangwei $
! $Date: 2010-11-15 10:33:52 -0700 (Mon, 15 Nov 2010) $
! $Id: nc_write_dimInfo.F90 144 2010-11-15 17:33:52Z huangwei $
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine nc_write_dimInfo(ncid,dim_name,dim_len,dim_id)

   use netcdf
!  use dmp_util_module, only : on_monitor

   implicit none
 
   integer, intent(in) :: ncid
   integer, intent(in) :: dim_len
   integer, intent(out) :: dim_id

   character(len = *), intent(in) :: dim_name
 
   integer :: status

!  if(on_monitor) then
   status = nf90_def_dim(ncid, dim_name, dim_len, dim_id)
   if(status /= nf90_noerr) then 
       write(unit=0, fmt='(3a)') "Problem to def dim: <", trim(dim_name), ">.", &
                                 "Error status: ", trim(nf90_strerror(status))

       write(unit=0, fmt='(3a,i6)') "file: ", __FILE__, ", line: ", __LINE__
       write(unit=0, fmt='(a, i6)') "ncid=", ncid
       stop
   end if
!  endif

end subroutine nc_write_dimInfo

