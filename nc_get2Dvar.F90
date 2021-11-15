!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! svn propset svn:keywords "URL Rev Author Date Id"
! $URL: file:///data/zhuming/.vdras_source_code/SVN_REPOSITORY/VDRAS/trunk/vdras/io/netcdf4/nc_get2Dvar.F90 $
! $Rev: 355 $
! $Author: zhuming $
! $Date: 2014-09-30 11:02:42 -0600 (Tue, 30 Sep 2014) $
! $Id: nc_get2Dvar.F90 355 2014-09-30 17:02:42Z zhuming $
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine nc_get2Dvar(ncid, var_name, var, nrec, &
                       nxs, nxe, nys, nye)

   use netcdf

   implicit none
  
   integer, intent(in) :: ncid, nrec
   integer, intent(in) :: nxs, nxe, nys, nye

   character(len = *), intent(in) :: var_name
   real(kind=8), dimension(nxs:nxe, nys:nye), intent(out) :: var

   integer, dimension(3) :: start, count

 ! Variable id
   integer :: varid

 ! Return status
   integer :: status

   status = nf90_inq_varid(ncid, var_name, varid)
   if(status /= nf90_noerr) then 
       write(unit=0, fmt='(3a)') "Problem to get id for: <", trim(var_name), ">.", &
                                 "Error status: ", trim(nf90_strerror(status))
       write(unit=0, fmt='(3a, i4)') &
            "Stop in file: <", __FILE__, ">, line: ", __LINE__
       stop
   end if

   start(1) = nxs
   start(2) = nys
   start(3) = nrec

   count(1) = nxe - nxs + 1
   count(2) = nye - nys + 1
   count(3) = 1

   status = nf90_get_var(ncid,varid,var,start=start(1:3),count=count(1:3))
   if(status /= nf90_noerr) then
       write(unit=0, fmt='(3a)') "Problem to read: <", trim(var_name), ">.", &
                                "Error status: ", trim(nf90_strerror(status))
       write(unit=0, fmt='(3a, i4)') &
            "Stop in file: <", __FILE__, ">, line: ", __LINE__
       stop
   end if

end subroutine nc_get2Dvar

