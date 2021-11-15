!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! svn propset svn:keywords "URL Rev Author Date Id"
! $URL: file:///data/zhuming/.vdras_source_code/SVN_REPOSITORY/VDRAS/trunk/vdras/io/netcdf4/nc_get5Dvar.F90 $
! $Rev: 144 $
! $Author: huangwei $
! $Date: 2010-11-15 10:33:52 -0700 (Mon, 15 Nov 2010) $
! $Id: nc_get5Dvar.F90 144 2010-11-15 17:33:52Z huangwei $
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine nc_get5Dvar(ncid, var_name, var, nrec, &
                       nxs, nxe, nys, nye, nzs, nze, nobs, nrad)

   use netcdf

   implicit none
  
   integer, intent(in) :: ncid, nrec
   integer, intent(in) :: nxs, nxe, nys, nye, nzs, nze, nobs, nrad

   character(len = *), intent(in) :: var_name
   real(kind=8), dimension(nxs:nxe, nys:nye, nzs:nze, nobs, nrad), intent(out) :: var

   integer, dimension(6) :: start, count

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
   start(3) = nzs
   start(4) = 1
   start(5) = 1
   start(6) = nrec

   count(1) = nxe - nxs + 1
   count(2) = nye - nys + 1
   count(3) = nze - nzs + 1
   count(4) = nobs
   count(5) = nrad
   count(6) = 1

#if 0
   write(unit=0, fmt='(a, 4i5)') 'ncid=', ncid
   write(unit=0, fmt='(a, 4i5)') 'varid=', varid
   write(unit=0, fmt='(a, a/)') 'var_name=', var_name
   write(unit=0, fmt='(a, 4i5)') 'nxs, nys, nzs=', nxs, nys, nzs
   write(unit=0, fmt='(a, 4i5)') 'nxe, nye, nze=', nxe, nye, nze
   write(unit=0, fmt='(a, 4i5)') 'nrec=', nrec
   write(unit=0, fmt='(a, i5)') 'start(1)=', start(1)
   write(unit=0, fmt='(a, i5)') 'start(2)=', start(2)
   write(unit=0, fmt='(a, i5)') 'start(3)=', start(3)
   write(unit=0, fmt='(a, i5)') 'start(4)=', start(4)
   write(unit=0, fmt='(a, i5)') 'count(1)=', count(1)
   write(unit=0, fmt='(a, i5)') 'count(2)=', count(2)
   write(unit=0, fmt='(a, i5)') 'count(3)=', count(3)
   write(unit=0, fmt='(a, i5)') 'count(4)=', count(4)
   write(unit=0, fmt='(a, i5)') 'count(5)=', count(5)
   write(unit=0, fmt='(a, i5)') 'count(6)=', count(6)
#endif


   status = nf90_get_var(ncid,varid,var,start=start,count=count)
   if(status /= nf90_noerr) then
       write(unit=0, fmt='(3a)') "Problem to read: <", trim(var_name), ">.", &
                                "Error status: ", trim(nf90_strerror(status))
       write(unit=0, fmt='(3a, i4)') &
            "Stop in file: <", __FILE__, ">, line: ", __LINE__
       stop
   end if

end subroutine nc_get5Dvar

