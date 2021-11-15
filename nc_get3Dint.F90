!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine nc_get3Dint(ncid, var_name, var, nrec, &
                       nxs, nxe, nys, nye, nzs, nze)

   use netcdf

   implicit none
  
   integer, intent(in) :: ncid, nrec
   integer, intent(in) :: nxs, nxe, nys, nye, nzs, nze

   character(len = *), intent(in) :: var_name
   integer, dimension(nxs:nxe, nys:nye, nzs:nze), intent(out) :: var

   integer, dimension(4) :: start, count

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
   start(4) = nrec

   count(1) = nxe - nxs + 1
   count(2) = nye - nys + 1
   count(3) = nze - nzs + 1
   count(4) = 1

   status = nf90_get_var(ncid,varid,var,start=start(1:4),count=count(1:4))
   if(status /= nf90_noerr) then
       write(unit=0, fmt='(3a)') "Problem to read: <", trim(var_name), ">.", &
                                "Error status: ", trim(nf90_strerror(status))
       write(unit=0, fmt='(3a, i4)') &
            "Stop in file: <", __FILE__, ">, line: ", __LINE__
       stop
   end if

end subroutine nc_get3Dint

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine nc_get3Dint0(ncid, var_name, var, &
                        nxs, nxe, nys, nye, nzs, nze)

   use netcdf

   implicit none
  
   integer, intent(in) :: ncid
   integer, intent(in) :: nxs, nxe, nys, nye, nzs, nze

   character(len = *), intent(in) :: var_name
   integer, dimension(nxs:nxe, nys:nye, nzs:nze), intent(out) :: var
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
   start(3) = nzs

   count(1) = nxe - nxs + 1
   count(2) = nye - nys + 1
   count(3) = nze - nzs + 1

   status = nf90_get_var(ncid,varid,var,start=start,count=count)
   if(status /= nf90_noerr) then
       write(unit=0, fmt='(3a)') "Problem to read: <", trim(var_name), ">.", &
                                "Error status: ", trim(nf90_strerror(status))
       write(unit=0, fmt='(3a, i4)') &
            "Stop in file: <", __FILE__, ">, line: ", __LINE__
       stop
   end if

end subroutine nc_get3Dint0


