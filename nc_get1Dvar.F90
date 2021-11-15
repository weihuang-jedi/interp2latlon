!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine nc_get1Dvar(ncid, var_name, var, nrec, &
                       nzs, nze)

   use netcdf

   implicit none
  
   integer, intent(in) :: ncid, nrec
   integer, intent(in) :: nzs, nze

   character(len = *), intent(in) :: var_name
   real(kind=8), dimension(nzs:nze), intent(out) :: var

   integer, dimension(2) :: start, count

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

   start(1) = nzs
   start(2) = nrec

   count(1) = nze - nzs + 1
   count(2) = 1

   status = nf90_get_var(ncid,varid,var,start=start,count=count)
   if(status /= nf90_noerr) then
       write(unit=0, fmt='(3a)') "Problem to read: <", trim(var_name), ">.", &
                                "Error status: ", trim(nf90_strerror(status))
       write(unit=0, fmt='(3a, i4)') &
            "Stop in file: <", __FILE__, ">, line: ", __LINE__
       stop
   end if

end subroutine nc_get1Dvar

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine nc_get1Dvar0(ncid, var_name, var, nzs, nze)

   use netcdf

   implicit none
  
   integer, intent(in) :: ncid
   integer, intent(in) :: nzs, nze

   character(len = *), intent(in) :: var_name
   real(kind=8), dimension(nzs:nze), intent(out) :: var

   integer, dimension(1) :: start, count

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

   start(1) = nzs

   count(1) = nze - nzs + 1

   status = nf90_get_var(ncid,varid,var,start=start,count=count)
   if(status /= nf90_noerr) then
       write(unit=0, fmt='(3a)') "Problem to read: <", trim(var_name), ">.", &
                                "Error status: ", trim(nf90_strerror(status))
       write(unit=0, fmt='(3a, i4)') &
            "Stop in file: <", __FILE__, ">, line: ", __LINE__
       stop
   end if

end subroutine nc_get1Dvar0

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine nc_get1Ddbl0(ncid, var_name, var, nzs, nze)

   use netcdf

   implicit none

   integer, intent(in) :: ncid
   integer, intent(in) :: nzs, nze

   character(len = *), intent(in) :: var_name
   real(kind=8), dimension(nzs:nze), intent(out) :: var

   integer, dimension(1) :: start, count

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

   start(1) = nzs

   count(1) = nze - nzs + 1

   status = nf90_get_var(ncid,varid,var,start=start,count=count)
   if(status /= nf90_noerr) then
       write(unit=0, fmt='(3a)') "Problem to read: <", trim(var_name), ">.", &
                                "Error status: ", trim(nf90_strerror(status))
       write(unit=0, fmt='(3a, i4)') &
            "Stop in file: <", __FILE__, ">, line: ", __LINE__
       stop
   end if

end subroutine nc_get1Ddbl0

