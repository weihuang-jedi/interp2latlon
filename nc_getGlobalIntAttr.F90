!----------------------------------------------------------------------------------

subroutine nc_getGlobalIntAttr(ncid,desc,var)

   use netcdf

   implicit none
 
   integer, intent(in) :: ncid
   character(len=*), intent(in) :: desc
   integer, intent(out) :: var

   integer :: status

   status = nf90_get_att(ncid, NF90_GLOBAL, desc, var)

   if(status /= nf90_noerr) then 
       write(unit=0, fmt='(3a)') "Problem to get att: <", trim(desc), ">.", &
                                 "Error status: ", trim(nf90_strerror(status))
       write(unit=0, fmt='(a, i6)') "ncid=", ncid
       write(unit=0, fmt='(a, i6)') "var=", var

       write(unit=0, fmt='(3a,i6)') "file: ", __FILE__, ", line: ", __LINE__
       stop
   end if

end subroutine nc_getGlobalIntAttr

