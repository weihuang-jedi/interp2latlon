!--------------------------------------------------------------------
PROGRAM fv3interp2latlon

   use namelist_module
   use tile_module
   use latlon_module

   IMPLICIT NONE

   type tiletype
      type(tilegrid), dimension(6) :: tile
   end type tiletype

   type(tiletype), dimension(max_types) :: types
   type(latlongrid)                     :: latlon
   integer :: n
   logical :: last

   call read_namelist('input.nml')

   call initialize_latlongrid(nlon, nlat, npnt, latlon)

   do n = 1, num_types
      print *, 'dirname: <', trim(dirname), &
               '>, data_types(', n, ') = <', trim(data_types(n)), '>'
      call initialize_tilegrid(types(n)%tile, trim(dirname), trim(data_types(n)))

      if(trim(data_types(n)) == 'fv_core.res.tile') then
         latlon%nlev = types(n)%tile(1)%nz
      else if(trim(data_types(n)) == 'sfc_data.tile') then
         latlon%nlay = types(n)%tile(1)%nz
      end if
   end do

   if(generate_weights) then
      call generate_weight(types(1)%tile, latlon)
      call write_latlongrid(latlon, wgt_flnm)
   else
      call read_weights(latlon, wgt_flnm)

      do n = 1, num_types
         last = (n == num_types)
        !print *, 'n = ', n
        !print *, 'last = ', last
         call generate_header(n, types(n)%tile, latlon, &
                              trim(data_types(n)), output_flnm, last)
      end do

      do n = 1, num_types
         call interp2latlongrid(trim(data_types(n)), types(n)%tile, latlon)
      end do
   end if

   do n = 1, num_types
      call finalize_tilegrid(types(n)%tile)
   end do

   call closefile(latlon)
   call finalize_latlongrid(latlon)

END PROGRAM fv3interp2latlon

