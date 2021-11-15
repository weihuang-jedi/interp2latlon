!-------------------------------------------------------------

MODULE namelist_module

  implicit none

  integer, parameter :: nml_unit = 7
  integer, parameter :: max_types = 5

  CHARACTER(LEN=1024) :: program_name
  character(len=1024) :: dirname
  character(len=1024) :: output_flnm, wgt_flnm
  character(len=128), dimension(max_types) :: data_types
  integer :: nlat, nlon, npnt, num_types
  logical :: generate_weights, debug_on

contains
  subroutine read_namelist(file_path)
    implicit none

    !! Reads Namelist from given file.
    character(len=*),  intent(in)  :: file_path
    integer :: rc

    ! Namelist definition.
    namelist /control_param/ program_name, dirname, &
                             output_flnm, wgt_flnm, &
                             nlat, nlon, npnt, &
                             num_types, data_types, &
                             generate_weights

    program_name = 'Interpolate FV3 to regular Lat-Lon Grid'
    dirname = '/work/noaa/gsienkf/weihuang/jedi/src/mitgcm/MITgcm/verification/fizhi-cs-aqualev20/run/mnc_test_0001/'

    output_flnm = 'latlon_grid.nc'
    wgt_flnm = 'weights.nc'

    nlon = 360
    nlat = 180
    npnt = 4
    num_types = 1

    data_types(1) = 'fv_core.res.tile'

    generate_weights = .false.
    debug_on = .false.

    ! Check whether file exists.
    inquire(file=file_path, iostat=rc)

    if(rc /= 0) then
      write(unit=0, fmt='(3a)') 'Error: input file "', &
                             trim(file_path), '" does not exist.'
      return
    end if

    ! Open and read Namelist file.
    open(action='read', file=file_path, iostat=rc, unit=nml_unit)
    read(nml=control_param, iostat=rc, unit=nml_unit)

    if(rc /= 0) then
      write(unit=0, fmt='(a)') 'Error: invalid Namelist format.'
    end if

    close(nml_unit)

   !print *, 'dirname: ', trim(dirname)
   !print *, 'data_types(1): ', trim(data_types(1))
   !print *, 'nlon, nlat, npnt = ', nlon, nlat, npnt

  end subroutine read_namelist

END MODULE namelist_module

