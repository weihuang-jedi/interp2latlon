!-----------------------------------------------------------------------
!  The tile module read both tile specification, and tiled data.
!-----------------------------------------------------------------------
module tile_module

  use netcdf

  implicit none

  !-----------------------------------------------------------------------
  ! Define interfaces and attributes for module routines

  private
  public :: vartype
  public :: tilegrid
  public :: initialize_tilegrid
  public :: finalize_tilegrid
  public :: check_status

  !-----------------------------------------------------------------------

  ! Define tile structure.

  type vartype
     integer             :: varid
     character(len=1024) :: name
     integer             :: xtype, ndims, nAtts, deflate_level, endianness
     logical             :: contiguous, shuffle, fletcher32
     integer, dimension(:), allocatable :: dimids
     integer, dimension(:), allocatable :: dimlen
     character(len=128), dimension(:), allocatable :: dimnames
    !integer, dimension(:), allocatable :: chunksizes
  end type vartype

  type tilegrid
     character(len=1024)                   :: filename
     integer                               :: fileid
     integer                               :: nDims, nVars, nGlobalAtts, unlimDimID
     integer                               :: nx, ny, nz, nt
     integer, dimension(:),    allocatable :: varids
     integer, dimension(:),    allocatable :: dimids
     integer, dimension(:),    allocatable :: dimlen
     character(len=128), dimension(:),    allocatable :: dimnames
     real(kind=8), dimension(:, :), allocatable :: lon, lat
     integer, dimension(:),    allocatable :: x, y, z, time

     real(kind=8), dimension(:),       allocatable :: var1d
     real(kind=8), dimension(:, :),    allocatable :: var2d
     real(kind=8), dimension(:, :, :), allocatable :: var3d

     type(vartype), dimension(:), allocatable :: vars
  end type tilegrid

  !-----------------------------------------------------------------------

contains

  !-----------------------------------------------------------------------

  subroutine initialize_tilegrid(tile, dirname, prefix)

    implicit none

    type(tilegrid), dimension(6), intent(out) :: tile
    character(len=*),             intent(in)  :: dirname, prefix

    integer :: i, k, n, rc
    integer :: ny2, ik
    integer :: include_parents, dimlen

    character(len=1024) :: dimname, varname

   !print *, 'Enter initialize_tilegrid'
   !print *, 'dirname: <', trim(dirname), '>'
   !print *, 'prefix: <', trim(prefix), '>'

    include_parents = 0
    ny2 = -1

    do n = 1, 6
      write(tile(n)%filename, fmt='(2a, i1, a)') &
            trim(dirname), trim(prefix), n, '.nc'
     !print *, 'Tile ', n, ', open filename: ', trim(tile(n)%filename)
      rc = nf90_open(trim(tile(n)%filename), nf90_nowrite, tile(n)%fileid)
      call check_status(rc)
     !print *, 'Tile ', n, ', fileid: ', tile(n)%fileid

      rc = nf90_inquire(tile(n)%fileid, tile(n)%nDims, tile(n)%nVars, &
               tile(n)%nGlobalAtts, tile(n)%unlimdimid)
      call check_status(rc)
     !print *, 'Tile ', n, ', nVars: ', tile(n)%nVars
     !print *, 'Tile ', n, ', nDims: ', tile(n)%nDims

      ! Allocate memory.
      allocate(tile(n)%dimids(tile(n)%nDims))
      allocate(tile(n)%dimlen(tile(n)%nDims))
      allocate(tile(n)%dimnames(tile(n)%nDims))

      rc = nf90_inq_dimids(tile(n)%fileid, tile(n)%nDims, tile(n)%dimids, include_parents)
      call check_status(rc)

     !print *, 'Tile ', n, ', dimids: ', tile(n)%dimids
      tile(n)%nz = 1
      tile(n)%nt = 1

      do i = 1, tile(n)%nDims
         rc = nf90_inquire_dimension(tile(n)%fileid, tile(n)%dimids(i), dimname, dimlen)
         call check_status(rc)
        !print *, 'Dim No. ', i, ': ', trim(dimname), ', dimlen=', dimlen

         if(trim(dimname) == 'X') then
            tile(n)%nx = dimlen
         else if(trim(dimname) == 'Y') then
            tile(n)%ny = dimlen
         else if(trim(dimname) == 'Z') then
            tile(n)%nz = dimlen
         else if(trim(dimname) == 'T') then
            tile(n)%nt = dimlen
         end if

         tile(n)%dimlen(i) = dimlen
         tile(n)%dimnames(i) = trim(dimname)
      end do

      print *, 'tile(n)%nx = ', tile(n)%nx, ', tile(n)%ny = ', tile(n)%ny, &
             ', tile(n)%nz = ', tile(n)%nz

      ! Allocate memory.
      allocate(tile(n)%varids(tile(n)%nVars))
      allocate(tile(n)%vars(tile(n)%nVars))

      allocate(tile(n)%var1d(tile(n)%nx))
      allocate(tile(n)%var2d(tile(n)%nx, tile(n)%ny))
      allocate(tile(n)%var3d(tile(n)%nx, tile(n)%ny, tile(n)%nz))

      rc = nf90_inq_varids(tile(n)%fileid, tile(n)%nVars, tile(n)%varids)
      call check_status(rc)

     !print *, 'Tile ', n, ', nvars = ', tile(n)%nVars, ', varids: ', tile(n)%varids

      do i = 1, tile(n)%nVars
         rc = nf90_inquire_variable(tile(n)%fileid, tile(n)%varids(i), &
                                    ndims=tile(n)%vars(i)%nDims, natts=tile(n)%vars(i)%nAtts)
         call check_status(rc)
        !print *, 'Var No. ', i, ': ndims = ', tile(n)%vars(i)%nDims

         allocate(tile(n)%vars(i)%dimids(tile(n)%vars(i)%nDims))
         allocate(tile(n)%vars(i)%dimlen(tile(n)%vars(i)%nDims))
         allocate(tile(n)%vars(i)%dimnames(tile(n)%vars(i)%nDims))

         rc = nf90_inquire_variable(tile(n)%fileid, tile(n)%varids(i), &
                                    dimids=tile(n)%vars(i)%dimids)
         call check_status(rc)
        !print *, 'Var No. ', i, ': tile(n)%vars(i)%dimids = ', tile(n)%vars(i)%dimids

         rc = nf90_inquire_variable(tile(n)%fileid, tile(n)%varids(i), &
                  name=tile(n)%vars(i)%name)
         call check_status(rc)
         print *, 'Var No. ', i, ': ', trim(tile(n)%vars(i)%name)

         if(trim(tile(n)%vars(i)%name) == 'XC') then
            allocate(tile(n)%lon(tile(n)%nx, tile(n)%ny))
            rc = nf90_get_var(tile(n)%fileid, tile(n)%varids(i), tile(n)%lon)
            call check_status(rc)
         else if(trim(tile(n)%vars(i)%name) == 'YC') then
            allocate(tile(n)%lat(tile(n)%nx, tile(n)%ny))
            rc = nf90_get_var(tile(n)%fileid, tile(n)%varids(i), tile(n)%lat)
            call check_status(rc)
         else if(trim(tile(n)%vars(i)%name) == 'X') then
            allocate(tile(n)%x(tile(n)%nx))
            rc = nf90_get_var(tile(n)%fileid, tile(n)%varids(i), tile(n)%x)
            call check_status(rc)
         else if(trim(tile(n)%vars(i)%name) == 'Y') then
            allocate(tile(n)%y(tile(n)%ny))
            rc = nf90_get_var(tile(n)%fileid, tile(n)%varids(i), tile(n)%y)
            call check_status(rc)
         else if(trim(tile(n)%vars(i)%name) == 'Z') then
            if(.not. allocated(tile(n)%z)) allocate(tile(n)%z(tile(n)%nz))
            rc = nf90_get_var(tile(n)%fileid, tile(n)%varids(i), tile(n)%z)
            call check_status(rc)
         else if(trim(tile(n)%vars(i)%name) == 'T') then
            if(.not. allocated(tile(n)%time)) allocate(tile(n)%time(tile(n)%nt))
            rc = nf90_get_var(tile(n)%fileid, tile(n)%varids(i), tile(n)%time)
            call check_status(rc)
         end if

         do k = 1, tile(n)%vars(i)%ndims
            ik = tile(n)%vars(i)%dimids(k)
            tile(n)%vars(i)%dimnames(k) = trim(tile(n)%dimnames(ik))
            tile(n)%vars(i)%dimlen(k) = tile(n)%dimlen(ik)
         end do
      end do
    end do

   !print *, 'Leave initialize_tilegrid'

  end subroutine initialize_tilegrid

  !----------------------------------------------------------------------
  subroutine finalize_tilegrid(tile)

    implicit none

    type(tilegrid), dimension(6), intent(inout) :: tile

    integer :: i, n, rc

    do n = 1, 6
      if(allocated(tile(n)%varids)) deallocate(tile(n)%varids)
      if(allocated(tile(n)%dimids)) deallocate(tile(n)%dimids)
      if(allocated(tile(n)%dimlen)) deallocate(tile(n)%dimlen)
      if(allocated(tile(n)%dimnames)) deallocate(tile(n)%dimnames)
      if(allocated(tile(n)%lon)) deallocate(tile(n)%lon)
      if(allocated(tile(n)%lat)) deallocate(tile(n)%lat)
      if(allocated(tile(n)%x)) deallocate(tile(n)%x)
      if(allocated(tile(n)%y)) deallocate(tile(n)%y)
      if(allocated(tile(n)%z)) deallocate(tile(n)%z)

      do i = 1, tile(n)%nVars
         if(allocated(tile(n)%vars(i)%dimids)) &
            deallocate(tile(n)%vars(i)%dimids)
         if(allocated(tile(n)%vars(i)%dimlen)) &
            deallocate(tile(n)%vars(i)%dimlen)
         if(allocated(tile(n)%vars(i)%dimnames)) &
            deallocate(tile(n)%vars(i)%dimnames)
      end do

      if(allocated(tile(n)%vars)) deallocate(tile(n)%vars)
      if(allocated(tile(n)%var1d)) deallocate(tile(n)%var1d)
      if(allocated(tile(n)%var2d)) deallocate(tile(n)%var2d)
      if(allocated(tile(n)%var3d)) deallocate(tile(n)%var3d)

     !print *, 'Tile ', n, ', close filename: ', trim(tile(n)%filename)
      rc = nf90_close(tile(n)%fileid)
      call check_status(rc)
    end do

  end subroutine finalize_tilegrid

  !----------------------------------------------------------------------
  subroutine check_status(rc)
    integer, intent(in) :: rc
    
    if(rc /= nf90_noerr) then 
      print *, trim(nf90_strerror(rc))
      print *, 'rc = ', rc, ', nf90_noerr = ', nf90_noerr
      stop 'in check_status'
    end if
  end subroutine check_status  

end module tile_module

