module latlon_module

  use netcdf
  use tile_module

  implicit none

  !-----------------------------------------------------------------------
  ! Define interfaces and attributes for module routines

  private
  public :: latlongrid
  public :: initialize_latlongrid
  public :: finalize_latlongrid
  public :: generate_weight

  !-----------------------------------------------------------------------

  type latlongrid
     character(len=1024)                   :: filename
     integer                               :: ncid
     integer                               :: dimidx, dimidy, dimidz, &
                                              dimidt
     integer                               :: nlon, nlat, nlev, npnt, ntim
     real(kind=8), dimension(:), allocatable :: lon, lat, lev, pnt, time
     integer, dimension(:, :), allocatable :: counter
     integer, dimension(:, :, :), allocatable :: tile
     integer, dimension(:, :, :), allocatable :: ilon, jlat
     real(kind=8),    dimension(:, :, :), allocatable :: dist, wgt
     real(kind=8),    dimension(:, :), allocatable :: pos
  end type latlongrid

  !-----------------------------------------------------------------------

contains

 !-----------------------------------------------------------------------
  subroutine initialize_latlongrid(nlon, nlat, npnt, latlon)

    implicit none

    integer,          intent(in)  :: nlon, nlat, npnt
    type(latlongrid), intent(out) :: latlon

    integer :: i, j, k
    real(kind=8) :: dlon, dlat

   !print *, 'enter initialize_latlongrid'

    dlon = 360.0/nlon
    dlat = 180.0/nlat

    latlon%nlon = nlon
    latlon%nlat = nlat
    latlon%nlev = 20
    latlon%npnt = npnt
    latlon%ntim = 2

    allocate(latlon%counter(nlon, nlat))
    allocate(latlon%tile(nlon, nlat, npnt))
    allocate(latlon%ilon(nlon, nlat, npnt))
    allocate(latlon%jlat(nlon, nlat, npnt))
    allocate(latlon%dist(nlon, nlat, npnt))
    allocate(latlon%wgt(nlon, nlat, npnt))
    allocate(latlon%pos(nlon, nlat))

    allocate(latlon%lon(nlon))
    allocate(latlon%lat(nlat))
    allocate(latlon%pnt(npnt))
    
    do i = 1, nlon
      latlon%lon(i) = dlon*dble(i-1) + 0.5*dlat
    end do
    
    do j = 1, nlat
      latlon%lat(j) = dlat*dble(j-1) - 90.0 + 0.5*dlat
      do i = 1, nlon
        latlon%pos(i, j) = -1.0
        latlon%counter(i, j) = 0

        do k = 1, npnt
          latlon%tile(i, j, k) = 0
          latlon%ilon(i, j, k) = 0
          latlon%jlat(i, j, k) = 0
          latlon%dist(i, j, k) = 1.0e36
          latlon%wgt(i, j, k) = 0.0
        end do
      end do
    end do
    
    do i = 1, npnt
      latlon%pnt(i) = dble(i)
    end do

   !print *, 'latlon%lon = ', latlon%lon
   !print *, 'latlon%lat = ', latlon%lat

   !print *, 'leave initialize_latlongrid'

  end subroutine initialize_latlongrid

 !----------------------------------------------------------------------
  subroutine finalize_latlongrid(latlon)

    implicit none

    type(latlongrid), intent(inout) :: latlon

    deallocate(latlon%lon)
    deallocate(latlon%lat)
    if(allocated(latlon%lev)) deallocate(latlon%lev)
    if(allocated(latlon%time)) deallocate(latlon%time)
    if(allocated(latlon%pnt)) deallocate(latlon%pnt)

    deallocate(latlon%counter)
    deallocate(latlon%tile)
    deallocate(latlon%ilon)
    deallocate(latlon%jlat)
    deallocate(latlon%dist)
    deallocate(latlon%wgt)
    deallocate(latlon%pos)

  end subroutine finalize_latlongrid

  !----------------------------------------------------------------------
  subroutine generate_weight(tile, latlon)

    implicit none

    type(tilegrid), dimension(6), intent(in) :: tile
    type(latlongrid), intent(inout) :: latlon

    integer :: ik, jk

    do jk = 1, latlon%nlat
    do ik = 1, latlon%nlon
       call process_point(ik, jk, tile, latlon)

       latlon%pos(ik, jk) = dble(latlon%tile(ik, jk, 1))
       if((mod(ik-1,10) == 0) .and. (mod(jk-1, 10) == 0)) then
          print *, 'ik,jk,latlon%pos(ik, jk),latlon%dist(ik, jk, :) = ', &
                    ik,jk,latlon%pos(ik, jk),latlon%dist(ik, jk, :)
       end if
    end do
    end do

    !Check pos info
    do jk = 1, latlon%nlat
    do ik = 1, latlon%nlon
       call weighting(latlon%npnt, latlon%npnt, &
                      latlon%dist(ik, jk, :), latlon%wgt(ik, jk, :))
    end do
    end do

  end subroutine generate_weight

  !----------------------------------------------------------------------
  subroutine process_point(ik, jk, tile, latlon)

    implicit none

    integer, intent(in) :: ik, jk
    type(tilegrid), dimension(6), intent(in) :: tile
    type(latlongrid), intent(inout) :: latlon

    integer :: i, j, n
    real(kind=8) :: plat, plon

    plon = latlon%lon(ik)
    plat = latlon%lat(jk)
    
    do n = 1, 6
    do j = 1, tile(n)%ny
    do i = 1, tile(n)%nx
      call check_point(ik, jk, i, j, n, plat, plon, &
           tile(n)%lat(i,j), tile(n)%lon(i,j), latlon)
    end do
    end do
    end do

  end subroutine process_point

  !----------------------------------------------------------------------
  subroutine check_point(ik, jk, i, j, n, xlat1, xlon1, xlat2, xlon2, latlon)
    implicit none

    integer, intent(in) :: ik, jk, i, j, n
    real(kind=8), intent(in)  :: xlat1, xlon1, xlat2, xlon2
    type(latlongrid), intent(inout) :: latlon

    real(kind=8) :: dist
    integer :: k

    call distance(xlat1, xlon1, xlat2, xlon2, dist)

    k = latlon%counter(ik, jk)
    if((k > latlon%npnt) .and. (dist >= latlon%dist(ik, jk, latlon%npnt))) then
       latlon%counter(ik, jk) = k + 1
       return
    end if

    call insert(ik, jk, i, j, n, dist, latlon)

  end subroutine check_point

  !----------------------------------------------------------------------
  subroutine insert(ik, jk, i, j, n, dist, latlon)
    implicit none

    integer, intent(in) :: ik, jk, i, j, n
    real(kind=8), intent(in)  :: dist
    type(latlongrid), intent(inout) :: latlon
    integer :: k, kk, m

    k = latlon%counter(ik, jk)
    latlon%counter(ik, jk) = k + 1
    if(k >= latlon%npnt) then
       k = latlon%npnt
    else
       k = k + 1
    end if

    do kk = 1, latlon%npnt
       m = k - 1
       if(m < 1) then
          exit
       end if

       if(dist < latlon%dist(ik, jk, m)) then
          latlon%tile(ik, jk, k) = latlon%tile(ik, jk, m)
          latlon%ilon(ik, jk, k) = latlon%ilon(ik, jk, m)
          latlon%jlat(ik, jk, k) = latlon%jlat(ik, jk, m)
          latlon%dist(ik, jk, k) = latlon%dist(ik, jk, m)
       else
          exit
       end if
       k = k - 1
    end do

    latlon%tile(ik, jk, k) = n
    latlon%ilon(ik, jk, k) = i
    latlon%jlat(ik, jk, k) = j
    latlon%dist(ik, jk, k) = dist

   !print *, 'ik,jk,latlon%dist(ik, jk, :) = ', ik,jk,latlon%dist(ik, jk, :)

  end subroutine insert

  !----------------------------------------------------------------------
  subroutine distance(xlat1, xlon1, xlat2, xlon2, dist)

    implicit none

    real(kind=8), intent(in)  :: xlat1, xlon1, xlat2, xlon2
    real(kind=8), intent(out) :: dist

    real(kind=8) :: lat1, lon1, lat2, lon2, dlon, dlat

    real(kind=8) :: deg2arc, ang, sindlat, sindlon
    deg2arc = 3.1415926536/180.0

    lat1 = xlat1 * deg2arc
    lat2 = xlat2 * deg2arc
    lon1 = xlon1 * deg2arc
    lon2 = xlon2 * deg2arc

    dlon = lon2 - lon1
    dlat = lat2 - lat1
    sindlat = sin(0.5*dlat)
    sindlon = sin(0.5*dlon)
    ang = sindlat * sindlat + cos(lat1) * cos(lat2) * sindlon * sindlon
   !dist = 2.0 * atan2(sqrt(ang), sqrt(1.0-ang))
    dist = 2.0 * asin(min(1.0,sqrt(ang)))

  end subroutine distance

  !----------------------------------------------------------------------
  subroutine weighting(n, m, dist, wgt)

    implicit none

    integer, intent(in)  :: n, m
    real(kind=8), dimension(n), intent(in)  :: dist
    real(kind=8), dimension(n), intent(out) :: wgt

    real(kind=8) :: total, factor
    integer :: k

    total = 0.0
    do k = 1, m
       total = total + dist(k)
    end do
    
    if(m > 1) then
      factor = float(m - 1) * total
    else
      factor = total
    end if

    do k = 1, m
       wgt(k) = (total - dist(k)) / factor
    end do

    do k = m+1, n
       wgt(k) = 0.0
    end do

  end subroutine weighting

end module latlon_module

