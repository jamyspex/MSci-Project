module param
!integer(4), parameter :: nx = 51	
!integer(4), parameter :: ny = 51	
integer(4), parameter :: nx=500
integer(4), parameter :: ny=500
      real, dimension(0:ny+1,0:nx+1) :: hzero
      real, dimension(0:ny+1,0:nx+1) :: h
      real, dimension(0:ny+1,0:nx+1) :: eta
      real, dimension(0:ny+1,0:nx+1) :: etan
      real, dimension(0:ny+1,0:nx+1) :: u
      real, dimension(0:ny+1,0:nx+1) :: un
      real, dimension(0:ny+1,0:nx+1) :: v
      real, dimension(0:ny+1,0:nx+1) :: vn
      real :: dt
      real :: dx
      real :: dy
      real :: g
      real :: eps
      integer :: j
      integer :: k
      integer, dimension(0:ny+1,0:nx+1) :: wet
      real :: hmin
end module param
