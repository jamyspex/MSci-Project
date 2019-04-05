program wave2d
      integer(4), parameter :: nx = 500 
      integer(4), parameter :: ny = 500 
      real :: dt
      real :: dx
      real :: dy
      real :: eps
      real, dimension(0:ny+1,0:nx+1) :: eta
      real, dimension(0:ny+1,0:nx+1) :: etan
      real :: g
      real, dimension(0:ny+1,0:nx+1) :: h
      real :: hmin
      real, dimension(0:ny+1,0:nx+1) :: hzero
      integer :: j
      integer :: k
      real, dimension(0:ny+1,0:nx+1) :: u
      real, dimension(0:ny+1,0:nx+1) :: un
      real, dimension(0:ny+1,0:nx+1) :: v
      real, dimension(0:ny+1,0:nx+1) :: vn
      integer, dimension(0:ny+1,0:nx+1) :: wet
      real :: hmax
      real :: time
      real :: dtmax
      real :: c
      real :: lambda
      integer :: n
      integer :: ntot
      integer :: nout
      integer :: dummy
    ntot = 10000
    eps = 0.05
    call init(hmin, dx, dy, dt, g, j, k, hzero, eta, etan, h, wet, u, un, v, vn)
    open(90, file='debug.dat', form='formatted')
    open(10, file='eta0.dat', form='formatted')
    do j = 0, ny+1, 1
        write(10, '(101F12.6)')(eta(j,k), k = 0, nx+1)
    end do
    close(10)
    open(10, file='h0.dat', form='formatted')
    do j = 0, ny+1, 1
        write(10, '(101F12.6)')(hzero(j,k), k = 0, nx+1)
    end do
    close(10)
    hmax = 0.
    do j = 1, ny, 1
        do k = 1, nx, 1
            hmax = max(hmax,h(j,k))
        end do
    end do
    dummy = 0
    c = sqrt(2*g*hmax)
    write(6, *)("c = ", c)
    lambda = dt*sqrt(g*hmax)/min(dx,dy)
    write(6, *)("lambda = ", lambda)
    if (lambda>1) then
        write(6, *)("This will not work. Do you know why?")
        stop 
    end if
    open(10, file='eta.dat', form='formatted')
    open(20, file='h.dat', form='formatted')
    open(30, file='u.dat', form='formatted')
    open(40, file='v.dat', form='formatted')
    do j = 26, 26, 1
        do k = 26, 26, 1
            eta(j,k) = 1.0
        end do
    end do
    do n = 1, ntot, 1
        time = REAL(n)*dt
        call dyn(j, k, dx, g, eta, dt, dy, un, u, wet, v, vn, h, etan)
        call shapiro(j, k, wet, etan, eps, eta)
        call vernieuw(dt, dx, dy, eps, eta, etan, g, h, hmin, hzero, j, k, u, un, v, vn, wet)
    end do
    do j = 0, ny+1, 1
        write(10, '(101F12.6)')(eta(j,k), k = 0, nx+1)
        write(20, '(101F12.6)')(h(j,k), k = 0, nx+1)
        write(30, '(101F12.6)')(u(j,k), k = 0, nx+1)
        write(40, '(101F12.6)')(v(j,k), k = 0, nx+1)
    end do
    write(6, *)("Data output at time = ", time/60.0, " min")

end program wave2d