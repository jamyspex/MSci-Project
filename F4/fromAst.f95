    ntot = 10000
    eps = 0.05
    open(90, file='debug.dat', form='formatted')
    open(10, file='eta0.dat', form='formatted')
    do j = 0, ny+1, 1
        write(10, '(101F12.6)')(eta(j,k), k = 0, nx+1)
    end do
    open(10, file='h0.dat', form='formatted')
    do j = 0, ny+1, 1
        write(10, '(101F12.6)')(hzero(j,k), k = 0, nx+1)
    end do
    hmax = 0.
    do j = 1, ny, 1
        do k = 1, nx, 1
                hmax = max(hmax,h(j,k))
        end do
    end do
    dummy = 0
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