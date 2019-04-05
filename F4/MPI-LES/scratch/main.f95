program main
      integer, parameter :: kp = 80 
      integer, parameter :: ip = 300 
      integer, parameter :: jp = 300 
      integer, parameter :: ipmax = ip 
      integer, parameter :: jpmax = jp 
      character*300 :: datafile = '../GIS/Kyoto_1km2_4m_with_buffer.txt' 
      real, parameter :: dxgrid = 4. 
      real, parameter :: dygrid = 4. 
      real, parameter :: cs0 = 0.14 
      integer, parameter :: i_anime = 1 
      integer, parameter :: avetime = 2 
      integer, parameter :: km_sl = 80 
      integer, parameter :: i_aveflow = 0 
      integer, parameter :: i_ifdata_out = 0 
      real, parameter :: dt_orig = 0.05 
      real(4) :: alpha
      integer :: ical
      integer :: im
      integer :: jm
      integer :: km
      integer :: n
      integer :: n0
      integer :: n1
      integer :: nif
      integer :: nmax
      real(4) :: beta
      character*70 :: data10
      character*70 :: data11
      character*70 :: data12
      character*70 :: data13
      character*70 :: data14
      character*70 :: data15
      character*70 :: data20
      character*70 :: data21
      character*70 :: data22
      character*70 :: data23
      character*70 :: data24
      character*70 :: data25
      character*70 :: data26
      character*70 :: data27
      character*70 :: data30
      character*70 :: data31
      real(4) :: dt
      real(4) :: ro
      real(4) :: time
      real(4) :: vn
      real(4), dimension(0:ip+1,0:jp+1,0:kp+1) :: amask1
      real(4), dimension(-1:ip+1,0:jp+1,0:kp+1) :: bmask1
      real(4), dimension(0:ip+1,-1:jp+1,0:kp+1) :: cmask1
      real(4), dimension(0:ip+1,0:jp+1,0:kp+1) :: dmask1
      real(4), dimension(ip,jp,kp) :: cn1
      real(4), dimension(ip) :: cn2l
      real(4), dimension(ip) :: cn2s
      real(4), dimension(jp) :: cn3l
      real(4), dimension(jp) :: cn3s
      real(4), dimension(kp) :: cn4l
      real(4), dimension(kp) :: cn4s
      real(4), dimension(kp) :: delx1
      real(4), dimension(-1:ip+2,0:jp+2,0:kp+2) :: diu1
      real(4), dimension(0:ip+2,0:jp+2,0:kp+2) :: diu2
      real(4), dimension(0:ip+2,0:jp+2,0:kp+2) :: diu3
      real(4), dimension(0:ip+2,0:jp+2,0:kp+2) :: diu4
      real(4), dimension(-1:ip+2,0:jp+2,0:kp+2) :: diu5
      real(4), dimension(0:ip+2,0:jp+2,0:kp+2) :: diu6
      real(4), dimension(0:ip+2,0:jp+2,0:kp+2) :: diu7
      real(4), dimension(0:ip+2,0:jp+2,0:kp+2) :: diu8
      real(4), dimension(0:ip+2,0:jp+2,0:kp+2) :: diu9
      real(4), dimension(-1:ip+1) :: dx1
      real(4), dimension(0:ip) :: dxl
      real(4), dimension(0:ip) :: dxs
      real(4), dimension(0:jp+1) :: dy1
      real(4), dimension(0:jp) :: dyl
      real(4), dimension(0:jp) :: dys
      real(4), dimension(-1:kp+2) :: dzn
      real(4), dimension(-1:kp+2) :: dzs
      real(4), dimension(0:ip,0:jp,0:kp) :: f
      real(4), dimension(ip,jp,kp) :: fold
      real(4), dimension(0:ip,0:jp,0:kp) :: fx
      real(4), dimension(0:ip,0:jp,0:kp) :: fy
      real(4), dimension(0:ip,0:jp,0:kp) :: fz
      real(4), dimension(0:ip,0:jp,0:kp) :: g
      real(4), dimension(ip,jp,kp) :: gold
      real(4), dimension(0:ip,0:jp,0:kp) :: h
      real(4), dimension(ip,jp,kp) :: hold
      real(4), dimension(-1:ip+2,0:jp+2,0:kp+2) :: nou1
      real(4), dimension(0:ip+2,0:jp+2,0:kp+2) :: nou2
      real(4), dimension(0:ip+2,0:jp+2,0:kp+2) :: nou3
      real(4), dimension(0:ip+2,0:jp+2,0:kp+2) :: nou4
      real(4), dimension(-1:ip+2,0:jp+2,0:kp+2) :: nou5
      real(4), dimension(0:ip+2,0:jp+2,0:kp+2) :: nou6
      real(4), dimension(0:ip+2,0:jp+2,0:kp+2) :: nou7
      real(4), dimension(0:ip+2,0:jp+2,0:kp+2) :: nou8
      real(4), dimension(0:ip+2,0:jp+2,0:kp+2) :: nou9
      real(4), dimension(0:1,0:ip+2,0:jp+2,0:kp+1) :: p
      real(4), dimension(0:ip+1,0:jp+1,0:kp+1) :: rhs
      real(4), dimension(-1:ip+1,-1:jp+1,0:kp+1) :: sm
      real(4), dimension(0:ip+1,-1:jp+1,0:kp+1) :: u
      real(4), dimension(0:ip,0:jp,0:kp) :: usum
      real(4), dimension(ip,jp,kp) :: uwfx
      real(4), dimension(ip,kp) :: uwfxs
      real(4), dimension(0:ip+1,-1:jp+1,0:kp+1) :: v
      real(4), dimension(0:ip,0:jp,0:kp) :: vsum
      real(4), dimension(0:ip+1,-1:jp+1,-1:kp+1) :: w
      real(4), dimension(0:ip,0:jp,0:kp) :: wsum
      real(4), dimension(0:kp+2) :: z2
      real(4), dimension(-1:ipmax+1,-1:jpmax+1) :: zbm
      integer :: clock_rate
      integer(4), dimension(0:9) :: timestamp
      integer(4) :: i
    call set(data10, data11, data20, data21, data22, data23, data24, data25, data26, data27, data30, data31, ical, nif, n0, n1, nmax, dt, ro, vn, alpha, beta, data12, data13, data14, data15)
    call grid(dx1, dxl, dy1, dyl, z2, dzn, dzs, dxs, dys)
    call init(u, v, w, p, cn2s, dxs, cn2l, cn3s, dys, cn3l, dzs, cn4s, cn4l, cn1, amask1, bmask1, cmask1, dmask1, zbm, z2, dzn)
    call ifdata(fold, gold, hold, time, n, u, v, w, p, usum, vsum, wsum, delx1, dx1, dy1, dzn, diu1, diu2, diu3, diu4, diu5, diu6, diu7, diu8, diu9, sm, f, g, h, z2, dt, dxs, vn, dzs, nou1, nou2, nou3, nou4, nou5, nou6, nou7, nou8, nou9, amask1, bmask1, cmask1, dmask1, alpha, beta, fx, fy, fz, zbm, ical, nif)
    do n = n0, nmax, 1
        time = float(n-n0)*dt
        call velnw(p, ro, dxs, u, dt, f, dys, v, g, dzs, w, h)
        call feedbf(usum, u, bmask1, vsum, v, cmask1, wsum, w, dmask1, alpha, dt, beta, fx, fy, fz, f, g, h, n)
        call les(delx1, dx1, dy1, dzn, diu1, diu2, diu3, diu4, diu5, diu6, diu7, diu8, diu9, sm, f, g, h, u, v, uspd, vspd, dxs, dys, n)
        call adam(n, nmax, data21, fold, gold, hold, f, g, h)
        call press(u, v, w, p, rhs, f, g, h, dx1, dy1, dzn, dxs, dys, dzs, dt, n, nmax)
    end do

end program main