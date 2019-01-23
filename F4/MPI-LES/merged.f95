module module_velnw_bondv1_velfg_feedbf_les_adam_press_merged
contains
   subroutine velnw_bondv1_velfg_feedbf_les_adam_press_merged(alpha, beta, bm&
   &ask1, cmask1, data21, delx1, diu1, diu2, diu3, diu4, diu5, diu6, diu7, diu8, diu9, dm&
   &ask1, dt, dx1, dxs, dy1, dys, dzn, dzs, f, fold, fx, fy, fz, g, gold, h, hold, n, n0, nmax,&
   &p, rhs, ro, sm, u, uspd, usum, v, vspd, vsum, w, wsum, z2)
      integer, parameter :: avetime = 2
      real, parameter :: cs0 = 0.14
      real, parameter :: dt_orig = 0.05
      real, parameter :: dxgrid = 4.
      real, parameter :: dygrid = 4.
      integer, parameter :: i_anime = 1
      integer, parameter :: i_aveflow = 0
      integer, parameter :: i_ifdata_out = 0
      integer, parameter :: ip = 300
      integer, parameter :: ipmax = 300
      integer, parameter :: jp = 300
      integer, parameter :: jpmax = 300
      integer, parameter :: km_sl = 80
      integer, parameter :: kp = 80
      integer, parameter :: nmaxp = 50
      real, parameter :: omega = 1.
      real, parameter :: pjuge = 0.0001
      integer, parameter :: u0 = 0
      real(4) :: aaa, bbb, uout, gaaa, gbbb
      real(4), intent(In) :: alpha
      real(4), intent(In) :: beta
      real(4), dimension(-1:301, 0:301, 0:81), intent(In) :: bmask1
      real(4), dimension(0:301, -1:301, 0:81), intent(In) :: cmask1
      real(4) :: cn1, cn2l, cn2s, cn3l, cn3s, cn4l, cn4s, dz1, dz2
      real(4) :: cov1_i, cov2_j, cov3_k, cov4_i, cov5_j, cov6_k, cov7_i, cov8_j&
&, cov9_k
      real(4) :: cov1_ip1, cov2_jp1, cov3_kp1, cov4_ip1, cov5_jp1, cov6_kp1, c&
&ov7_ip1, cov8_jp1, cov9_kp1
      real(4) :: covc, covx1, covy1, covz1
      real(4) :: csx1
      character*70, intent(In) :: data21
      character*300 :: datafile = '../GIS/Kyoto_1km2_4m_with_buffer.txt'
      real(4), dimension(80), intent(Out) :: delx1
      real(4), dimension(-1:302, 0:302, 0:82), intent(In) :: diu1
      real(4) :: diu1_, diu2_, diu3_, diu4_, diu5_, diu6_, diu7_, diu8_, diu9_
      real(4) :: diu1_ip1, diu2_jp1, diu3_kp1, diu4_ip1, diu5_jp1, diu6_kp1, d&
&iu7_ip1, diu8_jp1, diu9_kp1
      real(4), dimension(0:302, 0:302, 0:82), intent(In) :: diu2
      real(4), dimension(0:302, 0:302, 0:82), intent(In) :: diu3
      real(4), dimension(0:302, 0:302, 0:82), intent(In) :: diu4
      real(4), dimension(-1:302, 0:302, 0:82), intent(In) :: diu5
      real(4), dimension(0:302, 0:302, 0:82), intent(In) :: diu6
      real(4), dimension(0:302, 0:302, 0:82), intent(In) :: diu7
      real(4), dimension(0:302, 0:302, 0:82), intent(In) :: diu8
      real(4), dimension(0:302, 0:302, 0:82), intent(In) :: diu9
      real(4), dimension(0:301, 0:301, 0:81), intent(In) :: dmask1
      real(4), intent(In) :: dt
      real(4) :: dudxx1, dudyx1, dudzx1, dvdxx1, dvdyx1, dvdzx1, dwdxx1, dwdyx1&
&, dwdzx1
      real(4), dimension(-1:301), intent(In) :: dx1
      real(4), dimension(0:300), intent(In) :: dxs
      real(4), dimension(0:301), intent(In) :: dy1
      real(4), dimension(0:300), intent(In) :: dys
      real(4), dimension(-1:82), intent(In) :: dzn
      real(4), dimension(-1:82), intent(In) :: dzs
      real(4) :: evsx2, evsx1, evsy2, evsy1, evsz2, evsz1
      real(4), dimension(0:300, 0:300, 0:80), intent(InOut) :: f
      real(4) :: f1x, f1y, f1z, f2x, f2y, f2z
      real(4) :: fd, gd, hd
      real(4), dimension(300, 300, 80), intent(InOut) :: fold
      real(4), dimension(0:300, 0:300, 0:80), intent(Out) :: fx
      real(4), dimension(0:300, 0:300, 0:80), intent(Out) :: fy
      real(4), dimension(0:300, 0:300, 0:80), intent(Out) :: fz
      real(4), dimension(0:300, 0:300, 0:80), intent(InOut) :: g
      real(4), dimension(300, 300, 80), intent(InOut) :: gold
      real(4), dimension(0:300, 0:300, 0:80), intent(InOut) :: h
      real(4), dimension(300, 300, 80), intent(InOut) :: hold
      integer :: i, j, k
      integer, intent(In) :: n, n0
      integer, intent(In) :: nmax
      integer :: nn
      real(4) :: nou1_, nou2_, nou3_, nou4_, nou5_, nou6_, nou7_, nou8_, nou9_
      real(4) :: nou1_ip1, nou2_jp1, nou3_kp1, nou4_ip1, nou5_jp1, nou6_kp1, n&
&ou7_ip1, nou8_jp1, nou9_kp1
      real(4), dimension(0:1, 0:302, 0:302, 0:81) :: p
      real(4) :: pz
      real(4), dimension(0:301, 0:301, 0:81), intent(Out) :: rhs
      real(4) :: rhsav, pav, area, pco, sor, reltmp
      real(4), intent(In) :: ro
      real(4), dimension(-1:301, -1:301, 0:81), intent(Out) :: sm
      real(4), dimension(0:301, -1:301, 0:81), intent(InOut) :: u
      real(4) :: u_val
      real(4), dimension(0:301, 0:301), intent(In) :: uspd
      real(4), dimension(0:300, 0:300, 0:80), intent(InOut) :: usum
      real(4), dimension(0:301, -1:301, 0:81), intent(InOut) :: v
      real(4) :: vfu, vfv, vfw
      real(4) :: visux2, visux1, visuy2, visuy1, visuz2, visuz1
      real(4) :: visvx2, visvx1, visvy2, visvy1, visvz2, visvz1
      real(4) :: viswx2, viswx1, viswy2, viswy1, viswz2, viswz1
      real(4), dimension(0:301, 0:301), intent(In) :: vspd
      real(4), dimension(0:300, 0:300, 0:80), intent(InOut) :: vsum
      real(4), dimension(0:301, -1:301, -1:81), intent(InOut) :: w
      real(4), dimension(0:300, 0:300, 0:80), intent(InOut) :: wsum
      real(4), dimension(0:82), intent(In) :: z2
      ! OpenCLStencil (
      !        2 point stencil on 4D array p: [[C=0,1,0,0],[C=0,0,0,0]]
      !    ){
      ! OpenCLMap ( ["ro","dxs","dt","k","j","u"],["u"],["(k,1,80,1)","(j,1,300,1)","(i,1,300,1)"],[]) {
      pz = (-p(0, i, j, k) + p(0, i + 1, j, k))/ro/dxs(i)
      u(i, j, k) = u(i, j, k) + dt*(f(i, j, k) - pz)
      !}
      !}
      ! OpenCLStencil (
      !        2 point stencil on 4D array p: [[C=0,0,1,0],[C=0,0,0,0]]
      !    ){
      ! OpenCLMap ( ["ro","dys","dt","k","j"],["v"],["(k,1,80,1)","(j,1,300,1)","(i,1,300,1)"],[]) {
      pz = (-p(0, i, j, k) + p(0, i, j + 1, k))/ro/dys(j)
      v(i, j, k) = v(i, j, k) + dt*(g(i, j, k) - pz)
      !}
      !}
      ! OpenCLStencil (
      !        2 point stencil on 4D array p: [[C=0,0,0,0],[C=0,0,0,1]]
      !    ){
      ! OpenCLMap ( ["ro","dzs","dt","k","j"],["w"],["(k,1,79,1)","(j,1,300,1)","(i,1,300,1)"],[]) {
      pz = (-p(0, i, j, k) + p(0, i, j, k + 1))/ro/dzs(k)
      w(i, j, k) = w(i, j, k) + dt*(h(i, j, k) - pz)
      !}
      !}
      ! OpenCLMap ( [],["u","w","v"],["(i,0,1,1)","(k,1,78,1)","(j,1,300,1)"],[]) {
      u_val = 5.*((z2(k) + 0.5*dzn(k))/600.)**0.2
      u(i, j, k) = u_val
      v(i, j, k) = 0.0
      w(i, j, k) = 0.0
      !}
      ! OpenCLMap ( ["i"],["u","v","w"],["(i,0,1,1)","(k,79,80,1)","(j,1,300,1)"],[]) {
      u(i, j, k) = u(i, j, 77)
      v(i, j, k) = 0.0
      w(i, j, k) = 0.0
      !}
      if (n == n0) then
         ! OpenCLMap ( ["k","j"],["u","v","w"],["(k,1,80,1)","(j,1,300,1)","(i,2,300,1)"],[]) {
         u(i, j, k) = u(1, j, k)
         v(i, j, k) = v(1, j, k)
         w(i, j, k) = w(1, j, k)
         !}
      end if
      aaa = 0.0
      ! OpenCLReduce ( ["u"],["aaa"],["(k,1,80,1)","(j,1,300,1)"],[],["(aaa,0.0)"]) {
      aaa = amax1(aaa, u(300, j, k))
      !}
      gaaa = aaa
      bbb = 1e38
      ! OpenCLReduce ( ["u"],["bbb"],["(k,1,80,1)","(j,1,300,1)"],[],["(bbb,1e38)"]) {
      bbb = amin1(bbb, u(300, j, k))
      !}
      gbbb = bbb
      uout = (gaaa + gbbb)/2.
      ! OpenCLStencil (
      !        2 point stencil on 3D array u: [[C=299,0,0],[C=300,0,0]]
      !    ){
      ! OpenCLMap ( ["dt","uout","dxs","k"],["u"],["(k,1,80,1)","(j,1,300,1)"],[]) {
      u(300, j, k) = u(300, j, k) - dt*uout*(u(300, j, k) - u(299, j, k))/dxs(300)
      !}
      !}
      ! OpenCLStencil (
      !        2 point stencil on 3D array v: [[C=300,0,0],[C=301,0,0]]
      !    ){
      ! OpenCLMap ( ["dt","uout","dxs","k"],["v"],["(k,1,80,1)","(j,1,300,1)"],[]) {
      v(301, j, k) = v(301, j, k) - dt*uout*(v(301, j, k) - v(300, j, k))/dxs(300)
      !}
      !}
      ! OpenCLStencil (
      !        2 point stencil on 3D array w: [[C=300,0,0],[C=301,0,0]]
      !    ){
      ! OpenCLMap ( ["dt","uout","dxs","k"],["w"],["(k,1,80,1)","(j,1,300,1)"],[]) {
      w(301, j, k) = w(301, j, k) - dt*uout*(w(301, j, k) - w(300, j, k))/dxs(300)
      !}
      !}
      ! OpenCLStencil (
      !        2 point stencil on 3D array u: [[0,C=1,0],[0,C=300,0]]
      !    ){
      ! OpenCLMap ( ["k"],["u"],["(k,0,81,1)","(i,0,301,1)"],[]) {
      u(i, 0, k) = u(i, 300, k)
      u(i, 301, k) = u(i, 1, k)
      !}
      !}
      ! OpenCLStencil (
      !        2 point stencil on 3D array v: [[0,C=1,0],[0,C=300,0]]
      !    ){
      ! OpenCLMap ( ["k"],["v"],["(k,0,81,1)","(i,0,301,1)"],[]) {
      v(i, 0, k) = v(i, 300, k)
      v(i, 301, k) = v(i, 1, k)
      !}
      !}
      ! OpenCLStencil (
      !        2 point stencil on 3D array w: [[0,C=1,0],[0,C=300,0]]
      !    ){
      ! OpenCLMap ( ["k"],[],["(k,0,80,1)","(i,0,301,1)"],[]) {
      w(i, 0, k) = w(i, 300, k)
      w(i, 301, k) = w(i, 1, k)
      !}
      !}
      ! OpenCLStencil (
      !        2 point stencil on 3D array u: [[0,0,C=1],[0,0,C=80]]
      !    ){
      ! OpenCLMap ( ["j"],[],["(j,0,301,1)","(i,0,301,1)"],[]) {
      u(i, j, 0) = -u(i, j, 1)
      u(i, j, 81) = u(i, j, 80)
      !}
      !}
      ! OpenCLStencil (
      !        2 point stencil on 3D array v: [[0,0,C=1],[0,0,C=80]]
      !    ){
      ! OpenCLMap ( ["j"],["v"],["(j,0,301,1)","(i,0,301,1)"],[]) {
      v(i, j, 0) = -v(i, j, 1)
      v(i, j, 81) = v(i, j, 80)
      !}
      !}
      ! OpenCLMap ( [],[],["(j,-1,301,1)","(i,0,301,1)"],[]) {
      w(i, j, 0) = 0.0
      w(i, j, 80) = 0.0
      !}
      do j = 1, 300, 1
         do i = 1, 300, 1
            if (j == 300/2 .and. i == 300/2) then
               uspd = (u(i, j, 1)**2 + ((0.5*(v(i, j - 1, 1) + v(&
&i, j, 1))*dx1(i + 1) + 0.5*(v(i + 1, j - 1, 1) + v(i + 1, j, 1))*dx1(i))/(dx1(i) + dx1(i + 1))&
&)**2)**0.5
               vspd = (v(i, j, 1)**2 + ((0.5*(u(i - 1, j, 1) + u(&
&i, j, 1))*dy1(j + 1) + 0.5*(u(i - 1, j + 1, 1) + u(i, j + 1, 1))*dy1(j))/(dy1(j) + dy1(j + 1))&
&)**2)**0.5
               write (6, *) ('CHK_uspd_vspd=', uspd, vspd&
&)
            end if
         end do
      end do
      ! OpenCLStencil (
      !        2 point stencil on 1D array dx1: [[0],[1]]
      !        5 point stencil on 1D array dy1: [[C=0],[C=1],[0],[1],[-1]]
      !        2 point stencil on 1D array dzs: [[0],[-1]]
      !        10 point stencil on 3D array u: [[1,0,0],[0,C=0,0],[0,C=1,0],[0,1,0],[0,0,C=1],[0,0,0],[0,0,1],[0,0,-1],[0,-1,0],[-1,0,0]]
      !        6 point stencil on 3D array v: [[1,C=0,0],[1,0,0],[1,-1,0],[0,C=0,0],[0,0,0],[0,-1,0]]
      !        6 point stencil on 3D array w: [[1,0,C=1],[1,0,0],[1,0,-1],[0,0,C=1],[0,0,0],[0,0,-1]]
      !    ){
      ! OpenCLMap ( ["dx1","dzs","k","j"],[],["(k,1,80,1)","(j,1,300,1)","(i,1,300,1)"],[]) {
      nou1_ = (u(i - 1, j, k) + u(i, j, k))/2.
      diu1_ = (-u(i - 1, j, k) + u(i, j, k))/dx1(i)
      cov1_i = nou1_*diu1_
      nou1_ip1 = (u(i, j, k) + u(i + 1, j, k))/2.
      diu1_ip1 = (-u(i, j, k) + u(i + 1, j, k))/dx1(i + 1)
      cov1_ip1 = nou1_ip1*diu1_ip1
      if (i == 300) then
         cov1_ip1 = cov1_i
      end if
      nou2_ = (dx1(i + 1)*v(i, j - 1, k) + dx1(i)*v(i + 1, j - 1, k))/(dx1(i) + dx1(i + 1))
      diu2_ = 2.*(-u(i, j - 1, k) + u(i, j, k))/(dy1(j - 1) + dy1(j))
      cov2_j = nou2_*diu2_
      nou2_jp1 = (dx1(i + 1)*v(i, j, k) + dx1(i)*v(i + 1, j, k))/(dx1(i) + dx1(i + 1))
      diu2_jp1 = 2.*(-u(i, j, k) + u(i, j + 1, k))/(dy1(j) + dy1(j + 1))
      cov2_jp1 = nou2_jp1*diu2_jp1
      if (j == 300) then
         nou2_ = (dx1(i + 1)*v(i, 0, k) + dx1(i)*v(i + 1, 0, k))/(dx1(i) + dx1(i + 1))
         diu2_ = 2.*(-u(i, 0, k) + u(i, 1, k))/(dy1(0) + dy1(1))
         cov2_jp1 = nou2_*diu2_
      end if
      nou3_ = (dx1(i + 1)*w(i, j, k - 1) + dx1(i)*w(i + 1, j, k - 1))/(dx1(i) + dx1(i + 1))
      diu3_ = (-u(i, j, k - 1) + u(i, j, k))/dzs(k - 1)
      cov3_k = nou3_*diu3_
      nou3_kp1 = (dx1(i + 1)*w(i, j, k) + dx1(i)*w(i + 1, j, k))/(dx1(i) + dx1(i + 1))
      diu3_kp1 = (-u(i, j, k) + u(i, j, k + 1))/dzs(k)
      cov3_kp1 = nou3_kp1*diu3_kp1
      if (k == 1) then
         nou3_ = 0.5*(dx1(i + 1)*w(i, j, 1) + dx1(i)*w(i + 1, j, 1))/(dx1(i) + dx1(i +&
 &1))
         diu3_ = 0.4*u(i, j, 1)/(alog(5.0*dzn(1))*0.2*dzn(1))
         cov3_k = nou3_*diu3_
      end if
      covx1 = (dx1(i + 1)*cov1_i + dx1(i)*cov1_ip1)/(dx1(i) + dx1(i + 1))
      covy1 = (cov2_j + cov2_jp1)/2.
      covz1 = (cov3_k + cov3_kp1)/2.
      covc = covx1 + covy1 + covz1
      f(i, j, k) = (-covc)
      !}
      !}
      ! OpenCLStencil (
      !        3 point stencil on 1D array dx1: [[0],[1],[-1]]
      !        2 point stencil on 1D array dy1: [[0],[1]]
      !        2 point stencil on 1D array dzs: [[0],[-1]]
      !        4 point stencil on 3D array u: [[0,1,0],[0,0,0],[-1,1,0],[-1,0,0]]
      !        10 point stencil on 3D array v: [[1,0,0],[0,C=1,0],[0,C=2,0],[0,1,0],[0,0,C=1],[0,0,0],[0,0,1],[0,0,-1],[0,-1,0],[-1,0,0]]
      !        6 point stencil on 3D array w: [[0,1,C=1],[0,1,0],[0,1,-1],[0,0,C=1],[0,0,0],[0,0,-1]]
      !    ){
      ! OpenCLMap ( ["dy1","dzs","k","j"],[],["(k,1,80,1)","(j,1,300,1)","(i,1,300,1)"],[]) {
      nou4_ = (dy1(j + 1)*u(i - 1, j, k) + dy1(j)*u(i - 1, j + 1, k))/(dy1(j) + dy1(j + 1))
      diu4_ = 2.*(-v(i - 1, j, k) + v(i, j, k))/(dx1(i - 1) + dx1(i))
      cov4_i = (nou4_ - 0)*diu4_
      nou4_ip1 = (dy1(j + 1)*u(i, j, k) + dy1(j)*u(i, j + 1, k))/(dy1(j) + dy1(j + 1))
      diu4_ip1 = 2.*(-v(i, j, k) + v(i + 1, j, k))/(dx1(i) + dx1(i + 1))
      cov4_ip1 = (nou4_ip1 - 0)*diu4_ip1
      if (i == 300) then
         cov4_ip1 = cov4_i
      end if
      nou5_ = (v(i, j - 1, k) + v(i, j, k))/2.
      diu5_ = (-v(i, j - 1, k) + v(i, j, k))/dy1(j)
      cov5_j = nou5_*diu5_
      nou5_jp1 = (v(i, j, k) + v(i, j + 1, k))/2.
      diu5_jp1 = (-v(i, j, k) + v(i, j + 1, k))/dy1(j + 1)
      cov5_jp1 = nou5_jp1*diu5_jp1
      if (j == 300) then
         nou5_ = (v(i, 1, k) + v(i, 2, k))/2.
         diu5_ = (-v(i, 1, k) + v(i, 2, k))/dy1(j)
         cov5_jp1 = nou5_*diu5_
      end if
      nou6_ = (dy1(j + 1)*w(i, j, k - 1) + dy1(j)*w(i, j + 1, k - 1))/(dy1(j) + dy1(j + 1))
      diu6_ = (-v(i, j, k - 1) + v(i, j, k))/dzs(k - 1)
      cov6_k = nou6_*diu6_
      nou6_kp1 = (dy1(j + 1)*w(i, j, k) + dy1(j)*w(i, j + 1, k))/(dy1(j) + dy1(j + 1))
      diu6_kp1 = (-v(i, j, k) + v(i, j, k + 1))/dzs(k)
      cov6_kp1 = nou6_kp1*diu6_kp1
      if (k == 1) then
         nou6_ = 0.5*(dy1(j + 1)*w(i, j, 1) + dy1(j)*w(i, j + 1, 1))/(dy1(j) + dy1(j +&
 &1))
         diu6_ = 0.4*v(i, j, 1)/(alog(5.0*dzn(1))*0.2*dzn(1))
         cov6_k = nou6_*diu6_
      end if
      covx1 = (cov4_i + cov4_ip1)/2.
      covy1 = (dy1(j + 1)*cov5_j + dy1(j)*cov5_jp1)/(dy1(j) + dy1(j + 1))
      covz1 = (cov6_k + cov6_kp1)/2.
      covc = covx1 + covy1 + covz1
      g(i, j, k) = (-covc)
      !}
      !}
      ! OpenCLStencil (
      !        3 point stencil on 1D array dx1: [[0],[1],[-1]]
      !        5 point stencil on 1D array dy1: [[C=0],[C=1],[0],[1],[-1]]
      !        2 point stencil on 1D array dzn: [[0],[1]]
      !        4 point stencil on 3D array u: [[0,0,0],[0,0,1],[-1,0,0],[-1,0,1]]
      !        6 point stencil on 3D array v: [[0,C=0,0],[0,C=0,1],[0,0,0],[0,0,1],[0,-1,0],[0,-1,1]]
      !        9 point stencil on 3D array w: [[1,0,0],[0,C=0,0],[0,C=1,0],[0,1,0],[0,0,0],[0,0,1],[0,0,-1],[0,-1,0],[-1,0,0]]
      !    ){
      ! OpenCLMap ( ["dzn","k","j"],[],["(k,1,79,1)","(j,1,300,1)","(i,1,300,1)"],[]) {
      nou7_ = (dzn(k + 1)*u(i - 1, j, k) + dzn(k)*u(i - 1, j, k + 1))/(dzn(k) + dzn(k + 1))
      diu7_ = 2.*(-w(i - 1, j, k) + w(i, j, k))/(dx1(i - 1) + dx1(i))
      cov7_i = (nou7_ - 0)*diu7_
      nou7_ip1 = (dzn(k + 1)*u(i, j, k) + dzn(k)*u(i, j, k + 1))/(dzn(k) + dzn(k + 1))
      diu7_ip1 = 2.*(-w(i, j, k) + w(i + 1, j, k))/(dx1(i) + dx1(i + 1))
      cov7_ip1 = (nou7_ - 0)*diu7_
      if (i == 300) then
         cov7_ip1 = cov7_i
      end if
      nou8_ = (dzn(k + 1)*v(i, j - 1, k) + dzn(k)*v(i, j - 1, k + 1))/(dzn(k) + dzn(k + 1))
      diu8_ = 2.*(-w(i, j - 1, k) + w(i, j, k))/(dy1(j - 1) + dy1(j))
      cov8_j = nou8_*diu8_
      nou8_jp1 = (dzn(k + 1)*v(i, j, k) + dzn(k)*v(i, j, k + 1))/(dzn(k) + dzn(k + 1))
      diu8_jp1 = 2.*(-w(i, j, k) + w(i, j + 1, k))/(dy1(j) + dy1(j + 1))
      cov8_jp1 = nou8_jp1*diu8_jp1
      if (j == 300) then
         nou8_ = (dzn(k + 1)*v(i, 0, k) + dzn(k)*v(i, 0, k + 1))/(dzn(k) + dzn(k + 1))
         diu8_ = 2.*(-w(i, 0, k) + w(i, 1, k))/(dy1(0) + dy1(1))
         cov8_jp1 = nou8_*diu8_
      end if
      nou9_ = (w(i, j, k - 1) + w(i, j, k))/2.
      diu9_ = (-w(i, j, k - 1) + w(i, j, k))/dzn(k)
      cov9_k = nou9_*diu9_
      nou9_kp1 = (w(i, j, k) + w(i, j, k + 1))/2.
      diu9_kp1 = (-w(i, j, k) + w(i, j, k + 1))/dzn(k + 1)
      cov9_kp1 = nou9_kp1*diu9_kp1
      covx1 = (cov7_i + cov7_ip1)/2.
      covy1 = (cov8_j + cov8_jp1)/2.
      covz1 = (dzn(k + 1)*cov9_k + dzn(k)*cov9_kp1)/(dzn(k) + dzn(k + 1))
      covc = covx1 + covy1 + covz1
      h(i, j, k) = (-covc)
      !}
      !}
      ! OpenCLMap ( ["k"],[],["(k,1,80,1)","(j,1,300,1)"],[]) {
      f(0, j, k) = f(1, j, k)
      !}
      ! OpenCLMap ( ["k"],[],["(k,1,80,1)","(i,1,300,1)"],[]) {
      g(i, 0, k) = g(i, 300, k)
      !}
      ! OpenCLMap ( [],[],["(j,1,300,1)","(i,1,300,1)"],[]) {
      h(i, j, 0) = 0.0
      h(i, j, 80) = 0.0
      !}
      ! OpenCLMap ( ["bmask1","cmask1","dmask1","alpha","dt","beta","k","j","fx","fy","fz"],["fx","fy","fz","g"],["(k,1,80,1)","(j,1,300,1)","(i,1,300,1)"],[]) {
      usum(i, j, k) = (usum(i, j, k) + u(i, j, k))*bmask1(i, j, k)
      vsum(i, j, k) = (vsum(i, j, k) + v(i, j, k))*cmask1(i, j, k)
      wsum(i, j, k) = (wsum(i, j, k) + w(i, j, k))*dmask1(i, j, k)
      f1x = alpha*usum(i, j, k)*dt
      f1y = alpha*vsum(i, j, k)*dt
      f1z = alpha*wsum(i, j, k)*dt
      f2x = beta*u(i, j, k)*bmask1(i, j, k)
      f2y = beta*v(i, j, k)*cmask1(i, j, k)
      f2z = beta*w(i, j, k)*dmask1(i, j, k)
      fx(i, j, k) = f1x + f2x
      fy(i, j, k) = f1y + f2y
      fz(i, j, k) = f1z + f2z
      f(i, j, k) = f(i, j, k) + fx(i, j, k)
      g(i, j, k) = g(i, j, k) + fy(i, j, k)
      h(i, j, k) = h(i, j, k) + fz(i, j, k)
      !}
      ! OpenCLMap ( [],[],["(k,1,80,1)"],[]) {
      delx1(k) = (dx1(0)*dy1(0)*dzn(k))**(1./3.)
      !}
      ! OpenCLStencil (
      !        4 point stencil on 3D array diu2: [[0,1,0],[0,0,0],[-1,1,0],[-1,0,0]]
      !        4 point stencil on 3D array diu3: [[0,0,0],[0,0,1],[-1,0,0],[-1,0,1]]
      !        4 point stencil on 3D array diu4: [[1,0,0],[1,-1,0],[0,0,0],[0,-1,0]]
      !        4 point stencil on 3D array diu6: [[0,0,0],[0,0,1],[0,-1,0],[0,-1,1]]
      !        4 point stencil on 3D array diu7: [[1,0,0],[1,0,-1],[0,0,0],[0,0,-1]]
      !        4 point stencil on 3D array diu8: [[0,1,0],[0,1,-1],[0,0,0],[0,0,-1]]
      !    ){
      ! OpenCLMap ( ["diu1","diu5","diu9","k","j"],["sm"],["(k,1,80,1)","(j,1,300,1)","(i,1,300,1)"],[]) {
      dudxx1 = diu1(i, j, k)
      dudyx1 = (diu2(i - 1, j, k) + diu2(i - 1, j + 1, k) + diu2(i, j, k) + diu2(i, j + 1, k))*.&
  &25
      dudzx1 = (diu3(i - 1, j, k) + diu3(i - 1, j, k + 1) + diu3(i, j, k) + diu3(i, j, k + 1))*.&
  &25
      dvdxx1 = (diu4(i, j, k) + diu4(i, j - 1, k) + diu4(i + 1, j, k) + diu4(i + 1, j - 1, k))*.&
  &25
      dvdyx1 = diu5(i, j, k)
      dvdzx1 = (diu6(i, j - 1, k) + diu6(i, j - 1, k + 1) + diu6(i, j, k) + diu6(i, j, k + 1))*.&
  &25
      dwdxx1 = (diu7(i, j, k) + diu7(i, j, k - 1) + diu7(i + 1, j, k) + diu7(i + 1, j, k - 1))*.&
  &25
      dwdyx1 = (diu8(i, j, k) + diu8(i, j, k - 1) + diu8(i, j + 1, k) + diu8(i, j + 1, k - 1))*.&
  &25
      dwdzx1 = diu9(i, j, k)
      csx1 = 0.14
      sm(i, j, k) = (csx1*delx1(k))**2*sqrt(2.*(dudxx1**2 + dvdyx1**2 + dwdzx1**&
  &2) + (dudyx1 + dvdxx1)**2 + (dwdyx1 + dvdzx1)**2 + (dudzx1 + dwdxx1)**2)
      !}
      !}
      ! OpenCLStencil (
      !        2 point stencil on 3D array sm: [[C=1,0,0],[C=300,0,0]]
      !    ){
      ! OpenCLMap ( ["k"],["sm"],["(k,0,81,1)","(j,-1,301,1)"],[]) {
      sm(0, j, k) = sm(1, j, k)
      sm(301, j, k) = sm(300, j, k)
      !}
      !}
      ! OpenCLStencil (
      !        2 point stencil on 3D array sm: [[0,C=1,0],[0,C=300,0]]
      !    ){
      ! OpenCLMap ( ["k"],["sm"],["(k,0,81,1)","(i,0,301,1)"],[]) {
      sm(i, 301, k) = sm(i, 300, k)
      sm(i, 0, k) = sm(i, 1, k)
      !}
      !}
      ! OpenCLStencil (
      !        2 point stencil on 3D array sm: [[0,0,C=1],[0,0,C=80]]
      !    ){
      ! OpenCLMap ( ["j"],["sm"],["(j,-1,301,1)","(i,0,301,1)"],[]) {
      sm(i, j, 0) = -sm(i, j, 1)
      sm(i, j, 81) = sm(i, j, 80)
      !}
      !}
      ! OpenCLStencil (
      !        2 point stencil on 3D array diu1: [[1,0,0],[0,0,0]]
      !        2 point stencil on 3D array diu2: [[0,1,0],[0,0,0]]
      !        2 point stencil on 3D array diu3: [[0,0,0],[0,0,1]]
      !        2 point stencil on 3D array diu4: [[1,0,0],[1,-1,0]]
      !        2 point stencil on 3D array diu7: [[1,0,0],[1,0,-1]]
      !        2 point stencil on 1D array dx1: [[0],[1]]
      !        2 point stencil on 1D array dy1: [[0],[1]]
      !        3 point stencil on 1D array dzn: [[0],[1],[-1]]
      !        10 point stencil on 3D array sm: [[1,1,0],[1,0,0],[1,0,1],[1,0,-1],[1,-1,0],[0,1,0],[0,0,0],[0,0,1],[0,0,-1],[0,-1,0]]
      !    ){
      ! OpenCLMap ( ["sm","diu1","dxs","dy1","dzn","k","j"],[],["(k,2,80,1)","(j,1,300,1)","(i,1,300,1)"],[]) {
      evsx2 = sm(i + 1, j, k)
      evsx1 = sm(i, j, k)
      evsy2 = (dy1(j + 1)*((dx1(i + 1)*sm(i, j, k) + dx1(i)*sm(i + 1, j, k))/(dx1(i) + d&
  &x1(i + 1))) + dy1(j)*((dx1(i + 1)*sm(i, j + 1, k) + dx1(i)*sm(i + 1, j + 1, k))/(dx1(i) + dx&
  &1(i + 1))))/(dy1(j) + dy1(j + 1))
      evsy1 = (dy1(j + 1)*((dx1(i + 1)*sm(i, j - 1, k) + dx1(i)*sm(i + 1, j - 1, k))/(dx1(&
  &i) + dx1(i + 1))) + dy1(j)*((dx1(i + 1)*sm(i, j, k) + dx1(i)*sm(i + 1, j, k))/(dx1(i) + dx&
  &1(i + 1))))/(dy1(j) + dy1(j + 1))
      evsz2 = (dzn(k + 1)*((dx1(i + 1)*sm(i, j, k) + dx1(i)*sm(i + 1, j, k))/(dx1(i) + d&
  &x1(i + 1))) + dzn(k)*((dx1(i + 1)*sm(i, j, k + 1) + dx1(i)*sm(i + 1, j, k + 1))/(dx1(i) + dx&
  &1(i + 1))))/(dzn(k) + dzn(k + 1))
      evsz1 = (dzn(k)*((dx1(i + 1)*sm(i, j, k - 1) + dx1(i)*sm(i + 1, j, k - 1))/(dx1(i)&
  &+ dx1(i + 1))) + dzn(k - 1)*((dx1(i + 1)*sm(i, j, k) + dx1(i)*sm(i + 1, j, k))/(dx1(i) + dx&
  &1(i + 1))))/(dzn(k - 1) + dzn(k))
      visux2 = (evsx2)*2.*diu1(i + 1, j, k)
      visux1 = (evsx1)*2.*diu1(i, j, k)
      visuy2 = (evsy2)*(diu2(i, j + 1, k) + diu4(i + 1, j, k))
      visuy1 = (evsy1)*(diu2(i, j, k) + diu4(i + 1, j - 1, k))
      visuz2 = (evsz2)*(diu3(i, j, k + 1) + diu7(i + 1, j, k))
      visuz1 = (evsz1)*(diu3(i, j, k) + diu7(i + 1, j, k - 1))
      vfu = (visux2 - visux1)/dxs(i) + (visuy2 - visuy1)/dy1(j) + (visuz2 - visuz1)/&
  &dzn(k)
      f(i, j, k) = (f(i, j, k) + vfu)
      !}
      !}
      ! OpenCLStencil (
      !        2 point stencil on 3D array diu1: [[1,0,C=1],[0,0,C=1]]
      !        2 point stencil on 3D array diu2: [[0,1,C=1],[0,0,C=1]]
      !        2 point stencil on 3D array diu4: [[1,0,C=1],[1,-1,C=1]]
      !        2 point stencil on 1D array dx1: [[0],[1]]
      !        2 point stencil on 1D array dy1: [[0],[1]]
      !        2 point stencil on 1D array dzn: [[C=1],[C=2]]
      !        8 point stencil on 3D array sm: [[1,1,C=1],[1,0,C=1],[1,0,C=2],[1,-1,C=1],[0,1,C=1],[0,0,C=1],[0,0,C=2],[0,-1,C=1]]
      !    ){
      ! OpenCLMap ( ["sm","diu1","dxs","dy1","dzn","j"],["f"],["(j,1,300,1)","(i,1,300,1)"],[]) {
      evsx2 = sm(i + 1, j, 1)
      evsx1 = sm(i, j, 1)
      evsy2 = (dy1(j + 1)*((dx1(i + 1)*sm(i, j, 1) + dx1(i)*sm(i + 1, j, 1))/(dx1(i) + d&
  &x1(i + 1))) + dy1(j)*((dx1(i + 1)*sm(i, j + 1, 1) + dx1(i)*sm(i + 1, j + 1, 1))/(dx1(i) + dx&
  &1(i + 1))))/(dy1(j) + dy1(j + 1))
      evsy1 = (dy1(j + 1)*((dx1(i + 1)*sm(i, j - 1, 1) + dx1(i)*sm(i + 1, j - 1, 1))/(dx1(&
  &i) + dx1(i + 1))) + dy1(j)*((dx1(i + 1)*sm(i, j, 1) + dx1(i)*sm(i + 1, j, 1))/(dx1(i) + dx&
  &1(i + 1))))/(dy1(j) + dy1(j + 1))
      evsz2 = (dzn(2)*((dx1(i + 1)*sm(i, j, 1) + dx1(i)*sm(i + 1, j, 1))/(dx1(i) + dx1&
  &(i + 1))) + dzn(1)*((dx1(i + 1)*sm(i, j, 2) + dx1(i)*sm(i + 1, j, 2))/(dx1(i) + dx1(i + 1)&
  &)))/(dzn(1) + dzn(2))
      visux2 = (evsx2)*2.*diu1(i + 1, j, 1)
      visux1 = (evsx1)*2.*diu1(i, j, 1)
      visuy2 = (evsy2)*(diu2(i, j + 1, 1) + diu4(i + 1, j, 1))
      visuy1 = (evsy1)*(diu2(i, j, 1) + diu4(i + 1, j - 1, 1))
      visuz2 = (evsz2)*(diu3(i, j, 2) + diu7(i + 1, j, 1))
      visuz1 = (0.4*uspd(i, j)/alog(0.5*dzn(1)/0.1))**2*(u(i, j, 1)/uspd(i, j)&
  &)
      vfu = (visux2 - visux1)/dxs(i) + (visuy2 - visuy1)/dy1(j) + (visuz2 - visuz1)/&
  &dzn(1)
      f(i, j, 1) = (f(i, j, 1) + vfu)
      !}
      !}
      ! OpenCLStencil (
      !        2 point stencil on 3D array diu2: [[0,1,0],[-1,1,0]]
      !        2 point stencil on 3D array diu4: [[1,0,0],[0,0,0]]
      !        2 point stencil on 3D array diu5: [[0,1,0],[0,0,0]]
      !        2 point stencil on 3D array diu6: [[0,0,0],[0,0,1]]
      !        2 point stencil on 3D array diu8: [[0,1,0],[0,1,-1]]
      !        3 point stencil on 1D array dx1: [[0],[1],[-1]]
      !        2 point stencil on 1D array dy1: [[0],[1]]
      !        3 point stencil on 1D array dzn: [[0],[1],[-1]]
      !        10 point stencil on 3D array sm: [[1,1,0],[1,0,0],[1,0,1],[1,0,-1],[0,1,0],[0,0,0],[0,0,1],[0,0,-1],[-1,1,0],[-1,0,0]]
      !    ){
      ! OpenCLMap ( ["sm","diu5","dx1","dys","dzn","k","j"],["g"],["(k,2,80,1)","(j,1,300,1)","(i,1,300,1)"],[]) {
      evsy2 = sm(i, j + 1, k)
      evsy1 = sm(i, j, k)
      evsx2 = (dy1(j + 1)*((dx1(i + 1)*sm(i, j, k) + dx1(i)*sm(i + 1, j, k))/(dx1(i) + d&
  &x1(i + 1))) + dy1(j)*((dx1(i + 1)*sm(i, j + 1, k) + dx1(i)*sm(i + 1, j + 1, k))/(dx1(i) + dx&
  &1(i + 1))))/(dy1(j) + dy1(j + 1))
      evsx1 = (dy1(j + 1)*((dx1(i)*sm(i - 1, j, k) + dx1(i - 1)*sm(i, j, k))/(dx1(i - 1)&
  &+ dx1(i))) + dy1(j)*((dx1(i)*sm(i - 1, j + 1, k) + dx1(i - 1)*sm(i, j + 1, k))/(dx1(i - 1) +&
  &dx1(i))))/(dy1(j) + dy1(j + 1))
      evsz2 = (dzn(k + 1)*((dx1(i + 1)*sm(i, j, k) + dx1(i)*sm(i + 1, j, k))/(dx1(i) + d&
  &x1(i + 1))) + dzn(k)*((dx1(i + 1)*sm(i, j, k + 1) + dx1(i)*sm(i + 1, j, k + 1))/(dx1(i) + dx&
  &1(i + 1))))/(dzn(k) + dzn(k + 1))
      evsz1 = (dzn(k)*((dx1(i + 1)*sm(i, j, k - 1) + dx1(i)*sm(i + 1, j, k - 1))/(dx1(i)&
  &+ dx1(i + 1))) + dzn(k - 1)*((dx1(i + 1)*sm(i, j, k) + dx1(i)*sm(i + 1, j, k))/(dx1(i) + dx&
  &1(i + 1))))/(dzn(k - 1) + dzn(k))
      visvx2 = (evsx2)*(diu2(i, j + 1, k) + diu4(i + 1, j, k))
      visvx1 = (evsx1)*(diu2(i - 1, j + 1, k) + diu4(i, j, k))
      visvy2 = (evsy2)*2.*diu5(i, j + 1, k)
      visvy1 = (evsy1)*2.*diu5(i, j, k)
      visvz2 = (evsz2)*(diu6(i, j, k + 1) + diu8(i, j + 1, k))
      visvz1 = (evsz1)*(diu6(i, j, k) + diu8(i, j + 1, k - 1))
      vfv = (visvx2 - visvx1)/dx1(i) + (visvy2 - visvy1)/dys(j) + (visvz2 - visvz1)/&
  &dzn(k)
      g(i, j, k) = (g(i, j, k) + vfv)
      !}
      !}
      ! OpenCLStencil (
      !        2 point stencil on 3D array diu2: [[0,1,C=1],[-1,1,C=1]]
      !        2 point stencil on 3D array diu4: [[1,0,C=1],[0,0,C=1]]
      !        2 point stencil on 3D array diu5: [[0,1,C=1],[0,0,C=1]]
      !        3 point stencil on 1D array dx1: [[0],[1],[-1]]
      !        2 point stencil on 1D array dy1: [[0],[1]]
      !        2 point stencil on 1D array dzn: [[C=1],[C=2]]
      !        8 point stencil on 3D array sm: [[1,1,C=1],[1,0,C=1],[1,0,C=2],[0,1,C=1],[0,0,C=1],[0,0,C=2],[-1,1,C=1],[-1,0,C=1]]
      !    ){
      ! OpenCLMap ( ["sm","diu5","dx1","dys","dzn","j"],[],["(j,1,300,1)","(i,1,300,1)"],[]) {
      evsy2 = sm(i, j + 1, 1)
      evsy1 = sm(i, j, 1)
      evsx2 = (dy1(j + 1)*((dx1(i + 1)*sm(i, j, 1) + dx1(i)*sm(i + 1, j, 1))/(dx1(i) + d&
  &x1(i + 1))) + dy1(j)*((dx1(i + 1)*sm(i, j + 1, 1) + dx1(i)*sm(i + 1, j + 1, 1))/(dx1(i) + dx&
  &1(i + 1))))/(dy1(j) + dy1(j + 1))
      evsx1 = (dy1(j + 1)*((dx1(i)*sm(i - 1, j, 1) + dx1(i - 1)*sm(i, j, 1))/(dx1(i - 1)&
  &+ dx1(i))) + dy1(j)*((dx1(i)*sm(i - 1, j + 1, 1) + dx1(i - 1)*sm(i, j + 1, 1))/(dx1(i - 1) +&
  &dx1(i))))/(dy1(j) + dy1(j + 1))
      evsz2 = (dzn(2)*((dx1(i + 1)*sm(i, j, 1) + dx1(i)*sm(i + 1, j, 1))/(dx1(i) + dx1&
  &(i + 1))) + dzn(1)*((dx1(i + 1)*sm(i, j, 2) + dx1(i)*sm(i + 1, j, 2))/(dx1(i) + dx1(i + 1)&
  &)))/(dzn(1) + dzn(2))
      visvx2 = (evsx2)*(diu2(i, j + 1, 1) + diu4(i + 1, j, 1))
      visvx1 = (evsx1)*(diu2(i - 1, j + 1, 1) + diu4(i, j, 1))
      visvy2 = (evsy2)*2.*diu5(i, j + 1, 1)
      visvy1 = (evsy1)*2.*diu5(i, j, 1)
      visvz2 = (evsz2)*(diu6(i, j, 2) + diu8(i, j + 1, 1))
      visvz1 = (0.4*vspd(i, j)/alog(0.5*dzn(1)/0.1))**2*(v(i, j, 1)/vspd(i, j)&
  &)
      vfv = (visvx2 - visvx1)/dx1(i) + (visvy2 - visvy1)/dys(j) + (visvz2 - visvz1)/&
  &dzn(1)
      g(i, j, 1) = (g(i, j, 1) + vfv)
      !}
      !}
      ! OpenCLStencil (
      !        2 point stencil on 3D array diu3: [[0,0,1],[-1,0,1]]
      !        2 point stencil on 3D array diu6: [[0,0,1],[0,-1,1]]
      !        2 point stencil on 3D array diu7: [[1,0,0],[0,0,0]]
      !        2 point stencil on 3D array diu8: [[0,1,0],[0,0,0]]
      !        2 point stencil on 3D array diu9: [[0,0,0],[0,0,1]]
      !        3 point stencil on 1D array dx1: [[0],[1],[-1]]
      !        3 point stencil on 1D array dy1: [[0],[1],[-1]]
      !        2 point stencil on 1D array dzn: [[0],[1]]
      !        10 point stencil on 3D array sm: [[1,0,0],[1,0,1],[0,1,0],[0,1,1],[0,0,0],[0,0,1],[0,-1,0],[0,-1,1],[-1,0,0],[-1,0,1]]
      !    ){
      ! OpenCLMap ( ["sm","diu9","dx1","dy1","dzn","k","j"],[],["(k,1,80,1)","(j,1,300,1)","(i,1,300,1)"],[]) {
      evsz2 = sm(i, j, k + 1)
      evsz1 = sm(i, j, k)
      evsx2 = (dzn(k + 1)*((dx1(i + 1)*sm(i, j, k) + dx1(i)*sm(i + 1, j, k))/(dx1(i) + d&
  &x1(i + 1))) + dzn(k)*((dx1(i + 1)*sm(i, j, k + 1) + dx1(i)*sm(i + 1, j, k + 1))/(dx1(i) + dx&
  &1(i + 1))))/(dzn(k) + dzn(k + 1))
      evsx1 = (dzn(k + 1)*((dx1(i)*sm(i - 1, j, k) + dx1(i - 1)*sm(i, j, k))/(dx1(i - 1)&
  &+ dx1(i))) + dzn(k)*((dx1(i)*sm(i - 1, j, k + 1) + dx1(i - 1)*sm(i, j, k + 1))/(dx1(i - 1) +&
  &dx1(i))))/(dzn(k) + dzn(k + 1))
      evsy2 = (dzn(k + 1)*((dy1(j + 1)*sm(i, j, k) + dy1(j)*sm(i, j + 1, k))/(dy1(j) + d&
  &y1(j + 1))) + dzn(k)*((dy1(j + 1)*sm(i, j, k + 1) + dy1(j)*sm(i, j + 1, k + 1))/(dy1(j) + dy&
  &1(j + 1))))/(dzn(k) + dzn(k + 1))
      evsy1 = (dzn(k + 1)*((dy1(j)*sm(i, j - 1, k) + dy1(j - 1)*sm(i, j, k))/(dy1(j - 1)&
  &+ dy1(j))) + dzn(k)*((dy1(j)*sm(i, j - 1, k + 1) + dy1(j - 1)*sm(i, j, k + 1))/(dy1(j - 1) +&
  &dy1(j))))/(dzn(k) + dzn(k + 1))
      viswx2 = (evsx2)*(diu3(i, j, k + 1) + diu7(i + 1, j, k))
      viswx1 = (evsx1)*(diu3(i - 1, j, k + 1) + diu7(i, j, k))
      viswy2 = (evsy2)*(diu6(i, j, k + 1) + diu8(i, j + 1, k))
      viswy1 = (evsy1)*(diu6(i, j - 1, k + 1) + diu8(i, j, k))
      viswz2 = (evsz2)*2.*diu9(i, j, k + 1)
      viswz1 = (evsz1)*2.*diu9(i, j, k)
      vfw = (viswx2 - viswx1)/dx1(i) + (viswy2 - viswy1)/dy1(j) + (viswz2 - viswz1)/&
  &dzn(k)
      h(i, j, k) = (h(i, j, k) + vfw)
      !}
      !}
      ! OpenCLMap ( ["f","g","h","fold","gold","hold","k","j"],["f","g","h"],["(k,1,80,1)","(j,1,300,1)","(i,1,300,1)"],[]) {
      fd = f(i, j, k)
      gd = g(i, j, k)
      hd = h(i, j, k)
      f(i, j, k) = 1.5*f(i, j, k) - 0.5*fold(i, j, k)
      g(i, j, k) = 1.5*g(i, j, k) - 0.5*gold(i, j, k)
      h(i, j, k) = 1.5*h(i, j, k) - 0.5*hold(i, j, k)
      fold(i, j, k) = fd
      gold(i, j, k) = gd
      hold(i, j, k) = hd
      !}
      ! OpenCLMap ( ["k"],[],["(k,1,80,1)","(j,1,300,1)"],[]) {
      f(0, j, k) = f(1, j, k)
      !}
      ! OpenCLMap ( ["k"],[],["(k,1,80,1)","(i,1,300,1)"],[]) {
      g(i, 0, k) = g(i, 300, k)
      !}
      ! OpenCLMap ( [],[],["(j,1,300,1)","(i,1,300,1)"],[]) {
      h(i, j, 0) = 0.0
      h(i, j, 80) = 0.0
      !}
      ! OpenCLStencil (
      !        2 point stencil on 3D array f: [[0,0,0],[-1,0,0]]
      !        2 point stencil on 3D array g: [[0,0,0],[0,-1,0]]
      !        2 point stencil on 3D array h: [[0,0,0],[0,0,-1]]
      !        2 point stencil on 3D array u: [[0,0,0],[-1,0,0]]
      !        2 point stencil on 3D array v: [[0,0,0],[0,-1,0]]
      !        2 point stencil on 3D array w: [[0,0,0],[0,0,-1]]
      !    ){
      ! OpenCLMap ( ["dx1","dy1","dzn","dt","k","j"],["rhs"],["(k,1,80,1)","(j,1,300,1)","(i,1,300,1)"],[]) {
      rhs(i, j, k) = (-u(i - 1, j, k) + u(i, j, k))/dx1(i) + (-v(i, j - 1, k) + v(i, j, k))/dy&
  &1(j) + (-w(i, j, k - 1) + w(i, j, k))/dzn(k)
      rhs(i, j, k) = (f(i, j, k) - f(i - 1, j, k))/dx1(i) + (g(i, j, k) - g(i, j - 1, k))/dy1(&
  &j) + (h(i, j, k) - h(i, j, k - 1))/dzn(k) + rhs(i, j, k)/dt
      !}
      !}
      rhsav = 0.0
      area = 0.0
      ! OpenCLReduce ( ["dx1","dy1","dzn","rhs","i","j","k"],["rhsav","area"],["(k,1,80,1)","(j,1,300,1)","(i,1,300,1)"],[],["(rhsav,0.0)","(area,0.0)"]) {
      rhsav = rhsav + dx1(i)*dy1(j)*dzn(k)*rhs(i, j, k)
      area = area + dx1(i)*dy1(j)*dzn(k)
      !}
      rhsav = rhsav/area
      ! OpenCLMap ( ["rhsav","k","j"],[],["(k,1,80,1)","(j,1,300,1)","(i,1,300,1)"],[]) {
      rhs(i, j, k) = rhs(i, j, k) - rhsav
      !}
      do l = 1, 50, 1
         sor = 0.0
         do nrd = 0, 1, 1
            ! OpenCLStencil (
            !        2 point stencil on 1D array dxs: [[0],[-1]]
            !        2 point stencil on 1D array dys: [[0],[-1]]
            !        2 point stencil on 1D array dzs: [[0],[-1]]
            !        14 point stencil on 4D array p: [[C=0,1,0,0],[C=0,0,1,0],[C=0,0,0,0],[C=0,0,0,1],[C=0,0,0,-1],[C=0,0,-1,0],[C=0,-1,0,0],[C=1,1,0,0],[C=1,0,1,0],[C=1,0,0,0],[C=1,0,0,1],[C=1,0,0,-1],[C=1,0,-1,0],[C=1,-1,0,0]]
            !                ){
            ! OpenCLMap ( ["dzs","nrd","k","j"],["p"],["(k,1,80,1)","(j,1,300,1)","(i,1,300,1)"],["nrd","l"]) {
            dz1 = dzs(k - 1)
            dz2 = dzs(k)
            cn4s = 2./(dz1*(dz1 + dz2))
            cn4l = 2./(dz2*(dz1 + dz2))
            cn3s = 2./(dys(j - 1)*(dys(j - 1) + dys(j)))
            cn3l = 2./(dys(j)*(dys(j - 1) + dys(j)))
            cn2s = 2./(dxs(i - 1)*(dxs(i - 1) + dxs(i)))
            cn2l = 2./(dxs(i)*(dxs(i - 1) + dxs(i)))
            cn1 = 1./(2./(dxs(i - 1)*dxs(i)) + 2./(dys(j - 1)*dys(j)) + 2./(&
&dz1*dz2))
            if (nrd == 0) then
               reltmp = 1.0*(cn1*(cn2l*p(0, i + 1, j, k) + cn2&
&s*p(0, i - 1, j, k) + cn3l*p(0, i, j + 1, k) + cn3s*p(0, i, j - 1, k) + cn4l*p(0, i, j, k + 1) + cn4&
&s*p(0, i, j, k - 1) - rhs(i, j, k)) - p(0, i, j, k))
               p(1, i, j, k) = p(0, i, j, k) + reltmp
            else
               reltmp = 1.0*(cn1*(cn2l*p(1, i + 1, j, k) + cn2&
&s*p(1, i - 1, j, k) + cn3l*p(1, i, j + 1, k) + cn3s*p(1, i, j - 1, k) + cn4l*p(1, i, j, k + 1) + cn4&
&s*p(1, i, j, k - 1) - rhs(i, j, k)) - p(1, i, j, k))
               p(0, i, j, k) = p(1, i, j, k) + reltmp
            end if
            !}
            !}
            ! OpenCLStencil (
            !        2 point stencil on 4D array p: [[C=0,C=1,0,0],[C=0,C=300,0,0]]
            !                ){
            ! OpenCLMap ( ["k"],["p"],["(k,0,81,1)","(j,0,301,1)"],["nrd","l"]) {
            p(0, 0, j, k) = p(0, 1, j, k)
            p(0, 301, j, k) = p(0, 300, j, k)
            !}
            !}
            ! OpenCLStencil (
            !        2 point stencil on 4D array p: [[C=0,0,C=1,0],[C=0,0,C=300,0]]
            !                ){
            ! OpenCLMap ( ["k"],["p"],["(k,0,81,1)","(i,0,301,1)"],["nrd","l"]) {
            p(0, i, 0, k) = p(0, i, 300, k)
            p(0, i, 301, k) = p(0, i, 1, k)
            !}
            !}
         end do
         ! OpenCLStencil (
         !        2 point stencil on 4D array p: [[C=0,0,0,C=1],[C=0,0,0,C=80]]
         !        ){
         ! OpenCLMap ( ["j"],["p"],["(j,0,301,1)","(i,0,301,1)"],["l"]) {
         p(0, i, j, 0) = p(0, i, j, 1)
         p(0, i, j, 81) = p(0, i, j, 80)
         !}
         !}
      end do
      pav = 0.0
      pco = 0.0
      ! OpenCLReduce ( ["p","dx1","dy1","dzn","i","j","k"],["pav","pco"],["(k,1,80,1)","(j,1,300,1)","(i,1,300,1)"],[],["(pav,0.0)","(pco,0.0)"]) {
      pav = pav + p(0, i, j, k)*dx1(i)*dy1(j)*dzn(k)
      pco = pco + dx1(i)*dy1(j)*dzn(k)
      !}
      pav = pav/pco
      ! OpenCLMap ( ["pav","k","j"],["p"],["(k,1,80,1)","(j,1,300,1)","(i,1,300,1)"],[]) {
      p(0, i, j, k) = p(0, i, j, k) - pav
      !}
      ! OpenCLStencil (
      !        2 point stencil on 4D array p: [[C=0,C=1,0,0],[C=0,C=300,0,0]]
      !    ){
      ! OpenCLMap ( ["k"],["p"],["(k,0,81,1)","(j,0,301,1)"],[]) {
      p(0, 0, j, k) = p(0, 1, j, k)
      p(0, 301, j, k) = p(0, 300, j, k)
      !}
      !}
      ! OpenCLStencil (
      !        2 point stencil on 4D array p: [[C=0,0,C=1,0],[C=0,0,C=300,0]]
      !    ){
      ! OpenCLMap ( ["k"],["p"],["(k,0,81,1)","(i,0,301,1)"],[]) {
      p(0, i, 0, k) = p(0, i, 300, k)
      p(0, i, 301, k) = p(0, i, 1, k)
      !}
      !}
      ! OpenCLStencil (
      !        2 point stencil on 4D array p: [[C=0,0,0,C=1],[C=0,0,0,C=80]]
      !    ){
      ! OpenCLMap ( ["j"],[],["(j,0,301,1)","(i,0,301,1)"],[]) {
      p(0, i, j, 0) = p(0, i, j, 1)
      p(0, i, j, 81) = p(0, i, j, 80)
      !}
      !}
   end subroutine velnw_bondv1_velfg_feedbf_les_adam_press_merged
end module module_velnw_bondv1_velfg_feedbf_les_adam_press_merged