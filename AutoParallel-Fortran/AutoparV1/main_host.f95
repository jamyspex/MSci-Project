      program wave2d
      use module_vernieuw
use sub

use oclWrapper
use module_shapiro_dyn_vernieuw_superkernel_init


! Original declarations
      integer(4), parameter :: nx = 500  !!
      integer(4), parameter :: ny = 500  !!
      real :: dt !!
      real :: dx !!
      real :: dy !!
      real :: eps !!
      real, dimension(0:ny+1,0:nx+1) :: eta !!
      real, dimension(0:ny+1,0:nx+1) :: etan !!
      real :: g !!
      real, dimension(0:ny+1,0:nx+1) :: h !!
      real :: hmin !!
      real, dimension(0:ny+1,0:nx+1) :: hzero !!
      integer :: j_cunt !!
      integer :: k !!
      real, dimension(0:ny+1,0:nx+1) :: u !!
      real, dimension(0:ny+1,0:nx+1) :: un !!
      real, dimension(0:ny+1,0:nx+1) :: v !!
      real, dimension(0:ny+1,0:nx+1) :: vn !!
      integer, dimension(0:ny+1,0:nx+1) :: wet !!
      real :: hmax !!
      real :: time !!
      real :: dtmax !!
      real :: c !!
      real :: lambda !!
      integer :: n !!
      integer :: ntot !!
      integer :: nout !!
      integer :: dummy !!



!

! otherStatements

! remainingDecls

! Extra declarations
!NOTHING!
! Buffer declarations
      integer(8) :: state_ptr_buf
      integer(8) :: eps_buf
      integer(8) :: dt_buf
      integer(8) :: g_buf
      integer(8) :: dx_buf
      integer(8) :: dy_buf
      integer(8) :: u_buf
      integer(8) :: wet_buf
      integer(8) :: v_buf
      integer(8) :: h_buf
      integer(8) :: eta_buf
      integer(8) :: hzero_buf
      integer(8) :: hmin_buf
      integer(8) :: un_buf
      integer(8) :: vn_buf
      integer(8) :: etan_buf

integer, dimension(1) :: state_ptr

! Size declarations
      integer, dimension(1) :: state_ptr_sz
      integer, dimension(1) :: eps_ptr_sz
      integer, dimension(1) :: dt_ptr_sz
      integer, dimension(1) :: g_ptr_sz
      integer, dimension(1) :: dx_ptr_sz
      integer, dimension(1) :: dy_ptr_sz
      integer, dimension(2) :: u_sz
      integer, dimension(2) :: wet_sz
      integer, dimension(2) :: v_sz
      integer, dimension(2) :: h_sz
      integer, dimension(2) :: eta_sz
      integer, dimension(2) :: hzero_sz
      integer, dimension(1) :: hmin_ptr_sz
      integer, dimension(2) :: un_sz
      integer, dimension(2) :: vn_sz
      integer, dimension(2) :: etan_sz
      real, dimension(1) :: eps_ptr
      real, dimension(1) :: dt_ptr
      real, dimension(1) :: g_ptr
      real, dimension(1) :: dx_ptr
      real, dimension(1) :: dy_ptr
      real, dimension(1) :: hmin_ptr

call shapiro_dyn_vernieuw_superkernel_init()

! Size assignments
      state_ptr_sz = shape(state_ptr)
      eps_ptr_sz = shape(eps_ptr)
      dt_ptr_sz = shape(dt_ptr)
      g_ptr_sz = shape(g_ptr)
      dx_ptr_sz = shape(dx_ptr)
      dy_ptr_sz = shape(dy_ptr)
      u_sz = shape(u)
      wet_sz = shape(wet)
      v_sz = shape(v)
      h_sz = shape(h)
      eta_sz = shape(eta)
      hzero_sz = shape(hzero)
      hmin_ptr_sz = shape(hmin_ptr)
      un_sz = shape(un)
      vn_sz = shape(vn)
      etan_sz = shape(etan)

! Buffer loads
      call oclLoadBuffer(STATE_PTR_BUF_IDX, state_ptr_buf)
      call oclLoadBuffer(EPS_BUF_IDX, eps_buf)
      call oclLoadBuffer(DT_BUF_IDX, dt_buf)
      call oclLoadBuffer(G_BUF_IDX, g_buf)
      call oclLoadBuffer(DX_BUF_IDX, dx_buf)
      call oclLoadBuffer(DY_BUF_IDX, dy_buf)
      call oclLoadBuffer(U_BUF_IDX, u_buf)
      call oclLoadBuffer(WET_BUF_IDX, wet_buf)
      call oclLoadBuffer(V_BUF_IDX, v_buf)
      call oclLoadBuffer(H_BUF_IDX, h_buf)
      call oclLoadBuffer(ETA_BUF_IDX, eta_buf)
      call oclLoadBuffer(HZERO_BUF_IDX, hzero_buf)
      call oclLoadBuffer(HMIN_BUF_IDX, hmin_buf)
      call oclLoadBuffer(UN_BUF_IDX, un_buf)
      call oclLoadBuffer(VN_BUF_IDX, vn_buf)
      call oclLoadBuffer(ETAN_BUF_IDX, etan_buf)

! Original code with buffer writes and reads
ntot = 10000
eps = 0.05
eps_ptr(1) = eps
call oclWrite1DFloatArrayBuffer(eps_buf,eps_ptr_sz,eps_ptr)! Automatic conversion to array
call init(hmin,dx,dy,dt,g,j_cunt,k,hzero,eta,etan,h,wet,u,un,v,vn)
dt_ptr(1) = dt
call oclWrite1DFloatArrayBuffer(dt_buf,dt_ptr_sz,dt_ptr)! Automatic conversion to array
g_ptr(1) = g
call oclWrite1DFloatArrayBuffer(g_buf,g_ptr_sz,g_ptr)! Automatic conversion to array
dx_ptr(1) = dx
call oclWrite1DFloatArrayBuffer(dx_buf,dx_ptr_sz,dx_ptr)! Automatic conversion to array
dy_ptr(1) = dy
call oclWrite1DFloatArrayBuffer(dy_buf,dy_ptr_sz,dy_ptr)! Automatic conversion to array
call oclWrite2DFloatArrayBuffer(u_buf,u_sz,u)
call oclWrite2DIntArrayBuffer(wet_buf,wet_sz,wet)
call oclWrite2DFloatArrayBuffer(v_buf,v_sz,v)
call oclWrite2DFloatArrayBuffer(h_buf,h_sz,h)
call oclWrite2DFloatArrayBuffer(eta_buf,eta_sz,eta)
call oclWrite2DFloatArrayBuffer(hzero_buf,hzero_sz,hzero)
hmin_ptr(1) = hmin
call oclWrite1DFloatArrayBuffer(hmin_buf,hmin_ptr_sz,hmin_ptr)! Automatic conversion to array
call oclWrite2DFloatArrayBuffer(un_buf,un_sz,un)
call oclWrite2DFloatArrayBuffer(vn_buf,vn_sz,vn)
call oclWrite2DFloatArrayBuffer(etan_buf,etan_sz,etan)
CODE EXTRACTED FROM SOURCE FILE DIRECTLY
open(90,file ='debug.dat',form='formatted')
----------------------------------CODE EXTRACTED FROM SOURCE FILE DIRECTLY
open(10,file ='eta0.dat',form='formatted')
----------------------------------  do j = 0,ny+1
    write(10,'(101F12.6)')(eta(j,k),k=0,nx+1)
  end do
CODE EXTRACTED FROM SOURCE FILE DIRECTLY
close(10)
----------------------------------CODE EXTRACTED FROM SOURCE FILE DIRECTLY
open(10,file ='h0.dat',form='formatted')
----------------------------------  do j = 0,ny+1
    write(10,'(101F12.6)')(hzero(j,k),k=0,nx+1)
  end do
CODE EXTRACTED FROM SOURCE FILE DIRECTLY
close(10)
----------------------------------hmax = 0.
do j = 1,ny
do k = 1,nx
  hmax = max(hmax,h(j,k))
end do
end do
dummy = 0
c = sqrt(2*g*hmax)
CODE EXTRACTED FROM SOURCE FILE DIRECTLY
write(6,*)"c = ",c
----------------------------------lambda = dt*sqrt(g*hmax)/min(dx,dy)
CODE EXTRACTED FROM SOURCE FILE DIRECTLY
write(6,*)"lambda = ",lambda
----------------------------------if(lambda > 1)then
  write(6,*) "This will not work. Do you know why?"
  stop
end if
CODE EXTRACTED FROM SOURCE FILE DIRECTLY
open(10,file ='eta.dat',form='formatted')
----------------------------------CODE EXTRACTED FROM SOURCE FILE DIRECTLY
open(20,file ='h.dat',form='formatted')
----------------------------------CODE EXTRACTED FROM SOURCE FILE DIRECTLY
open(30,file ='u.dat',form='formatted')
----------------------------------CODE EXTRACTED FROM SOURCE FILE DIRECTLY
open(40,file ='v.dat',form='formatted')
----------------------------------do j = 26,26
do k = 26,26
eta(j,k) = 1.0
end do
end do
do n = 1,ntot
time = real(n)*dt
  call dyn(j,k,dx,g,eta,dt,dy,un,u,wet,v,vn,h,etan)
  call shapiro(j,k,wet,etan,eps,eta)
      call vernieuw(dt,dx,dy,eps,eta,etan,g,h,hmin,hzero,j,k,u,un,v,vn,wet)
end do
do j = 0,ny+1
  write(10,'(101F12.6)')(eta(j,k),k=0,nx+1)
  write(20,'(101F12.6)')(h(j,k) ,k=0,nx+1)
  write(30,'(101F12.6)')(u(j,k) ,k=0,nx+1)
  write(40,'(101F12.6)')(v(j,k) ,k=0,nx+1)
end do
CODE EXTRACTED FROM SOURCE FILE DIRECTLY
write(6,*)"Data output at time = ",time/60.0," min"
----------------------------------end program wave2d
