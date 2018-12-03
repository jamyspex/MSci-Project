module module_dyn
 contains
subroutine dyn(j,k,dx,g,eta,dt,dy,un,u,wet,v,vn,h,etan)

use module_shapiro_dyn_vernieuw_superkernel_init
use oclWrapper
!



! otherStatements

! remainingDecls
      integer(4), parameter :: ny = 500  !!
      integer(4), parameter :: nx = 500  !!
      integer :: j !!
      integer :: k !!
      real, intent(In) :: dx !!
      real, intent(In) :: g !!
      real, dimension(0:ny+1,0:nx+1) :: eta !!
      real, intent(In) :: dt !!
      real, intent(In) :: dy !!
      real, dimension(0:ny+1,0:nx+1), intent(InOut) :: un !!
      real, dimension(0:ny+1,0:nx+1), intent(In) :: u !!
      integer, dimension(0:ny+1,0:nx+1), intent(In) :: wet !!
      real, dimension(0:ny+1,0:nx+1), intent(In) :: v !!
      real, dimension(0:ny+1,0:nx+1), intent(InOut) :: vn !!
      real, dimension(0:ny+1,0:nx+1), intent(In) :: h !!
      real, dimension(0:ny+1,0:nx+1) :: etan !!
      real, dimension(0:ny+1,0:nx+1) :: du !!
      real, dimension(0:ny+1,0:nx+1) :: dv !!
      real :: uu !!
      real :: vv !!
      real :: duu !!
      real :: dvv !!
      real :: hue !!
      real :: huw !!
      real :: hwp !!
      real :: hwn !!
      real :: hen !!
      real :: hep !!
      real :: hvn !!
      real :: hvs !!
      real :: hsp !!
      real :: hsn !!
      real :: hnn !!
      real :: hnp !!

! Extra declarations
real (kind=4) :: exectime

! Buffer declarations
integer(8) :: state_ptr_buf
integer(8) :: dt_buf
integer(8) :: g_buf
integer(8) :: dx_buf
integer(8) :: dy_buf
integer(8) :: u_buf
integer(8) :: du_buf
integer(8) :: wet_buf
integer(8) :: v_buf
integer(8) :: dv_buf
integer(8) :: h_buf
integer(8) :: eta_buf
integer(8) :: un_buf
integer(8) :: vn_buf
integer(8) :: etan_buf

integer, dimension(1) :: state_ptr

! Size declarations
integer, dimension(1) :: state_ptr_sz
integer, dimension(1) :: dt_ptr_sz
integer, dimension(1) :: g_ptr_sz
integer, dimension(1) :: dx_ptr_sz
integer, dimension(1) :: dy_ptr_sz
integer, dimension(2) :: u_sz
integer, dimension(2) :: du_sz
integer, dimension(2) :: wet_sz
integer, dimension(2) :: v_sz
integer, dimension(2) :: dv_sz
integer, dimension(2) :: h_sz
integer, dimension(2) :: eta_sz
integer, dimension(2) :: un_sz
integer, dimension(2) :: vn_sz
integer, dimension(2) :: etan_sz
real, dimension(1) :: dt_ptr
real, dimension(1) :: g_ptr
real, dimension(1) :: dx_ptr
real, dimension(1) :: dy_ptr


! Size assignments
state_ptr_sz = shape(state_ptr)
dt_ptr_sz = shape(dt_ptr)
g_ptr_sz = shape(g_ptr)
dx_ptr_sz = shape(dx_ptr)
dy_ptr_sz = shape(dy_ptr)
u_sz = shape(u)
du_sz = shape(du)
wet_sz = shape(wet)
v_sz = shape(v)
dv_sz = shape(dv)
h_sz = shape(h)
eta_sz = shape(eta)
un_sz = shape(un)
vn_sz = shape(vn)
etan_sz = shape(etan)

! Buffer loads
call oclLoadBuffer(STATE_PTR_BUF_IDX, state_ptr_buf)
call oclLoadBuffer(DT_BUF_IDX, dt_buf)
call oclLoadBuffer(G_BUF_IDX, g_buf)
call oclLoadBuffer(DX_BUF_IDX, dx_buf)
call oclLoadBuffer(DY_BUF_IDX, dy_buf)
call oclLoadBuffer(U_BUF_IDX, u_buf)
call oclLoadBuffer(DU_BUF_IDX, du_buf)
call oclLoadBuffer(WET_BUF_IDX, wet_buf)
call oclLoadBuffer(V_BUF_IDX, v_buf)
call oclLoadBuffer(DV_BUF_IDX, dv_buf)
call oclLoadBuffer(H_BUF_IDX, h_buf)
call oclLoadBuffer(ETA_BUF_IDX, eta_buf)
call oclLoadBuffer(UN_BUF_IDX, un_buf)
call oclLoadBuffer(VN_BUF_IDX, vn_buf)
call oclLoadBuffer(ETAN_BUF_IDX, etan_buf)

! Original code with buffer writes and reads
! ---- BEGIN dyn_map_38 -------------------------------------------------------------------------------------------------------
oclGlobalRange = (500 * 500)
oclLocalRange = 0
state_ptr(1) = ST_DYN_MAP_38

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
j_ptr(1) = j
call oclWrite1DIntArrayBuffer(j_buf,j_ptr_sz,j_ptr)! Automatic conversion to array
call oclWrite2DFloatArrayBuffer(h_buf,h_sz,h)
call oclWrite2DFloatArrayBuffer(eta_buf,eta_sz,eta)
call oclWrite2DFloatArrayBuffer(du_buf,du_sz,du)
call oclWrite2DFloatArrayBuffer(dv_buf,dv_sz,dv)
call oclWrite1DIntArrayBuffer(state_ptr_buf,state_ptr_sz,state_ptr)
call runOcl(oclGlobalRange,oclLocalRange,exectime)
! call dyn_map_38

call oclRead2DFloatArrayBuffer(un_buf,un_sz,un)
call oclRead2DFloatArrayBuffer(vn_buf,vn_sz,vn)
call oclRead2DFloatArrayBuffer(etan_buf,etan_sz,etan)
call oclRead2DFloatArrayBuffer(du_buf,du_sz,du)
call oclRead2DFloatArrayBuffer(dv_buf,dv_sz,dv)
! ---- END --------------------------------------------------------------------------------------------------------------------
end subroutine dyn
! Footer (produceCode_progUnit c)
end module module_dyn
