module module_vernieuw
 contains
      subroutine vernieuw(dt,dx,dy,eps,eta,etan,g,h,hmin,hzero,j,k,u,un,v,vn,wet)

use module_shapiro_dyn_vernieuw_superkernel_init
use oclWrapper
!



! otherStatements

! remainingDecls
      integer(4), parameter :: nx = 500  !!
      integer(4), parameter :: ny = 500  !!
      real :: dt !!
      real :: dx !!
      real :: dy !!
      real :: eps !!
      real, dimension(0:ny+1,0:nx+1), intent(In) :: eta !!
      real, dimension(0:ny+1,0:nx+1) :: etan !!
      real :: g !!
      real, dimension(0:ny+1,0:nx+1), intent(InOut) :: h !!
      real, intent(In) :: hmin !!
      real, dimension(0:ny+1,0:nx+1), intent(In) :: hzero !!
      real, dimension(0:ny+1,0:nx+1), intent(Out) :: u !!
      real, dimension(0:ny+1,0:nx+1), intent(In) :: un !!
      real, dimension(0:ny+1,0:nx+1), intent(Out) :: v !!
      real, dimension(0:ny+1,0:nx+1), intent(In) :: vn !!
      integer, dimension(0:ny+1,0:nx+1), intent(Out) :: wet !!
      integer, intent(InOut) :: k !!
      integer, intent(InOut) :: j !!

! Extra declarations
real (kind=4) :: exectime

! Buffer declarations
integer(8) :: state_ptr_buf
integer(8) :: hzero_buf
integer(8) :: eta_buf
integer(8) :: h_buf
integer(8) :: hmin_buf
integer(8) :: un_buf
integer(8) :: vn_buf
integer(8) :: wet_buf
integer(8) :: u_buf
integer(8) :: v_buf

integer, dimension(1) :: state_ptr

! Size declarations
integer, dimension(1) :: state_ptr_sz
integer, dimension(2) :: hzero_sz
integer, dimension(2) :: eta_sz
integer, dimension(2) :: h_sz
integer, dimension(1) :: hmin_ptr_sz
integer, dimension(2) :: un_sz
integer, dimension(2) :: vn_sz
integer, dimension(2) :: wet_sz
integer, dimension(2) :: u_sz
integer, dimension(2) :: v_sz
real, dimension(1) :: hmin_ptr


! Size assignments
state_ptr_sz = shape(state_ptr)
hzero_sz = shape(hzero)
eta_sz = shape(eta)
h_sz = shape(h)
hmin_ptr_sz = shape(hmin_ptr)
un_sz = shape(un)
vn_sz = shape(vn)
wet_sz = shape(wet)
u_sz = shape(u)
v_sz = shape(v)

! Buffer loads
call oclLoadBuffer(STATE_PTR_BUF_IDX, state_ptr_buf)
call oclLoadBuffer(HZERO_BUF_IDX, hzero_buf)
call oclLoadBuffer(ETA_BUF_IDX, eta_buf)
call oclLoadBuffer(H_BUF_IDX, h_buf)
call oclLoadBuffer(HMIN_BUF_IDX, hmin_buf)
call oclLoadBuffer(UN_BUF_IDX, un_buf)
call oclLoadBuffer(VN_BUF_IDX, vn_buf)
call oclLoadBuffer(WET_BUF_IDX, wet_buf)
call oclLoadBuffer(U_BUF_IDX, u_buf)
call oclLoadBuffer(V_BUF_IDX, v_buf)

! Original code with buffer writes and reads
! ---- BEGIN vernieuw_map_23 --------------------------------------------------------------------------------------------------
oclGlobalRange = ((((500 + 1) - 0) + 1) * (((500 + 1) - 0) + 1))
oclLocalRange = 0
state_ptr(1) = ST_VERNIEUW_MAP_23

call oclWrite2DFloatArrayBuffer(hzero_buf,hzero_sz,hzero)
call oclWrite2DFloatArrayBuffer(eta_buf,eta_sz,eta)
hmin_ptr(1) = hmin
call oclWrite1DFloatArrayBuffer(hmin_buf,hmin_ptr_sz,hmin_ptr)! Automatic conversion to array
call oclWrite2DFloatArrayBuffer(un_buf,un_sz,un)
call oclWrite2DFloatArrayBuffer(vn_buf,vn_sz,vn)
j_ptr(1) = j
call oclWrite1DIntArrayBuffer(j_buf,j_ptr_sz,j_ptr)! Automatic conversion to array
call oclWrite2DFloatArrayBuffer(h_buf,h_sz,h)
call oclWrite1DIntArrayBuffer(state_ptr_buf,state_ptr_sz,state_ptr)
call runOcl(oclGlobalRange,oclLocalRange,exectime)
! call vernieuw_map_23

call oclRead2DIntArrayBuffer(wet_buf,wet_sz,wet)
call oclRead2DFloatArrayBuffer(u_buf,u_sz,u)
call oclRead2DFloatArrayBuffer(v_buf,v_sz,v)
call oclRead2DFloatArrayBuffer(h_buf,h_sz,h)
! ---- END --------------------------------------------------------------------------------------------------------------------
      end subroutine vernieuw
! Footer (produceCode_progUnit c)
end module module_vernieuw
