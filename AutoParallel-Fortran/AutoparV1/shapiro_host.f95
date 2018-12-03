module module_shapiro
 contains
subroutine shapiro(j,k,wet,etan,eps,eta)

use module_shapiro_dyn_vernieuw_superkernel_init
use oclWrapper
!



! otherStatements

! remainingDecls
      integer(4), parameter :: nx = 500  !!
      integer(4), parameter :: ny = 500  !!
      integer, intent(InOut) :: j !!
      integer, intent(InOut) :: k !!
      integer, dimension(0:ny+1,0:nx+1), intent(In) :: wet !!
      real, dimension(0:ny+1,0:nx+1), intent(In) :: etan !!
      real, intent(In) :: eps !!
      real, dimension(0:ny+1,0:nx+1), intent(Out) :: eta !!
      real :: term1 !!
      real :: term2 !!
      real :: term3 !!

! Extra declarations
real (kind=4) :: exectime

! Buffer declarations
integer(8) :: state_ptr_buf
integer(8) :: wet_buf
integer(8) :: etan_buf
integer(8) :: eps_buf
integer(8) :: eta_buf

integer, dimension(1) :: state_ptr

! Size declarations
integer, dimension(1) :: state_ptr_sz
integer, dimension(2) :: wet_sz
integer, dimension(2) :: etan_sz
integer, dimension(1) :: eps_ptr_sz
integer, dimension(2) :: eta_sz
real, dimension(1) :: eps_ptr


! Size assignments
state_ptr_sz = shape(state_ptr)
wet_sz = shape(wet)
etan_sz = shape(etan)
eps_ptr_sz = shape(eps_ptr)
eta_sz = shape(eta)

! Buffer loads
call oclLoadBuffer(STATE_PTR_BUF_IDX, state_ptr_buf)
call oclLoadBuffer(WET_BUF_IDX, wet_buf)
call oclLoadBuffer(ETAN_BUF_IDX, etan_buf)
call oclLoadBuffer(EPS_BUF_IDX, eps_buf)
call oclLoadBuffer(ETA_BUF_IDX, eta_buf)

! Original code with buffer writes and reads
! ---- BEGIN shapiro_map_15 ---------------------------------------------------------------------------------------------------
oclGlobalRange = (500 * 500)
oclLocalRange = 0
state_ptr(1) = ST_SHAPIRO_MAP_15

call oclWrite2DIntArrayBuffer(wet_buf,wet_sz,wet)
call oclWrite2DFloatArrayBuffer(etan_buf,etan_sz,etan)
eps_ptr(1) = eps
call oclWrite1DFloatArrayBuffer(eps_buf,eps_ptr_sz,eps_ptr)! Automatic conversion to array
j_ptr(1) = j
call oclWrite1DIntArrayBuffer(j_buf,j_ptr_sz,j_ptr)! Automatic conversion to array
call oclWrite1DIntArrayBuffer(state_ptr_buf,state_ptr_sz,state_ptr)
call runOcl(oclGlobalRange,oclLocalRange,exectime)
! call shapiro_map_15

call oclRead2DFloatArrayBuffer(eta_buf,eta_sz,eta)
! ---- END --------------------------------------------------------------------------------------------------------------------
end subroutine shapiro
! Footer (produceCode_progUnit c)
end module module_shapiro
