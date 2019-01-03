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

integer, dimension(1) :: state_ptr

! Size declarations
integer, dimension(1) :: state_ptr_sz


! Size assignments
state_ptr_sz = shape(state_ptr)

! Buffer loads
call oclLoadBuffer(STATE_PTR_BUF_IDX, state_ptr_buf)

! Original code with buffer writes and reads
! ---- BEGIN shapiro_map_15 ---------------------------------------------------------------------------------------------------
oclGlobalRange = (500 * 500)
oclLocalRange = 0
state_ptr(1) = ST_SHAPIRO_MAP_15

call oclWrite1DIntArrayBuffer(state_ptr_buf,state_ptr_sz,state_ptr)
call runOcl(oclGlobalRange,oclLocalRange,exectime)
! call shapiro_map_15

! ---- END --------------------------------------------------------------------------------------------------------------------
end subroutine shapiro
! Footer (produceCode_progUnit c)
end module module_shapiro
