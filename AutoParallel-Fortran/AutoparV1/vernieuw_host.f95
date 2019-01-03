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

integer, dimension(1) :: state_ptr

! Size declarations
integer, dimension(1) :: state_ptr_sz


! Size assignments
state_ptr_sz = shape(state_ptr)

! Buffer loads
call oclLoadBuffer(STATE_PTR_BUF_IDX, state_ptr_buf)

! Original code with buffer writes and reads
! ---- BEGIN vernieuw_map_23 --------------------------------------------------------------------------------------------------
oclGlobalRange = ((((500 + 1) - 0) + 1) * (((500 + 1) - 0) + 1))
oclLocalRange = 0
state_ptr(1) = ST_VERNIEUW_MAP_23

call oclWrite1DIntArrayBuffer(state_ptr_buf,state_ptr_sz,state_ptr)
call runOcl(oclGlobalRange,oclLocalRange,exectime)
! call vernieuw_map_23

! ---- END --------------------------------------------------------------------------------------------------------------------
      end subroutine vernieuw
! Footer (produceCode_progUnit c)
end module module_vernieuw
