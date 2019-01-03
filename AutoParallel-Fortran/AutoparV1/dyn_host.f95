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

integer, dimension(1) :: state_ptr

! Size declarations
integer, dimension(1) :: state_ptr_sz


! Size assignments
state_ptr_sz = shape(state_ptr)

! Buffer loads
call oclLoadBuffer(STATE_PTR_BUF_IDX, state_ptr_buf)

! Original code with buffer writes and reads
! ---- BEGIN dyn_map_38 -------------------------------------------------------------------------------------------------------
oclGlobalRange = (500 * 500)
oclLocalRange = 0
state_ptr(1) = ST_DYN_MAP_38

call oclWrite1DIntArrayBuffer(state_ptr_buf,state_ptr_sz,state_ptr)
call runOcl(oclGlobalRange,oclLocalRange,exectime)
! call dyn_map_38

! ---- END --------------------------------------------------------------------------------------------------------------------
end subroutine dyn
! Footer (produceCode_progUnit c)
end module module_dyn
