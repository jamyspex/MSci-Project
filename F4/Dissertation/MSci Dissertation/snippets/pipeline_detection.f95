subroutine dyn_0(dt,dx,dy,g,j,k)
      real :: dt
      real :: du_j_k
      real :: dv_j_k
      real :: dx
      real :: dy
      real :: eta_j_kp1
      real :: eta_jp1_k
      real :: eta_j_k
      real :: g
      integer :: j
      integer :: k
! OpenCLStencil (
!	3 point stencil on 2D array eta: 
!      [[1,0],[0,0],[0,1]]
!    ){
! OpenCLMap (j,1,500,1) (k,1,500,1) {
    j = count/502
    k = mod(count, 502)
    if ((j>=1) .and. (j<=500) .and. (k>=1) .and. 
       (k<=500)) then
        du_j_k = -dt*g*(eta_j_kp1-eta_j_k)/dx
        dv_j_k = -dt*g*(eta_jp1_k-eta_j_k)/dy
    end if
!}
!}
end subroutine dyn_0
