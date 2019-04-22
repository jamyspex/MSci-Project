subroutine dyn_0(dt,dx,dy,eta,g,j,k)
   real :: dt
   real, dimension(0:501,0:501) :: du
   real, dimension(0:501,0:501) :: dv
   real :: dx
   real :: dy
   real, dimension(0:501,0:501) :: eta
   real :: g
   integer :: j
   integer :: k
! OpenCLStencil (
!    3 point stencil on 2D array eta: 
!       [[1,0],[0,0],[0,1]]
!    ){
! OpenCLMap (j,1,500,1) (k,1,500,1) {
    j = driver_loop_idx/502
    k = mod(driver_loop_idx, 502)
    if ((j>=1) .and. (j<=500) .and. (k>=1) .and. 
        (k<=500)) then
       du(j,k) = -dt*g*(eta(j,k+1)-eta(j,k))/dx
       dv(j,k) = -dt*g*(eta(j+1,k)-eta(j,k))/dy
    end if
!}    
!}
end subroutine dyn_0