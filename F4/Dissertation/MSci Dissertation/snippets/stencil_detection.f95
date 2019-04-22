! OpenCLStencil (
!    3 point stencil on 2D array eta: 
!      [[1,0],[0,0],[0,1]]
!    ){
! OpenCLMap (j,1,500,1) (k,1,500,1) {
! OpenCLMap (k,1,500,1) {
    du(j,k) = -dt*g*(eta(j,k+1)-eta(j,k))/dx
    dv(j,k) = -dt*g*(eta(j+1,k)-eta(j,k))/dy
!}
!}    
!}