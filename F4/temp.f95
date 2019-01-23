real(4), dimension(0:1, 0:302, 0:302, 0:81) :: p
real(4), dimension(0:301, -1:301, 0:81), intent(InOut) :: u
real(4), dimension(0:300, 0:300, 0:80), intent(InOut) :: f
! OpenCLStencil (
!        2 point stencil on 4D array p: [[C=0,1,0,0],[C=0,0,0,0]]
!    ){
! OpenCLMap ( ["ro","dxs","dt","k","j","u"],["u"],["(k,1,80,1)","(j,1,300,1)","(i,1,300,1)"],[]) {
pz = (-p(0, i, j, k) + p(0, i + 1, j, k))/ro/dxs(i)
u(i, j, k) = u(i, j, k) + dt*(f(i, j, k) - pz)
!}
!}



real, dimension(0:501,0:501) :: du
real, dimension(0:501,0:501) :: dv
real, dimension(0:501,0:501) :: eta
! OpenCLStencil (
!	3 point stencil on 2D array eta: [[1,0],[0,0],[0,1]]
!    ){
! OpenCLMap ( ["dt","g","dx","dy"],["du","dv"],["(j,1,500,1)","(k,1,500,1)"],[]) {
du(j,k) = -dt*g*(eta(j,k+1)-eta(j,k))/dx
dv(j,k) = -dt*g*(eta(j+1,k)-eta(j,k))/dy
!}    
!}