--------------------------------------------------------------------------------
vernieuw
--------------------------------------------------------------------------------
subroutine vernieuw(dt,dx,dy,eps,eta,etan,g,h,hmin,hzero,j,k,u,un,v,vn,wet)
      integer(4), parameter :: nx = 500 
      integer(4), parameter :: ny = 500 
      real :: dt
      real :: dx
      real :: dy
      real :: eps
      real, dimension(0:ny+1,0:nx+1), intent(In) :: eta
      real, dimension(0:ny+1,0:nx+1) :: etan
      real :: g
      real, dimension(0:ny+1,0:nx+1), intent(InOut) :: h
      real, intent(In) :: hmin
      real, dimension(0:ny+1,0:nx+1), intent(In) :: hzero
      real, dimension(0:ny+1,0:nx+1), intent(Out) :: u
      real, dimension(0:ny+1,0:nx+1), intent(In) :: un
      real, dimension(0:ny+1,0:nx+1), intent(Out) :: v
      real, dimension(0:ny+1,0:nx+1), intent(In) :: vn
      integer, dimension(0:ny+1,0:nx+1), intent(Out) :: wet
      integer, intent(InOut) :: k
      integer, intent(InOut) :: j
! OpenCLMap ( ["hzero","eta","h","hmin","un","vn"],["h","wet","u","v"],["(j,0,500.0+1,1)","(k,0,500.0+1,1)"],[]) {
! OpenCLMap ( ["j","hzero","eta","h","hmin","un","vn"],["h","wet","u","v"],["(k,0,500.0+1,1)"],[]) {
    h(j,k) = hzero(j,k)+eta(j,k)
    wet(j,k) = 1
    if (h(j,k)<hmin) then
        wet(j,k) = 0
    end if
    u(j,k) = un(j,k)
    v(j,k) = vn(j,k)
    }
    }
end subroutine vernieuw

--------------------------------------------------------------------------------