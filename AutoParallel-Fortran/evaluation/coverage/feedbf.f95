module module_feedbf

contains

      subroutine feedbf(km,jm,im,usum,bmask1,u,vsum,cmask1,v,wsum,dmask1,w,dt,alpha,beta,fx,fy,fz,f,g,h)
      use params_common_sn!, only : kp, jp, ip !, only : kp, jp, ip
      integer, intent(In) :: km
      integer, intent(In) :: jm
      integer, intent(In) :: im
      real, dimension(0:ip,0:jp,0:kp), intent(InOut) :: usum
      real, dimension(-1:ip+1,0:jp+1,0:kp+1), intent(In) :: bmask1
      real, dimension(0:ip+1,-1:jp+1,0:kp+1), intent(In) :: u
      real, dimension(0:ip,0:jp,0:kp), intent(InOut) :: vsum
      real, dimension(0:ip+1,-1:jp+1,0:kp+1), intent(In) :: cmask1
      real, dimension(0:ip+1,-1:jp+1,0:kp+1), intent(In) :: v
      real, dimension(0:ip,0:jp,0:kp), intent(InOut) :: wsum
      real, dimension(0:ip+1,0:jp+1,0:kp+1), intent(In) :: dmask1
      real, dimension(0:ip+1,-1:jp+1,-1:kp+1), intent(In) :: w
      real, intent(In) :: dt
      real, intent(In) :: alpha
      real, intent(In) :: beta
      real, dimension(0:ip,0:jp,0:kp), intent(InOut) :: fx
      real, dimension(0:ip,0:jp,0:kp), intent(InOut) :: fy
      real, dimension(0:ip,0:jp,0:kp), intent(InOut) :: fz
      real, dimension(0:ip,0:jp,0:kp), intent(InOut) :: f
      real, dimension(0:ip,0:jp,0:kp), intent(InOut) :: g
      real, dimension(0:ip,0:jp,0:kp), intent(InOut) :: h
      integer :: k
      integer :: j
      integer :: i
      real :: f1x
      real :: f1y
      real :: f1z
      real :: f2x
      real :: f2y
      real :: f2z
! 
! 
      do k=1,km
      do j=1,jm
      do i=1,im
        usum(i,j,k)=(usum(i,j,k)+u(i,j,k))*bmask1(i,j,k)
        vsum(i,j,k)=(vsum(i,j,k)+v(i,j,k))*cmask1(i,j,k)
        wsum(i,j,k)=(wsum(i,j,k)+w(i,j,k))*dmask1(i,j,k)
        f1x=alpha*usum(i,j,k)*dt
        f1y=alpha*vsum(i,j,k)*dt
        f1z=alpha*wsum(i,j,k)*dt
        f2x=beta*u(i,j,k)*bmask1(i,j,k)
        f2y=beta*v(i,j,k)*cmask1(i,j,k)
        f2z=beta*w(i,j,k)*dmask1(i,j,k)
        fx(i,j,k)=f1x+f2x
        fy(i,j,k)=f1y+f2y
        fz(i,j,k)=f1z+f2z
       end do
       end do
       end do
       do k=1,km
       do j=1,jm
       do i=1,im
         f(i,j,k)=f(i,j,k)+fx(i,j,k)
         g(i,j,k)=g(i,j,k)+fy(i,j,k)
         h(i,j,k)=h(i,j,k)+fz(i,j,k)
       end do
       end do
       end do
! 
      return
      end subroutine feedbf

end module module_feedbf

