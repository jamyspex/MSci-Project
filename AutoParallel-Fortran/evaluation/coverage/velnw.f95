module module_velnw

contains

      subroutine velnw(km,jm,im,dxs,ro,p,u,dt,f,dys,v,g,dzs,w,h)
      use params_common_sn!, only : ip, jp, kp !, only : ip, jp, kp
      integer, intent(In) :: km
      integer, intent(In) :: jm
      integer, intent(In) :: im
      real, dimension(0:ip), intent(In) :: dxs
      real, intent(In) :: ro
      real, dimension(0:ip+2,0:jp+2,0:kp+1), intent(In) :: p
      real, dimension(0:ip+1,-1:jp+1,0:kp+1), intent(InOut) :: u
      real, intent(In) :: dt
      real, dimension(0:ip,0:jp,0:kp), intent(In) :: f
      real, dimension(0:jp), intent(In) :: dys
      real, dimension(0:ip+1,-1:jp+1,0:kp+1), intent(InOut) :: v
      real, dimension(0:ip,0:jp,0:kp), intent(In) :: g
      real, dimension(-1:kp+2), intent(In) :: dzs
      real, dimension(0:ip+1,-1:jp+1,-1:kp+1), intent(InOut) :: w
      real, dimension(0:ip,0:jp,0:kp), intent(In) :: h
      integer :: k
      integer :: j
      integer :: i
      real :: pz
! 
! 
! --u velocity
      do k=1,km
      do j=1,jm
      do i=1,im
        pz   = (-p(i,j,k)+p(i+1,j,k))/ro/dxs(i)
        u(i,j,k)=u(i,j,k)+dt*(f(i,j,k)-pz)
        u(i,j,k)=u(i,j,k)
   end do
      end do
      end do
! --v velocity
      do k=1,km
      do j=1,jm
      do i=1,im
        pz   = (-p(i,j,k)+p(i,j+1,k))/ro/dys(j)
!         if (k.eq.km/2 .and. j.eq.jm/2 .and. i.eq.im/2) then
!             print *,'timestep', p(i,j,k),p(i,j+1,k),v(i,j,k),v(i,j,k)
!      &       +dt*(g(i,j,k)-pz)
!         end if
        v(i,j,k)=v(i,j,k)+dt*(g(i,j,k)-pz)
        v(i,j,k)=v(i,j,k)
   end do
      end do
      end do
! --w velocity
      do k=1,km-1
      do j=1,jm
      do i=1,im
        pz   = (-p(i,j,k)+p(i,j,k+1))/ro/dzs(k)
        w(i,j,k)=w(i,j,k)+dt*(h(i,j,k)-pz)
        w(i,j,k)=w(i,j,k)
   end do
      end do
      end do
! 
      return
      end subroutine velnw

end module module_velnw

