module module_velnw
 contains
      subroutine velnw(km,jm,im,p,ro,dxs,u,dt,f,dys,v,g,dzs,w,h)
        real(kind=4), intent(In) :: dt
        real(kind=4), dimension(0:ip) , intent(In) :: dxs
        real(kind=4), dimension(0:jp) , intent(In) :: dys
        real(kind=4), dimension(-1:kp+2) , intent(In) :: dzs
        real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(In) :: f
        real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(In) :: g
        real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(In) :: h
        integer, intent(In) :: im
        integer, intent(In) :: jm
        integer, intent(In) :: km
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+1) , intent(In) :: p
        real(kind=4), intent(In) :: ro
        real(kind=4), dimension(0:ip+1,-1:jp+1,0:kp+1) , intent(InOut) :: u
        real(kind=4), dimension(0:ip+1,-1:jp+1,0:kp+1) , intent(InOut) :: v
        real(kind=4), dimension(0:ip+1,-1:jp+1,-1:kp+1) , intent(InOut) :: w
      do k = 1,km
      do j = 1,jm
      do i = 1,im
        pz = (-p(i,j,k)+p(i+1,j,k))/ro/dxs(i)
        u(i,j,k) = u(i,j,k)+dt*(f(i,j,k)-pz)
      end do
      end do
      end do
      do k = 1,km
      do j = 1,jm
      do i = 1,im
        pz = (-p(i,j,k)+p(i,j+1,k))/ro/dys(j)
        v(i,j,k) = v(i,j,k)+dt*(g(i,j,k)-pz)
      end do
      end do
      end do
      do k = 1,km-1
      do j = 1,jm
      do i = 1,im
        pz = (-p(i,j,k)+p(i,j,k+1))/ro/dzs(k)
        w(i,j,k) = w(i,j,k)+dt*(h(i,j,k)-pz)
      end do
      end do
      end do
      return
      end subroutine velnw
end module module_velnw
