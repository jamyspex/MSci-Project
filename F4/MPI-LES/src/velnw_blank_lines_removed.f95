module module_velnw
    implicit none
 contains
      subroutine velnw(p,ro,dxs,u,dt,f,dys,v,g,dzs,w,h)
    use params_common_sn
    implicit none
        real(kind=4), intent(In) :: dt
        real(kind=4), dimension(0:ip) , intent(In) :: dxs
        real(kind=4), dimension(0:jp) , intent(In) :: dys
        real(kind=4), dimension(-1:kp+2) , intent(In) :: dzs
        real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(In) :: f
        real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(In) :: g
        real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(In) :: h
        real(kind=4), dimension(0:1,0:ip+2,0:jp+2,0:kp+1) , intent(In) :: p
        real(kind=4), intent(In) :: ro
        real(kind=4), dimension(0:ip+1,-1:jp+1,0:kp+1) , intent(InOut) :: u
        real(kind=4), dimension(0:ip+1,-1:jp+1,0:kp+1) , intent(InOut) :: v
        real(kind=4), dimension(0:ip+1,-1:jp+1,-1:kp+1) , intent(InOut) :: w
        integer :: i,j,k
        real(kind=4) :: pz
      do k = 1,kp
      do j = 1,jp
      do i = 1,ip
        pz = (-p(0,i,j,k)+p(0,i+1,j,k))/ro/dxs(i)
        u(i,j,k) = u(i,j,k)+dt*(f(i,j,k)-pz)
      end do
      end do
      end do
      do k = 1,kp
      do j = 1,jp
      do i = 1,ip
        pz = (-p(0,i,j,k)+p(0,i,j+1,k))/ro/dys(j)
        v(i,j,k) = v(i,j,k)+dt*(g(i,j,k)-pz)
      end do
      end do
      end do
      do k = 1,kp-1
      do j = 1,jp
      do i = 1,ip
        pz = (-p(0,i,j,k)+p(0,i,j,k+1))/ro/dzs(k)
        w(i,j,k) = w(i,j,k)+dt*(h(i,j,k)-pz)
      end do
      end do
      end do
      return
      end subroutine velnw
end module module_velnw
