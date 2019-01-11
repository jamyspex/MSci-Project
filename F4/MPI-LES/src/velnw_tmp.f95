module module_velnw
    implicit none
 contains 
      subroutine velnw(p,ro,dxs,u,dt,f,dys,v,g,dzs,w,h)
#ifdef WV_NEW
    use params_common_sn
    implicit none
#else
    use common_sn ! create_new_include_statements() line 102
#endif
        real(kind=4), intent(In) :: dt
        real(kind=4), dimension(0:ip) , intent(In) :: dxs
        real(kind=4), dimension(0:jp) , intent(In) :: dys
        real(kind=4), dimension(-1:kp+2) , intent(In) :: dzs
        real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(In) :: f
        real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(In) :: g
        real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(In) :: h
#ifndef TWINNED_BUFFER
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+1) , intent(In) :: p
#else
        real(kind=4), dimension(0:1,0:ip+2,0:jp+2,0:kp+1) , intent(In) :: p
#endif
        real(kind=4), intent(In) :: ro
        real(kind=4), dimension(0:ip+1,-1:jp+1,0:kp+1) , intent(InOut) :: u
        real(kind=4), dimension(0:ip+1,-1:jp+1,0:kp+1) , intent(InOut) :: v
        real(kind=4), dimension(0:ip+1,-1:jp+1,-1:kp+1) , intent(InOut) :: w
        integer :: i,j,k
        real(kind=4) :: pz
! setting pz to 0 does not make the error go away
! commenting out the redundant lines also not
! Which means it's the values of f,g,h that are changing. g seems fine.
! --u velocity
      do k = 1,kp
      do j = 1,jp
      do i = 1,ip
#ifndef TWINNED_BUFFER
        pz = (-p(i,j,k)+p(i+1,j,k))/ro/dxs(i)
#else
        pz = (-p(0,i,j,k)+p(0,i+1,j,k))/ro/dxs(i)
#endif
        u(i,j,k) = u(i,j,k)+dt*(f(i,j,k)-pz)
!        u(i,j,k) = u(i,j,k)
      end do
      end do
      end do
! --v velocity (WV: OK)
      do k = 1,kp
      do j = 1,jp
      do i = 1,ip
#ifndef TWINNED_BUFFER
        pz = (-p(i,j,k)+p(i,j+1,k))/ro/dys(j)
#else
        pz = (-p(0,i,j,k)+p(0,i,j+1,k))/ro/dys(j)
#endif
!        if (k==kp/2  .and.  j==jp/2  .and.  i==ip/2) then
!            print *,'timestep', p(i,j,k),p(i,j+1,k),v(i,j,k),v(i,j,k)+dt*(g(i,j,k)-pz)
!        end if
        v(i,j,k) = v(i,j,k)+dt*(g(i,j,k)-pz)
      end do
      end do
      end do
! --w velocity (WV: NOK)
      do k = 1,kp-1
      do j = 1,jp
      do i = 1,ip
#ifndef TWINNED_BUFFER
        pz = (-p(i,j,k)+p(i,j,k+1))/ro/dzs(k)
#else
        pz = (-p(0,i,j,k)+p(0,i,j,k+1))/ro/dzs(k)
#endif
        w(i,j,k) = w(i,j,k)+dt*(h(i,j,k)-pz)
!        w(i,j,k) = w(i,j,k)
      end do
      end do
      end do
#ifdef WV_DEBUG
    print *,'F95 PSUM after velnw:',sum(p)/(ip*jp*kp)
    print *,'F95 FGHSUM after velnw:',sum(f)+sum(g)+sum(h)
    print *,'F95 UVWSUM after velnw:',sum(u)+sum(v)+sum(w)
    print *,'F95 USUM after velnw:',sum(u)
    print *,'F95 VSUM after velnw:',sum(v)
    print *,'F95 WSUM after velnw:',sum(w)
#endif
!
      return
      end subroutine velnw
end module module_velnw
