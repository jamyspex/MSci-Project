module module_bondv1

contains

      subroutine bondv1(jm,u,z2,dzn,v,w,km,ical,n,im,dt,dxs)
      use params_common_sn!, only : jp, kp, ip !, only : jp, kp, ip
      integer, intent(In) :: jm
      real, dimension(0:ip+1,-1:jp+1,0:kp+1), intent(InOut) :: u
      real, dimension(1:kp+2), intent(In) :: z2
      real, dimension(-1:kp+2), intent(In) :: dzn
      real, dimension(0:ip+1,-1:jp+1,0:kp+1), intent(InOut) :: v
      real, dimension(0:ip+1,-1:jp+1,-1:kp+1), intent(InOut) :: w
      integer, intent(In) :: km
      integer, intent(In) :: ical
      integer, intent(In) :: n
      integer, intent(In) :: im
      real, intent(In) :: dt
      real, dimension(0:ip), intent(In) :: dxs
      integer :: i
      integer :: k,k2
      integer :: j
      real :: aaa
      real :: bbb
      real :: uout
! 
! 
! -------------------inflow-------------------
! 
!      Setup for initial wind profile
! 
      do i=0,im
      do k=1,km
      do j=1,jm
      if (k<79) then
          k2=k
      else
          k2=77
      end if
      if (i<2) then
       u(i,j,k)=5.*((z2(k2)+0.5*dzn(k2))/600.)**0.2
       v(i,j,k)=0.0
       w(i,j,k)=0.0
       else
      if(ical == 0.and.n == 1) then
       u(i,j,k)=5.*((z2(k2)+0.5*dzn(k2))/600.)**0.2
       v(i,j,k)=0.0
       w(i,j,k)=0.0
       end if
       end if
      end do
      end do
      end do
! ------------- outflow condition ------------
!      advective condition
! 
      aaa=0.0
      bbb=0.0
      do k=1,km
      do j=1,jm
        aaa=amax1(aaa,u(im,j,k))
        bbb=amin1(bbb,u(im,j,k))
      end do
      end do
      uout=(aaa+bbb)/2.
! 
      do k=1,km
      do j=1,jm
       u(im,j,k)=u(im,j,k)-dt*uout*(u(im,j,k)-u(im-1,j,k))/dxs(im)
       v(im+1,j,k)=v(im+1,j,k)-dt*uout*(v(im+1,j,k)-v(im,j,k))/dxs(im)
       w(im+1,j,k)=w(im+1,j,k)-dt*uout*(w(im+1,j,k)-w(im,j,k))/dxs(im)
      end do
      end do
! --side flow condition; periodic
      do k=0,km+1
      do i=0,im+1
        u(i,   0,k) = u(i,jm  ,k)
        u(i,jm+1,k) = u(i,   1,k)
        v(i,   0,k) = v(i,jm  ,k)
        v(i,jm+1,k) = v(i,   1,k)
        if (k<km+1) then
        w(i,   0,k) = w(i,jm  ,k)
        w(i,jm+1,k) = w(i,   1,k)
        end if    
      end do
      end do 
! -------top and underground condition
      do j=-1,jm+2
      do i=-1,im+1
      if (i>-1 .and. j>-1 .and. j<jm+1) then
        u(i,j,   0) = -u(i,j, 1) 
        u(i,j,km+1) = u(i,j,km)
        v(i,j,   0) = -v(i,j, 1)
        v(i,j,km+1) = v(i,j,km)
        end if
        w(i,j, 0) = 0.0
        w(i,j,km) = 0.0
      end do
      end do
! =================================
      return
      end subroutine bondv1

end module module_bondv1

