module module_adam
contains
      subroutine adam(nmax,n,data21,fold,gold,hold,fghold,km,jm,im,f,g,h)
use params_common_sn
      integer, intent(in) :: nmax
      integer, intent(in) :: n
      character(len=70) :: data21
      real, dimension(1:ip,1:jp,1:kp) :: fold
      real, dimension(1:ip,1:jp,1:kp) :: gold
      real, dimension(1:ip,1:jp,1:kp) :: hold
      real, dimension(1:ip,1:jp,1:kp) :: fghold
      integer, intent(in) :: km
      integer, intent(in) :: jm
      integer, intent(in) :: im
      real, dimension(0:ip,0:jp,0:kp), intent(inout) :: f
      real, dimension(0:ip,0:jp,0:kp), intent(inout) :: g
      real, dimension(0:ip,0:jp,0:kp), intent(inout) :: h
      integer :: i
      integer :: j
      integer :: k
      real :: fd
      real :: gd
      real :: hd
! 
! 
#ifndef NO_IO
      if (mod(n,1000) == 0.or.n == nmax) then
       open(unit=21,file=data21,form=__PH0__,status=__PH1__)
       write(21) (((fold(i,j,k),i=1,im),j=1,jm),k=1,km)
       write(21) (((gold(i,j,k),i=1,im),j=1,jm),k=1,km)
       write(21) (((hold(i,j,k),i=1,im),j=1,jm),k=1,km)
       write(21) (((fghold(i,j,k),i=1,im),j=1,jm),k=1,km)
       close(unit=21)
      end if
#endif
      do k=1,km
      do j=1,jm
      do i=1,im
        fd=f(i,j,k)
        gd=g(i,j,k)
        hd=h(i,j,k)
        f(i,j,k)=1.5*f(i,j,k)-0.5*fold(i,j,k)
        g(i,j,k)=1.5*g(i,j,k)-0.5*gold(i,j,k)
        h(i,j,k)=1.5*h(i,j,k)-0.5*hold(i,j,k)
        fold(i,j,k)=fd
        gold(i,j,k)=gd
        hold(i,j,k)=hd
   end do
      end do
      end do
! 
      return
      end subroutine adam
end module module_adam
