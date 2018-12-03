module module_press
      use module_bondfg
      use module_boundp
contains
      subroutine press(km,jm,im,rhs,v,u,dzn,w,dy1,dx1,h,dt,g,f,cn2l,cn2s,cn4l,cn1,cn3s,cn4s,p,cn3l,n,nmax,data20,usum,vsum,wsum)
use params_common_sn
! 
! --only used mac method
      real, parameter :: pjuge=0.0001
!  WV: was 50, TEST!    
      integer, parameter :: nmaxp=50
      integer, intent(in) :: km
      integer :: jm
      integer :: im
      real, dimension(0:ip+1,0:jp+1,0:kp+1), intent(inout) :: rhs
      real, dimension(0:ip+1,-1:jp+1,0:kp+1) :: v
      real, dimension(0:ip+1,-1:jp+1,0:kp+1) :: u
      real, dimension(-1:kp+2), intent(in) :: dzn
      real, dimension(0:ip+1,-1:jp+1,-1:kp+1) :: w
      real, dimension(0:jp+1), intent(in) :: dy1
      real, dimension(-1:ip+1), intent(in) :: dx1
      real, dimension(0:ip,0:jp,0:kp), intent(inout) :: h
      real, intent(in) :: dt
      real, dimension(0:ip,0:jp,0:kp), intent(inout) :: g
      real, dimension(0:ip,0:jp,0:kp), intent(inout) :: f
      real, dimension(1:ip), intent(in) :: cn2l
      real, dimension(1:ip), intent(in) :: cn2s
      real, dimension(1:kp), intent(in) :: cn4l
      real, dimension(1:ip,1:jp,1:kp), intent(in) :: cn1
      real, dimension(1:jp), intent(in) :: cn3s
      real, dimension(1:kp), intent(in) :: cn4s
      real, dimension(0:ip+2,0:jp+2,0:kp+1) :: p
      real, dimension(1:jp), intent(in) :: cn3l
      integer :: n
      integer, intent(in) :: nmax
      character(len=70) :: data20
      real, dimension(0:ip,0:jp,0:kp) :: usum
      real, dimension(0:ip,0:jp,0:kp) :: vsum
      real, dimension(0:ip,0:jp,0:kp) :: wsum
      integer :: k
      integer :: j
      integer :: i
      real :: rhsav
      real :: area
      integer :: l
      real :: sor
      integer :: nrd
      real :: reltmp
      real :: pav
      real :: pco
      real :: cflu
      real :: cflv
      real :: cflw
      real, parameter :: omega=1.
!      
      call bondfg(km,jm,f,im,g,h)
! 
      do k=1,km
      do j=1,jm
      do i=1,im
        rhs(i,j,k)     =(-u(i-1,j,k)+u(i,j,k))/dx1(i)     +(-v(i,j-1,k)+v(i,j,k))/dy1(j)     +(-w(i,j,k-1)+w(i,j,k))/dzn(k)
! --stretch
        rhs(i,j,k)=(f(i,j,k)-f(i-1,j,k))/dx1(i)        +(g(i,j,k)-g(i,j-1,k))/dy1(j)        +(h(i,j,k)-h(i,j,k-1))/dzn(k)+rhs(i,j,k)/dt
! 
   end do
      end do
      end do
! 
      rhsav=0.0
      area=0.0
      do k=1,km
      do j=1,jm
      do i=1,im
        rhsav = rhsav+dx1(i)*dy1(j)*dzn(k)*rhs(i,j,k)
        area  = area +dx1(i)*dy1(j)*dzn(k)
! 
   end do
      end do
      end do
! 
      rhsav = rhsav/area
      do k=1,km
      do j=1,jm
      do i=1,im
        rhs(i,j,k) = rhs(i,j,k)-rhsav
   end do
      end do
      end do
! --SOR
      do l=1,nmaxp                         
        sor = 0.0                                
        do nrd=0,1
        do k=1,km
        do j=1,jm
        do i=1+mod(k+j+nrd,2),im,2
          reltmp = omega*(cn1(i,j,k)              *(cn2l(i)*p(i+1,j,k)               +cn2s(i)*p(i-1,j,k)               +cn3l(j)*p(i,j+1,k)               +cn3s(j)*p(i,j-1,k)               +cn4l(k)*p(i,j,k+1)               +cn4s(k)*p(i,j,k-1)               -rhs(i,j,k))-p(i,j,k))
! 
          p(i,j,k) = p(i,j,k) +reltmp
          sor    = sor+reltmp*reltmp
   end do
      end do
      end do
        call boundp1(km,jm,p,im)
   end do
        call boundp2(jm,im,p,km)
! --check
#ifndef NO_IO
      if ((mod(n-1,10) == 0).and.(mod(l,20) == 0)) then
        write(6,*) __PH0__,n,l,sor
      end if
#endif
! 
!        if (sor < pjuge) goto 510                         
   end do
  510 continue                                         
! 
      pav = 0.0                                        
      pco = 0.0                                        
      do k=1,km                                    
      do j=1,jm                                    
      do i=1,im                                    
        pav = pav+p(i,j,k)*dx1(i)*dy1(j)*dzn(k)
        pco = pco+dx1(i)*dy1(j)*dzn(k)
   end do
      end do
      end do
! 
      pav = pav/pco                                    
      do k=1,km                                    
      do j=1,jm                                    
      do i=1,im                                    
        p(i,j,k)=p(i,j,k)-pav
   end do
      end do
      end do
! 
      call boundp1(km,jm,p,im)
      call boundp2(jm,im,p,km)
! 
#ifndef NO_IO
      if (mod(n-1,20) == 0) then
         write(6,*) __PH0__,n,l,sor
      end if
! --check
      if (mod(n-1,20) == 0) then
      do  k=1,km,10
      write(6,*) __PH0__,k,__PH1__,u(0,jm/2,k),v(0,jm/2,k),w(0,jm/2,k)
      end do
      do  k=1,km,10
      write(6,*) __PH0__,k,__PH1__,u(im/2,jm/2,k),v(im/2,jm/2,k),w(im/2,jm/2,k)
      end do
! 
      cflu=0.
      cflv=0.
      cflw=0.
      do k=1,km
      do j=1,jm
      do i=1,im
       cflu=amax1(cflu,abs(u(i,j,k)*dt/dx1(i)))      
       cflv=amax1(cflv,abs(v(i,j,k)*dt/dy1(j)))
       cflw=amax1(cflw,abs(w(i,j,k)*dt/dzn(k)))
      end do 
      end do
      end do
      end if
      if (mod(n-1,20) == 0) then
      write(6,*) __PH0__,cflu,cflv,cflw
      end if
! 
      if(mod(n,1000) == 0.or.n == nmax) then
      ! wv: added for sanity check
      !print *, __PH0__,n,__PH1__,p(ip/2,jp/2,kp/2)
!      &      'vel at centre: ', 
!      &  u(ip/2,jp/2,kp/2),v(ip/2,jp/2,kp/2),w(ip/2,jp/2,kp/2)
      open(unit=20,file=data20,form=__PH0__,status=__PH1__)
        write(20) (((u(i,j,k),i=1,im),j=1,jm),k=1,km)
        write(20) (((v(i,j,k),i=1,im),j=1,jm),k=1,km)
        write(20) (((w(i,j,k),i=1,im),j=1,jm),k=1,km)
        write(20) (((p(i,j,k),i=1,im),j=1,jm),k=1,km)
        write(20) (((usum(i,j,k),i=1,im),j=1,jm),k=1,km)
        write(20) (((vsum(i,j,k),i=1,im),j=1,jm),k=1,km)
        write(20) (((wsum(i,j,k),i=1,im),j=1,jm),k=1,km)
      close(unit=20)
      end if
#endif
! 
      return
      end subroutine press
end module module_press
