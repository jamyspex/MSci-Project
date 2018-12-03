module module_vel2

contains

      subroutine vel2(km,jm,im,nou1,u,diu1,dx1,nou5,v,diu5,dy1,nou9,w,diu9,dzn,cov1,cov5,cov9,nou2,diu2,cov2,nou3,diu3,dzs,cov3,nou4,diu4,cov4,nou6,diu6,cov6,nou7,diu7,cov7,nou8,diu8,cov8)
      use params_common_sn!, only : kp, jp, ip !, only : kp, jp, ip
! 
      integer, intent(In) :: km
      integer, intent(In) :: jm
      integer, intent(In) :: im
      real(kind=4), dimension(-1:ip+2,0:jp+2,0:kp+2), intent(Out) :: nou1
      real, dimension(0:ip+1,-1:jp+1,0:kp+1), intent(In) :: u
      real, dimension(-1:ip+2,0:jp+2,0:kp+2), intent(InOut) :: diu1
      real, dimension(-1:ip+1), intent(In) :: dx1
      real(kind=4), dimension(-1:ip+2,0:jp+2,0:kp+2), intent(Out) :: nou5
      real, dimension(0:ip+1,-1:jp+1,0:kp+1), intent(In) :: v
      real, dimension(-1:ip+2,0:jp+2,0:kp+2), intent(InOut) :: diu5
      real, dimension(0:jp+1), intent(In) :: dy1
      real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2), intent(Out) :: nou9
      real, dimension(0:ip+1,-1:jp+1,-1:kp+1), intent(In) :: w
      real, dimension(0:ip+2,0:jp+2,0:kp+2), intent(InOut) :: diu9
      real, dimension(-1:kp+2), intent(In) :: dzn
      real, dimension(-1:ip+2,0:jp+2,0:kp+2), intent(InOut) :: cov1
      real, dimension(-1:ip+2,0:jp+2,0:kp+2), intent(InOut) :: cov5
      real, dimension(0:ip+2,0:jp+2,0:kp+2), intent(Out) :: cov9
      real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2), intent(Out) :: nou2
      real, dimension(0:ip+2,0:jp+2,0:kp+2), intent(InOut) :: diu2
      real, dimension(0:ip+2,0:jp+2,0:kp+2), intent(InOut) :: cov2
      real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2), intent(Out) :: nou3
      real, dimension(0:ip+2,0:jp+2,0:kp+2), intent(InOut) :: diu3
      real, dimension(-1:kp+2), intent(In) :: dzs
      real, dimension(0:ip+2,0:jp+2,0:kp+2), intent(Out) :: cov3
      real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2), intent(Out) :: nou4
      real, dimension(0:ip+2,0:jp+2,0:kp+2), intent(InOut) :: diu4
      real, dimension(0:ip+2,0:jp+2,0:kp+2), intent(InOut) :: cov4
      real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2), intent(Out) :: nou6
      real, dimension(0:ip+2,0:jp+2,0:kp+2), intent(InOut) :: diu6
      real, dimension(0:ip+2,0:jp+2,0:kp+2), intent(Out) :: cov6
      real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2), intent(Out) :: nou7
      real, dimension(0:ip+2,0:jp+2,0:kp+2), intent(InOut) :: diu7
      real, dimension(0:ip+2,0:jp+2,0:kp+2), intent(InOut) :: cov7
      real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2), intent(Out) :: nou8
      real, dimension(0:ip+2,0:jp+2,0:kp+2), intent(InOut) :: diu8
      real, dimension(0:ip+2,0:jp+2,0:kp+2), intent(InOut) :: cov8
      integer :: k
      integer :: j
      integer :: i
      integer, parameter :: u0=0
! 
      do k=1,km+1
      do j=1,jm
      do i=1,im
      if (k<km+1) then
        nou1(i,j,k) = (u(i-1,j,k)+u(i,j,k))/2.
        diu1(i,j,k) = (-u(i-1,j,k)+u(i,j,k))/dx1(i)
        nou5(i,j,k) = (v(i,j-1,k)+v(i,j,k))/2.
        diu5(i,j,k) = (-v(i,j-1,k)+v(i,j,k))/dy1(j)
        nou9(i,j,k) = (w(i,j,k-1)+w(i,j,k))/2.
        diu9(i,j,k) = (-w(i,j,k-1)+w(i,j,k))/dzn(k)
! 
        cov1(i,j,k) = nou1(i,j,k)*diu1(i,j,k)
        cov5(i,j,k) = nou5(i,j,k)*diu5(i,j,k)
        cov9(i,j,k) = nou9(i,j,k)*diu9(i,j,k)
        nou2(i,j,k) = (dx1(i+1)*v(i,j-1,k)+dx1(i)*v(i+1,j-1,k))               /(dx1(i)+dx1(i+1))
        diu2(i,j,k)=2.*(-u(i,j-1,k)+u(i,j,k))/(dy1(j-1)+dy1(j))
        cov2(i,j,k)=nou2(i,j,k)*diu2(i,j,k)
        nou4(i,j,k) = (dy1(j+1)*u(i-1,j,k)+dy1(j)*u(i-1,j+1,k))               /(dy1(j)+dy1(j+1))
        diu4(i,j,k) = 2.*(-v(i-1,j,k)+v(i,j,k))/(dx1(i-1)+dx1(i))
        cov4(i,j,k) = (nou4(i,j,k)-u0)*diu4(i,j,k)
    end if
    if (k<= km-1) then
        nou7(i,j,k) = (dzn(k+1)*u(i-1,j,k)+dzn(k)*u(i-1,j,k+1))               /(dzn(k)+dzn(k+1))
        diu7(i,j,k) = 2.*(-w(i-1,j,k)+w(i,j,k))/(dx1(i-1)+dx1(i))
        cov7(i,j,k) = (nou7(i,j,k)-u0)*diu7(i,j,k)
        nou8(i,j,k) = (dzn(k+1)*v(i,j-1,k)+dzn(k)*v(i,j-1,k+1))               /(dzn(k)+dzn(k+1))
        diu8(i,j,k) = 2.*(-w(i,j-1,k)+w(i,j,k))/(dy1(j-1)+dy1(j))
        cov8(i,j,k) = nou8(i,j,k)*diu8(i,j,k)
        end if
        nou3(i,j,k) = (dx1(i+1)*w(i,j,k-1)+dx1(i)*w(i+1,j,k-1))               /(dx1(i)+dx1(i+1))
        diu3(i,j,k) = (-u(i,j,k-1)+u(i,j,k))/dzs(k-1)
        cov3(i,j,k) = nou3(i,j,k)*diu3(i,j,k)
        nou6(i,j,k) = (dy1(j+1)*w(i,j,k-1)+dy1(j)*w(i,j+1,k-1))               /(dy1(j)+dy1(j+1))
        diu6(i,j,k) = (-v(i,j,k-1)+v(i,j,k))/dzs(k-1)
        cov6(i,j,k) = nou6(i,j,k)*diu6(i,j,k)
   end do
      end do
      end do
! ====================================
      do k=1,km+1
      do j=1,jm+1
      if (k<km+1 .and. j<jm+1) then
        nou1(im+1,j,k)=nou1(im,j,k)
        diu1(im+1,j,k)=diu1(im,j,k)
        cov1(im+1,j,k)=cov1(im,j,k)
        nou4(im+1,j,k)=nou4(im,j,k)
        diu4(im+1,j,k)=diu4(im,j,k)
        cov4(im+1,j,k)=cov4(im,j,k)
      if (k<km) then
        nou7(im+1,j,k)=nou7(im,j,k)
        diu7(im+1,j,k)=diu7(im,j,k)
        cov7(im+1,j,k)=cov7(im,j,k)
       end if
   end if
! --les
        diu2(im+1,j,k)=diu2(im,j,k)
        diu3(im+1,j,k)=diu3(im,j,k)
      end do
      end do

      do k=1,km+1
      do i=1,im+1
      if (k<km+1 .and. i<im+1) then
        nou2(i,0,k)=nou2(i,jm,k)
        diu2(i,0,k)=diu2(i,jm,k)
        cov2(i,0,k)=cov2(i,jm,k)
        nou2(i,jm+1,k)=nou2(i,1,k)
        diu2(i,jm+1,k)=diu2(i,1,k)
        cov2(i,jm+1,k)=cov2(i,1,k)
        nou5(i,0,k)=nou5(i,jm,k)
        diu5(i,0,k)=diu5(i,jm,k)
        cov5(i,0,k)=cov5(i,jm,k)
        nou5(i,jm+1,k)=nou5(i,1,k)
        diu5(i,jm+1,k)=diu5(i,1,k)
        cov5(i,jm+1,k)=cov5(i,1,k)
      if (k<km) then
        nou8(i,0,k)=nou8(i,jm,k)
        diu8(i,0,k)=diu8(i,jm,k)
        cov8(i,0,k)=cov8(i,jm,k)
        nou8(i,jm+1,k)=nou8(i,1,k)
        diu8(i,jm+1,k)=diu8(i,1,k)
        cov8(i,jm+1,k)=cov8(i,1,k)
      end if
        end if
        diu4(i,0,k)=diu4(i,jm,k)
        diu6(i,0,k)=diu6(i,jm,k)

      end do
      end do
      

! 
      return
      end subroutine vel2

end module module_vel2

