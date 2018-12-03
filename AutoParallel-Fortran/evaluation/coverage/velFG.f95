module module_velFG

      use module_vel2
contains

      subroutine velfg(km,jm,im,cov1,dx1,cov2,cov3,dfu1,diu2,diu3,dzn,diu1,dy1,vn,f,cov4,cov5,cov6,dfv1,diu4,diu5,diu6,g,cov7,cov8,cov9,dfw1,diu9,diu7,dzs,diu8,h,nou1,u,nou5,v,nou9,w,nou2,nou3,nou4,nou6,nou7,nou8)
      use params_common_sn!, only : ip, kp, jp !, only : ip, kp, jp
      integer, intent(In) :: km
      integer, intent(In) :: jm
      integer, intent(In) :: im
      real, dimension(-1:ip+2,0:jp+2,0:kp+2), intent(InOut) :: cov1
      real, dimension(-1:ip+1), intent(In) :: dx1
      real, dimension(0:ip+2,0:jp+2,0:kp+2), intent(InOut) :: cov2
      real, dimension(0:ip+2,0:jp+2,0:kp+2), intent(InOut) :: cov3
      real, dimension(0:ip,1:jp,1:kp), intent(InOut) :: dfu1
      real, dimension(0:ip+2,0:jp+2,0:kp+2), intent(InOut) :: diu2
      real, dimension(0:ip+2,0:jp+2,0:kp+2), intent(InOut) :: diu3
      real, dimension(-1:kp+2), intent(In) :: dzn
      real, dimension(-1:ip+2,0:jp+2,0:kp+2), intent(InOut) :: diu1
      real, dimension(0:jp+1), intent(In) :: dy1
      real, intent(In) :: vn
      real, dimension(0:ip,0:jp,0:kp), intent(Out) :: f
      real, dimension(0:ip+2,0:jp+2,0:kp+2), intent(InOut) :: cov4
      real, dimension(-1:ip+2,0:jp+2,0:kp+2), intent(InOut) :: cov5
      real, dimension(0:ip+2,0:jp+2,0:kp+2), intent(InOut) :: cov6
      real, dimension(1:ip,0:jp,1:kp), intent(InOut) :: dfv1
      real, dimension(0:ip+2,0:jp+2,0:kp+2), intent(InOut) :: diu4
      real, dimension(-1:ip+2,0:jp+2,0:kp+2), intent(InOut) :: diu5
      real, dimension(0:ip+2,0:jp+2,0:kp+2), intent(InOut) :: diu6
      real, dimension(0:ip,0:jp,0:kp), intent(Out) :: g
      real, dimension(0:ip+2,0:jp+2,0:kp+2), intent(InOut) :: cov7
      real, dimension(0:ip+2,0:jp+2,0:kp+2), intent(InOut) :: cov8
      real, dimension(0:ip+2,0:jp+2,0:kp+2), intent(InOut) :: cov9
      real, dimension(1:ip,1:jp,1:kp), intent(InOut) :: dfw1
      real, dimension(0:ip+2,0:jp+2,0:kp+2), intent(InOut) :: diu9
      real, dimension(0:ip+2,0:jp+2,0:kp+2), intent(InOut) :: diu7
      real, dimension(-1:kp+2), intent(In) :: dzs
      real, dimension(0:ip+2,0:jp+2,0:kp+2), intent(InOut) :: diu8
      real, dimension(0:ip,0:jp,0:kp), intent(Out) :: h
      real(kind=4), dimension(-1:ip+2,0:jp+2,0:kp+2), intent(Out) :: nou1
      real, dimension(0:ip+1,-1:jp+1,0:kp+1), intent(In) :: u
      real(kind=4), dimension(-1:ip+2,0:jp+2,0:kp+2), intent(Out) :: nou5
      real, dimension(0:ip+1,-1:jp+1,0:kp+1), intent(In) :: v
      real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2), intent(Out) :: nou9
      real, dimension(0:ip+1,-1:jp+1,-1:kp+1), intent(In) :: w
      real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2), intent(Out) :: nou2
      real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2), intent(Out) :: nou3
      real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2), intent(Out) :: nou4
      real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2), intent(Out) :: nou6
      real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2), intent(Out) :: nou7
      real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2), intent(Out) :: nou8
      integer :: k
      integer :: j
      integer :: i
      real :: covx1
      real :: covy1
      real :: covz1
      real :: covc
      real :: df
! 
! 
      call vel2(km,jm,im,nou1,u,diu1,dx1,nou5,v,diu5,dy1,nou9,w,diu9,dzn,cov1,cov5,cov9,nou2,diu2,cov2,nou3,diu3,dzs,cov3,nou4,diu4,cov4,nou6,diu6,cov6,nou7,diu7,cov7,nou8,diu8,cov8)

! --u velocity
      do k=1,km
      do j=1,jm
      do i=1,im
        covx1 = (dx1(i+1)*cov1(i,j,k)+dx1(i)*cov1(i+1,j,k))         /(dx1(i)+dx1(i+1))   
        covy1 = (cov2(i,j,k)+cov2(i,j+1,k))/2.
        covz1 = (cov3(i,j,k)+cov3(i,j,k+1))/2.
        covc = covx1+covy1+covz1
        dfu1(i,j,k)=2.*(-diu1(i,j,k)+diu1(i+1,j,k))/(dx1(i)+dx1(i+1))              +   (-diu2(i,j,k)+diu2(i,j+1,k))/dy1(j)             +   (-diu3(i,j,k)+diu3(i,j,k+1))/dzn(k) 
        df   = vn*dfu1(i,j,k)
        f(i,j,k)=(-covc+df)  
   end do
      end do
      end do
! =======================================
! --v velocity
      do k=1,km
      do j=1,jm
      do i=1,im
        covx1=(cov4(i,j,k)+cov4(i+1,j,k))/2.
        covy1=(dy1(j+1)*cov5(i,j,k)+dy1(j)*cov5(i,j+1,k))       /(dy1(j)+dy1(j+1))   
        covz1=(cov6(i,j,k)+cov6(i,j,k+1))/2.
        covc = covx1+covy1+covz1
        dfv1(i,j,k)=(-diu4(i,j,k)+diu4(i+1,j,k))/dx1(i)           +2.*(-diu5(i,j,k)+diu5(i,j+1,k))/(dy1(j)+dy1(j+1))             +(-diu6(i,j,k)+diu6(i,j,k+1))/dzn(k)
        df   = vn*dfv1(i,j,k)
        g(i,j,k)=(-covc+df)
   end do
      end do
      end do
! 
! =======================================
! --w velocity
      do k=1,km-1
      do j=1,jm
      do i=1,im
       covx1=(cov7(i,j,k)+cov7(i+1,j,k))/2.
       covy1=(cov8(i,j,k)+cov8(i,j+1,k))/2.
       covz1=(dzn(k+1)*cov9(i,j,k)+dzn(k)*cov9(i,j,k+1))      /(dzn(k)+dzn(k+1))   
       covc = covx1+covy1+covz1
        dfw1(i,j,k)=(-diu7(i,j,k)+diu7(i+1,j,k))/dx1(i)              +(-diu8(i,j,k)+diu8(i,j+1,k))/dy1(j)             +(-diu9(i,j,k)+diu9(i,j,k+1))/dzs(k)
        df   = vn*dfw1(i,j,k)                                         
        h(i,j,k)=(-covc+df)
   end do
      end do
      end do
!                                          
! =======================================
      return
      end subroutine velfg

end module module_velFG

