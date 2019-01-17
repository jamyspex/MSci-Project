module module_vel2
    implicit none
 contains
      subroutine vel2( &
      nou1,nou5,nou9,nou2,nou3,nou4,nou6,nou7,nou8,&
      diu1,diu2,diu3,diu4,diu5,diu6,diu7,diu8,diu9,&
      cov1,cov2,cov3,cov4,cov5,cov6,cov7,cov8,cov9,&
      u,v,w,dx1,dy1,dzn,dzs,uspd,vspd)
    use params_common_sn
    implicit none
        real(kind=4), dimension(-1:ip+2,0:jp+2,0:kp+2) , intent(Out) :: cov1
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(Out) :: cov2
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(Out) :: cov3
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(Out) :: cov4
        real(kind=4), dimension(-1:ip+2,0:jp+2,0:kp+2) , intent(Out) :: cov5
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(Out) :: cov6
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(Out) :: cov7
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(Out) :: cov8
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(Out) :: cov9
        real(kind=4), dimension(-1:ip+2,0:jp+2,0:kp+2) , intent(Out) :: diu1
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(Out) :: diu2
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(Out) :: diu3
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(Out) :: diu4
        real(kind=4), dimension(-1:ip+2,0:jp+2,0:kp+2) , intent(Out) :: diu5
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(Out) :: diu6
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(Out) :: diu7
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(Out) :: diu8
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(Out) :: diu9
        real(kind=4), dimension(-1:ip+1) , intent(In) :: dx1
        real(kind=4), dimension(0:jp+1) , intent(In) :: dy1
        real(kind=4), dimension(-1:kp+2) , intent(In) :: dzn
        real(kind=4), dimension(-1:kp+2) , intent(In) :: dzs
        real(kind=4), dimension(-1:ip+2,0:jp+2,0:kp+2) , intent(Out) :: nou1
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(Out) :: nou2
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(Out) :: nou3
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(Out) :: nou4
        real(kind=4), dimension(-1:ip+2,0:jp+2,0:kp+2) , intent(Out) :: nou5
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(Out) :: nou6
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(Out) :: nou7
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(Out) :: nou8
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(Out) :: nou9
        real(kind=4), dimension(0:ip+1,-1:jp+1,0:kp+1) , intent(In) :: u
        real(kind=4), dimension(0:ip+1,-1:jp+1,0:kp+1) , intent(In) :: v
        real(kind=4), dimension(0:ip+1,-1:jp+1,-1:kp+1) , intent(In) :: w
        real(kind=4), dimension(0:ip+1,0:jp+1) , intent(out) :: uspd
        real(kind=4), dimension(0:ip+1,0:jp+1) , intent(out) :: vspd
        integer :: i,j,k
        integer, parameter :: u0 = 0
      do j=1,jp
        do i=1,ip
         uspd(i,j)=(u(i,j,1)**2+((0.5*(v(i,j-1,1)+v(i,j,1))*dx1(i+1)&
     +0.5*(v(i+1,j-1,1)+v(i+1,j,1))*dx1(i))/(dx1(i)+dx1(i+1)))**2)**0.5
        end do
        end do
        do j=1,jp
        do i=1,ip
         vspd(i,j)=(v(i,j,1)**2+((0.5*(u(i-1,j,1)+u(i,j,1))*dy1(j+1)&
     +0.5*(u(i-1,j+1,1)+u(i,j+1,1))*dy1(j))/(dy1(j)+dy1(j+1)))**2)**0.5
        end do
        end do
        write(6,*) 'CHK_uspd_vspd=',uspd(ip/2,jp/2),vspd(ip/2,jp/2)
      do k = 1,kp
      do j = 1,jp
      do i = 1,ip
        nou1(i,j,k) = (u(i-1,j,k)+u(i,j,k))/2.
        diu1(i,j,k) = (-u(i-1,j,k)+u(i,j,k))/dx1(i)
        nou5(i,j,k) = (v(i,j-1,k)+v(i,j,k))/2.
        diu5(i,j,k) = (-v(i,j-1,k)+v(i,j,k))/dy1(j)
        nou9(i,j,k) = (w(i,j,k-1)+w(i,j,k))/2.
        diu9(i,j,k) = (-w(i,j,k-1)+w(i,j,k))/dzn(k)
        cov1(i,j,k) = nou1(i,j,k)*diu1(i,j,k)
        cov5(i,j,k) = nou5(i,j,k)*diu5(i,j,k)
        cov9(i,j,k) = nou9(i,j,k)*diu9(i,j,k)
      end do
      end do
      end do
      do k = 1,kp
      do j = 1,jp
      do i = 1,ip
        nou2(i,j,k) = (dx1(i+1)*v(i,j-1,k)+dx1(i)*v(i+1,j-1,k)) /(dx1(i)+dx1(i+1))
        diu2(i,j,k) = 2.*(-u(i,j-1,k)+u(i,j,k))/(dy1(j-1)+dy1(j))
        cov2(i,j,k) = nou2(i,j,k)*diu2(i,j,k)
      end do
      end do
      end do
      do k = 2,kp+1
      do j = 1,jp
      do i = 1,ip
        nou3(i,j,k) = (dx1(i+1)*w(i,j,k-1)+dx1(i)*w(i+1,j,k-1)) /(dx1(i)+dx1(i+1))
        diu3(i,j,k) = (-u(i,j,k-1)+u(i,j,k))/dzs(k-1)
        cov3(i,j,k) = nou3(i,j,k)*diu3(i,j,k)
      end do
      end do
      end do
      do j=1,jp
      do i=1,ip
       nou3(i,j,1) = 0.5*(dx1(i+1)*w(i,j,1)+dx1(i)*w(i+1,j,1))/(dx1(i)+dx1(i+1))
       diu3(i,j,1) = uspd(i,j)*0.4/alog(0.5*dzn(1)/0.1)/(0.5*dzn(1))/0.4*u(i,j,1)/uspd(i,j)
       cov3(i,j,1) = nou3(i,j,1)*diu3(i,j,1)
      end do
      end do
      do k = 1,kp
      do j = 1,jp
      do i = 1,ip
        nou4(i,j,k) = (dy1(j+1)*u(i-1,j,k)+dy1(j)*u(i-1,j+1,k)) /(dy1(j)+dy1(j+1))
        diu4(i,j,k) = 2.*(-v(i-1,j,k)+v(i,j,k))/(dx1(i-1)+dx1(i))
        cov4(i,j,k) = (nou4(i,j,k)-u0)*diu4(i,j,k)
      end do
      end do
      end do
      do k = 2,kp+1
      do j = 1,jp
      do i = 1,ip
        nou6(i,j,k) = (dy1(j+1)*w(i,j,k-1)+dy1(j)*w(i,j+1,k-1)) /(dy1(j)+dy1(j+1))
        diu6(i,j,k) = (-v(i,j,k-1)+v(i,j,k))/dzs(k-1)
        cov6(i,j,k) = nou6(i,j,k)*diu6(i,j,k)
      end do
      end do
      end do
      do j=1,jp
      do i=1,ip
       nou6(i,j,1) = 0.5*(dy1(j+1)*w(i,j,1)+dy1(j)*w(i,j+1,1))/(dy1(j)+dy1(j+1))
       diu6(i,j,1)=vspd(i,j)*0.4/alog(0.5*dzn(1)/0.1)/(0.5*dzn(1))/0.4*v(i,j,1)/vspd(i,j)
       cov6(i,j,1) = nou6(i,j,1)*diu6(i,j,1)
      end do
      end do
      do k = 1,kp-1
      do j = 1,jp
      do i = 1,ip
        nou7(i,j,k) = (dzn(k+1)*u(i-1,j,k)+dzn(k)*u(i-1,j,k+1)) /(dzn(k)+dzn(k+1))
        diu7(i,j,k) = 2.*(-w(i-1,j,k)+w(i,j,k))/(dx1(i-1)+dx1(i))
        cov7(i,j,k) = (nou7(i,j,k)-u0)*diu7(i,j,k)
      end do
      end do
      end do
      do k = 1,kp-1
      do j = 1,jp
      do i = 1,ip
        nou8(i,j,k) = (dzn(k+1)*v(i,j-1,k)+dzn(k)*v(i,j-1,k+1)) /(dzn(k)+dzn(k+1))
        diu8(i,j,k) = 2.*(-w(i,j-1,k)+w(i,j,k))/(dy1(j-1)+dy1(j))
        cov8(i,j,k) = nou8(i,j,k)*diu8(i,j,k)
      end do
      end do
      end do
      do k = 1,kp
      do j = 1,jp
        nou1(ip+1,j,k) = nou1(ip,j,k)
        diu1(ip+1,j,k) = diu1(ip,j,k)
        cov1(ip+1,j,k) = cov1(ip,j,k)
      end do
      end do
      do k = 1,kp
      do i = 1,ip
        nou2(i,0,k) = nou2(i,jp,k)
        diu2(i,0,k) = diu2(i,jp,k)
        cov2(i,0,k) = cov2(i,jp,k)
        nou2(i,jp+1,k) = nou2(i,1,k)
        diu2(i,jp+1,k) = diu2(i,1,k)
        cov2(i,jp+1,k) = cov2(i,1,k)
      end do
      end do
      do k = 1,kp
      do j = 1,jp
        nou4(ip+1,j,k) = nou4(ip,j,k)
        diu4(ip+1,j,k) = diu4(ip,j,k)
        cov4(ip+1,j,k) = cov4(ip,j,k)
      end do
      end do
      do k = 1,kp
      do i = 1,ip
        nou5(i,0,k) = nou5(i,jp,k)
        diu5(i,0,k) = diu5(i,jp,k)
        cov5(i,0,k) = cov5(i,jp,k)
        nou5(i,jp+1,k) = nou5(i,1,k)
        diu5(i,jp+1,k) = diu5(i,1,k)
        cov5(i,jp+1,k) = cov5(i,1,k)
      end do
      end do
      do k = 1,kp-1
      do j = 1,jp
        nou7(ip+1,j,k) = nou7(ip,j,k)
        diu7(ip+1,j,k) = diu7(ip,j,k)
        cov7(ip+1,j,k) = cov7(ip,j,k)
      end do
      end do
      do k = 1,kp-1
      do i = 1,ip
        nou8(i,0,k) = nou8(i,jp,k)
        diu8(i,0,k) = diu8(i,jp,k)
        cov8(i,0,k) = cov8(i,jp,k)
        nou8(i,jp+1,k) = nou8(i,1,k)
        diu8(i,jp+1,k) = diu8(i,1,k)
        cov8(i,jp+1,k) = cov8(i,1,k)
      end do
      end do
      do k = 1,kp+1
      do j = 1,jp+1
        diu2(ip+1,j,k) = diu2(ip,j,k)
        diu3(ip+1,j,k) = diu3(ip,j,k)
      end do
      end do
      do k = 1,kp+1
      do j = 1,jp+1
        diu2(0,j,k) = diu2(1,j,k)
        diu3(0,j,k) = diu3(1,j,k)
      end do
      end do
      do k = 1,kp+1
      do i = 1,ip+1
        diu4(i,0,k) = diu4(i,jp,k)
        diu6(i,0,k) = diu6(i,jp,k)
      end do
      end do
end subroutine vel2
end module module_vel2
