module module_vel2
#ifdef MPI
    use communication_helper_real
#endif
    implicit none
 contains 
      subroutine vel2( &
#ifndef WV_NEW_LES2
      nou1,nou5,nou9,nou2,nou3,nou4,nou6,nou7,nou8,&
#endif
#ifndef WV_NEW_LES
      diu1,diu2,diu3,diu4,diu5,diu6,diu7,diu8,diu9,&
#endif
      cov1,cov2,cov3,cov4,cov5,cov6,cov7,cov8,cov9,&
      u,v,w,dx1,dy1,dzn,dzs,uspd,vspd)
#ifdef WV_NEW
    use params_common_sn
    implicit none
#else
    use common_sn ! create_new_include_statements() line 102
#endif
        real(kind=4), dimension(-1:ip+2,0:jp+2,0:kp+2) , intent(Out) :: cov1
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(Out) :: cov2
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(Out) :: cov3
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(Out) :: cov4
        real(kind=4), dimension(-1:ip+2,0:jp+2,0:kp+2) , intent(Out) :: cov5
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(Out) :: cov6
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(Out) :: cov7
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(Out) :: cov8
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(Out) :: cov9
#ifndef WV_NEW_LES
        real(kind=4), dimension(-1:ip+2,0:jp+2,0:kp+2) , intent(Out) :: diu1
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(Out) :: diu2
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(Out) :: diu3
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(Out) :: diu4
        real(kind=4), dimension(-1:ip+2,0:jp+2,0:kp+2) , intent(Out) :: diu5
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(Out) :: diu6
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(Out) :: diu7
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(Out) :: diu8
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(Out) :: diu9
#else
        real(kind=4) :: diu1_,diu2_,diu3_, diu4_,diu5_,diu6_, diu7_,diu8_,diu9_
        real(kind=4) :: diu3_ij1,diu6_ij1
        real(kind=4) :: diu1_ip1,diu2_jp1,diu3_kp1,diu4_ip1,diu5_jp1,diu6_kp1,diu7_ip1,diu8_jp1,diu9_kp1
#endif
        real(kind=4), dimension(-1:ip+1) , intent(In) :: dx1
        real(kind=4), dimension(0:jp+1) , intent(In) :: dy1
        real(kind=4), dimension(-1:kp+2) , intent(In) :: dzn
        real(kind=4), dimension(-1:kp+2) , intent(In) :: dzs
#ifndef WV_NEW_LES2
        real(kind=4), dimension(-1:ip+2,0:jp+2,0:kp+2) , intent(Out) :: nou1
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(Out) :: nou2
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(Out) :: nou3
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(Out) :: nou4
        real(kind=4), dimension(-1:ip+2,0:jp+2,0:kp+2) , intent(Out) :: nou5
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(Out) :: nou6
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(Out) :: nou7
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(Out) :: nou8
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(Out) :: nou9
#else
        real(kind=4) :: nou1_,nou2_,nou3_,nou4_,nou5_,nou6_,nou7_,nou8_,nou9_
        real(kind=4) :: nou3_ij1, nou6_ij1
#endif
        real(kind=4), dimension(0:ip+1,-1:jp+1,0:kp+1) , intent(In) :: u
        real(kind=4), dimension(0:ip+1,-1:jp+1,0:kp+1) , intent(In) :: v
        real(kind=4), dimension(0:ip+1,-1:jp+1,-1:kp+1) , intent(In) :: w
!
!wall function
        real(kind=4), dimension(0:ip+1,0:jp+1) , intent(out) :: uspd
        real(kind=4), dimension(0:ip+1,0:jp+1) , intent(out) :: vspd
        integer :: i,j,k
        integer, parameter  :: u0 = 0
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
! WV: to get a point somewhere near the middle of the domain
#ifdef MPI
       if (rank == mpi_size / 2 + procPerRow / 2 - 1 ) then
#endif
        write(6,*) 'CHK_uspd_vspd=',uspd(ip/2,jp/2),vspd(ip/2,jp/2)
#ifdef MPI
       end if
#endif
!
      do k = 1,kp
      do j = 1,jp
      do i = 1,ip
#ifndef WV_NEW_LES
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
#else
#ifndef WV_NEW_LES2
        nou1(i,j,k) = ( u(i-1,j,k)+u(i,j,k))/2. ! i.e 2-point average of u in i-direction
        diu1_ = (-u(i-1,j,k)+u(i,j,k))/dx1(i) ! i.e. du/dx
        nou5(i,j,k) = ( v(i,j-1,k)+v(i,j,k))/2.
        diu5_ = (-v(i,j-1,k)+v(i,j,k))/dy1(j)
        nou9(i,j,k) = ( w(i,j,k-1)+w(i,j,k))/2.
        diu9_ = (-w(i,j,k-1)+w(i,j,k))/dzn(k)
!
        cov1(i,j,k) = nou1(i,j,k)*diu1_ !
        cov5(i,j,k) = nou5(i,j,k)*diu5_
        cov9(i,j,k) = nou9(i,j,k)*diu9_
#else
        nou1_ = ( u(i-1,j,k)+u(i,j,k))/2. ! i.e 2-point average of u in i-direction
        diu1_ = (-u(i-1,j,k)+u(i,j,k))/dx1(i) ! i.e. du/dx
        nou5_ = ( v(i,j-1,k)+v(i,j,k))/2.
        diu5_ = (-v(i,j-1,k)+v(i,j,k))/dy1(j)
        nou9_ = ( w(i,j,k-1)+w(i,j,k))/2.
        diu9_ = (-w(i,j,k-1)+w(i,j,k))/dzn(k)
!
        cov1(i,j,k) = nou1_*diu1_ !
        cov5(i,j,k) = nou5_*diu5_
        cov9(i,j,k) = nou9_*diu9_
#ifdef WV_NEW_VEL2
        cov1(ip+1,j,k) = cov1(ip,j,k)
        ! I don't think this is ever used anywhere!
        cov5(i,0,k) = cov5(i,jp,k)
        ! But this is used
        cov5(i,jp+1,k) = cov5(i,1,k)
#endif
#endif
#endif
      end do
      end do
      end do
#ifdef GR_DEBUG
    print*, 'GR: SUM(nou1) = ', sum(nou1)
    print*, 'GR: SUM(diu1) = ', sum(diu1)
    print*, 'GR: SUM(nou5) = ', sum(nou5)
    print*, 'GR: SUM(diu5) = ', sum(diu5)
    print*, 'GR: SUM(nou9) = ', sum(nou9)
    print*, 'GR: SUM(diu9) = ', sum(diu9)
    print*, 'GR: SUM(cov1) = ', sum(cov1)
    print*, 'GR: SUM(cov5) = ', sum(cov5)
    print*, 'GR: SUM(cov9) = ', sum(cov9)
#endif
      do k = 1,kp
      do j = 1,jp
      do i = 1,ip
#ifndef WV_NEW_LES
        nou2(i,j,k) = (dx1(i+1)*v(i,j-1,k)+dx1(i)*v(i+1,j-1,k)) /(dx1(i)+dx1(i+1))
        diu2(i,j,k) = 2.*(-u(i,j-1,k)+u(i,j,k))/(dy1(j-1)+dy1(j))
        cov2(i,j,k) = nou2(i,j,k)*diu2(i,j,k)
#else
#ifndef WV_NEW_LES2
        nou2(i,j,k) = (dx1(i+1)*v(i,j-1,k)+dx1(i)*v(i+1,j-1,k)) /(dx1(i)+dx1(i+1))
        diu2_ = 2.*(-u(i,j-1,k)+u(i,j,k))/(dy1(j-1)+dy1(j))
        cov2(i,j,k) = nou2(i,j,k)*diu2_
#else
        nou2_ = (dx1(i+1)*v(i,j-1,k)+dx1(i)*v(i+1,j-1,k)) /(dx1(i)+dx1(i+1))
        diu2_ = 2.*(-u(i,j-1,k)+u(i,j,k))/(dy1(j-1)+dy1(j))
        cov2(i,j,k) = nou2_*diu2_
#ifdef WV_NEW_VEL2
! Could be anything, really
        if (j==1) then
        ! This is used
        cov2(i,jp+1,k) = cov2(i,1,k)
        end if
#endif
#endif
#endif
      end do
      end do
      end do
#ifdef GR_DEBUG
    print*, 'GR: SUM(nou2) = ', sum(nou2)
    print*, 'GR: SUM(diu2) = ', sum(diu2)
    print*, 'GR: SUM(cov2) = ', sum(cov2)
#endif
!
      do k = 2,kp+1
      do j = 1,jp
      do i = 1,ip
#ifndef WV_NEW_LES
        nou3(i,j,k) = (dx1(i+1)*w(i,j,k-1)+dx1(i)*w(i+1,j,k-1)) /(dx1(i)+dx1(i+1))
        diu3(i,j,k) = (-u(i,j,k-1)+u(i,j,k))/dzs(k-1)
        cov3(i,j,k) = nou3(i,j,k)*diu3(i,j,k)
#else
#ifndef WV_NEW_LES2
        nou3(i,j,k) = (dx1(i+1)*w(i,j,k-1)+dx1(i)*w(i+1,j,k-1)) /(dx1(i)+dx1(i+1))
        diu3_ = (-u(i,j,k-1)+u(i,j,k))/dzs(k-1)
        cov3(i,j,k) = nou3(i,j,k)*diu3_
#else
        nou3_ = (dx1(i+1)*w(i,j,k-1)+dx1(i)*w(i+1,j,k-1)) /(dx1(i)+dx1(i+1))
        diu3_ = (-u(i,j,k-1)+u(i,j,k))/dzs(k-1)
        cov3(i,j,k) = nou3_*diu3_
#ifdef WV_NEW_VEL2
!This guard is ad-hoc, we could always compute this
        if (k==2) then
        nou3_ = 0.5*(dx1(i+1)*w(i,j,1)+dx1(i)*w(i+1,j,1))/(dx1(i)+dx1(i+1))
        diu3_ = uspd(i,j)*0.4/alog(0.5*dzn(1)/0.1)/(0.5*dzn(1))/0.4*u(i,j,1)/uspd(i,j)
        cov3(i,j,1) = nou3_*diu3_
        end if
#endif
#endif
#endif
      end do
      end do
      end do
! So here we set cov3(i,j,1) but it is independent of the other values, so it could be in the same loop with a condition
      do j=1,jp
      do i=1,ip
#ifndef WV_NEW_LES
       nou3(i,j,1) = 0.5*(dx1(i+1)*w(i,j,1)+dx1(i)*w(i+1,j,1))/(dx1(i)+dx1(i+1))
       diu3(i,j,1) = uspd(i,j)*0.4/alog(0.5*dzn(1)/0.1)/(0.5*dzn(1))/0.4*u(i,j,1)/uspd(i,j)
       cov3(i,j,1) = nou3(i,j,1)*diu3(i,j,1)
#else
#ifndef WV_NEW_LES2
       nou3(i,j,1) = 0.5*(dx1(i+1)*w(i,j,1)+dx1(i)*w(i+1,j,1))/(dx1(i)+dx1(i+1))
       diu3_ij1 = uspd(i,j)*0.4/alog(0.5*dzn(1)/0.1)/(0.5*dzn(1))/0.4*u(i,j,1)/uspd(i,j)
       cov3(i,j,1) = nou3(i,j,1)*diu3_ij1
#else
#ifndef WV_NEW_VEL2
       nou3_ij1 = 0.5*(dx1(i+1)*w(i,j,1)+dx1(i)*w(i+1,j,1))/(dx1(i)+dx1(i+1))
       diu3_ij1 = uspd(i,j)*0.4/alog(0.5*dzn(1)/0.1)/(0.5*dzn(1))/0.4*u(i,j,1)/uspd(i,j)
       cov3(i,j,1) = nou3_ij1*diu3_ij1
#endif
#endif
#endif
      end do
      end do
      do k = 1,kp
      do j = 1,jp
      do i = 1,ip
#ifndef WV_NEW_LES
        nou4(i,j,k) = (dy1(j+1)*u(i-1,j,k)+dy1(j)*u(i-1,j+1,k)) /(dy1(j)+dy1(j+1))
        diu4(i,j,k) = 2.*(-v(i-1,j,k)+v(i,j,k))/(dx1(i-1)+dx1(i))
        cov4(i,j,k) = (nou4(i,j,k)-u0)*diu4(i,j,k)
#else
#ifndef WV_NEW_LES2
        nou4(i,j,k) = (dy1(j+1)*u(i-1,j,k)+dy1(j)*u(i-1,j+1,k)) /(dy1(j)+dy1(j+1))
        diu4_ = 2.*(-v(i-1,j,k)+v(i,j,k))/(dx1(i-1)+dx1(i))
        cov4(i,j,k) = (nou4(i,j,k)-u0)*diu4_
#else
        nou4_ = (dy1(j+1)*u(i-1,j,k)+dy1(j)*u(i-1,j+1,k)) /(dy1(j)+dy1(j+1))
        diu4_ = 2.*(-v(i-1,j,k)+v(i,j,k))/(dx1(i-1)+dx1(i))
        cov4(i,j,k) = (nou4_-u0)*diu4_
#ifdef WV_NEW_VEL2
        ! Don't need the guard, we could simply recompute
        if (i==ip) then
        cov4(ip+1,j,k) = cov4(ip,j,k)
        end if
#endif
#endif
#endif
      end do
      end do
      end do
!
      do k = 2,kp+1
      do j = 1,jp
      do i = 1,ip
#ifndef WV_NEW_LES
        nou6(i,j,k) = (dy1(j+1)*w(i,j,k-1)+dy1(j)*w(i,j+1,k-1)) /(dy1(j)+dy1(j+1))
        diu6(i,j,k) = (-v(i,j,k-1)+v(i,j,k))/dzs(k-1)
        cov6(i,j,k) = nou6(i,j,k)*diu6(i,j,k)
#else
#ifndef WV_NEW_LES2
        nou6(i,j,k) = (dy1(j+1)*w(i,j,k-1)+dy1(j)*w(i,j+1,k-1)) /(dy1(j)+dy1(j+1))
        diu6_ = (-v(i,j,k-1)+v(i,j,k))/dzs(k-1)
        cov6(i,j,k) = nou6(i,j,k)*diu6_
#else
        nou6_ = (dy1(j+1)*w(i,j,k-1)+dy1(j)*w(i,j+1,k-1)) /(dy1(j)+dy1(j+1))
        diu6_ = (-v(i,j,k-1)+v(i,j,k))/dzs(k-1)
        cov6(i,j,k) = nou6_*diu6_
#ifdef WV_NEW_VEL2
        if (k==1) then
        nou6_ij1 = 0.5*(dy1(j+1)*w(i,j,1)+dy1(j)*w(i,j+1,1))/(dy1(j)+dy1(j+1))
        diu6_ij1=vspd(i,j)*0.4/alog(0.5*dzn(1)/0.1)/(0.5*dzn(1))/0.4*v(i,j,1)/vspd(i,j)
        cov6(i,j,1) = nou6_ij1*diu6_ij1
        end if
#endif
#endif
#endif
      end do
      end do
      end do
! Same here for cov6(i,j,1), the loops could be merged
      do j=1,jp
      do i=1,ip
#ifndef WV_NEW_LES
       nou6(i,j,1) = 0.5*(dy1(j+1)*w(i,j,1)+dy1(j)*w(i,j+1,1))/(dy1(j)+dy1(j+1))
       diu6(i,j,1)=vspd(i,j)*0.4/alog(0.5*dzn(1)/0.1)/(0.5*dzn(1))/0.4*v(i,j,1)/vspd(i,j)
       cov6(i,j,1) = nou6(i,j,1)*diu6(i,j,1)
#else
#ifndef WV_NEW_LES2
       nou6(i,j,1) = 0.5*(dy1(j+1)*w(i,j,1)+dy1(j)*w(i,j+1,1))/(dy1(j)+dy1(j+1))
       diu6_ij1=vspd(i,j)*0.4/alog(0.5*dzn(1)/0.1)/(0.5*dzn(1))/0.4*v(i,j,1)/vspd(i,j)
       cov6(i,j,1) = nou6(i,j,1)*diu6_ij1
#else
#ifndef WV_NEW_VEL2
       nou6_ij1 = 0.5*(dy1(j+1)*w(i,j,1)+dy1(j)*w(i,j+1,1))/(dy1(j)+dy1(j+1))
       diu6_ij1=vspd(i,j)*0.4/alog(0.5*dzn(1)/0.1)/(0.5*dzn(1))/0.4*v(i,j,1)/vspd(i,j)
       cov6(i,j,1) = nou6_ij1*diu6_ij1
#endif
#endif
#endif
      end do
      end do
      do k = 1,kp-1
      do j = 1,jp
      do i = 1,ip
#ifndef WV_NEW_LES
        nou7(i,j,k) = (dzn(k+1)*u(i-1,j,k)+dzn(k)*u(i-1,j,k+1)) /(dzn(k)+dzn(k+1))
        diu7(i,j,k) = 2.*(-w(i-1,j,k)+w(i,j,k))/(dx1(i-1)+dx1(i))
        cov7(i,j,k) = (nou7(i,j,k)-u0)*diu7(i,j,k)
#else
#ifndef WV_NEW_LES2
        nou7(i,j,k) = (dzn(k+1)*u(i-1,j,k)+dzn(k)*u(i-1,j,k+1)) /(dzn(k)+dzn(k+1))
        diu7_ = 2.*(-w(i-1,j,k)+w(i,j,k))/(dx1(i-1)+dx1(i))
        cov7(i,j,k) = (nou7(i,j,k)-u0)*diu7_
#else
        nou7_ = (dzn(k+1)*u(i-1,j,k)+dzn(k)*u(i-1,j,k+1)) /(dzn(k)+dzn(k+1))
        diu7_ = 2.*(-w(i-1,j,k)+w(i,j,k))/(dx1(i-1)+dx1(i))
        cov7(i,j,k) = (nou7_-u0)*diu7_
#ifdef WV_NEW_VEL2
        if (i==ip) then
        cov7(ip+1,j,k) = cov7(ip,j,k)
        end if
#endif
#endif
#endif
      end do
      end do
      end do
!
      do k = 1,kp-1
      do j = 1,jp
      do i = 1,ip
#ifndef WV_NEW_LES
        nou8(i,j,k) = (dzn(k+1)*v(i,j-1,k)+dzn(k)*v(i,j-1,k+1)) /(dzn(k)+dzn(k+1))
        diu8(i,j,k) = 2.*(-w(i,j-1,k)+w(i,j,k))/(dy1(j-1)+dy1(j))
        cov8(i,j,k) = nou8(i,j,k)*diu8(i,j,k)
#else
#ifndef WV_NEW_LES2
        nou8(i,j,k) = (dzn(k+1)*v(i,j-1,k)+dzn(k)*v(i,j-1,k+1)) /(dzn(k)+dzn(k+1))
        diu8_ = 2.*(-w(i,j-1,k)+w(i,j,k))/(dy1(j-1)+dy1(j))
        cov8(i,j,k) = nou8(i,j,k)*diu8_
#else
        nou8_ = (dzn(k+1)*v(i,j-1,k)+dzn(k)*v(i,j-1,k+1)) /(dzn(k)+dzn(k+1))
        diu8_ = 2.*(-w(i,j-1,k)+w(i,j,k))/(dy1(j-1)+dy1(j))
        cov8(i,j,k) = nou8_*diu8_
#ifndef WV_NEW_LES2
        if (j==1) then
        cov8(i,jp+1,k) = cov8(i,1,k)
        end if
#endif
#endif
#endif
      end do
      end do
      end do
! ====================================
! This again could be combined in the main loop
#ifdef MPI
    if (isBottomRow(procPerRow)) then
#endif
      do k = 1,kp
      do j = 1,jp
#ifndef WV_NEW_LES2
        nou1(ip+1,j,k) = nou1(ip,j,k)
#endif
#ifndef WV_NEW_LES
        diu1(ip+1,j,k) = diu1(ip,j,k)
#endif
#ifndef WV_NEW_VEL2
        cov1(ip+1,j,k) = cov1(ip,j,k)
#endif
      end do
      end do
#ifdef MPI
    end if
#endif
! And again
#if !defined(MPI) || (PROC_PER_ROW==1)
      do k = 1,kp
      do i = 1,ip
#ifndef WV_NEW_LES2
        nou2(i,0,k) = nou2(i,jp,k)
#endif
#ifndef WV_NEW_LES
        diu2(i,0,k) = diu2(i,jp,k)
#endif
#ifndef WV_NEW_VEL2
        cov2(i,0,k) = cov2(i,jp,k)
#endif
#ifndef WV_NEW_LES2
        nou2(i,jp+1,k) = nou2(i,1,k)
#endif
#ifndef WV_NEW_LES
        diu2(i,jp+1,k) = diu2(i,1,k)
#endif
#ifndef WV_NEW_VEL2
        cov2(i,jp+1,k) = cov2(i,1,k)
#endif
      end do
      end do
#else
#if !defined( WV_NEW_LES ) && !defined( WV_NEW_LES2 )
    call sideflowRightLeft(nou2, procPerRow, jp+1, 1, 1, 2, 1, 2)
    call sideflowRightLeft(diu2, procPerRow, jp+1, 1, 1, 2, 1, 2)
    call sideflowRightLeft(cov2, procPerRow, jp+1, 1, 1, 2, 1, 2)
    call sideflowLeftRight(nou2, procPerRow, 2, jp+2, 1, 2, 1, 2)
    call sideflowLeftRight(diu2, procPerRow, 2, jp+2, 1, 2, 1, 2)
    call sideflowLeftRight(cov2, procPerRow, 2, jp+2, 1, 2, 1, 2)
#endif
#endif
! Can be merged
#ifdef MPI
    if (isBottomRow(procPerRow)) then
#endif
      do k = 1,kp
      do j = 1,jp
#ifndef WV_NEW_LES
        nou4(ip+1,j,k) = nou4(ip,j,k)
        diu4(ip+1,j,k) = diu4(ip,j,k)
        cov4(ip+1,j,k) = cov4(ip,j,k)
#else
#ifndef WV_NEW_LES2
        nou4(ip+1,j,k) = nou4(ip,j,k)
#endif
#ifndef WV_NEW_VEL2
        cov4(ip+1,j,k) = cov4(ip,j,k)
#endif
#endif
      end do
      end do
#ifdef MPI
    end if
#endif
! Can be merged
#if !defined(MPI) || (PROC_PER_ROW==1)
      do k = 1,kp
      do i = 1,ip
#ifndef WV_NEW_LES
        nou5(i,0,k) = nou5(i,jp,k)
        diu5(i,0,k) = diu5(i,jp,k)
        cov5(i,0,k) = cov5(i,jp,k)
        nou5(i,jp+1,k) = nou5(i,1,k)
        diu5(i,jp+1,k) = diu5(i,1,k)
        cov5(i,jp+1,k) = cov5(i,1,k)
#else
#ifndef WV_NEW_LES2
        nou5(i,0,k) = nou5(i,jp,k)
        nou5(i,jp+1,k) = nou5(i,1,k)
#endif
#ifndef WV_NEW_VEL2
        cov5(i,0,k) = cov5(i,jp,k)
        cov5(i,jp+1,k) = cov5(i,1,k)
#endif
#endif
      end do
      end do
#else
#if !defined( WV_NEW_LES ) && !defined( WV_NEW_LES2 )
    call sideflowRightLeft(nou5, procPerRow, jp+2, 2, 2, 2, 1, 2)
    call sideflowRightLeft(diu5, procPerRow, jp+2, 2, 2, 2, 1, 2)
    call sideflowRightLeft(cov5, procPerRow, jp+2, 2, 2, 2, 1, 2)
    call sideflowLeftRight(nou5, procPerRow, 3, jp+3, 2, 2, 1, 2)
    call sideflowLeftRight(diu5, procPerRow, 3, jp+3, 2, 2, 1, 2)
    call sideflowLeftRight(cov5, procPerRow, 3, jp+3, 2, 2, 1, 2)
#endif
#endif
#ifdef MPI
    if (isBottomRow(procPerRow)) then
#endif
      do k = 1,kp-1
      do j = 1,jp
#ifndef WV_NEW_LES
        nou7(ip+1,j,k) = nou7(ip,j,k)
        diu7(ip+1,j,k) = diu7(ip,j,k)
        cov7(ip+1,j,k) = cov7(ip,j,k)
#else
#ifndef WV_NEW_LES2
        nou7(ip+1,j,k) = nou7(ip,j,k)
#endif
#ifndef WV_NEW_VEL2
        cov7(ip+1,j,k) = cov7(ip,j,k)
#endif
#endif
      end do
      end do
#ifdef MPI
    end if
#endif
#if !defined(MPI) || (PROC_PER_ROW==1)
      do k = 1,kp-1
      do i = 1,ip
#ifndef WV_NEW_LES
        nou8(i,0,k) = nou8(i,jp,k)
        diu8(i,0,k) = diu8(i,jp,k)
        cov8(i,0,k) = cov8(i,jp,k)
        nou8(i,jp+1,k) = nou8(i,1,k)
        diu8(i,jp+1,k) = diu8(i,1,k)
        cov8(i,jp+1,k) = cov8(i,1,k)
#else
#ifndef WV_NEW_LES2
        nou8(i,0,k) = nou8(i,jp,k)
#endif
#ifndef WV_NEW_VEL2
        cov8(i,0,k) = cov8(i,jp,k)
#endif
#ifndef WV_NEW_LES2
        nou8(i,jp+1,k) = nou8(i,1,k)
#endif
#ifndef WV_NEW_VEL2
        cov8(i,jp+1,k) = cov8(i,1,k)
#endif
#endif
      end do
      end do
#else
#if !defined( WV_NEW_LES ) && !defined( WV_NEW_LES2 )
    call sideflowRightLeft(nou8, procPerRow, jp+1, 1, 1, 2, 1, 3)
    call sideflowRightLeft(diu8, procPerRow, jp+1, 1, 1, 2, 1, 3)
    call sideflowRightLeft(cov8, procPerRow, jp+1, 1, 1, 2, 1, 3)
    call sideflowLeftRight(nou8, procPerRow, 2, jp+2, 1, 2, 1, 3)
    call sideflowLeftRight(diu8, procPerRow, 2, jp+2, 1, 2, 1, 3)
    call sideflowLeftRight(cov8, procPerRow, 2, jp+2, 1, 2, 1, 3)
#endif
#endif
! --les
#ifdef MPI
    if (isBottomRow(procPerRow)) then
#endif
      do k = 1,kp+1
      do j = 1,jp+1
#ifndef WV_NEW_LES
        diu2(ip+1,j,k) = diu2(ip,j,k)
        diu3(ip+1,j,k) = diu3(ip,j,k)
#else
#endif
      end do
      end do
#ifdef MPI
    end if
    if (isTopRow(procPerRow)) then
#endif
      do k = 1,kp+1
      do j = 1,jp+1
#ifndef WV_NEW_LES
        diu2(0,j,k) = diu2(1,j,k)
        diu3(0,j,k) = diu3(1,j,k)
#endif
      end do
      end do
#ifdef MPI
    end if
#endif
#if !defined(MPI) || (PROC_PER_ROW==1)
      do k = 1,kp+1
      do i = 1,ip+1
#ifndef WV_NEW_LES
        diu4(i,0,k) = diu4(i,jp,k)
        diu6(i,0,k) = diu6(i,jp,k)
#endif
      end do
      end do
#else
#ifndef WV_NEW_LES
    call sideflowRightLeft(diu4, procPerRow, jp+1, 1, 1, 1, 1, 1)
    call sideflowRightLeft(diu6, procPerRow, jp+1, 1, 1, 1, 1, 1)
#endif
#endif
#ifdef MPI
#ifdef NESTED_LES
   if (syncTicks == 0) then
#endif
#if !defined( WV_NEW_LES ) && !defined( WV_NEW_LES2 ) && !defined( WV_NEW_VELFG )
    call exchangeRealHalos(nou1, procPerRow, neighbours, 1, 2, 2, 2)
    call exchangeRealHalos(diu1, procPerRow, neighbours, 1, 2, 2, 2)
    call exchangeRealHalos(cov1, procPerRow, neighbours, 1, 2, 2, 2)
    call exchangeRealHalos(nou2, procPerRow, neighbours, 1, 2, 1, 2)
    call exchangeRealHalos(diu2, procPerRow, neighbours, 1, 2, 1, 2)
    call exchangeRealHalos(cov2, procPerRow, neighbours, 1, 2, 1, 2)
    call exchangeRealHalos(nou3, procPerRow, neighbours, 1, 2, 1, 2)
    call exchangeRealHalos(diu3, procPerRow, neighbours, 1, 2, 1, 2)
    call exchangeRealHalos(cov3, procPerRow, neighbours, 1, 2, 1, 2)
    call exchangeRealHalos(nou4, procPerRow, neighbours, 1, 2, 1, 2)
    call exchangeRealHalos(diu4, procPerRow, neighbours, 1, 2, 1, 2)
    call exchangeRealHalos(cov4, procPerRow, neighbours, 1, 2, 1, 2)
    call exchangeRealHalos(nou5, procPerRow, neighbours, 1, 2, 2, 2)
    call exchangeRealHalos(diu5, procPerRow, neighbours, 1, 2, 2, 2)
    call exchangeRealHalos(cov5, procPerRow, neighbours, 1, 2, 2, 2)
    call exchangeRealHalos(nou6, procPerRow, neighbours, 1, 2, 1, 2)
    call exchangeRealHalos(diu6, procPerRow, neighbours, 1, 2, 1, 2)
    call exchangeRealHalos(cov6, procPerRow, neighbours, 1, 2, 1, 2)
    call exchangeRealHalos(nou7, procPerRow, neighbours, 1, 2, 1, 2)
    call exchangeRealHalos(diu7, procPerRow, neighbours, 1, 2, 1, 2)
    call exchangeRealHalos(cov7, procPerRow, neighbours, 1, 2, 1, 2)
    call exchangeRealHalos(nou8, procPerRow, neighbours, 1, 2, 1, 2)
    call exchangeRealHalos(diu8, procPerRow, neighbours, 1, 2, 1, 2)
    call exchangeRealHalos(cov8, procPerRow, neighbours, 1, 2, 1, 2)
    call exchangeRealHalos(nou9, procPerRow, neighbours, 1, 2, 1, 2)
    call exchangeRealHalos(diu9, procPerRow, neighbours, 1, 2, 1, 2)
    call exchangeRealHalos(cov9, procPerRow, neighbours, 1, 2, 1, 2)
#endif
#ifdef NESTED_LES
   end if
#endif
#endif
!uspd,vspd
!        do j=1,jp
!        do i=1,ip
!         uspd(i,j)=u(i,j,1)/uspd(i,j)
!         vspd(i,j)=v(i,j,1)/vspd(i,j)
!        end do
!        end do
#ifdef WV_DEBUG
    print *, 'F95 DIU SUMS:',sum(diu1),sum(diu2),sum(diu3),sum(diu4),sum(diu5),sum(diu6),sum(diu7),sum(diu8),sum(diu9)
#endif
end subroutine vel2
end module module_vel2
