module module_velFG
#ifdef MPI
    use communication_helper_mpi
#endif
#ifndef WV_NEW_VELFG
!_VELFG
    use module_vel2 ! add_module_decls() line 156
#endif
#ifdef WV_NEW_VELFG
    use module_bondfg
#endif
    implicit none

contains
#ifndef WV_NEW_VELFG
!_VELFG
!      subroutine velfg(dx1,cov1,cov2,cov3,dfu1,diu1,diu2,dy1,diu3,dzn,vn,f,cov4,cov5,cov6, &
!      dfv1,diu4,diu5,diu6,g,cov7,cov8,cov9,dfw1,diu7,diu8,diu9,dzs,h,nou1,u,nou5,v,nou9,w,nou2, &
!      nou3,nou4,nou6,nou7,nou8,uspd,vspd)
        subroutine velfg(dx1,dy1,dzn,f,g,h,u,v,w, &
        dfu1,dfv1,dfw1,vn,dzs, &
#ifndef WV_NEW_LES
        diu1,diu2,diu3,diu4,diu5,diu6,diu7,diu8,diu9, &
#endif
        cov1,cov2,cov3,cov4,cov5,cov6,cov7,cov8,cov9, &
#ifndef WV_NEW_LES2
        nou1,nou2,nou3,nou4,nou5,nou6,nou7,nou8,nou9, &
#endif
        uspd,vspd) !WV: calls vel2 which uses halos

#else
      subroutine velfg(dx1,dy1,dzn,f, &
      g,dzs,h,u,v,w, &
      uspd,vspd)
#endif
#ifdef WV_NEW
    use params_common_sn
    implicit none
#else
    use common_sn ! create_new_include_statements() line 102
#endif

#ifndef WV_NEW_VELFG
!_VELFG
        real(kind=4), dimension(-1:ip+2,0:jp+2,0:kp+2) , intent(Out) :: cov1
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(Out) :: cov2
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(Out) :: cov3
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(Out) :: cov4
        real(kind=4), dimension(-1:ip+2,0:jp+2,0:kp+2) , intent(Out) :: cov5
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(Out) :: cov6
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(Out) :: cov7
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(Out) :: cov8
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(Out) :: cov9
        real(kind=4), dimension(0:ip,jp,kp) , intent(Out) :: dfu1
        real(kind=4), dimension(ip,0:jp,kp) , intent(Out) :: dfv1
        real(kind=4), dimension(ip,jp,kp) , intent(Out) :: dfw1
        real(kind=4), intent(In) :: vn
#endif
#ifndef WV_NEW_VELFG
#ifndef WV_NEW_LES
!_VELFG
        real(kind=4), dimension(-1:ip+2,0:jp+2,0:kp+2) , intent(Out) :: diu1
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(Out) :: diu2
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(Out) :: diu3
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(Out) :: diu4
        real(kind=4), dimension(-1:ip+2,0:jp+2,0:kp+2) , intent(Out) :: diu5
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(Out) :: diu6
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(Out) :: diu7
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(Out) :: diu8
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(Out) :: diu9
#endif
#endif
        real(kind=4), dimension(-1:ip+1) , intent(In) :: dx1
        real(kind=4), dimension(0:jp+1) , intent(In) :: dy1
        real(kind=4), dimension(-1:kp+2) , intent(In) :: dzn
        real(kind=4), dimension(-1:kp+2) , intent(In) :: dzs
        real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(Out) :: f
        real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(Out) :: g
        real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(Out) :: h
#ifndef WV_NEW_VELFG
!_VELFG
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
#endif
#endif
        real(kind=4) :: nou1_i,nou2_j,nou3_k, nou4_i,nou5_j,nou6_k, nou7_i,nou8_j,nou9_k
        real(kind=4) :: diu1_i,diu2_j,diu3_k, diu4_i,diu5_j,diu6_k, diu7_i,diu8_j,diu9_k
        real(kind=4) :: cov1_i,cov2_j,cov3_k, cov4_i,cov5_j,cov6_k, cov7_i,cov8_j,cov9_k
        real(kind=4) :: nou1_ip1,nou2_jp1,nou3_kp1,nou4_ip1,nou5_jp1,nou6_kp1,nou7_ip1,nou8_jp1,nou9_kp1
        real(kind=4) :: diu1_ip1,diu2_jp1,diu3_kp1,diu4_ip1,diu5_jp1,diu6_kp1,diu7_ip1,diu8_jp1,diu9_kp1
        real(kind=4) :: cov1_ip1,cov2_jp1,cov3_kp1,cov4_ip1,cov5_jp1,cov6_kp1,cov7_ip1,cov8_jp1,cov9_kp1

        real(kind=4), dimension(0:ip+1,-1:jp+1,0:kp+1) , intent(In) :: u
        real(kind=4), dimension(0:ip+1,-1:jp+1,0:kp+1) , intent(In) :: v
        real(kind=4), dimension(0:ip+1,-1:jp+1,-1:kp+1) , intent(In) :: w

!wall function
        real(kind=4), dimension(0:ip+1,0:jp+1) , intent(out) :: uspd
        real(kind=4), dimension(0:ip+1,0:jp+1) , intent(out) :: vspd
 
        integer :: i,j,k
        real(kind=4) :: covc,covx1,covy1,covz1
#ifdef WV_NEW_VELFG
!_VELFG
        integer, parameter  :: u0 = 0
#endif
!
!
#ifndef WV_NEW_VELFG
      call vel2( &
#ifndef WV_NEW_LES2
            nou1,nou5,nou9,nou2,nou3,nou4,nou6,nou7,nou8,&
#endif
#ifndef WV_NEW_LES
            diu1,diu2,diu3,diu4,diu5,diu6,diu7,diu8,diu9,&
#endif
            cov1,cov2,cov3,cov4,cov5,cov6,cov7,cov8,cov9,&
            u,v,w,dx1,dy1,dzn,dzs,uspd,vspd)
#else
!
!wall function

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
#endif

! --u velocity
      do k = 1,kp
      do j = 1,jp
      do i = 1,ip
        ! So here we could say:
        ! calculate cov1:
#ifdef WV_NEW_VELFG
!_VELFG
        nou1_i = (u(i-1,j,k)+u(i,j,k))/2.
        diu1_i = (-u(i-1,j,k)+u(i,j,k))/dx1(i)
        cov1_i = nou1_i*diu1_i

        nou1_ip1 = (u(i,j,k)+u(i+1,j,k))/2.
        diu1_ip1 = (-u(i,j,k)+u(i+1,j,k))/dx1(i+1)
        cov1_ip1 = nou1_ip1*diu1_ip1

        nou2_j = (dx1(i+1)*v(i,j-1,k)+dx1(i)*v(i+1,j-1,k)) /(dx1(i)+dx1(i+1))
        diu2_j = 2.*(-u(i,j-1,k)+u(i,j,k))/(dy1(j-1)+dy1(j))
        cov2_j = nou2_j*diu2_j

        nou2_jp1 = (dx1(i+1)*v(i,j,k)+dx1(i)*v(i+1,j,k)) /(dx1(i)+dx1(i+1))
        diu2_jp1 = 2.*(-u(i,j,k)+u(i,j+1,k))/(dy1(j)+dy1(j+1))
        cov2_jp1 = nou2_jp1*diu2_jp1



        nou3_kp1 = (dx1(i+1)*w(i,j,k)+dx1(i)*w(i+1,j,k)) /(dx1(i)+dx1(i+1))
        diu3_kp1 = (-u(i,j,k)+u(i,j,k+1))/dzs(k)
        cov3_kp1 = nou3_kp1*diu3_kp1

        if (k==1) then
        nou3_k = 0.5*(dx1(i+1)*w(i,j,1)+dx1(i)*w(i+1,j,1))/(dx1(i)+dx1(i+1))
        diu3_k = uspd(i,j)*0.4/alog(0.5*dzn(1)/0.1)/(0.5*dzn(1))/0.4*u(i,j,1)/uspd(i,j)
        else
        nou3_k = (dx1(i+1)*w(i,j,k-1)+dx1(i)*w(i+1,j,k-1)) /(dx1(i)+dx1(i+1))
        diu3_k = (-u(i,j,k-1)+u(i,j,k))/dzs(k-1)
        end if
        cov3_k = nou3_k*diu3_k

        covx1 = (dx1(i+1)*cov1_i+dx1(i)*cov1_ip1) /(dx1(i)+dx1(i+1))
        covy1 = (cov2_j+cov2_jp1)/2.
        covz1 = (cov3_k+cov3_kp1)/2.

#else
        covx1 = (dx1(i+1)*cov1(i,j,k)+dx1(i)*cov1(i+1,j,k)) /(dx1(i)+dx1(i+1))
        covy1 = (cov2(i,j,k)+cov2(i,j+1,k))/2.
        covz1 = (cov3(i,j,k)+cov3(i,j,k+1))/2.
#endif
        covc = covx1+covy1+covz1
!-- molecular viscous term is neglected
!        dfu1(i,j,k) = 2.*(-diu1(i,j,k)+diu1(i+1,j,k))/(dx1(i)+dx1(i+1))  +   (-diu2(i,j,k)+diu2(i, &
!      j+1,k))/dy1(j) +   (-diu3(i,j,k)+diu3(i,j,k+1))/dzn(k)
!        df = vn*dfu1(i,j,k)
!        f(i,j,k) = (-covc+df)
!--
        f(i,j,k) = (-covc)
      end do
      end do
      end do
! =======================================
! --v velocity
      do k = 1,kp
      do j = 1,jp
      do i = 1,ip
#ifdef WV_NEW_VELFG
!_VELFG
        nou4_i = (dy1(j+1)*u(i-1,j,k)+dy1(j)*u(i-1,j+1,k)) /(dy1(j)+dy1(j+1))
        diu4_i = 2.*(-v(i-1,j,k)+v(i,j,k))/(dx1(i-1)+dx1(i))
        cov4_i = (nou4_i-u0)*diu4_i

        nou4_ip1 = (dy1(j+1)*u(i,j,k)+dy1(j)*u(i,j+1,k)) /(dy1(j)+dy1(j+1))
        diu4_ip1 = 2.*(-v(i,j,k)+v(i+1,j,k))/(dx1(i)+dx1(i+1))
        cov4_ip1 = (nou4_ip1-u0)*diu4_ip1

        nou5_j = ( v(i,j-1,k)+v(i,j,k))/2.
        diu5_j = (-v(i,j-1,k)+v(i,j,k))/dy1(j)
        cov5_j = nou5_j*diu5_j

        nou5_jp1 = ( v(i,j,k)+v(i,j+1,k))/2.
        diu5_jp1 = (-v(i,j,k)+v(i,j+1,k))/dy1(j+1)
        cov5_jp1 = nou5_jp1*diu5_jp1

        nou6_k = (dy1(j+1)*w(i,j,k-1)+dy1(j)*w(i,j+1,k-1)) /(dy1(j)+dy1(j+1))
        diu6_k = (-v(i,j,k-1)+v(i,j,k))/dzs(k-1)
        cov6_k = nou6_k*diu6_k

        if (k==1) then
        nou6_k = 0.5*(dy1(j+1)*w(i,j,1)+dy1(j)*w(i,j+1,1))/(dy1(j)+dy1(j+1))
        diu6_k=vspd(i,j)*0.4/alog(0.5*dzn(1)/0.1)/(0.5*dzn(1))/0.4*v(i,j,1)/vspd(i,j)
        else
        nou6_k = (dy1(j+1)*w(i,j,k-1)+dy1(j)*w(i,j+1,k-1)) /(dy1(j)+dy1(j+1))
        diu6_k = (-v(i,j,k-1)+v(i,j,k))/dzs(k-1)
        end if
        cov6_k = nou6_k*diu6_k

        nou6_kp1 = (dy1(j+1)*w(i,j,k)+dy1(j)*w(i,j+1,k)) /(dy1(j)+dy1(j+1))
        diu6_kp1 = (-v(i,j,k)+v(i,j,k+1))/dzs(k)
        cov6_kp1 = nou6_kp1*diu6_kp1

        covx1 = (cov4_i+cov4_ip1)/2.
        covy1 = (dy1(j+1)*cov5_j+dy1(j)*cov5_jp1) /(dy1(j)+dy1(j+1))
        covz1 = (cov6_k+cov6_kp1)/2.
#else
        covx1 = (cov4(i,j,k)+cov4(i+1,j,k))/2.
        covy1 = (dy1(j+1)*cov5(i,j,k)+dy1(j)*cov5(i,j+1,k)) /(dy1(j)+dy1(j+1))
        covz1 = (cov6(i,j,k)+cov6(i,j,k+1))/2.
#endif
        covc = covx1+covy1+covz1
!-- molecular viscous term is neglected
!        dfv1(i,j,k) = (-diu4(i,j,k)+diu4(i+1,j,k))/dx1(i)  +2.*(-diu5(i,j,k)+diu5(i,j+1, &
!      k))/(dy1(j)+dy1(j+1)) +(-diu6(i,j,k)+diu6(i,j,k+1))/dzn(k)
!        df = vn*dfv1(i,j,k) 
!        g(i,j,k) = (-covc+df)
!--
        g(i,j,k) = (-covc)
      end do
      end do
      end do
!
! =======================================
! --w velocity
      do k = 1,kp-1
      do j = 1,jp
      do i = 1,ip
#ifdef WV_NEW_VELFG
!_VELFG
        nou7_i = (dzn(k+1)*u(i-1,j,k)+dzn(k)*u(i-1,j,k+1)) /(dzn(k)+dzn(k+1))
        diu7_i = 2.*(-w(i-1,j,k)+w(i,j,k))/(dx1(i-1)+dx1(i))
        cov7_i = (nou7_i-u0)*diu7_i

        nou7_ip1 = (dzn(k+1)*u(i,j,k)+dzn(k)*u(i,j,k+1)) /(dzn(k)+dzn(k+1))
        diu7_ip1 = 2.*(-w(i,j,k)+w(i+1,j,k))/(dx1(i)+dx1(i+1))
        cov7_ip1 = (nou7_ip1-u0)*diu7_ip1

        nou8_j = (dzn(k+1)*v(i,j-1,k)+dzn(k)*v(i,j-1,k+1)) /(dzn(k)+dzn(k+1))
        diu8_j = 2.*(-w(i,j-1,k)+w(i,j,k))/(dy1(j-1)+dy1(j))
        cov8_j = nou8_j*diu8_j

        nou8_jp1 = (dzn(k+1)*v(i,j,k)+dzn(k)*v(i,j,k+1)) /(dzn(k)+dzn(k+1))
        diu8_jp1 = 2.*(-w(i,j,k)+w(i,j+1,k))/(dy1(j)+dy1(j+1))
        cov8_jp1 = nou8_jp1*diu8_jp1

        nou9_k = ( w(i,j,k-1)+w(i,j,k))/2.
        diu9_k = (-w(i,j,k-1)+w(i,j,k))/dzn(k)
        cov9_k = nou9_k*diu9_k

        nou9_kp1 = ( w(i,j,k)+w(i,j,k+1))/2.
        diu9_kp1 = (-w(i,j,k)+w(i,j,k+1))/dzn(k+1)
        cov9_kp1 = nou9_kp1*diu9_kp1

       covx1 = (cov7_i+cov7_ip1)/2.
       covy1 = (cov8_j+cov8_jp1)/2.
       covz1 = (dzn(k+1)*cov9_k+dzn(k)*cov9_kp1) /(dzn(k)+dzn(k+1))
#else
       covx1 = (cov7(i,j,k)+cov7(i+1,j,k))/2.
       covy1 = (cov8(i,j,k)+cov8(i,j+1,k))/2.
       covz1 = (dzn(k+1)*cov9(i,j,k)+dzn(k)*cov9(i,j,k+1)) /(dzn(k)+dzn(k+1))

#endif
       covc = covx1+covy1+covz1
!-- molecular viscous term is neglected
!        dfw1(i,j,k) = (-diu7(i,j,k)+diu7(i+1,j,k))/dx1(i)  +(-diu8(i,j,k)+diu8(i,j+1, &
!      k))/dy1(j) +(-diu9(i,j,k)+diu9(i,j,k+1))/dzs(k)
!        df = vn*dfw1(i,j,k)  
!        h(i,j,k) = (-covc+df)
!--
        h(i,j,k) = (-covc)
      end do
      end do
      end do
! WV: This seems not necessary, perhaps because it is called in press.
#ifdef WV_NEW_VELFG
#if !defined( INLINE_BOUND_CALCS ) || defined( MPI )
    call bondfg(f,g,h)
#else
    ! --inflow condition
        do k = 1,kp
            do j = 1,jp
                f( 0,j,k) = f(1  ,j,k)
            end do
        end do
! --sideflow condition
    do k = 1,kp
        do i = 1,ip
            g(i, 0,k) = g(i,jp  ,k) ! GR: Why only right->left? What about left->right?
        end do
    end do
! --ground and top condition
    do j = 1,jp
        do i = 1,ip
            h(i,j, 0) = 0.0
            h(i,j,kp) = 0.0
        end do
    end do
#endif
#endif
!
! =======================================
#ifdef WV_DEBUG
    print *, 'F95 FGHSUM after velfg:',sum(f)+sum(g)+sum(h)
    print *, 'F95 FSUM after velfg:',sum(f)
    print *, 'F95 GSUM after velfg:',sum(g)
    print *, 'F95 HSUM after velfg:',sum(h)
    print *, 'F95 UVWSUM after velfg:', sum(u)+sum(v)+sum(w)
    print *, 'F95 USUM after velfg:', sum(u)
    print *, 'F95 VSUM after velfg:', sum(v)
    print *, 'F95 WSUM after velfg:', sum(w)
#endif
      return
      end subroutine velFG

end module module_velFG
