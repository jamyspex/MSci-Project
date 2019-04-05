module module_les

      use module_boundsm ! add_module_decls() line 156
      implicit none
contains

#ifndef WV_NEW_LES
      subroutine les(delx1,dx1,dy1,dzn,diu1,diu2,diu3,diu4,diu5,diu6,diu7,diu8,diu9,sm,f,g, &
      h,u,v,uspd,vspd,dxs,dys,n)
#else
        subroutine les(u,v,w,f,g,h,sm,dx1,dy1,dzn,dzs,dxs,dys)
#endif
#ifdef WV_NEW
    use params_common_sn
    implicit none
#else
    use common_sn ! create_new_include_statements() line 102
#endif
#ifndef WV_NEW_LES
        real(kind=4), dimension(kp) , intent(Out) :: delx1
        real(kind=4), dimension(-1:ip+2,0:jp+2,0:kp+2) , intent(In) :: diu1
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(In) :: diu2
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(In) :: diu3
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(In) :: diu4
        real(kind=4), dimension(-1:ip+2,0:jp+2,0:kp+2) , intent(In) :: diu5
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(In) :: diu6
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(In) :: diu7
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(In) :: diu8
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(In) :: diu9
        integer, intent(In) :: n
#else
        real(kind=4), dimension(kp) :: delx1
        real(kind=4) :: diu1_i_j_k, diu2_i_j_k,diu3_i_j_k,diu4_i_j_k,diu5_i_j_k,diu6_i_j_k,diu7_i_j_k,diu8_i_j_k,diu9_i_j_k
        real(kind=4) :: diu1_ip1_j_k, diu2_im1_j_k , diu2_im1_jp1_k , diu2_i_jp1_k , diu3_im1_j_k , diu3_im1_j_kp1 , diu3_i_j_kp1
        real(kind=4) :: diu4_i_jm1_k , diu4_ip1_j_k , diu4_ip1_jm1_k , diu5_i_jp1_k
        real(kind=4) :: diu6_i_jm1_k , diu6_i_jm1_kp1 , diu6_i_j_kp1 , diu7_i_j_km1 , diu7_ip1_j_k , diu7_ip1_j_km1
        real(kind=4) :: diu8_i_j_km1 , diu8_i_jp1_k , diu8_i_jp1_km1, diu9_i_j_kp1
        real(kind=4) :: diu1_i_j_1, diu1_ip1_j_1, diu2_i_j_1, diu2_i_jp1_1, diu4_ip1_j_1, diu2_im1_jp1_1
        real(kind=4) :: diu3_i_j_2, diu4_i_j_1, diu4_ip1_jm1_1, diu5_i_jp1_1, diu5_i_j_1, diu6_i_j_2, diu7_ip1_j_1, diu8_i_jp1_1
#endif
#ifdef WV_NEW_LES
        real(kind=4), dimension(-1:kp+2)  :: dzs
#endif
        real(kind=4), dimension(-1:ip+1) , intent(In) :: dx1
        real(kind=4), dimension(0:jp+1) , intent(In) :: dy1
        real(kind=4), dimension(-1:kp+2) , intent(In) :: dzn
        real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(InOut) :: f
        real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(InOut) :: g
        real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(InOut) :: h
        real(kind=4), dimension(-1:ip+1,-1:jp+1,0:kp+1) , intent(Out) :: sm

        real(kind=4), dimension(0:ip+1,-1:jp+1,0:kp+1) , intent(In) :: u
        real(kind=4), dimension(0:ip+1,-1:jp+1,0:kp+1) , intent(In) :: v
#ifdef WV_NEW_LES
        real(kind=4), dimension(0:ip+1,-1:jp+1,-1:kp+1) , intent(In)  :: w
#endif
#ifndef WV_NEW_LES
!wall function
        real(kind=4), dimension(0:ip+1,0:jp+1) , intent(in) :: uspd
        real(kind=4), dimension(0:ip+1,0:jp+1) , intent(in) :: vspd
#endif
        real(kind=4), dimension(0:ip) , intent(in) :: dxs
        real(kind=4), dimension(0:jp) , intent(in) :: dys
        integer :: i,j,k
        real(kind=4) :: csx1
        real(kind=4) :: dudxx1 , dudyx1 , dudzx1 , dvdxx1 , dvdyx1 , dvdzx1 , dwdxx1 , dwdyx1 , dwdzx1
        real(kind=4) :: visux2, visux1, visuy2, visuy1, visuz2, visuz1
        real(kind=4) :: visvx2, visvx1, visvy2, visvy1, visvz2, visvz1
        real(kind=4) :: viswx2, viswx1, viswy2, viswy1, viswz2, viswz1
        real(kind=4) :: evsx2, evsx1, evsy2, evsy1, evsz2, evsz1
        real(kind=4) :: vfu,vfv,vfw

#ifdef CALC_BOUNDS
! WV: as this is passed in, why is it defined here?
! --length scale
        do k = 1,kp
!  WV: was          delx1(k)=(dx1(i)*dy1(j)*dzn(k))**(1./3.)
! WV: turns out that dy1(0) is not defined!!!
          delx1(k) = (dx1(0)*dy1(0)*dzn(k))**(1./3.)
        end do
#endif
!WV: so the next loop produces the undefined values ...
! ----
      do k = 1,kp
      do j = 1,jp
      do i = 1,ip
! --calculation of sgs eddy viscosity coeficient
#ifndef WV_NEW_LES
      dudxx1 =  diu1(i,j,k)
      dudyx1 =  (diu2(i-1,j,k)+diu2(i-1,j+1,k) +diu2(i  ,j,k)+diu2(i  ,j+1,k) ) *.25
      dudzx1 =  (diu3(i-1,j,k)+diu3(i-1,j,k+1) +diu3(i  ,j,k)+diu3(i  ,j,k+1) ) *.25
      dvdxx1 =  (diu4(i  ,j,k)+diu4(i  ,j-1,k) +diu4(i+1,j,k)+diu4(i+1,j-1,k) ) *.25
      dvdyx1 =  diu5(i,j,k)
      dvdzx1 =  (diu6(i,j-1,k)+diu6(i,j-1,k+1) +diu6(i,j  ,k)+diu6(i,j  ,k+1) )  *.25
      dwdxx1 =  (diu7(i  ,j,k)+diu7(i  ,j,k-1) +diu7(i+1,j,k)+diu7(i+1,j,k-1) ) *.25
      dwdyx1 =  (diu8(i,j  ,k)+diu8(i,j  ,k-1) +diu8(i,j+1,k)+diu8(i,j+1,k-1) ) *.25
      dwdzx1 =  diu9(i,j,k)
#else
        diu1_i_j_k = (-u(i-1,j,k)+u(i,j,k))/dx1(i) ! i.e. du/dx
        dudxx1 = diu1_i_j_k

        diu2_i_j_k = 2.*(-u(i,j-1,k)+u(i,j,k))/(dy1(j-1)+dy1(j))
        diu2_im1_j_k = 2.*(-u(i-1,j-1,k)+u(i-1,j,k))/(dy1(j-1)+dy1(j))
        diu2_im1_jp1_k = 2.*(-u(i-1,j,k)+u(i-1,j+1,k))/(dy1(j)+dy1(j+1))
        diu2_i_jp1_k = 2.*(-u(i,j,k)+u(i,j+1,k))/(dy1(j)+dy1(j+1))
        dudyx1 =  (diu2_im1_j_k+diu2_im1_jp1_k +diu2_i_j_k+diu2_i_jp1_k) *.25

        diu3_i_j_k = (-u(i,j,k-1)+u(i,j,k))/dzs(k-1)
        diu3_im1_j_k = (-u(i-1,j,k-1)+u(i-1,j,k))/dzs(k-1)
        diu3_im1_j_kp1 = (-u(i-1,j,k)+u(i-1,j,k+1))/dzs(k)
        diu3_i_j_kp1 = (-u(i,j,k)+u(i,j,k+1))/dzs(k)
        dudzx1 =  (diu3_im1_j_k+diu3_im1_j_kp1 +diu3_i_j_k+diu3_i_j_kp1) *.25

        diu4_i_j_k = 2.*(-v(i-1,j,k)+v(i,j,k))/(dx1(i-1)+dx1(i))
        diu4_i_jm1_k = 2.*(-v(i-1,j-1,k)+v(i,j-1,k))/(dx1(i-1)+dx1(i))
        diu4_ip1_j_k = 2.*(-v(i,j,k)+v(i+1,j,k))/(dx1(i)+dx1(i+1))
        diu4_ip1_jm1_k = 2.*(-v(i,j-1,k)+v(i+1,j-1,k))/(dx1(i)+dx1(i+1))
        dvdxx1 =  (diu4_i_j_k+diu4_i_jm1_k +diu4_ip1_j_k+diu4_ip1_jm1_k ) *.25

        diu5_i_j_k = (-v(i,j-1,k)+v(i,j,k))/dy1(j)
        dvdyx1 =  diu5_i_j_k

        diu6_i_j_k = (-v(i,j,k-1)+v(i,j,k))/dzs(k-1)
        diu6_i_jm1_k = (-v(i,j-1,k-1)+v(i,j-1,k))/dzs(k-1)
        diu6_i_jm1_kp1 = (-v(i,j-1,k)+v(i,j-1,k+1))/dzs(k)
        diu6_i_j_kp1 = (-v(i,j,k)+v(i,j,k+1))/dzs(k)
        dvdzx1 =  (diu6_i_jm1_k+diu6_i_jm1_kp1 +diu6_i_j_k+diu6_i_j_kp1 )  *.25

        diu7_i_j_k = 2.*(-w(i-1,j,k)+w(i,j,k))/(dx1(i-1)+dx1(i))
        diu7_i_j_km1 = 2.*(-w(i-1,j,k-1)+w(i,j,k-1))/(dx1(i-1)+dx1(i))
        diu7_ip1_j_k = 2.*(-w(i,j,k)+w(i+1,j,k))/(dx1(i)+dx1(i+1))
        diu7_ip1_j_km1 = 2.*(-w(1,j,k-1)+w(i+1,j,k-1))/(dx1(i)+dx1(i+1))
        dwdxx1 =  (diu7_i_j_k+diu7_i_j_km1 +diu7_ip1_j_k+diu7_ip1_j_km1 ) *.25

        diu8_i_j_k = 2.*(-w(i,j-1,k)+w(i,j,k))/(dy1(j-1)+dy1(j))
        diu8_i_j_km1 = 2.*(-w(i,j-1,k-1)+w(i,j,k-1))/(dy1(j-1)+dy1(j))
        diu8_i_jp1_k = 2.*(-w(i,j,k)+w(i,j+1,k))/(dy1(j)+dy1(j+1))
        diu8_i_jp1_km1 = 2.*(-w(i,j,k-1)+w(i,j+1,k-1))/(dy1(j)+dy1(j+1))
        dwdyx1 =  (diu8_i_j_k+diu8_i_j_km1 +diu8_i_jp1_k+diu8_i_jp1_km1 ) *.25

        diu9_i_j_k = (-w(i,j,k-1)+w(i,j,k))/dzn(k)
        dwdzx1 =  diu9_i_j_k

#endif
      csx1 = cs0
! --abl or channel
      sm(i,j,k) = ( csx1*delx1(k) )**2  * sqrt( 2.*( dudxx1**2+dvdyx1**2+dwdzx1**2 ) +( dudyx1+dvdxx1 )**2  &
      +( dwdyx1+dvdzx1 )**2 +( dudzx1+dwdxx1 )**2 )
      end do
      end do
      end do
!      if (isMaster()) then
!      do i=20,30
!          write(*,*) "sm",sm(i,10,1),i,n
!      end do
!      end if
#ifdef WV_DEBUG
    print *, 'F95 FGHSUM after calc_sm:',sum(f)+sum(g)+sum(h)
    print *, 'F95 FSUM after calc_sm:',sum(f)
    print *, 'F95 GSUM after calc_sm:',sum(g)
    print *, 'F95 HSUM after calc_sm:',sum(h)
#endif

!
#if !defined( INLINE_BOUND_CALCS ) || defined( MPI )
      call boundsm(sm)
#else
#ifdef CALC_BOUNDS
! =================================
        do k = 0,kp+1
            do j = -1,jp+1
                    sm(   0,j,k) = sm(1 ,j,k) ! GR: Why not sm(-1,,) = sm(0,,)?
                    sm(ip+1,j,k) = sm(ip,j,k)
            end do
        end do
! --side flow condition
        do k = 0,kp+1
            do i = 0,ip+1
                    sm(i,jp+1,k) = sm(i,jp  ,k)
                    sm(i,0,k) = sm(i,1   ,k) ! GR: Why not sm(,-1,) = sm(,0,)?
            end do
        end do
! --underground condition
    do j = -1,jp+1
        do i = 0,ip+1
            sm(i,j,   0) = -sm(i,j, 1)
            sm(i,j,kp+1) = sm(i,j,kp)
        end do
    end do
#endif
#endif

#ifdef WV_DEBUG
    print *, 'F95 FGHSUM after boundsm:',sum(f)+sum(g)+sum(h)
    print *, 'F95 FSUM after boundsm:',sum(f)
    print *, 'F95 GSUM after boundsm:',sum(g)
    print *, 'F95 HSUM after boundsm:',sum(h)
#endif
! --calculation of viscosity terms in momentum eq.(x-comp.)
      do k = 2,kp
      do j = 1,jp
      do i = 1,ip
! --eddyviscosity on face
      evsx2 = sm(i+1,j,k)
      evsx1 = sm(i,j,k)
      evsy2 = (dy1(j+1)*((dx1(i+1)*sm(i,j,k)+dx1(i)*sm(i+1,j, &
      k)) /(dx1(i)+dx1(i+1))) +dy1(j)*((dx1(i+1)*sm(i,j+1,k)+dx1(i)*sm(i+1,j+1, &
      k)) /(dx1(i)+dx1(i+1)))) /(dy1(j)+dy1(j+1))
      evsy1 = (dy1(j+1)*((dx1(i+1)*sm(i,j-1,k)+dx1(i)*sm(i+1,j-1, &
      k)) /(dx1(i)+dx1(i+1))) +dy1(j)*((dx1(i+1)*sm(i,j,k)+dx1(i)*sm(i+1,j, &
      k)) /(dx1(i)+dx1(i+1)))) /(dy1(j)+dy1(j+1))
      evsz2 = (dzn(k+1)*((dx1(i+1)*sm(i,j,k)+dx1(i)*sm(i+1,j, &
      k)) /(dx1(i)+dx1(i+1))) +dzn(k)*((dx1(i+1)*sm(i,j,k+1)+dx1(i)*sm(i+1,j, &
      k+1)) /(dx1(i)+dx1(i+1)))) /(dzn(k)+dzn(k+1))
      evsz1 = (dzn(k)*((dx1(i+1)*sm(i,j,k-1)+dx1(i)*sm(i+1,j, &
      k-1)) /(dx1(i)+dx1(i+1))) +dzn(k-1)*((dx1(i+1)*sm(i,j,k)+dx1(i)*sm(i+1,j, &
      k)) /(dx1(i)+dx1(i+1)))) /(dzn(k-1)+dzn(k))
!
#ifndef WV_NEW_LES
      visux2 = (evsx2)*2.*diu1(i+1,j  ,k  )
      visux1 = (evsx1)*2.*diu1(i  ,j,  k  )
      visuy2 = (evsy2)* ( diu2(i  ,j+1,k  )+diu4(i+1,j  ,k  ) )
      visuy1 = (evsy1)* ( diu2(i  ,j  ,k  )+diu4(i+1,j-1,k  ) )
      visuz2 = (evsz2)* ( diu3(i  ,j  ,k+1)+diu7(i+1,j  ,k  ) )
      visuz1 = (evsz1)* ( diu3(i  ,j  ,k  )+diu7(i+1,j  ,k-1) )
#else
      diu1_i_j_k = (-u(i-1,j,k)+u(i,j,k))/dx1(i) ! i.e. du/dx
      diu1_ip1_j_k = (-u(i,j,k)+u(i+1,j,k))/dx1(i+1) ! i.e. du/dx
      visux2 = (evsx2)*2.*diu1_ip1_j_k
      visux1 = (evsx1)*2.*diu1_i_j_k

      diu2_i_jp1_k = 2.*(-u(i,j,k)+u(i,j+1,k))/(dy1(j)+dy1(j+1))
      diu4_ip1_j_k = 2.*(-v(i,j,k)+v(i+1,j,k))/(dx1(i)+dx1(i+1))
      visuy2 = (evsy2)* ( diu2_i_jp1_k + diu4_ip1_j_k  )

      diu2_i_j_k = 2.*(-u(i,j-1,k)+u(i,j,k))/(dy1(j-1)+dy1(j))
      diu4_ip1_jm1_k = 2.*(-v(i,j-1,k)+v(i+1,j-1,k))/(dx1(i)+dx1(i+1))
      visuy1 = (evsy1)* ( diu2_i_j_k  + diu4_ip1_jm1_k  )

      diu3_i_j_kp1 = (-u(i,j,k)+u(i,j,k+1))/dzs(k)
      diu7_ip1_j_k = 2.*(-w(i,j,k)+w(i+1,j,k))/(dx1(i)+dx1(i+1))
      visuz2 = (evsz2)* ( diu3_i_j_kp1 + diu7_ip1_j_k  )

      diu3_i_j_k = (-u(i,j,k-1)+u(i,j,k))/dzs(k-1)
      diu7_ip1_j_km1 = 2.*(-w(1,j,k-1)+w(i+1,j,k-1))/(dx1(i)+dx1(i+1))
      visuz1 = (evsz1)* ( diu3_i_j_k + diu7_ip1_j_km1)
#endif
!
      vfu = (visux2-visux1)/dxs(i) +(visuy2-visuy1)/dy1(j) +(visuz2-visuz1)/dzn(k)
!
      f(i,j,k) = (f(i,j,k)+vfu)
      end do
      end do
      end do

!wall function
#ifdef CALC_BOUNDS
      do j=1,jp
      do i=1,ip
      evsx2=sm(i+1,j,1)
      evsx1=sm(i,j,1)
      evsy2=(dy1(j+1)*((dx1(i+1)*sm(i,j,1)+dx1(i)*sm(i+1,j,1))/(dx1(i)+dx1(i+1)))&
      +dy1(j)*((dx1(i+1)*sm(i,j+1,1)+dx1(i)*sm(i+1,j+1,1))/(dx1(i)+dx1(i+1))))/(dy1(j)+dy1(j+1))
      evsy1=(dy1(j+1)*((dx1(i+1)*sm(i,j-1,1)+dx1(i)*sm(i+1,j-1,1))/(dx1(i)+dx1(i+1)))&
      +dy1(j)*((dx1(i+1)*sm(i,j,1)+dx1(i)*sm(i+1,j,1))/(dx1(i)+dx1(i+1))))/(dy1(j)+dy1(j+1))
      evsz2=(dzn(2)*((dx1(i+1)*sm(i,j,1)+dx1(i)*sm(i+1,j,1))/(dx1(i)+dx1(i+1)))&
      +dzn(1)*((dx1(i+1)*sm(i,j,2)+dx1(i)*sm(i+1,j,2))/(dx1(i)+dx1(i+1))))/(dzn(1)+dzn(2))

#ifndef WV_NEW_LES
      visux2=(evsx2)*2.*diu1(i+1,j  ,1  )
      visux1=(evsx1)*2.*diu1(i  ,j,  1  )
      visuy2=(evsy2)* ( diu2(i  ,j+1,1  )+diu4(i+1,j  ,1 ) )
      visuy1=(evsy1)* ( diu2(i  ,j  ,1  )+diu4(i+1,j-1,1 ) )
      visuz2=(evsz2)* ( diu3(i  ,j  ,2  )+diu7(i+1,j  ,1 ) )
#else
      diu1_i_j_1 = (-u(i-1,j,1)+u(i,j,1))/dx1(i) ! i.e. du/dx
      diu1_ip1_j_1 = (-u(i,j,1)+u(i+1,j,1))/dx1(i+1) ! i.e. du/dx
      visux2=(evsx2)*2.*diu1_ip1_j_1
      visux1=(evsx1)*2.*diu1_i_j_1

      diu2_i_jp1_1 = 2.*(-u(i,j,1)+u(i,j+1,1))/(dy1(j)+dy1(j+1))
      diu4_ip1_j_1 = 2.*(-v(i,j,1)+v(i+1,j,1))/(dx1(i)+dx1(i+1))
      visuy2=(evsy2)* ( diu2_i_jp1_1 + diu4_ip1_j_1 )

      diu2_i_j_1 = 2.*(-u(i,j-1,1)+u(i,j,1))/(dy1(j-1)+dy1(j))
      diu4_ip1_jm1_1 = 2.*(-v(i,j-1,1)+v(i+1,j-1,1))/(dx1(i)+dx1(i+1))
      visuy1=(evsy1)* ( diu2_i_j_1 +diu4_ip1_jm1_1 )

      diu3_i_j_2 = (-u(i,j,1)+u(i,j,2))/dzs(1)
      diu7_ip1_j_1 = 2.*(-w(i,j,1)+w(i+1,j,1))/(dx1(i)+dx1(i+1))
      visuz2=(evsz2)* ( diu3_i_j_2 + diu7_ip1_j_1 )
#endif
#ifndef WV_NEW_LES
      visuz1=(0.4*uspd(i,j)/alog(0.5*dzn(1)/0.1))**2*(u(i,j,1)/uspd(i,j))
#else
      visuz1=(0.4/alog(0.5*dzn(1)/0.1))**2*u(i,j,1)
#endif
!
      vfu= (visux2-visux1)/dxs(i)+(visuy2-visuy1)/dy1(j)+(visuz2-visuz1)/dzn(1)
!
      f(i,j,1)=(f(i,j,1)+vfu)
      end do
      end do
#endif

! --calculation of viscosity terms in momentum eq.(y-comp.)
      do k = 2,kp
      do j = 1,jp
      do i = 1,ip
! --eddyviscosity on face
      evsy2 = sm(i,j+1,k)
      evsy1 = sm(i,j,k)
      evsx2 = (dy1(j+1)*((dx1(i+1)*sm(i,j,k)+dx1(i)*sm(i+1,j, &
      k)) /(dx1(i)+dx1(i+1))) +dy1(j)*((dx1(i+1)*sm(i,j+1,k)+dx1(i)*sm(i+1,j+1, &
      k)) /(dx1(i)+dx1(i+1)))) /(dy1(j)+dy1(j+1))
      evsx1 = (dy1(j+1)*((dx1(i)*sm(i-1,j,k)+dx1(i-1)*sm(i,j, &
      k)) /(dx1(i-1)+dx1(i))) +dy1(j)*((dx1(i)*sm(i-1,j+1,k)+dx1(i-1)*sm(i,j+1, &
      k)) /(dx1(i-1)+dx1(i)))) /(dy1(j)+dy1(j+1))
      evsz2 = (dzn(k+1)*((dx1(i+1)*sm(i,j,k)+dx1(i)*sm(i+1,j, &
      k)) /(dx1(i)+dx1(i+1))) +dzn(k)*((dx1(i+1)*sm(i,j,k+1)+dx1(i)*sm(i+1,j, &
      k+1)) /(dx1(i)+dx1(i+1)))) /(dzn(k)+dzn(k+1))
      evsz1 = (dzn(k)*((dx1(i+1)*sm(i,j,k-1)+dx1(i)*sm(i+1,j, &
      k-1)) /(dx1(i)+dx1(i+1))) +dzn(k-1)*((dx1(i+1)*sm(i,j,k)+dx1(i)*sm(i+1,j, &
      k)) /(dx1(i)+dx1(i+1)))) /(dzn(k-1)+dzn(k))
!
#ifndef WV_NEW_LES
      visvx2 = (evsx2)* ( diu2(i  ,j+1,k  )+diu4(i+1,j  ,k  ) )
      visvx1 = (evsx1)* ( diu2(i-1,j+1,k  )+diu4(i  ,j  ,k  ) )
      visvy2 = (evsy2)*2.*diu5(i  ,j+1,k  )
      visvy1 = (evsy1)*2.*diu5(i  ,j  ,k  )
      visvz2 = (evsz2)* ( diu6(i  ,j  ,k+1)+diu8(i  ,j+1,k  ) )
      visvz1 = (evsz1)* ( diu6(i  ,j  ,k  )+diu8(i  ,j+1,k-1) )
#else
      diu2_i_jp1_k = 2.*(-u(i,j,k)+u(i,j+1,k))/(dy1(j)+dy1(j+1))
      diu4_ip1_j_k = 2.*(-v(i,j,k)+v(i+1,j,k))/(dx1(i)+dx1(i+1))
      visvx2 = (evsx2)* ( diu2_i_jp1_k+diu4_ip1_j_k )

      diu2_im1_jp1_k = 2.*(-u(i-1,j,k)+u(i-1,j+1,k))/(dy1(j)+dy1(j+1))
      diu4_i_j_k = 2.*(-v(i-1,j,k)+v(i,j,k))/(dx1(i-1)+dx1(i))
      visvx1 = (evsx1)* ( diu2_im1_jp1_k+diu4_i_j_k )

      diu5_i_jp1_k = (-v(i,j,k)+v(i,j+1,k))/dy1(j+1)
      visvy2 = (evsy2)*2.*diu5_i_jp1_k

      diu5_i_j_k = (-v(i,j-1,k)+v(i,j,k))/dy1(j)
      visvy1 = (evsy1)*2.*diu5_i_j_k

      diu6_i_j_kp1 = (-v(i,j,k)+v(i,j,k+1))/dzs(k)
      diu8_i_jp1_k = 2.*(-w(i,j,k)+w(i,j+1,k))/(dy1(j)+dy1(j+1))
      visvz2 = (evsz2)* ( diu6_i_j_kp1+diu8_i_jp1_k )

      diu6_i_j_k = (-v(i,j,k-1)+v(i,j,k))/dzs(k-1)
      diu8_i_jp1_km1 = 2.*(-w(i,j,k-1)+w(i,j+1,k-1))/(dy1(j)+dy1(j+1))
      visvz1 = (evsz1)* ( diu6_i_j_k+diu8_i_jp1_km1 )
#endif
!
      vfv = (visvx2-visvx1)/dx1(i) +(visvy2-visvy1)/dys(j) +(visvz2-visvz1)/dzn(k)
!
      g(i,j,k) = (g(i,j,k)+vfv)
      end do
      end do
      end do

!wall function
#ifdef CALC_BOUNDS
      do j=1,jp
      do i=1,ip
!c--eddyviscosity on face
      evsy2=sm(i,j+1,1)
      evsy1=sm(i,j,1)
      evsx2=(dy1(j+1)*((dx1(i+1)*sm(i,j,1)+dx1(i)*sm(i+1,j,1))/(dx1(i)+dx1(i+1)))&
      +dy1(j)*((dx1(i+1)*sm(i,j+1,1)+dx1(i)*sm(i+1,j+1,1))/(dx1(i)+dx1(i+1))))/(dy1(j)+dy1(j+1))
      evsx1=(dy1(j+1)*((dx1(i)*sm(i-1,j,1)+dx1(i-1)*sm(i,j,1))/(dx1(i-1)+dx1(i)))&
      +dy1(j)*((dx1(i)*sm(i-1,j+1,1)+dx1(i-1)*sm(i,j+1,1))/(dx1(i-1)+dx1(i))))/(dy1(j)+dy1(j+1))
      evsz2=(dzn(2)*((dx1(i+1)*sm(i,j,1)+dx1(i)*sm(i+1,j,1))/(dx1(i)+dx1(i+1)))&
      +dzn(1)*((dx1(i+1)*sm(i,j,2)+dx1(i)*sm(i+1,j,2))/(dx1(i)+dx1(i+1))))/(dzn(1)+dzn(2))
!
#ifndef WV_NEW_LES
      visvx2=(evsx2)* ( diu2(i  ,j+1,1  )+diu4(i+1,j  ,1  ) )
      visvx1=(evsx1)* ( diu2(i-1,j+1,1  )+diu4(i  ,j  ,1  ) )
      visvy2=(evsy2)*2.*diu5(i  ,j+1,1  )
      visvy1=(evsy1)*2.*diu5(i  ,j  ,1  )
      visvz2=(evsz2)* ( diu6(i  ,j  ,2  )+diu8(i  ,j+1,1  ) )
#else
      diu2_i_jp1_1 = 2.*(-u(i,j,1)+u(i,j+1,1))/(dy1(j)+dy1(j+1))
      diu4_ip1_j_1 = 2.*(-v(i,j,1)+v(i+1,j,1))/(dx1(i)+dx1(i+1))
      visvx2=(evsx2)* ( diu2_i_jp1_1+diu4_ip1_j_1)

      diu2_im1_jp1_1 = 2.*(-u(i-1,j,1)+u(i-1,j+1,1))/(dy1(j)+dy1(j+1))
      diu4_i_j_1 = 2.*(-v(i-1,j,1)+v(i,j,1))/(dx1(i-1)+dx1(i))
      visvx1=(evsx1)* ( diu2_im1_jp1_1+diu4_i_j_1)

      diu5_i_jp1_1 = (-v(i,j,1)+v(i,j+1,1))/dy1(j+1)
      visvy2=(evsy2)*2.*diu5_i_jp1_1

      diu5_i_j_1 = (-v(i,j-1,1)+v(i,j,1))/dy1(j)
      visvy1=(evsy1)*2.*diu5_i_j_1

      diu6_i_j_2 = (-v(i,j,1)+v(i,j,2))/dzs(1)
      diu8_i_jp1_1 = 2.*(-w(i,j,1)+w(i,j+1,1))/(dy1(j)+dy1(j+1))
      visvz2=(evsz2)* ( diu6_i_j_2+diu8_i_jp1_1)
#endif
#ifndef WV_NEW_LES
      visvz1=(0.4*vspd(i,j)/alog(0.5*dzn(1)/0.1))**2*(v(i,j,1)/vspd(i,j))
#else
      visvz1=(0.4/alog(0.5*dzn(1)/0.1))**2*v(i,j,1)
#endif
!
      vfv=(visvx2-visvx1)/dx1(i)+(visvy2-visvy1)/dys(j)+(visvz2-visvz1)/dzn(1)
!
      g(i,j,1)=(g(i,j,1)+vfv)
      end do
      end do
#endif


! --calculation of viscosity terms in momentum eq.(z-comp.)
      do k = 1,kp
      do j = 1,jp
      do i = 1,ip
! --eddyviscosity on face
      evsz2 = sm(i,j,k+1)
      evsz1 = sm(i,j,k)
      evsx2 = (dzn(k+1)*((dx1(i+1)*sm(i,j,k)+dx1(i)*sm(i+1,j, &
      k)) /(dx1(i)+dx1(i+1))) +dzn(k)*((dx1(i+1)*sm(i,j,k+1)+dx1(i)*sm(i+1,j, &
      k+1)) /(dx1(i)+dx1(i+1)))) /(dzn(k)+dzn(k+1))
      evsx1 = (dzn(k+1)*((dx1(i)*sm(i-1,j,k)+dx1(i-1)*sm(i,j, &
      k)) /(dx1(i-1)+dx1(i))) +dzn(k)*((dx1(i)*sm(i-1,j,k+1)+dx1(i-1)*sm(i,j, &
      k+1)) /(dx1(i-1)+dx1(i)))) /(dzn(k)+dzn(k+1))
      evsy2 = (dzn(k+1)*((dy1(j+1)*sm(i,j,k)+dy1(j)*sm(i,j+1, &
      k)) /(dy1(j)+dy1(j+1))) +dzn(k)*((dy1(j+1)*sm(i,j,k+1)+dy1(j)*sm(i,j+1, &
      k+1)) /(dy1(j)+dy1(j+1)))) /(dzn(k)+dzn(k+1))
      evsy1 = (dzn(k+1)*((dy1(j)*sm(i,j-1,k)+dy1(j-1)*sm(i,j, &
      k)) /(dy1(j-1)+dy1(j))) +dzn(k)*((dy1(j)*sm(i,j-1,k+1)+dy1(j-1)*sm(i,j, &
      k+1)) /(dy1(j-1)+dy1(j)))) /(dzn(k)+dzn(k+1))
!
#ifndef WV_NEW_LES
      viswx2 = (evsx2)* ( diu3(i  ,j  ,k+1)+diu7(i+1,j  ,k  ) )
      viswx1 = (evsx1)* ( diu3(i-1,j  ,k+1)+diu7(i  ,j  ,k  ) )
      viswy2 = (evsy2)* ( diu6(i  ,j  ,k+1)+diu8(i  ,j+1,k  ) )
      viswy1 = (evsy1)* ( diu6(i  ,j-1,k+1)+diu8(i  ,j  ,k  ) )
      viswz2 = (evsz2)*2.*diu9(i  ,j  ,k+1)
      viswz1 = (evsz1)*2.*diu9(i  ,j  ,k  )
#else
      diu3_i_j_kp1 = (-u(i,j,k)+u(i,j,k+1))/dzs(k)
      diu7_ip1_j_k = 2.*(-w(i,j,k)+w(i+1,j,k))/(dx1(i)+dx1(i+1))
      viswx2 = (evsx2)* ( diu3_i_j_kp1 +diu7_ip1_j_k )

      diu3_im1_j_kp1 = (-u(i-1,j,k)+u(i-1,j,k+1))/dzs(k)
      diu7_i_j_k = 2.*(-w(i-1,j,k)+w(i,j,k))/(dx1(i-1)+dx1(i))
      viswx1 = (evsx1)* ( diu3_im1_j_kp1 +diu7_i_j_k )

      diu6_i_j_kp1 = (-v(i,j,k)+v(i,j,k+1))/dzs(k)
      diu8_i_jp1_k = 2.*(-w(i,j,k)+w(i,j+1,k))/(dy1(j)+dy1(j+1))
      viswy2 = (evsy2)* ( diu6_i_j_kp1 +diu8_i_jp1_k )

      diu6_i_jm1_kp1 = (-v(i,j-1,k)+v(i,j-1,k+1))/dzs(k)
      diu8_i_j_k = 2.*(-w(i,j-1,k)+w(i,j,k))/(dy1(j-1)+dy1(j))
      viswy1 = (evsy1)* ( diu6_i_jm1_kp1 +diu8_i_j_k )

      diu9_i_j_kp1 = (-w(i,j,k)+w(i,j,k+1))/dzn(k+1)
      viswz2 = (evsz2)*2.*diu9_i_j_kp1
      diu9_i_j_k = (-w(i,j,k-1)+w(i,j,k))/dzn(k)
      viswz1 = (evsz1)*2.*diu9_i_j_k
#endif
!
      vfw = (viswx2-viswx1)/dx1(i) +(viswy2-viswy1)/dy1(j) +(viswz2-viswz1)/dzn(k)
!     if (i == 1 .and. j == 1 .and. k == 78) then
!    print *,'vis F:',viswx2,viswx1,viswy2,viswy1,viswz2,viswz1, vfw
!    end if

!
      h(i,j,k) = (h(i,j,k)+vfw)
      end do
      end do
      end do
!
      return

      end subroutine les




end module module_les
