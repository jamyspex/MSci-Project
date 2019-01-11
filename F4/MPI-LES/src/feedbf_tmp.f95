module module_feedbf
#ifdef WV_NEW
    implicit none
#endif
 contains 
#ifdef WV_NEW_FEEDBF
#ifdef NOT_INLINED
subroutine calc_abcd_mask(zbm, z2, dzn, i,j,k, abcd_mask1)
    use params_common_sn
    integer, intent(In) :: i,j,k
    real(kind=4), dimension(-1:kp+2) , intent(In) :: dzn
    real(kind=4), dimension(0:kp+2) , intent(In) :: z2
    real(kind=4), dimension(-1:ipmax+1,-1:jpmax+1) , intent(In) :: zbm
    real(kind=4), dimension(0:3), intent(Out) :: abcd_mask1
    abcd_mask1(0) = 1.
    abcd_mask1(1) = 0.
    abcd_mask1(2) = 0.
    abcd_mask1(3) = 0.
    if(zbm(i,j) > z2(k)+0.5*dzn(k)) then
        abcd_mask1(0) = 0.0
        abcd_mask1(1) = 1.0
        abcd_mask1(2) = 1.0
        abcd_mask1(3) = 1.0
    end if
end subroutine calc_abcd_mask
#endif
subroutine feedbf(u,v,w,f,g,h,usum,vsum,wsum,dzn,z2,zbm,alpha,beta,dt)
#else
subroutine feedbf(usum,u,bmask1,vsum,v,cmask1,wsum,w,dmask1,alpha,&
                  dt,beta,fx,fy,fz,f,g,h,n)
#endif
#ifdef WV_NEW_FEEDBF
    use params_common_sn
#else
    use common_sn ! create_new_include_statements() line 102
#endif
    real(kind=4), intent(In) :: alpha
    real(kind=4), intent(In) :: beta
#ifndef WV_NEW_FEEDBF
    real(kind=4), dimension(-1:ip+1,0:jp+1,0:kp+1) , intent(In) :: bmask1
    real(kind=4), dimension(0:ip+1,-1:jp+1,0:kp+1) , intent(In) :: cmask1
    real(kind=4), dimension(0:ip+1,0:jp+1,0:kp+1) , intent(In) :: dmask1
#else
    real(kind=4), dimension(-1:kp+2) , intent(In) :: dzn
    real(kind=4), dimension(0:kp+2) , intent(In) :: z2
    real(kind=4), dimension(-1:ipmax+1,-1:jpmax+1) , intent(InOut) :: zbm
    real(kind=4), dimension(0:3) :: abcd_mask
#endif
    real(kind=4), intent(In) :: dt
    real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(InOut) :: f
#ifndef WV_NEW_FEEDBF
    real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(Out) :: fx
    real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(Out) :: fy
    real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(Out) :: fz
    integer, intent(In) :: n
#else
    real(kind=4) :: fx,fy,fz
#endif
    real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(InOut) :: g
    real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(InOut) :: h
    real(kind=4), dimension(0:ip+1,-1:jp+1,0:kp+1) , intent(In) :: u
    real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(InOut) :: usum
    real(kind=4), dimension(0:ip+1,-1:jp+1,0:kp+1) , intent(In) :: v
    real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(InOut) :: vsum
    real(kind=4), dimension(0:ip+1,-1:jp+1,-1:kp+1) , intent(In) :: w
    real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(InOut) :: wsum
!#ifdef WV_NEW_FEEDBF
    integer :: i,j,k
    real(kind=4) :: f1x,f1y,f1z,f2x,f2y,f2z
!#endif
#ifdef WV_DEBUG
    print *, 'F95 UVWSUMSUM after bondv1:',sum(usum)+sum(vsum)+sum(wsum)
    print *, 'F95 USUMSUM after bondv1:',sum(usum)
    print *, 'F95 VSUMSUM after bondv1:',sum(vsum)
    print *, 'F95 WSUMSUM after bondv1:',sum(wsum)
#endif
!    if (isMaster()) then
!      do i=20,30
!      write(*,*) "u",u(i,5,2),i,rank,n
!      end do
!    end if 
#ifndef WV_NEW_FEEDBF
    do k = 1,kp
        do j = 1,jp
            do i = 1,ip
                usum(i,j,k) = (usum(i,j,k)+u(i,j,k))*bmask1(i,j,k)
                vsum(i,j,k) = (vsum(i,j,k)+v(i,j,k))*cmask1(i,j,k)
                wsum(i,j,k) = (wsum(i,j,k)+w(i,j,k))*dmask1(i,j,k)
                f1x = alpha*usum(i,j,k)*dt
                f1y = alpha*vsum(i,j,k)*dt
                f1z = alpha*wsum(i,j,k)*dt
                f2x = beta*u(i,j,k)*bmask1(i,j,k)
                f2y = beta*v(i,j,k)*cmask1(i,j,k)
                f2z = beta*w(i,j,k)*dmask1(i,j,k)
                fx(i,j,k) = f1x+f2x
                fy(i,j,k) = f1y+f2y
                fz(i,j,k) = f1z+f2z
            end do
        end do
    end do
    do k = 1,kp
        do j = 1,jp
            do i = 1,ip
                f(i,j,k) = f(i,j,k)+fx(i,j,k)
                g(i,j,k) = g(i,j,k)+fy(i,j,k)
                h(i,j,k) = h(i,j,k)+fz(i,j,k)
            end do
        end do
    end do
#else
    do k = 1,kp
        do j = 1,jp
            do i = 1,ip
#ifdef NOT_INLINED
                call calc_abcd_mask(zbm, z2, dzn, i,j,k, abcd_mask)
#else
                abcd_mask(0) = 1.
                abcd_mask(1) = 0.
                abcd_mask(2) = 0.
                abcd_mask(3) = 0.
                if(zbm(i,j) > z2(k)+0.5*dzn(k)) then
                    abcd_mask(0) = 0.0
                    abcd_mask(1) = 1.0
                    abcd_mask(2) = 1.0
                    abcd_mask(3) = 1.0
                end if
#endif
                usum(i,j,k) = (usum(i,j,k)+u(i,j,k))*abcd_mask(1)
                vsum(i,j,k) = (vsum(i,j,k)+v(i,j,k))*abcd_mask(2)
                wsum(i,j,k) = (wsum(i,j,k)+w(i,j,k))*abcd_mask(3)
                f1x = alpha*usum(i,j,k)*dt
                f1y = alpha*vsum(i,j,k)*dt
                f1z = alpha*wsum(i,j,k)*dt
                f2x = beta*u(i,j,k)*abcd_mask(1)
                f2y = beta*v(i,j,k)*abcd_mask(2)
                f2z = beta*w(i,j,k)*abcd_mask(3)
                fx = f1x+f2x
                fy = f1y+f2y
                fz = f1z+f2z
                f(i,j,k) = f(i,j,k)+fx
                g(i,j,k) = g(i,j,k)+fy
                h(i,j,k) = h(i,j,k)+fz
            end do
        end do
    end do
#endif
#ifdef WV_DEBUG
    print *, 'F95 FGHSUM after feedbf:',sum(f)+sum(g)+sum(h)
    print *, 'F95 FSUM after feedbf:',sum(f)
    print *, 'F95 GSUM after feedbf:',sum(g)
    print *, 'F95 HSUM after feedbf:',sum(h)
    print *, 'F95 UVWSUMSUM after feedbf:',sum(usum)+sum(vsum)+sum(wsum)
    print *, 'F95 USUMSUM after feedbf:',sum(usum)
    print *, 'F95 VSUMSUM after feedbf:',sum(vsum)
    print *, 'F95 WSUMSUM after feedbf:',sum(wsum)
    print *, 'F95 UVWSUM after feedbf:', sum(u)+sum(v)+sum(w)
    print *, 'F95 USUM after feedbf:', sum(u)
    print *, 'F95 VSUM after feedbf:', sum(v)
    print *, 'F95 WSUM after feedbf:', sum(w)
#endif
end subroutine feedbf
end module module_feedbf
