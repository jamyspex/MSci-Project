module module_feedbf
 contains
subroutine feedbf(km,jm,im,usum,u,bmask1,vsum,v,cmask1,wsum,w,dmask1,alpha,&
                  dt,beta,fx,fy,fz,f,g,h)
    use common_sn ! create_new_include_statements() line 102
    real(kind=4), intent(In) :: alpha
    real(kind=4), intent(In) :: beta
    real(kind=4), dimension(-1:ip+1,0:jp+1,0:kp+1) , intent(In) :: bmask1
    real(kind=4), dimension(0:ip+1,-1:jp+1,0:kp+1) , intent(In) :: cmask1
    real(kind=4), dimension(0:ip+1,0:jp+1,0:kp+1) , intent(In) :: dmask1
    real(kind=4), intent(In) :: dt
    real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(InOut) :: f
    real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(Out) :: fx
    real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(Out) :: fy
    real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(Out) :: fz
    real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(InOut) :: g
    real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(InOut) :: h
    integer, intent(In) :: im
    integer, intent(In) :: jm
    integer, intent(In) :: km
    real(kind=4), dimension(0:ip+1,-1:jp+1,0:kp+1) , intent(In) :: u
    real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(InOut) :: usum
    real(kind=4), dimension(0:ip+1,-1:jp+1,0:kp+1) , intent(In) :: v
    real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(InOut) :: vsum
    real(kind=4), dimension(0:ip+1,-1:jp+1,-1:kp+1) , intent(In) :: w
    real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(InOut) :: wsum
    do k = 1,km
        do j = 1,jm
            do i = 1,im
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
    do k = 1,km
        do j = 1,jm
            do i = 1,im
                f(i,j,k) = f(i,j,k)+fx(i,j,k)
                g(i,j,k) = g(i,j,k)+fy(i,j,k)
                h(i,j,k) = h(i,j,k)+fz(i,j,k)
            end do
        end do
    end do
end subroutine feedbf
end module module_feedbf
