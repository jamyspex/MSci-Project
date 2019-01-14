module module_bondv1
 contains
subroutine bondv1(jm,u,z2,dzn,v,w,km,n,im,dt,dxs)
      implicit none
    real(kind=4), intent(In) :: dt
    real(kind=4), dimension(0:ip) , intent(In) :: dxs
    real(kind=4), dimension(-1:kp+2) , intent(In) :: dzn
    integer, intent(In) :: im, jm, km, n
    real(kind=4), dimension(0:ip+1,-1:jp+1,0:kp+1) , intent(InOut) :: u
    real(kind=4), dimension(0:ip+1,-1:jp+1,0:kp+1) , intent(InOut) :: v
    real(kind=4), dimension(0:ip+1,-1:jp+1,-1:kp+1) , intent(InOut) :: w
    real(kind=4), dimension(0:kp+2) , intent(In) :: z2
    real(kind=4) :: u_val
    integer :: i, j, k
    real(kind=4) :: aaa, bbb, uout
        do i = 0,1
            do k = 1,78 
                do j = 1,jm
                    u(i,j,k) = 5.0
                    v(i,j,k) = 0.0
                    w(i,j,k) = 0.0
                end do
            end do
        end do
        do i = 0,1
            do k = 79,km
                do j = 1,jm
                    u(i,j,k) = u(i,j,77)
                    v(i,j,k) = 0.0
                    w(i,j,k) = 0.0
                end do
            end do
        end do
    if(n == 1) then
        do k = 1,km
            do j = 1,jm
                do i = 2, im
                    u(i,j,k) = u(1,j,k)
                    v(i,j,k) = v(1,j,k)
                    w(i,j,k) = w(1,j,k)
                end do
            end do
        end do
    endif
    aaa = 0.0
    bbb = 0.0
    do k = 1,km
        do j = 1,jm
            aaa = amax1(aaa,u(im,j,k))
            bbb = amin1(bbb,u(im,j,k))
        end do
    end do
    uout = (aaa+bbb)/2.
    do k = 1,km
        do j = 1,jm
            u(im,j,k) = u(im,j,k)-dt*uout *(u(im,j,k)-u(im-1,j,k))/dxs(im)
        end do
    end do
    do k = 1,km
        do j = 1,jm
            v(im+1,j,k) = v(im+1,j,k)-dt*uout *(v(im+1,j,k)-v(im,j,k))/dxs(im)
        end do
    end do
    do k = 1,km
        do j = 1,jm
            w(im+1,j,k) = w(im+1,j,k)-dt*uout *(w(im+1,j,k)-w(im,j,k))/dxs(im)
        end do
    end do
    do k = 0,km+1
        do i = 0,im+1
            u(i, 0,k) = u(i,jm ,k)
            u(i,jm+1,k) = u(i, 1,k)
        end do
    end do
    do k = 0,km+1
        do i = 0,im+1
            v(i, 0,k) = v(i,jm ,k)
            v(i,jm+1,k) = v(i, 1,k)
        end do
    end do
    do k = 0,km
        do i = 0,im+1
            w(i, 0,k) = w(i,jm ,k)
            w(i,jm+1,k) = w(i, 1,k)
        end do
    end do
    do j = 0,jm+1
        do i = 0,im+1
            u(i,j, 0) = -u(i,j, 1)
            u(i,j,km+1) = u(i,j,km)
        end do
    end do
    do j = 0,jm+1
        do i = 0,im+1
            v(i,j, 0) = -v(i,j, 1)
            v(i,j,km+1) = v(i,j,km)
        end do
    end do
    do j = -1,jm+1 
        do i = 0,im+1
            w(i,j, 0) = 0.0
            w(i,j,km) = 0.0
        end do
    end do
end subroutine bondv1
end module module_bondv1
