module module_bondv1
 contains
subroutine bondv1(u,z2,dzn,v,w,n,n0,dt,dxs)
      implicit none
integer, parameter :: kp=80
    integer, parameter :: ip = 300
    integer, parameter :: jp = 300
    integer, parameter :: ipmax = ip
    integer, parameter :: jpmax = jp
    character(300) :: datafile = '../GIS/Kyoto_1km2_4m_with_buffer.txt'
    real, parameter :: dxgrid = 4.
    real, parameter :: dygrid = 4.
    real, parameter :: cs0 = 0.14
    integer, parameter :: i_anime=1
    integer, parameter :: avetime=2
    integer, parameter :: km_sl=80
    integer, parameter :: i_aveflow=0
    integer, parameter :: i_ifdata_out=0
    real, parameter :: dt_orig = 0.05 
    real(kind=4), intent(In) :: dt
    real(kind=4), dimension(0:ip) , intent(In) :: dxs
    real(kind=4), dimension(-1:kp+2) , intent(In) :: dzn
    integer, intent(In) :: n, n0
    real(kind=4), dimension(0:ip+1,-1:jp+1,0:kp+1) , intent(InOut) :: u
    real(kind=4), dimension(0:ip+1,-1:jp+1,0:kp+1) , intent(InOut) :: v
    real(kind=4), dimension(0:ip+1,-1:jp+1,-1:kp+1) , intent(InOut) :: w
    real(kind=4), dimension(0:kp+2) , intent(In) :: z2
    real(kind=4) :: u_val
    integer :: i, j, k
    real(kind=4) :: aaa, bbb, uout, gaaa, gbbb
        do i = 0,1
            do k = 1,78 
                do j = 1,jp
                    u_val = 5.*((z2(k)+0.5*dzn(k))/600.)**0.2
                    u(i,j,k) = u_val
                    v(i,j,k) = 0.0
                    w(i,j,k) = 0.0
                end do
            end do
        end do
        do i = 0,1
            do k = 79,kp
                do j = 1,jp
                    u(i,j,k) = u(i,j,77)
                    v(i,j,k) = 0.0
                    w(i,j,k) = 0.0
                end do
            end do
        end do
    if(n == n0) then
        do k = 1,kp
            do j = 1,jp
                do i = 2, ip
                    u(i,j,k) = u(1,j,k)
                    v(i,j,k) = v(1,j,k)
                    w(i,j,k) = w(1,j,k)
                end do
            end do
        end do
    endif
    aaa = 0.0
    do k = 1,kp
        do j = 1,jp
            aaa = amax1(aaa,u(ip,j,k))
        end do
    end do
    gaaa = aaa
    bbb = 1e38 
    do k = 1,kp
        do j = 1,jp
            bbb = amin1(bbb,u(ip,j,k))
        end do
    end do
    gbbb=bbb
    uout = (gaaa+gbbb)/2.
      do k = 1,kp
          do j = 1,jp
              u(ip,j,k) = u(ip,j,k)-dt*uout *(u(ip,j,k)-u(ip-1,j,k))/dxs(ip)
          end do
      end do
      do k = 1,kp
          do j = 1,jp
              v(ip+1,j,k) = v(ip+1,j,k)-dt*uout *(v(ip+1,j,k)-v(ip,j,k))/dxs(ip)
          end do
      end do
      do k = 1,kp
          do j = 1,jp
              w(ip+1,j,k) = w(ip+1,j,k)-dt*uout *(w(ip+1,j,k)-w(ip,j,k))/dxs(ip)
          end do
      end do
    do k = 0,kp+1
        do i = 0,ip+1
            u(i, 0,k) = u(i,jp ,k)
            u(i,jp+1,k) = u(i, 1,k)
        end do
    end do
    do k = 0,kp+1
        do i = 0,ip+1
            v(i, 0,k) = v(i,jp ,k)
            v(i,jp+1,k) = v(i, 1,k)
        end do
    end do
    do k = 0,kp
        do i = 0,ip+1
            w(i, 0,k) = w(i,jp ,k)
            w(i,jp+1,k) = w(i, 1,k)
        end do
    end do
    do j = 0,jp+1
        do i = 0,ip+1
            u(i,j, 0) = -u(i,j, 1)
            u(i,j,kp+1) = u(i,j,kp)
        end do
    end do
    do j = 0,jp+1
        do i = 0,ip+1
            v(i,j, 0) = -v(i,j, 1)
            v(i,j,kp+1) = v(i,j,kp)
        end do
    end do
    do j = -1,jp+1 
        do i = 0,ip+1
            w(i,j, 0) = 0.0
            w(i,j,kp) = 0.0
        end do
    end do
end subroutine bondv1
end module module_bondv1
