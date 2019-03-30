subroutine bondv1(u,z2,dzn,v,w,n,n0,dt,dxs)
      integer, parameter :: kp = 80 
      integer, parameter :: ip = 300 
      integer, parameter :: jp = 300 
      integer, parameter :: ipmax = 300 
      integer, parameter :: jpmax = 300 
      character*300 :: datafile = '../GIS/Kyoto_1km2_4m_with_buffer.txt' 
      real, parameter :: dxgrid = 4. 
      real, parameter :: dygrid = 4. 
      real, parameter :: cs0 = 0.14 
      integer, parameter :: i_anime = 1 
      integer, parameter :: avetime = 2 
      integer, parameter :: km_sl = 80 
      integer, parameter :: i_aveflow = 0 
      integer, parameter :: i_ifdata_out = 0 
      real, parameter :: dt_orig = 0.05 
      real(4), intent(In) :: dt
      real(4), dimension(0:300), intent(In) :: dxs
      real(4), dimension(-1:82), intent(In) :: dzn
      integer, intent(In) :: n,n0
      real(4), dimension(0:301,-1:301,0:81), intent(InOut) :: u
      real(4), dimension(0:301,-1:301,0:81), intent(InOut) :: v
      real(4), dimension(0:301,-1:301,-1:81), intent(InOut) :: w
      real(4), dimension(0:82), intent(In) :: z2
      real(4) :: u_val
      integer :: i,j,k
      real(4) :: aaa,bbb,uout,gaaa,gbbb
    do i = 0, 1, 1
        do k = 1, 78, 1
            do j = 1, 300, 1
                u_val = 5.*((z2(k)+0.5*dzn(k))/600.)**0.2
                u(i,j,k) = u_val
                v(i,j,k) = 0.0
                w(i,j,k) = 0.0
            end do
        end do
    end do
    do i = 0, 1, 1
        do k = 79, 80, 1
            do j = 1, 300, 1
                u(i,j,k) = u(i,j,77)
                v(i,j,k) = 0.0
                w(i,j,k) = 0.0
            end do
        end do
    end do
    if (n==n0) then
        do k = 1, 80, 1
            do j = 1, 300, 1
                do i = 2, 300, 1
                    u(i,j,k) = u(1,j,k)
                    v(i,j,k) = v(1,j,k)
                    w(i,j,k) = w(1,j,k)
                end do
            end do
        end do
    end if
    aaa = 0.0
    do k = 1, 80, 1
        do j = 1, 300, 1
            aaa = amax1(aaa,u(300,j,k))
        end do
    end do
    gaaa = aaa
    bbb = 1e38
    do k = 1, 80, 1
        do j = 1, 300, 1
            bbb = amin1(bbb,u(300,j,k))
        end do
    end do
    gbbb = bbb
    uout = (gaaa+gbbb)/2.
    do k = 1, 80, 1
        do j = 1, 300, 1
            u(300,j,k) = u(300,j,k)-dt*uout*(u(300,j,k)-u(299,j,k))/dxs(300)
        end do
    end do
    do k = 1, 80, 1
        do j = 1, 300, 1
            v(301,j,k) = v(301,j,k)-dt*uout*(v(301,j,k)-v(300,j,k))/dxs(300)
        end do
    end do
    do k = 1, 80, 1
        do j = 1, 300, 1
            w(301,j,k) = w(301,j,k)-dt*uout*(w(301,j,k)-w(300,j,k))/dxs(300)
        end do
    end do
    do k = 0, 81, 1
        do i = 0, 301, 1
            u(i,0,k) = u(i,300,k)
            u(i,301,k) = u(i,1,k)
        end do
    end do
    do k = 0, 81, 1
        do i = 0, 301, 1
            v(i,0,k) = v(i,300,k)
            v(i,301,k) = v(i,1,k)
        end do
    end do
    do k = 0, 80, 1
        do i = 0, 301, 1
            w(i,0,k) = w(i,300,k)
            w(i,301,k) = w(i,1,k)
        end do
    end do
    do j = 0, 301, 1
        do i = 0, 301, 1
            u(i,j,0) = -u(i,j,1)
            u(i,j,81) = u(i,j,80)
        end do
    end do
    do j = 0, 301, 1
        do i = 0, 301, 1
            v(i,j,0) = -v(i,j,1)
            v(i,j,81) = v(i,j,80)
        end do
    end do
    do j = -1, 301, 1
        do i = 0, 301, 1
            w(i,j,0) = 0.0
            w(i,j,80) = 0.0
        end do
    end do
end subroutine bondv1
