subroutine velnw(p,ro,dxs,u,dt,f,dys,v,g,dzs,w,h)
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
      real(4), dimension(0:300), intent(In) :: dys
      real(4), dimension(-1:82), intent(In) :: dzs
      real(4), dimension(0:300,0:300,0:80), intent(In) :: f
      real(4), dimension(0:300,0:300,0:80), intent(In) :: g
      real(4), dimension(0:300,0:300,0:80), intent(In) :: h
      real(4), dimension(0:1,0:302,0:302,0:81), intent(In) :: p
      real(4), intent(In) :: ro
      real(4), dimension(0:301,-1:301,0:81), intent(InOut) :: u
      real(4), dimension(0:301,-1:301,0:81), intent(InOut) :: v
      real(4), dimension(0:301,-1:301,-1:81), intent(InOut) :: w
      integer :: i,j,k
      real(4) :: pz
      integer :: synthIdx3
    do k = 1, 80, 1
        do j = 1, 300, 1
            do i = 1, 300, 1
                do synthIdx3 = 0, 1, 1
                    if (synthIdx3==0) then
                        pz = (-p(synthIdx3,i,j,k)+p(synthIdx3,i+1,j,k))/ro/dxs(i)
                        u(i,j,k) = u(i,j,k)+dt*(f(i,j,k)-pz)
                    end if
                end do
            end do
        end do
    end do
    do k = 1, 80, 1
        do j = 1, 300, 1
            do i = 1, 300, 1
                do synthIdx3 = 0, 1, 1
                    if (synthIdx3==0) then
                        pz = (-p(synthIdx3,i,j,k)+p(synthIdx3,i,j+1,k))/ro/dys(j)
                        v(i,j,k) = v(i,j,k)+dt*(g(i,j,k)-pz)
                    end if
                end do
            end do
        end do
    end do
    do k = 1, 79, 1
        do j = 1, 300, 1
            do i = 1, 300, 1
                do synthIdx3 = 0, 1, 1
                    if (synthIdx3==0) then
                        pz = (-p(synthIdx3,i,j,k)+p(synthIdx3,i,j,k+1))/ro/dzs(k)
                        w(i,j,k) = w(i,j,k)+dt*(h(i,j,k)-pz)
                    end if
                end do
            end do
        end do
    end do
    return 
end subroutine velnw
