subroutine press(u,v,w,p,rhs,f,g,h,dx1,dy1,dzn,dxs,dys,dzs,dt,n,nmax)
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
      real(4), dimension(0:300), intent(In) :: dxs
      real(4), dimension(0:300), intent(In) :: dys
      real(4), dimension(-1:82), intent(In) :: dzs
      real(4) :: cn1,cn2l,cn2s,cn3l,cn3s,cn4l,cn4s,dz1,dz2
      real(4), intent(In) :: dt
      real(4), dimension(-1:301), intent(In) :: dx1
      real(4), dimension(0:301), intent(In) :: dy1
      real(4), dimension(-1:82), intent(In) :: dzn
      real(4), dimension(0:300,0:300,0:80), intent(InOut) :: f
      real(4), dimension(0:300,0:300,0:80), intent(InOut) :: g
      real(4), dimension(0:300,0:300,0:80), intent(InOut) :: h
      integer, intent(In) :: n
      integer, intent(In) :: nmax
      real(4), dimension(0:302,0:302,0:81) :: p0
      real(4), dimension(0:302,0:302,0:81) :: p1
      real(4), dimension(0:301,0:301,0:81), intent(Out) :: rhs
      real(4), dimension(0:301,-1:301,0:81), intent(In) :: u
      real(4), dimension(0:301,-1:301,0:81), intent(In) :: v
      real(4), dimension(0:301,-1:301,-1:81), intent(In) :: w
      integer :: nn
      integer :: i,j,k,l,nrd
      real(4) :: rhsav,pav,area,pco,sor,reltmp
      real, parameter :: pjuge = 0.0001 
      integer, parameter :: nmaxp = 50 
      real, parameter :: omega = 1. 
    do k = 1, 80, 1
        do j = 1, 300, 1
            do i = 1, 300, 1
                rhs(i,j,k) = (-u(i-1,j,k)+u(i,j,k))/dx1(i)+(-v(i,j-1,k)+v(i,j,k))/dy1(j)+(-w(i,j,k-1)+w(i,j,k))/dzn(k)
                rhs(i,j,k) = (f(i,j,k)-f(i-1,j,k))/dx1(i)+(g(i,j,k)-g(i,j-1,k))/dy1(j)+(h(i,j,k)-h(i,j,k-1))/dzn(k)+rhs(i,j,k)/dt
            end do
        end do
    end do
    rhsav = 0.0
    area = 0.0
    do k = 1, 80, 1
        do j = 1, 300, 1
            do i = 1, 300, 1
                rhsav = rhsav+dx1(i)*dy1(j)*dzn(k)*rhs(i,j,k)
                area = area+dx1(i)*dy1(j)*dzn(k)
            end do
        end do
    end do
    rhsav = rhsav/area
    do k = 1, 80, 1
        do j = 1, 300, 1
            do i = 1, 300, 1
                rhs(i,j,k) = rhs(i,j,k)-rhsav
            end do
        end do
    end do
    pav = 0.0
    pco = 0.0
    do k = 1, 80, 1
        do j = 1, 300, 1
            do i = 1, 300, 1
                pav = pav+p0(i,j,k)*dx1(i)*dy1(j)*dzn(k)
                pco = pco+dx1(i)*dy1(j)*dzn(k)
            end do
        end do
    end do
    pav = pav/pco
    do k = 1, 80, 1
        do j = 1, 300, 1
            do i = 1, 300, 1
                p0(i,j,k) = p0(i,j,k)-pav
            end do
        end do
    end do
    do k = 0, 81, 1
        do j = 0, 301, 1
            p0(0,j,k) = p0(1,j,k)
            p0(301,j,k) = p0(300,j,k)
        end do
    end do
    do k = 0, 81, 1
        do i = 0, 301, 1
            p0(i,0,k) = p0(i,300,k)
            p0(i,301,k) = p0(i,1,k)
        end do
    end do
    do j = 0, 301, 1
        do i = 0, 301, 1
            p0(i,j,0) = p0(i,j,1)
            p0(i,j,81) = p0(i,j,80)
        end do
    end do
end subroutine press
