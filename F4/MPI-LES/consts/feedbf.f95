subroutine feedbf(usum,u,bmask1,vsum,v,cmask1,wsum,w,dmask1,alpha,dt,beta,fx,fy,fz,f,g,h,n)
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
      real(4), intent(In) :: alpha
      real(4), intent(In) :: beta
      real(4), dimension(-1:301,0:301,0:81), intent(In) :: bmask1
      real(4), dimension(0:301,-1:301,0:81), intent(In) :: cmask1
      real(4), dimension(0:301,0:301,0:81), intent(In) :: dmask1
      real(4), intent(In) :: dt
      real(4), dimension(0:300,0:300,0:80), intent(InOut) :: f
      real(4), dimension(0:300,0:300,0:80), intent(Out) :: fx
      real(4), dimension(0:300,0:300,0:80), intent(Out) :: fy
      real(4), dimension(0:300,0:300,0:80), intent(Out) :: fz
      integer, intent(In) :: n
      real(4), dimension(0:300,0:300,0:80), intent(InOut) :: g
      real(4), dimension(0:300,0:300,0:80), intent(InOut) :: h
      real(4), dimension(0:301,-1:301,0:81), intent(In) :: u
      real(4), dimension(0:300,0:300,0:80), intent(InOut) :: usum
      real(4), dimension(0:301,-1:301,0:81), intent(In) :: v
      real(4), dimension(0:300,0:300,0:80), intent(InOut) :: vsum
      real(4), dimension(0:301,-1:301,-1:81), intent(In) :: w
      real(4), dimension(0:300,0:300,0:80), intent(InOut) :: wsum
      integer :: i,j,k
      real(4) :: f1x,f1y,f1z,f2x,f2y,f2z
    do k = 1, 80, 1
        do j = 1, 300, 1
            do i = 1, 300, 1
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
    do k = 1, 80, 1
        do j = 1, 300, 1
            do i = 1, 300, 1
                f(i,j,k) = f(i,j,k)+fx(i,j,k)
                g(i,j,k) = g(i,j,k)+fy(i,j,k)
                h(i,j,k) = h(i,j,k)+fz(i,j,k)
            end do
        end do
    end do
end subroutine feedbf
