subroutine les(delx1,dx1,dy1,dzn,diu1,diu2,diu3,diu4,diu5,diu6,diu7,diu8,diu9,sm,f,g,h,u,v,uspd,vspd,dxs,dys,n)
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
      real(4), dimension(80), intent(Out) :: delx1
      real(4), dimension(-1:302,0:302,0:82), intent(In) :: diu1
      real(4), dimension(0:302,0:302,0:82), intent(In) :: diu2
      real(4), dimension(0:302,0:302,0:82), intent(In) :: diu3
      real(4), dimension(0:302,0:302,0:82), intent(In) :: diu4
      real(4), dimension(-1:302,0:302,0:82), intent(In) :: diu5
      real(4), dimension(0:302,0:302,0:82), intent(In) :: diu6
      real(4), dimension(0:302,0:302,0:82), intent(In) :: diu7
      real(4), dimension(0:302,0:302,0:82), intent(In) :: diu8
      real(4), dimension(0:302,0:302,0:82), intent(In) :: diu9
      integer, intent(In) :: n
      real(4), dimension(-1:301), intent(In) :: dx1
      real(4), dimension(0:301), intent(In) :: dy1
      real(4), dimension(-1:82), intent(In) :: dzn
      real(4), dimension(0:300,0:300,0:80), intent(InOut) :: f
      real(4), dimension(0:300,0:300,0:80), intent(InOut) :: g
      real(4), dimension(0:300,0:300,0:80), intent(InOut) :: h
      real(4), dimension(-1:301,-1:301,0:81), intent(Out) :: sm
      real(4), dimension(0:301,-1:301,0:81), intent(In) :: u
      real(4), dimension(0:301,-1:301,0:81), intent(In) :: v
      real(4), dimension(0:301,0:301), intent(In) :: uspd
      real(4), dimension(0:301,0:301), intent(In) :: vspd
      real(4), dimension(0:300), intent(In) :: dxs
      real(4), dimension(0:300), intent(In) :: dys
      integer :: i,j,k
      real(4) :: csx1
      real(4) :: dudxx1,dudyx1,dudzx1,dvdxx1,dvdyx1,dvdzx1,dwdxx1,dwdyx1,dwdzx1
      real(4) :: visux2,visux1,visuy2,visuy1,visuz2,visuz1
      real(4) :: visvx2,visvx1,visvy2,visvy1,visvz2,visvz1
      real(4) :: viswx2,viswx1,viswy2,viswy1,viswz2,viswz1
      real(4) :: evsx2,evsx1,evsy2,evsy1,evsz2,evsz1
      real(4) :: vfu,vfv,vfw
    do k = 1, 80, 1
        do j = 1, 300, 1
            do i = 1, 300, 1
                dudxx1 = diu1(i,j,k)
                dudyx1 = (diu2(i-1,j,k)+diu2(i-1,j+1,k)+diu2(i,j,k)+diu2(i,j+1,k))*.25
                dudzx1 = (diu3(i-1,j,k)+diu3(i-1,j,k+1)+diu3(i,j,k)+diu3(i,j,k+1))*.25
                dvdxx1 = (diu4(i,j,k)+diu4(i,j-1,k)+diu4(i+1,j,k)+diu4(i+1,j-1,k))*.25
                dvdyx1 = diu5(i,j,k)
                dvdzx1 = (diu6(i,j-1,k)+diu6(i,j-1,k+1)+diu6(i,j,k)+diu6(i,j,k+1))*.25
                dwdxx1 = (diu7(i,j,k)+diu7(i,j,k-1)+diu7(i+1,j,k)+diu7(i+1,j,k-1))*.25
                dwdyx1 = (diu8(i,j,k)+diu8(i,j,k-1)+diu8(i,j+1,k)+diu8(i,j+1,k-1))*.25
                dwdzx1 = diu9(i,j,k)
                csx1 = 0.14
                sm(i,j,k) = (csx1*delx1(k))**2*sqrt(2.*(dudxx1**2+dvdyx1**2+dwdzx1**2)+(dudyx1+dvdxx1)**2+(dwdyx1+dvdzx1)**2+(dudzx1+dwdxx1)**2)
            end do
        end do
    end do
    do k = 2, 80, 1
        do j = 1, 300, 1
            do i = 1, 300, 1
                evsx2 = sm(i+1,j,k)
                evsx1 = sm(i,j,k)
                evsy2 = (dy1(j+1)*((dx1(i+1)*sm(i,j,k)+dx1(i)*sm(i+1,j,k))/(dx1(i)+dx1(i+1)))+dy1(j)*((dx1(i+1)*sm(i,j+1,k)+dx1(i)*sm(i+1,j+1,k))/(dx1(i)+dx1(i+1))))/(dy1(j)+dy1(j+1))
                evsy1 = (dy1(j+1)*((dx1(i+1)*sm(i,j-1,k)+dx1(i)*sm(i+1,j-1,k))/(dx1(i)+dx1(i+1)))+dy1(j)*((dx1(i+1)*sm(i,j,k)+dx1(i)*sm(i+1,j,k))/(dx1(i)+dx1(i+1))))/(dy1(j)+dy1(j+1))
                evsz2 = (dzn(k+1)*((dx1(i+1)*sm(i,j,k)+dx1(i)*sm(i+1,j,k))/(dx1(i)+dx1(i+1)))+dzn(k)*((dx1(i+1)*sm(i,j,k+1)+dx1(i)*sm(i+1,j,k+1))/(dx1(i)+dx1(i+1))))/(dzn(k)+dzn(k+1))
                evsz1 = (dzn(k)*((dx1(i+1)*sm(i,j,k-1)+dx1(i)*sm(i+1,j,k-1))/(dx1(i)+dx1(i+1)))+dzn(k-1)*((dx1(i+1)*sm(i,j,k)+dx1(i)*sm(i+1,j,k))/(dx1(i)+dx1(i+1))))/(dzn(k-1)+dzn(k))
                visux2 = (evsx2)*2.*diu1(i+1,j,k)
                visux1 = (evsx1)*2.*diu1(i,j,k)
                visuy2 = (evsy2)*(diu2(i,j+1,k)+diu4(i+1,j,k))
                visuy1 = (evsy1)*(diu2(i,j,k)+diu4(i+1,j-1,k))
                visuz2 = (evsz2)*(diu3(i,j,k+1)+diu7(i+1,j,k))
                visuz1 = (evsz1)*(diu3(i,j,k)+diu7(i+1,j,k-1))
                vfu = (visux2-visux1)/dxs(i)+(visuy2-visuy1)/dy1(j)+(visuz2-visuz1)/dzn(k)
                f(i,j,k) = (f(i,j,k)+vfu)
            end do
        end do
    end do
    do k = 2, 80, 1
        do j = 1, 300, 1
            do i = 1, 300, 1
                evsy2 = sm(i,j+1,k)
                evsy1 = sm(i,j,k)
                evsx2 = (dy1(j+1)*((dx1(i+1)*sm(i,j,k)+dx1(i)*sm(i+1,j,k))/(dx1(i)+dx1(i+1)))+dy1(j)*((dx1(i+1)*sm(i,j+1,k)+dx1(i)*sm(i+1,j+1,k))/(dx1(i)+dx1(i+1))))/(dy1(j)+dy1(j+1))
                evsx1 = (dy1(j+1)*((dx1(i)*sm(i-1,j,k)+dx1(i-1)*sm(i,j,k))/(dx1(i-1)+dx1(i)))+dy1(j)*((dx1(i)*sm(i-1,j+1,k)+dx1(i-1)*sm(i,j+1,k))/(dx1(i-1)+dx1(i))))/(dy1(j)+dy1(j+1))
                evsz2 = (dzn(k+1)*((dx1(i+1)*sm(i,j,k)+dx1(i)*sm(i+1,j,k))/(dx1(i)+dx1(i+1)))+dzn(k)*((dx1(i+1)*sm(i,j,k+1)+dx1(i)*sm(i+1,j,k+1))/(dx1(i)+dx1(i+1))))/(dzn(k)+dzn(k+1))
                evsz1 = (dzn(k)*((dx1(i+1)*sm(i,j,k-1)+dx1(i)*sm(i+1,j,k-1))/(dx1(i)+dx1(i+1)))+dzn(k-1)*((dx1(i+1)*sm(i,j,k)+dx1(i)*sm(i+1,j,k))/(dx1(i)+dx1(i+1))))/(dzn(k-1)+dzn(k))
                visvx2 = (evsx2)*(diu2(i,j+1,k)+diu4(i+1,j,k))
                visvx1 = (evsx1)*(diu2(i-1,j+1,k)+diu4(i,j,k))
                visvy2 = (evsy2)*2.*diu5(i,j+1,k)
                visvy1 = (evsy1)*2.*diu5(i,j,k)
                visvz2 = (evsz2)*(diu6(i,j,k+1)+diu8(i,j+1,k))
                visvz1 = (evsz1)*(diu6(i,j,k)+diu8(i,j+1,k-1))
                vfv = (visvx2-visvx1)/dx1(i)+(visvy2-visvy1)/dys(j)+(visvz2-visvz1)/dzn(k)
                g(i,j,k) = (g(i,j,k)+vfv)
            end do
        end do
    end do
    do k = 1, 80, 1
        do j = 1, 300, 1
            do i = 1, 300, 1
                evsz2 = sm(i,j,k+1)
                evsz1 = sm(i,j,k)
                evsx2 = (dzn(k+1)*((dx1(i+1)*sm(i,j,k)+dx1(i)*sm(i+1,j,k))/(dx1(i)+dx1(i+1)))+dzn(k)*((dx1(i+1)*sm(i,j,k+1)+dx1(i)*sm(i+1,j,k+1))/(dx1(i)+dx1(i+1))))/(dzn(k)+dzn(k+1))
                evsx1 = (dzn(k+1)*((dx1(i)*sm(i-1,j,k)+dx1(i-1)*sm(i,j,k))/(dx1(i-1)+dx1(i)))+dzn(k)*((dx1(i)*sm(i-1,j,k+1)+dx1(i-1)*sm(i,j,k+1))/(dx1(i-1)+dx1(i))))/(dzn(k)+dzn(k+1))
                evsy2 = (dzn(k+1)*((dy1(j+1)*sm(i,j,k)+dy1(j)*sm(i,j+1,k))/(dy1(j)+dy1(j+1)))+dzn(k)*((dy1(j+1)*sm(i,j,k+1)+dy1(j)*sm(i,j+1,k+1))/(dy1(j)+dy1(j+1))))/(dzn(k)+dzn(k+1))
                evsy1 = (dzn(k+1)*((dy1(j)*sm(i,j-1,k)+dy1(j-1)*sm(i,j,k))/(dy1(j-1)+dy1(j)))+dzn(k)*((dy1(j)*sm(i,j-1,k+1)+dy1(j-1)*sm(i,j,k+1))/(dy1(j-1)+dy1(j))))/(dzn(k)+dzn(k+1))
                viswx2 = (evsx2)*(diu3(i,j,k+1)+diu7(i+1,j,k))
                viswx1 = (evsx1)*(diu3(i-1,j,k+1)+diu7(i,j,k))
                viswy2 = (evsy2)*(diu6(i,j,k+1)+diu8(i,j+1,k))
                viswy1 = (evsy1)*(diu6(i,j-1,k+1)+diu8(i,j,k))
                viswz2 = (evsz2)*2.*diu9(i,j,k+1)
                viswz1 = (evsz1)*2.*diu9(i,j,k)
                vfw = (viswx2-viswx1)/dx1(i)+(viswy2-viswy1)/dy1(j)+(viswz2-viswz1)/dzn(k)
                h(i,j,k) = (h(i,j,k)+vfw)
            end do
        end do
    end do
    return 
end subroutine les
