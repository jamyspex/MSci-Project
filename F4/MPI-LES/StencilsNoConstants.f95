subroutine velnw_feedbf_les_adam_press_merged(alpha,beta,bmask1,cmask1,data21,delx1,diu1,diu2,diu3,diu4,diu5,diu6,diu7,diu8,diu9,dma&
&sk1,dt,dx1,dxs,dy1,dys,dzn,dzs,f,fold,fx,fy,fz,g,gold,h,hold,n,nmax,p,rhs,ro,sm,u,uspd,usum,v,vspd,vsum,w,wsum)
      integer, parameter :: avetime = 2
      real, parameter :: cs0 = 0.14
      real, parameter :: dt_orig = 0.05
      real, parameter :: dxgrid = 4.
      real, parameter :: dygrid = 4.
      integer, parameter :: i_anime = 1
      integer, parameter :: i_aveflow = 0
      integer, parameter :: i_ifdata_out = 0
      integer, parameter :: ip = 300
      integer, parameter :: ipmax = 300
      integer, parameter :: jp = 300
      integer, parameter :: jpmax = 300
      integer, parameter :: km_sl = 80
      integer, parameter :: kp = 80
      integer, parameter :: nmaxp = 50
      real, parameter :: omega = 1.
      real, parameter :: pjuge = 0.0001
      real(4), intent(In) :: alpha
      real(4), intent(In) :: beta
      real(4), dimension(-1:301,0:301,0:81), intent(In) :: bmask1
      real(4), dimension(0:301,-1:301,0:81), intent(In) :: cmask1
      real(4) :: cn1
      real(4) :: cn2l
      real(4) :: cn2s
      real(4) :: cn3l
      real(4) :: cn3s
      real(4) :: cn4l
      real(4) :: cn4s
      real(4) :: dz1
      real(4) :: dz2
      real(4) :: csx1
      character(70), intent(In) :: data21
      character(300) :: datafile = '../GIS/Kyoto_1km2_4m_with_buffer.txt'
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
      real(4), dimension(0:301,0:301,0:81), intent(In) :: dmask1
      real(4), intent(In) :: dt
      real(4) :: dudxx1
      real(4) :: dudyx1
      real(4) :: dudzx1
      real(4) :: dvdxx1
      real(4) :: dvdyx1
      real(4) :: dvdzx1
      real(4) :: dwdxx1
      real(4) :: dwdyx1
      real(4) :: dwdzx1
      real(4), dimension(-1:301), intent(In) :: dx1
      real(4), dimension(0:300), intent(In) :: dxs
      real(4), dimension(0:301), intent(In) :: dy1
      real(4), dimension(0:300), intent(In) :: dys
      real(4), dimension(-1:82), intent(In) :: dzn
      real(4), dimension(-1:82), intent(In) :: dzs
      real(4) :: evsx2
      real(4) :: evsx1
      real(4) :: evsy2
      real(4) :: evsy1
      real(4) :: evsz2
      real(4) :: evsz1
      real(4), dimension(0:300,0:300,0:80), intent(InOut) :: f
      real(4) :: f1x
      real(4) :: f1y
      real(4) :: f1z
      real(4) :: f2x
      real(4) :: f2y
      real(4) :: f2z
      real(4) :: fd
      real(4) :: gd
      real(4) :: hd
      real(4), dimension(300,300,80), intent(InOut) :: fold
      real(4), dimension(0:300,0:300,0:80), intent(Out) :: fx
      real(4), dimension(0:300,0:300,0:80), intent(Out) :: fy
      real(4), dimension(0:300,0:300,0:80), intent(Out) :: fz
      real(4), dimension(0:300,0:300,0:80), intent(InOut) :: g
      real(4), dimension(300,300,80), intent(InOut) :: gold
      real(4), dimension(0:300,0:300,0:80), intent(InOut) :: h
      real(4), dimension(300,300,80), intent(InOut) :: hold
      integer :: i
      integer :: j
      integer :: k
      integer, intent(In) :: n
      integer, intent(In) :: nmax
      integer :: nn
      real(4), dimension(0:1,0:302,0:302,0:81) :: p
      real(4) :: pz
      real(4), dimension(0:301,0:301,0:81), intent(Out) :: rhs
      real(4) :: rhsav
      real(4) :: pav
      real(4) :: area
      real(4) :: pco
      real(4) :: sor
      real(4) :: reltmp
      real(4), intent(In) :: ro
      real(4), dimension(-1:301,-1:301,0:81), intent(Out) :: sm
      integer :: synthIdx0
      integer :: synthIdx1
      integer :: synthIdx2
      integer :: synthIdx3
      real(4), dimension(0:301,-1:301,0:81), intent(InOut) :: u
      real(4), dimension(0:301,0:301), intent(In) :: uspd
      real(4), dimension(0:300,0:300,0:80), intent(InOut) :: usum
      real(4), dimension(0:301,-1:301,0:81), intent(InOut) :: v
      real(4) :: vfu
      real(4) :: vfv
      real(4) :: vfw
      real(4) :: visux2
      real(4) :: visux1
      real(4) :: visuy2
      real(4) :: visuy1
      real(4) :: visuz2
      real(4) :: visuz1
      real(4) :: visvx2
      real(4) :: visvx1
      real(4) :: visvy2
      real(4) :: visvy1
      real(4) :: visvz2
      real(4) :: visvz1
      real(4) :: viswx2
      real(4) :: viswx1
      real(4) :: viswy2
      real(4) :: viswy1
      real(4) :: viswz2
      real(4) :: viswz1
      real(4), dimension(0:301,0:301), intent(In) :: vspd
      real(4), dimension(0:300,0:300,0:80), intent(InOut) :: vsum
      real(4), dimension(0:301,-1:301,-1:81), intent(InOut) :: w
      real(4), dimension(0:300,0:300,0:80), intent(InOut) :: wsum
! Original Subroutine Name: velnw {
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
!}
! Original Subroutine Name: feedbf {
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
!}
! Original Subroutine Name: les {
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
                sm(i,j,k) = (csx1*delx1(k))**2*sqrt(2.*(dudxx1**2+dvdyx1**2+dwdzx1**2)+(dudyx1+dvdxx1)**2+(dwdyx1+dvdzx1)**2+(dudzx1&
&+dwdxx1)**2)
            end do
        end do
    end do
    do k = 2, 80, 1
        do j = 1, 300, 1
            do i = 1, 300, 1
                evsx2 = sm(i+1,j,k)
                evsx1 = sm(i,j,k)
                evsy2 = (dy1(j+1)*((dx1(i+1)*sm(i,j,k)+dx1(i)*sm(i+1,j,k))/(dx1(i)+dx1(i+1)))+dy1(j)*((dx1(i+1)*sm(i,j+1,k)+dx1(i)*s&
&m(i+1,j+1,k))/(dx1(i)+dx1(i+1))))/(dy1(j)+dy1(j+1))
                evsy1 = (dy1(j+1)*((dx1(i+1)*sm(i,j-1,k)+dx1(i)*sm(i+1,j-1,k))/(dx1(i)+dx1(i+1)))+dy1(j)*((dx1(i+1)*sm(i,j,k)+dx1(i)&
&*sm(i+1,j,k))/(dx1(i)+dx1(i+1))))/(dy1(j)+dy1(j+1))
                evsz2 = (dzn(k+1)*((dx1(i+1)*sm(i,j,k)+dx1(i)*sm(i+1,j,k))/(dx1(i)+dx1(i+1)))+dzn(k)*((dx1(i+1)*sm(i,j,k+1)+dx1(i)*s&
&m(i+1,j,k+1))/(dx1(i)+dx1(i+1))))/(dzn(k)+dzn(k+1))
                evsz1 = (dzn(k)*((dx1(i+1)*sm(i,j,k-1)+dx1(i)*sm(i+1,j,k-1))/(dx1(i)+dx1(i+1)))+dzn(k-1)*((dx1(i+1)*sm(i,j,k)+dx1(i)&
&*sm(i+1,j,k))/(dx1(i)+dx1(i+1))))/(dzn(k-1)+dzn(k))
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
                evsx2 = (dy1(j+1)*((dx1(i+1)*sm(i,j,k)+dx1(i)*sm(i+1,j,k))/(dx1(i)+dx1(i+1)))+dy1(j)*((dx1(i+1)*sm(i,j+1,k)+dx1(i)*s&
&m(i+1,j+1,k))/(dx1(i)+dx1(i+1))))/(dy1(j)+dy1(j+1))
                evsx1 = (dy1(j+1)*((dx1(i)*sm(i-1,j,k)+dx1(i-1)*sm(i,j,k))/(dx1(i-1)+dx1(i)))+dy1(j)*((dx1(i)*sm(i-1,j+1,k)+dx1(i-1)&
&*sm(i,j+1,k))/(dx1(i-1)+dx1(i))))/(dy1(j)+dy1(j+1))
                evsz2 = (dzn(k+1)*((dx1(i+1)*sm(i,j,k)+dx1(i)*sm(i+1,j,k))/(dx1(i)+dx1(i+1)))+dzn(k)*((dx1(i+1)*sm(i,j,k+1)+dx1(i)*s&
&m(i+1,j,k+1))/(dx1(i)+dx1(i+1))))/(dzn(k)+dzn(k+1))
                evsz1 = (dzn(k)*((dx1(i+1)*sm(i,j,k-1)+dx1(i)*sm(i+1,j,k-1))/(dx1(i)+dx1(i+1)))+dzn(k-1)*((dx1(i+1)*sm(i,j,k)+dx1(i)&
&*sm(i+1,j,k))/(dx1(i)+dx1(i+1))))/(dzn(k-1)+dzn(k))
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
                evsx2 = (dzn(k+1)*((dx1(i+1)*sm(i,j,k)+dx1(i)*sm(i+1,j,k))/(dx1(i)+dx1(i+1)))+dzn(k)*((dx1(i+1)*sm(i,j,k+1)+dx1(i)*s&
&m(i+1,j,k+1))/(dx1(i)+dx1(i+1))))/(dzn(k)+dzn(k+1))
                evsx1 = (dzn(k+1)*((dx1(i)*sm(i-1,j,k)+dx1(i-1)*sm(i,j,k))/(dx1(i-1)+dx1(i)))+dzn(k)*((dx1(i)*sm(i-1,j,k+1)+dx1(i-1)&
&*sm(i,j,k+1))/(dx1(i-1)+dx1(i))))/(dzn(k)+dzn(k+1))
                evsy2 = (dzn(k+1)*((dy1(j+1)*sm(i,j,k)+dy1(j)*sm(i,j+1,k))/(dy1(j)+dy1(j+1)))+dzn(k)*((dy1(j+1)*sm(i,j,k+1)+dy1(j)*s&
&m(i,j+1,k+1))/(dy1(j)+dy1(j+1))))/(dzn(k)+dzn(k+1))
                evsy1 = (dzn(k+1)*((dy1(j)*sm(i,j-1,k)+dy1(j-1)*sm(i,j,k))/(dy1(j-1)+dy1(j)))+dzn(k)*((dy1(j)*sm(i,j-1,k+1)+dy1(j-1)&
&*sm(i,j,k+1))/(dy1(j-1)+dy1(j))))/(dzn(k)+dzn(k+1))
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
!}
! Original Subroutine Name: adam {
    do k = 1, 80, 1
        do j = 1, 300, 1
            do i = 1, 300, 1
                fd = f(i,j,k)
                gd = g(i,j,k)
                hd = h(i,j,k)
                f(i,j,k) = 1.5*f(i,j,k)-0.5*fold(i,j,k)
                g(i,j,k) = 1.5*g(i,j,k)-0.5*gold(i,j,k)
                h(i,j,k) = 1.5*h(i,j,k)-0.5*hold(i,j,k)
                fold(i,j,k) = fd
                gold(i,j,k) = gd
                hold(i,j,k) = hd
            end do
        end do
    end do
!}
! Original Subroutine Name: press {
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
    do l = 1, 50, 1
        sor = 0.0
        do nrd = 0, 1, 1
            do k = 1, 80, 1
                do j = 1, 300, 1
                    do i = 1, 300, 1
                        do synthIdx3 = 0, 1, 1
                            dz1 = dzs(k-1)
                            dz2 = dzs(k)
                            cn4s = 2./(dz1*(dz1+dz2))
                            cn4l = 2./(dz2*(dz1+dz2))
                            cn3s = 2./(dys(j-1)*(dys(j-1)+dys(j)))
                            cn3l = 2./(dys(j)*(dys(j-1)+dys(j)))
                            cn2s = 2./(dxs(i-1)*(dxs(i-1)+dxs(i)))
                            cn2l = 2./(dxs(i)*(dxs(i-1)+dxs(i)))
                            cn1 = 1./(2./(dxs(i-1)*dxs(i))+2./(dys(j-1)*dys(j))+2./(dz1*dz2))
                            if (nrd==0) then
                                if (synthIdx3==1) then
                                    reltmp = 1.0*(cn1*(cn2l*p(synthIdx3-1,i+1,j,k)+cn2s*p(synthIdx3-1,i-1,j,k)+cn3l*p(synthIdx3-1,i,&
&j+1,k)+cn3s*p(synthIdx3-1,i,j-1,k)+cn4l*p(synthIdx3-1,i,j,k+1)+cn4s*p(synthIdx3-1,i,j,k-1)-rhs(i,j,k))-p(synthIdx3-1,i,j,k))
                                    p(synthIdx3,i,j,k) = p(synthIdx3-1,i,j,k)+reltmp
                                end if
                            else
                                if (synthIdx3==0) then
                                    reltmp = 1.0*(cn1*(cn2l*p(synthIdx3+1,i+1,j,k)+cn2s*p(synthIdx3+1,i-1,j,k)+cn3l*p(synthIdx3+1,i,&
&j+1,k)+cn3s*p(synthIdx3+1,i,j-1,k)+cn4l*p(synthIdx3+1,i,j,k+1)+cn4s*p(synthIdx3+1,i,j,k-1)-rhs(i,j,k))-p(synthIdx3+1,i,j,k))
                                    p(synthIdx3,i,j,k) = p(synthIdx3+1,i,j,k)+reltmp
                                end if
                            end if
                        end do
                    end do
                end do
            end do
            do k = 0, 81, 1
                do j = 0, 301, 1
                    do synthIdx2 = 0, 302, 1
                        do synthIdx3 = 0, 1, 1
                            if (synthIdx2==0 .and. synthIdx3==0) then
                                p(synthIdx3,synthIdx2,j,k) = p(synthIdx3,synthIdx2+1,j,k)
                            end if
                            if (synthIdx3==0 .and. synthIdx2==301) then
                                p(synthIdx3,synthIdx2,j,k) = p(synthIdx3,synthIdx2-1,j,k)
                            end if
                        end do
                    end do
                end do
            end do
            do k = 0, 81, 1
                do synthIdx1 = 0, 302, 1
                    do i = 0, 301, 1
                        do synthIdx3 = 0, 1, 1
                            if (synthIdx1==0 .and. synthIdx3==0) then
                                p(synthIdx3,i,synthIdx1,k) = p(synthIdx3,i,synthIdx1+300,k)
                            end if
                            if (synthIdx3==0 .and. synthIdx1==301) then
                                p(synthIdx3,i,synthIdx1,k) = p(synthIdx3,i,synthIdx1-300,k)
                            end if
                        end do
                    end do
                end do
            end do
        end do
        do synthIdx0 = 0, 81, 1
            do j = 0, 301, 1
                do i = 0, 301, 1
                    do synthIdx3 = 0, 1, 1
                        if (synthIdx0==0 .and. synthIdx3==0) then
                            p(synthIdx3,i,j,synthIdx0) = p(synthIdx3,i,j,synthIdx0+1)
                        end if
                        if (synthIdx3==0 .and. synthIdx0==81) then
                            p(synthIdx3,i,j,synthIdx0) = p(synthIdx3,i,j,synthIdx0-1)
                        end if
                    end do
                end do
            end do
        end do
    end do
    pav = 0.0
    pco = 0.0
    do k = 1, 80, 1
        do j = 1, 300, 1
            do i = 1, 300, 1
                do synthIdx3 = 0, 1, 1
                    if (synthIdx3==0) then
                        pav = pav+p(synthIdx3,i,j,k)*dx1(i)*dy1(j)*dzn(k)
                        pco = pco+dx1(i)*dy1(j)*dzn(k)
                    end if
                end do
            end do
        end do
    end do
    pav = pav/pco
    do k = 1, 80, 1
        do j = 1, 300, 1
            do i = 1, 300, 1
                do synthIdx3 = 0, 1, 1
                    if (synthIdx3==0) then
                        p(synthIdx3,i,j,k) = p(synthIdx3,i,j,k)-pav
                    end if
                end do
            end do
        end do
    end do
    do k = 0, 81, 1
        do j = 0, 301, 1
            do synthIdx2 = 0, 302, 1
                do synthIdx3 = 0, 1, 1
                    if (synthIdx2==0 .and. synthIdx3==0) then
                        p(synthIdx3,synthIdx2,j,k) = p(synthIdx3,synthIdx2+1,j,k)
                    end if
                    if (synthIdx3==0 .and. synthIdx2==301) then
                        p(synthIdx3,synthIdx2,j,k) = p(synthIdx3,synthIdx2-1,j,k)
                    end if
                end do
            end do
        end do
    end do
    do k = 0, 81, 1
        do synthIdx1 = 0, 302, 1
            do i = 0, 301, 1
                do synthIdx3 = 0, 1, 1
                    if (synthIdx1==0 .and. synthIdx3==0) then
                        p(synthIdx3,i,synthIdx1,k) = p(synthIdx3,i,synthIdx1+300,k)
                    end if
                    if (synthIdx3==0 .and. synthIdx1==301) then
                        p(synthIdx3,i,synthIdx1,k) = p(synthIdx3,i,synthIdx1-300,k)
                    end if
                end do
            end do
        end do
    end do
    do synthIdx0 = 0, 81, 1
        do j = 0, 301, 1
            do i = 0, 301, 1
                do synthIdx3 = 0, 1, 1
                    if (synthIdx0==0 .and. synthIdx3==0) then
                        p(synthIdx3,i,j,synthIdx0) = p(synthIdx3,i,j,synthIdx0+1)
                    end if
                    if (synthIdx3==0 .and. synthIdx0==81) then
                        p(synthIdx3,i,j,synthIdx0) = p(synthIdx3,i,j,synthIdx0-1)
                    end if
                end do
            end do
        end do
    end do
!}
end subroutine velnw_feedbf_les_adam_press_merged
