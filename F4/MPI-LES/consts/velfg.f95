subroutine velfg(dx1,dy1,dzn,f,g,dzs,h,u,v,w)
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
      real(4), dimension(-1:301), intent(In) :: dx1
      real(4), dimension(0:301), intent(In) :: dy1
      real(4), dimension(-1:82), intent(In) :: dzn
      real(4), dimension(-1:82), intent(In) :: dzs
      real(4), dimension(0:300,0:300,0:80), intent(Out) :: f
      real(4), dimension(0:300,0:300,0:80), intent(Out) :: g
      real(4), dimension(0:300,0:300,0:80), intent(Out) :: h
      real(4) :: nou1_,nou2_,nou3_,nou4_,nou5_,nou6_,nou7_,nou8_,nou9_
      real(4) :: diu1_,diu2_,diu3_,diu4_,diu5_,diu6_,diu7_,diu8_,diu9_
      real(4) :: cov1_i,cov2_j,cov3_k,cov4_i,cov5_j,cov6_k,cov7_i,cov8_j,cov9_k
      real(4) :: nou1_ip1,nou2_jp1,nou3_kp1,nou4_ip1,nou5_jp1,nou6_kp1,nou7_ip1,nou8_jp1,nou9_kp1
      real(4) :: diu1_ip1,diu2_jp1,diu3_kp1,diu4_ip1,diu5_jp1,diu6_kp1,diu7_ip1,diu8_jp1,diu9_kp1
      real(4) :: cov1_ip1,cov2_jp1,cov3_kp1,cov4_ip1,cov5_jp1,cov6_kp1,cov7_ip1,cov8_jp1,cov9_kp1
      real(4), dimension(0:301,-1:301,0:81), intent(In) :: u
      real(4), dimension(0:301,-1:301,0:81), intent(In) :: v
      real(4), dimension(0:301,-1:301,-1:81), intent(In) :: w
      integer :: i,j,k
      real(4) :: covc,covx1,covy1,covz1
      integer, parameter :: u0 = 0 
    do j = 1, 300, 1
        do i = 1, 300, 1
            if (j==300/2 .and. i==300/2) then
                uspd = (u(i,j,1)**2+((0.5*(v(i,j-1,1)+v(i,j,1))*dx1(i+1)+0.5*(v(i+1,j-1,1)+v(i+1,j,1))*dx1(i))/(dx1(i)+dx1(i+1)))**2)**0.5
                vspd = (v(i,j,1)**2+((0.5*(u(i-1,j,1)+u(i,j,1))*dy1(j+1)+0.5*(u(i-1,j+1,1)+u(i,j+1,1))*dy1(j))/(dy1(j)+dy1(j+1)))**2)**0.5
                write(6, *)('CHK_uspd_vspd=', uspd, vspd)
            end if
        end do
    end do
    do k = 1, 80, 1
        do j = 1, 300, 1
            do i = 1, 300, 1
                nou1_ = (u(i-1,j,k)+u(i,j,k))/2.
                diu1_ = (-u(i-1,j,k)+u(i,j,k))/dx1(i)
                cov1_i = nou1_*diu1_
                nou1_ip1 = (u(i,j,k)+u(i+1,j,k))/2.
                diu1_ip1 = (-u(i,j,k)+u(i+1,j,k))/dx1(i+1)
                cov1_ip1 = nou1_ip1*diu1_ip1
                if (i==300) then
                    cov1_ip1 = cov1_i
                end if
                nou2_ = (dx1(i+1)*v(i,j-1,k)+dx1(i)*v(i+1,j-1,k))/(dx1(i)+dx1(i+1))
                diu2_ = 2.*(-u(i,j-1,k)+u(i,j,k))/(dy1(j-1)+dy1(j))
                cov2_j = nou2_*diu2_
                nou2_jp1 = (dx1(i+1)*v(i,j,k)+dx1(i)*v(i+1,j,k))/(dx1(i)+dx1(i+1))
                diu2_jp1 = 2.*(-u(i,j,k)+u(i,j+1,k))/(dy1(j)+dy1(j+1))
                cov2_jp1 = nou2_jp1*diu2_jp1
                if (j==300) then
                    nou2_ = (dx1(i+1)*v(i,0,k)+dx1(i)*v(i+1,0,k))/(dx1(i)+dx1(i+1))
                    diu2_ = 2.*(-u(i,0,k)+u(i,1,k))/(dy1(0)+dy1(1))
                    cov2_jp1 = nou2_*diu2_
                end if
                nou3_ = (dx1(i+1)*w(i,j,k-1)+dx1(i)*w(i+1,j,k-1))/(dx1(i)+dx1(i+1))
                diu3_ = (-u(i,j,k-1)+u(i,j,k))/dzs(k-1)
                cov3_k = nou3_*diu3_
                nou3_kp1 = (dx1(i+1)*w(i,j,k)+dx1(i)*w(i+1,j,k))/(dx1(i)+dx1(i+1))
                diu3_kp1 = (-u(i,j,k)+u(i,j,k+1))/dzs(k)
                cov3_kp1 = nou3_kp1*diu3_kp1
                if (k==1) then
                    nou3_ = 0.5*(dx1(i+1)*w(i,j,1)+dx1(i)*w(i+1,j,1))/(dx1(i)+dx1(i+1))
                    diu3_ = 0.4*u(i,j,1)/(alog(5.0*dzn(1))*0.2*dzn(1))
                    cov3_k = nou3_*diu3_
                end if
                covx1 = (dx1(i+1)*cov1_i+dx1(i)*cov1_ip1)/(dx1(i)+dx1(i+1))
                covy1 = (cov2_j+cov2_jp1)/2.
                covz1 = (cov3_k+cov3_kp1)/2.
                covc = covx1+covy1+covz1
                f(i,j,k) = (-covc)
            end do
        end do
    end do
    do k = 1, 80, 1
        do j = 1, 300, 1
            do i = 1, 300, 1
                nou4_ = (dy1(j+1)*u(i-1,j,k)+dy1(j)*u(i-1,j+1,k))/(dy1(j)+dy1(j+1))
                diu4_ = 2.*(-v(i-1,j,k)+v(i,j,k))/(dx1(i-1)+dx1(i))
                cov4_i = (nou4_-0)*diu4_
                nou4_ip1 = (dy1(j+1)*u(i,j,k)+dy1(j)*u(i,j+1,k))/(dy1(j)+dy1(j+1))
                diu4_ip1 = 2.*(-v(i,j,k)+v(i+1,j,k))/(dx1(i)+dx1(i+1))
                cov4_ip1 = (nou4_ip1-0)*diu4_ip1
                if (i==300) then
                    cov4_ip1 = cov4_i
                end if
                nou5_ = (v(i,j-1,k)+v(i,j,k))/2.
                diu5_ = (-v(i,j-1,k)+v(i,j,k))/dy1(j)
                cov5_j = nou5_*diu5_
                nou5_jp1 = (v(i,j,k)+v(i,j+1,k))/2.
                diu5_jp1 = (-v(i,j,k)+v(i,j+1,k))/dy1(j+1)
                cov5_jp1 = nou5_jp1*diu5_jp1
                if (j==300) then
                    nou5_ = (v(i,1,k)+v(i,2,k))/2.
                    diu5_ = (-v(i,1,k)+v(i,2,k))/dy1(j)
                    cov5_jp1 = nou5_*diu5_
                end if
                nou6_ = (dy1(j+1)*w(i,j,k-1)+dy1(j)*w(i,j+1,k-1))/(dy1(j)+dy1(j+1))
                diu6_ = (-v(i,j,k-1)+v(i,j,k))/dzs(k-1)
                cov6_k = nou6_*diu6_
                nou6_kp1 = (dy1(j+1)*w(i,j,k)+dy1(j)*w(i,j+1,k))/(dy1(j)+dy1(j+1))
                diu6_kp1 = (-v(i,j,k)+v(i,j,k+1))/dzs(k)
                cov6_kp1 = nou6_kp1*diu6_kp1
                if (k==1) then
                    nou6_ = 0.5*(dy1(j+1)*w(i,j,1)+dy1(j)*w(i,j+1,1))/(dy1(j)+dy1(j+1))
                    diu6_ = 0.4*v(i,j,1)/(alog(5.0*dzn(1))*0.2*dzn(1))
                    cov6_k = nou6_*diu6_
                end if
                covx1 = (cov4_i+cov4_ip1)/2.
                covy1 = (dy1(j+1)*cov5_j+dy1(j)*cov5_jp1)/(dy1(j)+dy1(j+1))
                covz1 = (cov6_k+cov6_kp1)/2.
                covc = covx1+covy1+covz1
                g(i,j,k) = (-covc)
            end do
        end do
    end do
    do k = 1, 79, 1
        do j = 1, 300, 1
            do i = 1, 300, 1
                nou7_ = (dzn(k+1)*u(i-1,j,k)+dzn(k)*u(i-1,j,k+1))/(dzn(k)+dzn(k+1))
                diu7_ = 2.*(-w(i-1,j,k)+w(i,j,k))/(dx1(i-1)+dx1(i))
                cov7_i = (nou7_-0)*diu7_
                nou7_ip1 = (dzn(k+1)*u(i,j,k)+dzn(k)*u(i,j,k+1))/(dzn(k)+dzn(k+1))
                diu7_ip1 = 2.*(-w(i,j,k)+w(i+1,j,k))/(dx1(i)+dx1(i+1))
                cov7_ip1 = (nou7_-0)*diu7_
                if (i==300) then
                    cov7_ip1 = cov7_i
                end if
                nou8_ = (dzn(k+1)*v(i,j-1,k)+dzn(k)*v(i,j-1,k+1))/(dzn(k)+dzn(k+1))
                diu8_ = 2.*(-w(i,j-1,k)+w(i,j,k))/(dy1(j-1)+dy1(j))
                cov8_j = nou8_*diu8_
                nou8_jp1 = (dzn(k+1)*v(i,j,k)+dzn(k)*v(i,j,k+1))/(dzn(k)+dzn(k+1))
                diu8_jp1 = 2.*(-w(i,j,k)+w(i,j+1,k))/(dy1(j)+dy1(j+1))
                cov8_jp1 = nou8_jp1*diu8_jp1
                if (j==300) then
                    nou8_ = (dzn(k+1)*v(i,0,k)+dzn(k)*v(i,0,k+1))/(dzn(k)+dzn(k+1))
                    diu8_ = 2.*(-w(i,0,k)+w(i,1,k))/(dy1(0)+dy1(1))
                    cov8_jp1 = nou8_*diu8_
                end if
                nou9_ = (w(i,j,k-1)+w(i,j,k))/2.
                diu9_ = (-w(i,j,k-1)+w(i,j,k))/dzn(k)
                cov9_k = nou9_*diu9_
                nou9_kp1 = (w(i,j,k)+w(i,j,k+1))/2.
                diu9_kp1 = (-w(i,j,k)+w(i,j,k+1))/dzn(k+1)
                cov9_kp1 = nou9_kp1*diu9_kp1
                covx1 = (cov7_i+cov7_ip1)/2.
                covy1 = (cov8_j+cov8_jp1)/2.
                covz1 = (dzn(k+1)*cov9_k+dzn(k)*cov9_kp1)/(dzn(k)+dzn(k+1))
                covc = covx1+covy1+covz1
                h(i,j,k) = (-covc)
            end do
        end do
    end do
    do k = 1, 80, 1
        do j = 1, 300, 1
            f(0,j,k) = f(1,j,k)
        end do
    end do
    do k = 1, 80, 1
        do i = 1, 300, 1
            g(i,0,k) = g(i,300,k)
        end do
    end do
    do j = 1, 300, 1
        do i = 1, 300, 1
            h(i,j,0) = 0.0
            h(i,j,80) = 0.0
        end do
    end do
    return 
end subroutine velfg
