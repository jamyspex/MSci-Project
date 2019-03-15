subroutine dyn_shapiro_vernieuw_merged(dt,dx,dy,eps,eta,etan,g,h,hmin,hzero,j,k,u,un,v,vn,wet)
      integer(4), parameter :: nx = 500
      integer(4), parameter :: ny = 500
      real :: dt
      real, dimension(0:501,0:501) :: du
      real :: duu
      real, dimension(0:501,0:501) :: dv
      real :: dvv
      real :: dx
      real :: dy
      real :: eps
      real, dimension(0:501,0:501), intent(InOut) :: eta
      real, dimension(0:501,0:501) :: etan
      real :: g
      real, dimension(0:501,0:501), intent(InOut) :: h
      real :: hen
      real :: hep
      real, intent(In) :: hmin
      real :: hnn
      real :: hnp
      real :: hsn
      real :: hsp
      real :: hue
      real :: huw
      real :: hvn
      real :: hvs
      real :: hwn
      real :: hwp
      real, dimension(0:501,0:501), intent(In) :: hzero
      integer, intent(InOut) :: j
      integer, intent(InOut) :: k
      real :: term1
      real :: term2
      real :: term3
      real, dimension(0:501,0:501), intent(Out) :: u
      real, dimension(0:501,0:501), intent(InOut) :: un
      real :: uu
      real, dimension(0:501,0:501), intent(Out) :: v
      real, dimension(0:501,0:501), intent(InOut) :: vn
      real :: vv
      integer, dimension(0:501,0:501), intent(Out) :: wet
! Original Subroutine Name: dyn {
    do j = 1, 500, 1
        do k = 1, 500, 1
                du(j,k) = -dt*g*(eta(j,k+1)-eta(j,k))/dx
                dv(j,k) = -dt*g*(eta(j+1,k)-eta(j,k))/dy
        end do
    end do
    do j = 1, 500, 1
        do k = 1, 500, 1
                un(j,k) = 0.0
                uu = u(j,k)
                duu = du(j,k)
                if (wet(j,k)==1) then
                                if ((wet(j,k+1)==1) .or. (duu>0.0)) then
                                                                un(j,k) = uu+duu
                                end if
                else
                                if ((wet(j,k+1)==1) .and. (duu<0.0)) then
                                                                un(j,k) = uu+duu
                                end if
                end if
                vv = v(j,k)
                dvv = dv(j,k)
                vn(j,k) = 0.0
                if (wet(j,k)==1) then
                                if ((wet(j+1,k)==1) .or. (dvv>0.0)) then
                                                                vn(j,k) = vv+dvv
                                end if
                else
                                if ((wet(j+1,k)==1) .and. (dvv<0.0)) then
                                                                vn(j,k) = vv+dvv
                                end if
                end if
        end do
    end do
    do j = 1, 500, 1
        do k = 1, 500, 1
                hep = 0.5*(un(j,k)+abs(un(j,k)))*h(j,k)
                hen = 0.5*(un(j,k)-abs(un(j,k)))*h(j,k+1)
                hue = hep+hen
                hwp = 0.5*(un(j,k-1)+abs(un(j,k-1)))*h(j,k-1)
                hwn = 0.5*(un(j,k-1)-abs(un(j,k-1)))*h(j,k)
                huw = hwp+hwn
                hnp = 0.5*(vn(j,k)+abs(vn(j,k)))*h(j,k)
                hnn = 0.5*(vn(j,k)-abs(vn(j,k)))*h(j+1,k)
                hvn = hnp+hnn
                hsp = 0.5*(vn(j-1,k)+abs(vn(j-1,k)))*h(j-1,k)
                hsn = 0.5*(vn(j-1,k)-abs(vn(j-1,k)))*h(j,k)
                hvs = hsp+hsn
                etan(j,k) = eta(j,k)-dt*(hue-huw)/dx-dt*(hvn-hvs)/dy
        end do
    end do
!}
! Original Subroutine Name: shapiro {
    do j = 1, 500, 1
        do k = 1, 500, 1
                if (wet(j,k)==1) then
                                term1 = (1.0-0.25*eps*(wet(j,k+1)+wet(j,k-1)+wet(j+1,k)+wet(j-1,k)))*etan(j,k)
                                term2 = 0.25*eps*(wet(j,k+1)*etan(j,k+1)+wet(j,k-1)*etan(j,k-1))
                                term3 = 0.25*eps*(wet(j+1,k)*etan(j+1,k)+wet(j-1,k)*etan(j-1,k))
                                eta(j,k) = term1+term2+term3
                else
                                eta(j,k) = etan(j,k)
                end if
        end do
    end do
!}
! Original Subroutine Name: vernieuw {
    do j = 0, 501, 1
        do k = 0, 501, 1
                h(j,k) = hzero(j,k)+eta(j,k)
                wet(j,k) = 1
                if (h(j,k)<hmin) then
                                wet(j,k) = 0
                end if
                u(j,k) = un(j,k)
                v(j,k) = vn(j,k)
        end do
    end do
!}
end subroutine dyn_shapiro_vernieuw_merged
