module module_adam
 contains
subroutine adam(n,nmax,data21,fold,gold,hold,&
f,g,h)
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
    character(len=70), intent(In) :: data21
    real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(InOut) :: f
    real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(InOut) :: g
    real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(InOut) :: h
    real(kind=4), dimension(ip,jp,kp) , intent(InOut) :: fold
    real(kind=4), dimension(ip,jp,kp) , intent(InOut) :: gold
    real(kind=4), dimension(ip,jp,kp) , intent(InOut) :: hold
    integer, intent(In) :: n
    integer, intent(In) :: nmax
    integer :: i,j,k
    real(kind=4) :: fd,gd,hd
    do k = 1,kp
        do j = 1,jp
            do i = 1,ip
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
end subroutine adam
end module module_adam
