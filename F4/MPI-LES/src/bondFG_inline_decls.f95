module module_bondFG
 contains
subroutine bondfg(f,g,h)
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
    real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(InOut) :: f
    real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(InOut) :: g
    real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(Out) :: h
    integer :: i, j, k
        do k = 1,kp
            do j = 1,jp
                f( 0,j,k) = f(1 ,j,k)
            end do
        end do
    do k = 1,kp
        do i = 1,ip
            g(i, 0,k) = g(i,jp ,k) 
        end do
    end do
    do j = 1,jp
        do i = 1,ip
            h(i,j, 0) = 0.0
            h(i,j,kp) = 0.0
        end do
    end do
end subroutine bondFG
end module module_bondFG
