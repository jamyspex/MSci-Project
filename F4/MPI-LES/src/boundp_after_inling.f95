module module_boundp
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
 contains
subroutine boundp2(p)
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
    real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+1) , intent(InOut) :: p
    integer :: i, j
    do j = 0,jp+1
        do i = 0,ip+1
            p(i,j, 0) = p(i,j,1)
            p(i,j,kp+1) = p(i,j,kp)
        end do
    end do
end subroutine boundp2
subroutine boundp1(p)
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
    real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+1) , intent(InOut) :: p
    integer :: i, j, k
        do k = 0,kp+1
            do j = 0,jp+1
                    p( 0,j,k) = p(1 ,j,k)
                    p(ip+1,j,k) = p(ip,j,k)
            end do
        end do
    do k = 0,kp+1
        do i = 0,ip+1
            p(i, 0,k) = p(i,jp,k) 
            p(i,jp+1,k) = p(i, 1,k) 
        end do
    end do
end subroutine boundp1
end module module_boundp
