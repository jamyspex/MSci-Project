module module_boundsm
 contains
subroutine boundsm(sm)
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
    real(kind=4), dimension(-1:ip+1,-1:jp+1,0:kp+1) , intent(InOut) :: sm
    integer :: i, j, k
        do k = 0,kp+1
            do j = -1,jp+1
                    sm( 0,j,k) = sm(1 ,j,k) 
                    sm(ip+1,j,k) = sm(ip,j,k)
            end do
        end do
        do k = 0,kp+1
            do i = 0,ip+1
                    sm(i,jp+1,k) = sm(i,jp ,k)
                    sm(i,0,k) = sm(i,1 ,k) 
            end do
        end do
    do j = -1,jp+1
        do i = 0,ip+1
            sm(i,j, 0) = -sm(i,j, 1)
            sm(i,j,kp+1) = sm(i,j,kp)
        end do
    end do
end subroutine boundsm
end module module_boundsm
