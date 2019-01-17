module common_sn
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
    implicit real*4(a-h,o-z)
    implicit integer(i-n)
    real a1(1:ip,1:jp+1,1:kp+1),a2(1:ip,1:jp+1,1:kp+1) ,a3(1:ip,1:jp+1,1:kp+1)
    integer irec
   character(len=70) :: filename
end module common_sn
