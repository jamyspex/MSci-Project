module params_common_sn
! WV I think it would be better to have
! use params_mpi
! which would contain
integer, parameter :: kp=80
! NO MPI
 ! so @4m, max is 1200m x 1200m
    integer, parameter :: ip = 300
    integer, parameter :: jp = 300
    integer, parameter :: ipmax = ip
    integer, parameter :: jpmax = jp
! WV: unused: integer, parameter :: bipmax = 300, bjpmax = 300, bx = 0, by = 0
! WV: not nice
    character(300) :: datafile = '../GIS/Kyoto_1km2_4m_with_buffer.txt'
!-- grid in meters
    real, parameter :: dxgrid = 4.
    real, parameter :: dygrid = 4.
!-- les
 !smagorinsky constant
    real, parameter :: cs0 = 0.14
!-- parameter for anime
    integer, parameter :: i_anime=1
    ! WV: was 20
    integer, parameter :: avetime=2
    integer, parameter :: km_sl=80
!-- parameter for aveflow
    integer, parameter :: i_aveflow=0
!-- for output-if
    integer, parameter :: i_ifdata_out=0
    real, parameter :: dt_orig = 0.05 !0.10 ! seconds
end module params_common_sn
