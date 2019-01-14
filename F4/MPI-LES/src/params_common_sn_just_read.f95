module params_common_sn
#ifdef MPI
    use communication_helper
    integer, parameter :: procPerRow = PROC_PER_ROW, procPerCol = PROC_PER_COL, dimensions = 2
    integer :: dimensionSizes(dimensions)
    logical :: periodicDimensions(dimensions)
    integer :: coordinates(dimensions), neighbours(2*dimensions)
    logical :: reorder
    data dimensionSizes /procPerCol,procPerRow/, periodicDimensions /.false.,.false./, &
    reorder /.false./
#endif
    integer, parameter :: ipmax = 3000, jpmax = 300
#ifndef TEST_SMALL_DOMAIN
#ifdef MPI
    integer, parameter :: ip = 3000/PROC_PER_COL ! rows per process
    integer, parameter :: jp = 300/PROC_PER_ROW ! columns per process
    integer, parameter :: kp=105
#else
    integer, parameter :: ip = 300, jp = 300, kp = 105
#endif
#else
    integer, parameter :: ip = 25, jp = 25, kp = 105
#endif
end module params_common_sn
