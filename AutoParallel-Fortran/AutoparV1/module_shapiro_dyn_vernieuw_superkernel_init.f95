module module_shapiro_dyn_vernieuw_superkernel_init

integer, parameter :: ST_SHAPIRO_MAP_15 = 0 !  shapiro_map_15
integer, parameter :: ST_DYN_MAP_38 = 1 !  dyn_map_38
integer, parameter :: ST_VERNIEUW_MAP_23 = 2 !  vernieuw_map_23
        integer, parameter ::DT_BUF_IDX = 5
        integer, parameter ::DU_BUF_IDX = 10
        integer, parameter ::DV_BUF_IDX = 12
        integer, parameter ::DX_BUF_IDX = 7
        integer, parameter ::DY_BUF_IDX = 8
        integer, parameter ::EPS_BUF_IDX = 3
        integer, parameter ::ETA_BUF_IDX = 4
        integer, parameter ::ETAN_BUF_IDX = 2
        integer, parameter ::G_BUF_IDX = 6
        integer, parameter ::H_BUF_IDX = 13
        integer, parameter ::HMIN_BUF_IDX = 17
        integer, parameter ::HZERO_BUF_IDX = 16
        integer, parameter ::STATE_PTR_BUF_IDX = 18
        integer, parameter ::U_BUF_IDX = 9
        integer, parameter ::UN_BUF_IDX = 14
        integer, parameter ::V_BUF_IDX = 11
        integer, parameter ::VN_BUF_IDX = 15
        integer, parameter ::WET_BUF_IDX = 1

contains

    subroutine shapiro_dyn_vernieuw_superkernel_init()

        use oclWrapper
        character(len=*), parameter :: srcstr = "module_shapiro_dyn_vernieuw_superkernel.cl"
        character(len=*), parameter :: kstr   = "shapiro_dyn_vernieuw_superkernel"
! parameters
              integer(4), parameter :: ny = 500 
              integer(4), parameter :: nx = 500 
! declarations
        integer, dimension(0:(ny + 1),0:(nx + 1)) :: wet
        real, dimension(0:(ny + 1),0:(nx + 1)) :: etan
        real :: eps
        real :: dt
        real :: g
        real :: dx
        real :: dy
        real, dimension(0:(ny + 1),0:(nx + 1)) :: u
        real, dimension(0:(ny + 1),0:(nx + 1)) :: v
        real, dimension(0:(ny + 1),0:(nx + 1)) :: h
        real, dimension(0:(ny + 1),0:(nx + 1)) :: eta
        real, dimension(0:(ny + 1),0:(nx + 1)) :: hzero
        real :: hmin
        real, dimension(0:(ny + 1),0:(nx + 1)) :: un
        real, dimension(0:(ny + 1),0:(nx + 1)) :: vn
        real, dimension(0:(ny + 1),0:(nx + 1)) :: du
        real, dimension(0:(ny + 1),0:(nx + 1)) :: dv
        integer, dimension(1) :: state_ptr
! buffer declarations
        integer(8) :: wet_buf
        integer(8) :: etan_buf
        integer(8) :: eps_buf
        integer(8) :: dt_buf
        integer(8) :: g_buf
        integer(8) :: dx_buf
        integer(8) :: dy_buf
        integer(8) :: u_buf
        integer(8) :: v_buf
        integer(8) :: h_buf
        integer(8) :: eta_buf
        integer(8) :: hzero_buf
        integer(8) :: hmin_buf
        integer(8) :: un_buf
        integer(8) :: vn_buf
        integer(8) :: du_buf
        integer(8) :: dv_buf
        integer(8) :: state_ptr_buf
        integer, dimension(2) :: wet_sz
        integer, dimension(2) :: etan_sz
        integer, dimension(1) :: eps_ptr_sz
        integer, dimension(1) :: dt_ptr_sz
        integer, dimension(1) :: g_ptr_sz
        integer, dimension(1) :: dx_ptr_sz
        integer, dimension(1) :: dy_ptr_sz
        integer, dimension(2) :: u_sz
        integer, dimension(2) :: v_sz
        integer, dimension(2) :: h_sz
        integer, dimension(2) :: eta_sz
        integer, dimension(2) :: hzero_sz
        integer, dimension(1) :: hmin_ptr_sz
        integer, dimension(2) :: un_sz
        integer, dimension(2) :: vn_sz
        integer, dimension(2) :: du_sz
        integer, dimension(2) :: dv_sz
        integer, dimension(1) :: state_ptr_sz
        real, dimension(1) :: eps_ptr
        real, dimension(1) :: dt_ptr
        real, dimension(1) :: g_ptr
        real, dimension(1) :: dx_ptr
        real, dimension(1) :: dy_ptr
        real, dimension(1) :: hmin_ptr

        call oclInit(srcstr,kstr)

        wet_sz = shape(wet)
        etan_sz = shape(etan)
        eps_ptr_sz = shape(eps_ptr)
        dt_ptr_sz = shape(dt_ptr)
        g_ptr_sz = shape(g_ptr)
        dx_ptr_sz = shape(dx_ptr)
        dy_ptr_sz = shape(dy_ptr)
        u_sz = shape(u)
        v_sz = shape(v)
        h_sz = shape(h)
        eta_sz = shape(eta)
        hzero_sz = shape(hzero)
        hmin_ptr_sz = shape(hmin_ptr)
        un_sz = shape(un)
        vn_sz = shape(vn)
        du_sz = shape(du)
        dv_sz = shape(dv)
        state_ptr_sz = shape(state_ptr)

        call oclMake2DIntArrayReadWriteBuffer(wet_buf,wet_sz,wet)
        call oclMake2DFloatArrayReadWriteBuffer(etan_buf,etan_sz,etan)
        call oclMake1DFloatArrayReadWriteBuffer(eps_buf,eps_ptr_sz,eps_ptr)! Automatic conversion to array
        call oclMake1DFloatArrayReadWriteBuffer(dt_buf,dt_ptr_sz,dt_ptr)! Automatic conversion to array
        call oclMake1DFloatArrayReadWriteBuffer(g_buf,g_ptr_sz,g_ptr)! Automatic conversion to array
        call oclMake1DFloatArrayReadWriteBuffer(dx_buf,dx_ptr_sz,dx_ptr)! Automatic conversion to array
        call oclMake1DFloatArrayReadWriteBuffer(dy_buf,dy_ptr_sz,dy_ptr)! Automatic conversion to array
        call oclMake2DFloatArrayReadWriteBuffer(u_buf,u_sz,u)
        call oclMake2DFloatArrayReadWriteBuffer(v_buf,v_sz,v)
        call oclMake2DFloatArrayReadWriteBuffer(h_buf,h_sz,h)
        call oclMake2DFloatArrayReadWriteBuffer(eta_buf,eta_sz,eta)
        call oclMake2DFloatArrayReadWriteBuffer(hzero_buf,hzero_sz,hzero)
        call oclMake1DFloatArrayReadWriteBuffer(hmin_buf,hmin_ptr_sz,hmin_ptr)! Automatic conversion to array
        call oclMake2DFloatArrayReadWriteBuffer(un_buf,un_sz,un)
        call oclMake2DFloatArrayReadWriteBuffer(vn_buf,vn_sz,vn)
        call oclMake2DFloatArrayReadWriteBuffer(du_buf,du_sz,du)
        call oclMake2DFloatArrayReadWriteBuffer(dv_buf,dv_sz,dv)
        call oclMake1DIntArrayReadWriteBuffer(state_ptr_buf,state_ptr_sz,state_ptr)

        call oclSetIntArrayArg(0, wet_buf)
        call oclSetFloatArrayArg(1, etan_buf)
        call oclSetFloatArrayArg(2, eps_buf)
        call oclSetFloatArrayArg(4, dt_buf)
        call oclSetFloatArrayArg(5, g_buf)
        call oclSetFloatArrayArg(6, dx_buf)
        call oclSetFloatArrayArg(7, dy_buf)
        call oclSetFloatArrayArg(8, u_buf)
        call oclSetFloatArrayArg(10, v_buf)
        call oclSetFloatArrayArg(12, h_buf)
        call oclSetFloatArrayArg(3, eta_buf)
        call oclSetFloatArrayArg(15, hzero_buf)
        call oclSetFloatArrayArg(16, hmin_buf)
        call oclSetFloatArrayArg(13, un_buf)
        call oclSetFloatArrayArg(14, vn_buf)
        call oclSetFloatArrayArg(9, du_buf)
        call oclSetFloatArrayArg(11, dv_buf)
        call oclSetIntArrayArg(17, state_ptr_buf)

        call oclStoreBuffer(DT_BUF_IDX, dt_buf)
        call oclStoreBuffer(DU_BUF_IDX, du_buf)
        call oclStoreBuffer(DV_BUF_IDX, dv_buf)
        call oclStoreBuffer(DX_BUF_IDX, dx_buf)
        call oclStoreBuffer(DY_BUF_IDX, dy_buf)
        call oclStoreBuffer(EPS_BUF_IDX, eps_buf)
        call oclStoreBuffer(ETA_BUF_IDX, eta_buf)
        call oclStoreBuffer(ETAN_BUF_IDX, etan_buf)
        call oclStoreBuffer(G_BUF_IDX, g_buf)
        call oclStoreBuffer(H_BUF_IDX, h_buf)
        call oclStoreBuffer(HMIN_BUF_IDX, hmin_buf)
        call oclStoreBuffer(HZERO_BUF_IDX, hzero_buf)
        call oclStoreBuffer(STATE_PTR_BUF_IDX, state_ptr_buf)
        call oclStoreBuffer(U_BUF_IDX, u_buf)
        call oclStoreBuffer(UN_BUF_IDX, un_buf)
        call oclStoreBuffer(V_BUF_IDX, v_buf)
        call oclStoreBuffer(VN_BUF_IDX, vn_buf)
        call oclStoreBuffer(WET_BUF_IDX, wet_buf)


    end subroutine shapiro_dyn_vernieuw_superkernel_init
end module module_shapiro_dyn_vernieuw_superkernel_init