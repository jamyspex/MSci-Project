program main
    use module_init
    use module_grid
    use module_set
    use module_timdata
#ifdef TIMSERIS_FIXED
    use module_timseris
#endif
    use module_aveflow
    use module_ifdata
#if IANIME == 1
    use module_anime
#endif
#ifdef _OPENCL_LES_WV
    use module_LES_combined_ocl
#else
    use module_velnw
    use module_bondv1
    use module_bondv1_data24
    use module_velFG
#if IFBF == 1
    use module_feedbf
#endif
    use module_les
    use module_press
    use module_adam
#endif
    use common_sn
    real(kind=4) :: alpha
    integer :: ianime
    integer :: ical
    integer :: ifbf
    integer :: im
    integer :: jm
    integer :: km
    integer :: n
    integer :: n0
    integer :: n1
    integer :: nmax
    real(kind=4) :: beta
    character(len=70) :: data10
    character(len=70) :: data11
    character(len=70) :: data12
    character(len=70) :: data13
    character(len=70) :: data14
    character(len=70) :: data20
    character(len=70) :: data21
    character(len=70) :: data22
    character(len=70) :: data23
    character(len=70) :: data24
    character(len=70) :: data25
    character(len=70) :: data26
    character(len=70) :: data27
    character(len=70) :: data30
    character(len=70) :: data31
    real(kind=4) :: dt
    real(kind=4) :: ro
    real(kind=4) :: time
    real(kind=4) :: vn
#ifdef TIMINGS
    integer :: clock_rate
#endif
    real(kind=4), dimension(0:ip+1,0:jp+1,0:kp+1)  :: amask1
    real(kind=4), dimension(ip,jp,kp)  :: avel
    real(kind=4), dimension(ip,jp,kp)  :: avep
    real(kind=4), dimension(ip,jp,kp)  :: avesm
    real(kind=4), dimension(ip,jp,kp)  :: avesmsm
    real(kind=4), dimension(ip,kp)  :: avesu
    real(kind=4), dimension(ip,kp)  :: avesuu
    real(kind=4), dimension(ip,kp)  :: avesv
    real(kind=4), dimension(ip,kp)  :: avesvv
    real(kind=4), dimension(ip,kp)  :: avesw
    real(kind=4), dimension(ip,kp)  :: avesww
    real(kind=4), dimension(ip,jp,0:kp)  :: aveu
    real(kind=4), dimension(ip,jp,kp)  :: aveuu
    real(kind=4), dimension(ip,jp,0:kp)  :: avev
    real(kind=4), dimension(ip,jp,kp)  :: avevv
    real(kind=4), dimension(ip+1,jp,0:kp+2)  :: avew
    real(kind=4), dimension(ip,jp,kp)  :: aveww
    real(kind=4), dimension(-1:ip+1,0:jp+1,0:kp+1)  :: bmask1
    real(kind=4), dimension(0:ip+1,-1:jp+1,0:kp+1)  :: cmask1
    real(kind=4), dimension(ip,jp,kp)  :: cn1
    real(kind=4), dimension(ip)  :: cn2l
    real(kind=4), dimension(ip)  :: cn2s
    real(kind=4), dimension(jp)  :: cn3l
    real(kind=4), dimension(jp)  :: cn3s
    real(kind=4), dimension(kp)  :: cn4l
    real(kind=4), dimension(kp)  :: cn4s
    real(kind=4), dimension(-1:ip+2,0:jp+2,0:kp+2)  :: cov1
    real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2)  :: cov2
    real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2)  :: cov3
    real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2)  :: cov4
    real(kind=4), dimension(-1:ip+2,0:jp+2,0:kp+2)  :: cov5
    real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2)  :: cov6
    real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2)  :: cov7
    real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2)  :: cov8
    real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2)  :: cov9
    real(kind=4), dimension(kp)  :: delx1
    real(kind=4), dimension(0:ip,jp,kp)  :: dfu1
    real(kind=4), dimension(ip,0:jp,kp)  :: dfv1
    real(kind=4), dimension(ip,jp,kp)  :: dfw1
    real(kind=4), dimension(-1:ip+2,0:jp+2,0:kp+2)  :: diu1
    real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2)  :: diu2
    real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2)  :: diu3
    real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2)  :: diu4
    real(kind=4), dimension(-1:ip+2,0:jp+2,0:kp+2)  :: diu5
    real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2)  :: diu6
    real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2)  :: diu7
    real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2)  :: diu8
    real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2)  :: diu9
    real(kind=4), dimension(0:ip+1,0:jp+1,0:kp+1)  :: dmask1
    real(kind=4), dimension(-1:ip+1)  :: dx1
    real(kind=4), dimension(0:ip)  :: dxl
    real(kind=4), dimension(0:ip)  :: dxs
    real(kind=4), dimension(0:jp+1)  :: dy1
    real(kind=4), dimension(0:jp)  :: dyl
    real(kind=4), dimension(0:jp)  :: dys
    real(kind=4), dimension(-1:kp+2)  :: dzn
    real(kind=4), dimension(-1:kp+2)  :: dzs
    real(kind=4), dimension(0:ip,0:jp,0:kp)  :: f
#if ICAL == 1
    real(kind=4), dimension(ip,jp,kp)  :: fghold
#endif
    real(kind=4), dimension(ip,jp,kp)  :: fold
    real(kind=4), dimension(0:ip,0:jp,0:kp)  :: fx
    real(kind=4), dimension(0:ip,0:jp,0:kp)  :: fy
    real(kind=4), dimension(0:ip,0:jp,0:kp)  :: fz
    real(kind=4), dimension(0:ip,0:jp,0:kp)  :: g
    real(kind=4), dimension(ip,jp,kp)  :: gold
    real(kind=4), dimension(0:ip,0:jp,0:kp)  :: h
    real(kind=4), dimension(ip,jp,kp)  :: hold
#ifndef _OPENCL_LES_WV
    real(kind=4), dimension(ip,jp,kp)  :: fghold
#endif
    real(kind=4), dimension(-1:ip+2,0:jp+2,0:kp+2)  :: nou1
    real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2)  :: nou2
    real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2)  :: nou3
    real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2)  :: nou4
    real(kind=4), dimension(-1:ip+2,0:jp+2,0:kp+2)  :: nou5
    real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2)  :: nou6
    real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2)  :: nou7
    real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2)  :: nou8
    real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2)  :: nou9
    real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+1)  :: p
    real(kind=4), dimension(0:ip+1,0:jp+1,0:kp+1)  :: rhs
    real(kind=4), dimension(-1:ip+1,-1:jp+1,0:kp+1)  :: sm
    real(kind=4), dimension(0:ip+1,-1:jp+1,0:kp+1)  :: u
    real(kind=4), dimension(0:ip,0:jp,0:kp)  :: usum
    real(kind=4), dimension(ip,jp,kp)  :: uwfx
    real(kind=4), dimension(ip,kp)  :: uwfxs
    real(kind=4), dimension(0:ip+1,-1:jp+1,0:kp+1)  :: v
    real(kind=4), dimension(0:ip,0:jp,0:kp)  :: vsum
    real(kind=4), dimension(0:ip+1,-1:jp+1,-1:kp+1)  :: w
    real(kind=4), dimension(0:ip,0:jp,0:kp)  :: wsum
    real(kind=4), dimension(0:kp+2)  :: z2
    real(kind=4), dimension(-1:ipmax+1,-1:jpmax+1)  :: zbm
!wall function
    real(kind=4), dimension(0:ip+1,0:jp+1)  :: uspd
    real(kind=4), dimension(0:ip+1,0:jp+1)  :: vspd
!idata24
    integer :: idata24
!jdata24
    integer :: jdata24
!nspec
    integer :: nspec
    real(kind=4), dimension(1,1,36001,kp) :: ut_x1_2
    real(kind=4), dimension(1,1,36001,kp) :: ut_x2_2
    real(kind=4), dimension(1,1,36001,kp) :: vt_x1_2
    real(kind=4), dimension(1,1,36001,kp) :: vt_x2_2
    real(kind=4), dimension(1,1,36001,kp) :: wt_x1_2
    real(kind=4), dimension(1,1,36001,kp) :: wt_x2_2
    real(kind=4), dimension(1,kp,36001)  :: u_spany2
    real(kind=4), dimension(1,kp,36001)  :: v_spany2
    real(kind=4), dimension(1,kp,36001)  :: w_spany2
    real(kind=4), dimension(1,kp,36001)  :: u_spany3
    real(kind=4), dimension(1,kp,36001)  :: v_spany3
    real(kind=4), dimension(1,kp,36001)  :: w_spany3
    real(kind=4), dimension(19,kp,36001)  :: u_x1_19_spany2
    real(kind=4), dimension(19,kp,36001)  :: v_x1_19_spany2
    real(kind=4), dimension(19,kp,36001)  :: w_x1_19_spany2
    real(kind=4), dimension(19,kp,36001)  :: u_x1_19_spany3
    real(kind=4), dimension(19,kp,36001)  :: v_x1_19_spany3
    real(kind=4), dimension(19,kp,36001)  :: w_x1_19_spany3
#ifdef TIMINGS
    integer (kind=4), dimension(0:9) :: timestamp
#endif
#ifdef MPI
    call initialise_mpi()
    if (mpi_size .ne. procPerRow * procPerCol) then
        print*, 'Needed ', (procPerRow * procPerCol), ' processes, got ', mpi_size
        call MPI_Abort(communicator, 1, ierror)
    end if
    call setupCartesianVirtualTopology(dimensions, dimensionSizes, &
                                       periodicDimensions, coordinates, &
                                       neighbours, reorder)
#endif
#ifdef USE_NETCDF_OUTPUT
    call init_netcdf_file()
#endif
    call set(data10,data11,data20,data21,data22,data23,data24,data25,data26,&
             data27,data30,data31,im,jm,km,ifbf,ianime,ical,n0,n1,nmax,dt,ro,&
             vn,alpha,beta,data12,data13,data14,idata24,nspec,jdata24)
    call grid(dx1,dxl,dy1,dyl,z2,dzn,dzs,dxs,dys)
    call timdata()
    call init(km,jm,im,u,v,w,p,cn2s,dxs,cn2l,cn3s,dys,cn3l,dzs,cn4s,cn4l,cn1,&
              amask1,bmask1,cmask1,dmask1,zbm,z2,dzn)
!    n0=200
    call ifdata( &
!#if ICAL == 1
                data30,data31, fold,gold,hold,fghold, time, &
!#endif
                n,u,im,jm,km,v,w,p,usum,vsum,wsum,delx1,dx1,dy1,dzn,diu1,diu2,&
                diu3,diu4,diu5,diu6,diu7,diu8,diu9,sm,f,g,h,z2,dt,dxs,cov1, &
                cov2,cov3,dfu1,vn,cov4,cov5,cov6,dfv1,cov7,cov8,cov9,dfw1,dzs,&
                nou1,nou5,nou9,nou2,nou3,nou4,nou6,nou7,nou8,bmask1,cmask1,&
                dmask1,alpha,beta,fx,fy,fz,amask1,zbm,ical)
!     n=n0
#ifdef _OPENCL_LES_WV
    call initialise_LES_kernel(p,u,v,w,usum,vsum,wsum,f,g,h,fold,gold,hold, &
                               diu1, diu2, diu3, diu4, diu5, diu6, diu7, diu8, &
                               diu9, amask1, bmask1, cmask1, dmask1,cn1, cn2l, &
                               cn2s, cn3l, cn3s, cn4l, cn4s,rhs, sm, dxs, dys, &
                               dzs, dx1, dy1, dzn, z2,dt, im, jm, km)
#endif
#ifdef VERBOSE
#ifdef _OPENCL_LES_WV
    print *,'MAIN: calling OpenCL run_LES_kernel for ', nmax-n0+1, ' time steps, domain = ',im,'x',jm,'x',km
#else
    print *,'MAIN: running reference LES code for ', nmax-n0+1, ' time steps, domain = ',im,'x',jm,'x',km
#endif
#endif
! --main loop
#ifdef TIMINGS
!    nmax=201
    call system_clock(timestamp(8), clock_rate)
#endif
    do n = n0,nmax
        time = float(n-1)*dt
! -------calculate turbulent flow--------c
#ifdef _OPENCL_LES_WV
        call run_LES_kernel(n, nmax)
#else
! -------calculate turbulent flow--------c
#ifdef TIMINGS
        print *, 'run_LES_reference: time step = ',n
#endif
#ifdef TIMINGS
        call system_clock(timestamp(0), clock_rate)
#endif
        call velnw(km,jm,im,p,ro,dxs,u,dt,f,dys,v,g,dzs,w,h)
#ifdef TIMINGS
        call system_clock(timestamp(1), clock_rate)
#endif
     if(jdata24.eq.0) then
        call bondv1(jm,u,z2,dzn,v,w,km,n,im,dt,dxs)
     else
        call bondv1_data24(jm,u,z2,dzn,v,w,km,n,im,dt,dxs)
     end if
#ifdef TIMINGS
        call system_clock(timestamp(2), clock_rate)
#endif
        call velfg(km,jm,im,dx1,cov1,cov2,cov3,dfu1,diu1,diu2,dy1,diu3,dzn, &
                   vn,f,cov4,cov5,cov6,dfv1,diu4,diu5,diu6,g,cov7,cov8,cov9, &
                   dfw1,diu7,diu8,diu9,dzs,h,nou1,u,nou5,v,nou9,w,nou2,nou3, &
                   nou4,nou6,nou7,nou8,uspd,vspd)
#ifdef TIMINGS
        call system_clock(timestamp(3), clock_rate)
#endif
#if IFBF == 1
        call feedbf(km,jm,im,usum,u,bmask1,vsum,v,cmask1,wsum,w,dmask1,alpha, &
                    dt,beta,fx,fy,fz,f,g,h)
#endif
#ifdef TIMINGS
        call system_clock(timestamp(4), clock_rate)
#endif
        call les(km,delx1,dx1,dy1,dzn,jm,im,diu1,diu2,diu3,diu4,diu5,diu6, &
                 diu7,diu8,diu9,sm,f,g,h,uspd,vspd,dxs,dys)
#ifdef TIMINGS
        call system_clock(timestamp(5), clock_rate)
#endif
        call adam(n,nmax,data21,fold,im,jm,km,gold,hold,fghold,f,g,h)
#ifdef TIMINGS
        call system_clock(timestamp(6), clock_rate)
#endif
        call press(km,jm,im,rhs,u,dx1,v,dy1,w,dzn,f,g,h,dt,cn1,cn2l,p,cn2s, &
                   cn3l,cn3s,cn4l,cn4s,n, nmax,data20,usum,vsum,wsum)
#ifdef TIMINGS
        call system_clock(timestamp(7), clock_rate)
        do i=1, 7
            print '("Time for state ",i2," = ",f6.3," s")',i, &
                  (timestamp(i)-timestamp(i-1))/ real(clock_rate)
        end do
#endif
#endif
! -------data output ---------------------c
! WV: This is clearly broken, as the dimensions for u/v/w are 150x150x90
#ifdef TIMSERIS_FIXED
        call timseris(n,dt,u,v,w)
#endif
#if IANIME == 1
        call anime(n,n0,nmax,km,jm,im,dxl,dx1,dyl,dy1,z2,data22,data23,u,w,v,&
                   amask1,zbm)
        if (idata24.eq.1) then
        call anime_bond(n,n0,nmax,km,jm,im,dxl,dx1,dyl,dy1,z2,data22,data23,u,w,v,amask1,zbm)
        end if
#endif
        call ifdata_out(n,n0,n1,nmax,time,km,jm,im,u,w,v,p,usum,vsum,wsum,f,g,h,fold,gold,hold)
        call aveflow(n,n1,km,jm,im,aveu,avev,avew,avep,avel,aveuu,avevv,aveww, &
                     avesm,avesmsm,uwfx,avesu,avesv,avesw,avesuu,avesvv, &
                     avesww,u,v,w,p,sm,nmax,uwfxs,data10,time,data11,data13,data14,amask1)
        call timestep_out_all_k(n,n0,n1,nmax,km,jm,im,z2,data22,data23,u,w,v,amask1&
,ut_x1_2,vt_x1_2,wt_x1_2,ut_x2_2,vt_x2_2,wt_x2_2,nspec&
,u_spany2,v_spany2,w_spany2,u_spany3,v_spany3,w_spany3&
,u_x1_19_spany2,v_x1_19_spany2,w_x1_19_spany2,u_x1_19_spany3,v_x1_19_spany3,w_x1_19_spany3)
        end do
#ifdef USE_NETCDF_OUTPUT
    call close_netcdf_file()
#endif
#ifdef TIMINGS
    call system_clock(timestamp(9))
    print *,"Total time:" ,(timestamp(9)-timestamp(8))/real(clock_rate), &
          "s for ",nmax-n0,"iterations"
#endif
#ifdef MPI
    call finalise_mpi()
#endif
end program
