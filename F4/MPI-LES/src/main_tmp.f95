program main
#ifdef MPI
    use communication_helper_mpi
#ifdef NESTED_LES
    use nesting_support
#endif
#endif
    use module_init
    use module_grid
    use module_set
#ifdef TIMDATA
    use module_timdata
#endif
#ifdef TIMSERIS_FIXED
    use module_timseris
#endif
    use module_aveflow
    use module_ifdata
#ifdef I_ANIME
    use module_anime
#endif
!#ifdef _OPENCL_LES_WV
!    use module_LES_combined_ocl
!#else
    use module_velnw
    use module_bondv1
    use module_velFG
#if IFBF == 1
    use module_feedbf
#endif
    use module_les
    use module_press
    use module_adam
!#endif
#ifdef WV_NEW
    use params_common_sn
    implicit none
#else
    use common_sn ! create_new_include_statements() line 102
#endif
    real(kind=4) :: alpha
!    integer :: ianime
    integer :: ical
!    integer :: ifbf
    integer :: im
    integer :: jm
    integer :: km
    integer :: n
    integer :: n0
    integer :: n1
    integer :: nif
    integer :: nmax
    real(kind=4) :: beta
    character(len=70) :: data10
    character(len=70) :: data11
    character(len=70) :: data12
    character(len=70) :: data13
    character(len=70) :: data14
    character(len=70) :: data15
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
#ifdef I_AVEFLOW
    real(kind=4), dimension(ip,jp,kp)  :: avel
    real(kind=4), dimension(ip,jp,kp)  :: avep
    real(kind=4), dimension(ip,jp,kp)  :: avesm
    real(kind=4), dimension(ip,jp,kp)  :: avesmsm
#ifndef WV_NEW
    real(kind=4), dimension(ip,kp)  :: avesu
    real(kind=4), dimension(ip,kp)  :: avesuu
    real(kind=4), dimension(ip,kp)  :: avesv
    real(kind=4), dimension(ip,kp)  :: avesvv
    real(kind=4), dimension(ip,kp)  :: avesw
    real(kind=4), dimension(ip,kp)  :: avesww
#endif
    real(kind=4), dimension(ip,jp,0:kp)  :: aveu
    real(kind=4), dimension(ip,jp,kp)  :: aveuu
    real(kind=4), dimension(ip,jp,0:kp)  :: avev
    real(kind=4), dimension(ip,jp,kp)  :: avevv
    real(kind=4), dimension(ip+1,jp,0:kp+2)  :: avew
    real(kind=4), dimension(ip,jp,kp)  :: aveww
#endif
#ifndef WV_NEW_FEEEDBF
    real(kind=4), dimension(0:ip+1,0:jp+1,0:kp+1)  :: amask1
    real(kind=4), dimension(-1:ip+1,0:jp+1,0:kp+1)  :: bmask1
    real(kind=4), dimension(0:ip+1,-1:jp+1,0:kp+1)  :: cmask1
    real(kind=4), dimension(0:ip+1,0:jp+1,0:kp+1)  :: dmask1
#endif
#if !defined( WV_NEW ) || (defined( WV_NEW ) && !defined( WV_NEW_FEEDBF ))
    real(kind=4), dimension(ip,jp,kp)  :: cn1
    real(kind=4), dimension(ip)  :: cn2l
    real(kind=4), dimension(ip)  :: cn2s
    real(kind=4), dimension(jp)  :: cn3l
    real(kind=4), dimension(jp)  :: cn3s
    real(kind=4), dimension(kp)  :: cn4l
    real(kind=4), dimension(kp)  :: cn4s
#endif
#ifndef WV_NEW_VELFG
    real(kind=4), dimension(-1:ip+2,0:jp+2,0:kp+2)  :: cov1
    real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2)  :: cov2
    real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2)  :: cov3
    real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2)  :: cov4
    real(kind=4), dimension(-1:ip+2,0:jp+2,0:kp+2)  :: cov5
    real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2)  :: cov6
    real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2)  :: cov7
    real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2)  :: cov8
    real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2)  :: cov9
#endif
    real(kind=4), dimension(kp)  :: delx1
#ifndef WV_NEW_VELFG
    real(kind=4), dimension(0:ip,jp,kp)  :: dfu1
    real(kind=4), dimension(ip,0:jp,kp)  :: dfv1
    real(kind=4), dimension(ip,jp,kp)  :: dfw1
#endif
#ifndef WV_NEW_LES
    real(kind=4), dimension(-1:ip+2,0:jp+2,0:kp+2)  :: diu1
    real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2)  :: diu2
    real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2)  :: diu3
    real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2)  :: diu4
    real(kind=4), dimension(-1:ip+2,0:jp+2,0:kp+2)  :: diu5
    real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2)  :: diu6
    real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2)  :: diu7
    real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2)  :: diu8
    real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2)  :: diu9
#endif
    real(kind=4), dimension(-1:ip+1)  :: dx1
    real(kind=4), dimension(0:ip)  :: dxl
    real(kind=4), dimension(0:ip)  :: dxs
    real(kind=4), dimension(0:jp+1)  :: dy1
    real(kind=4), dimension(0:jp)  :: dyl
    real(kind=4), dimension(0:jp)  :: dys
    real(kind=4), dimension(-1:kp+2)  :: dzn
    real(kind=4), dimension(-1:kp+2)  :: dzs
    real(kind=4), dimension(0:ip,0:jp,0:kp)  :: f
!#if ICAL == 1
!    real(kind=4), dimension(ip,jp,kp)  :: fghold
!#endif
    real(kind=4), dimension(ip,jp,kp)  :: fold
#ifndef WV_NEW_FEEDBF
    real(kind=4), dimension(0:ip,0:jp,0:kp)  :: fx
    real(kind=4), dimension(0:ip,0:jp,0:kp)  :: fy
    real(kind=4), dimension(0:ip,0:jp,0:kp)  :: fz
#endif
    real(kind=4), dimension(0:ip,0:jp,0:kp)  :: g
    real(kind=4), dimension(ip,jp,kp)  :: gold
    real(kind=4), dimension(0:ip,0:jp,0:kp)  :: h
    real(kind=4), dimension(ip,jp,kp)  :: hold
!#ifndef _OPENCL_LES_WV
!    real(kind=4), dimension(ip,jp,kp)  :: fghold
!#endif
#ifndef WV_NEW_LES2
    real(kind=4), dimension(-1:ip+2,0:jp+2,0:kp+2)  :: nou1
    real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2)  :: nou2
    real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2)  :: nou3
    real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2)  :: nou4
    real(kind=4), dimension(-1:ip+2,0:jp+2,0:kp+2)  :: nou5
    real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2)  :: nou6
    real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2)  :: nou7
    real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2)  :: nou8
    real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2)  :: nou9
#endif
#ifndef TWINNED_BUFFER
    real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+1)  :: p
#else
    real(kind=4), dimension(0:1,0:ip+2,0:jp+2,0:kp+1)  :: p
#endif
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
#ifndef WV_NEW_VELFG
    real(kind=4), dimension(0:ip+1,0:jp+1)  :: uspd
    real(kind=4), dimension(0:ip+1,0:jp+1)  :: vspd
#endif
!#ifdef TIMINGS
    integer :: clock_rate
    integer (kind=4), dimension(0:9) :: timestamp
    integer (kind=4) :: i
!#endif
#ifdef NESTED_LES
!    integer :: syncTicksLocal
    logical :: inNest
#endif
!      u=0.0
!      v=0.0
!      w=0.0
!      f=0.0
!      g=0.0
!      h=0.0
!      rhs=0.0
!      p=0.0
#ifdef MPI
    call initialise_mpi()
    if (mpi_size .ne. procPerRow * procPerCol) then
        print*, 'Needed ', (procPerRow * procPerCol), ' processes, got ', mpi_size
        call MPI_Abort(communicator, 1, ierror)
    end if
    call setupCartesianVirtualTopology(dimensions, dimensionSizes, &
                                       periodicDimensions, coordinates, &
                                       neighbours, reorder)
#ifdef MPI_NEW_WV
    call createBottomRowCommunicator(procPerRow)
#endif
#ifdef NESTED_LES
!    syncTicksLocal = 0
    syncTicks = 0
#endif
#endif
#ifdef USE_NETCDF_OUTPUT
    call init_netcdf_file()
#endif
    call set(data10,data11,data20,data21,data22,data23,data24,data25,data26,&
             data27,data30,data31,ical,nif,n0,n1,nmax,dt,ro,&
             vn,alpha,beta,data12,data13,data14,data15)
    call grid(dx1,dxl,dy1,dyl,z2,dzn,dzs,dxs,dys)
#ifdef TIMDATA
    call timdata()
#endif
#if defined( WV_NEW ) && defined( WV_NEW_FEEDBF )
    call init(u,v,w,p,zbm)
#else
    call init(u,v,w,p,cn2s,dxs,cn2l,cn3s,dys,cn3l,dzs,cn4s,cn4l,cn1,&
#ifndef WV_NEW_FEEDBF
              amask1,bmask1,cmask1,dmask1, &
#endif
              zbm,z2,dzn)
#endif
!    n0=200
#if defined( WV_NEW ) && defined( WV_NEW_LES ) && defined( WV_NEW_LES2 ) && defined( WV_NEW_FEEDBF ) && defined( WV_NEW_VELFG )
!#ifndef WV_DEBUG_MPI
    call ifdata( &
!#if ICAL == 1
                fold,gold,hold, time, &
!#endif
                n,u,v,w,p,usum,vsum,wsum,&
                f,g,h, &
                ical,nif)
!#endif
#else
!#ifndef WV_DEBUG_MPI
    call ifdata( &
!#if ICAL == 1
                fold,gold,hold,&
                !fghold,&
                 time, &
!#endif
                n,u,v,w,p,usum,vsum,wsum,delx1,dx1,dy1,dzn,&
#ifndef WV_NEW_LES
                diu1,diu2,diu3,diu4,diu5,diu6,diu7,diu8,diu9,&
#endif
                sm,f,g,h,z2,dt,dxs,vn, &
#ifndef WV_NEW_VELFG
                dfu1,dfv1,dfw1,&
                cov1,cov2,cov3,cov4,cov5,cov6,cov7,cov8,cov9,&
#endif
                dzs,&
#ifndef WV_NEW_LES2
                nou1,nou2,nou3,nou4,nou5,nou6,nou7,nou8,nou9,&
#endif
#ifndef WV_NEW_FEEDBF
                amask1,bmask1,cmask1,dmask1,&
#endif
                alpha,beta,fx,fy,fz,zbm,ical,nif)
!#endif
#endif
!     n=n0
#ifdef VERBOSE
!#ifdef _OPENCL_LES_WV
!    print *,'MAIN: calling OpenCL run_LES_kernel for ', nmax-n0+1, ' time steps, domain = ',ip,'x',jp,'x',kp
!#else
#ifndef MPI
    print *,'MAIN: running reference LES code for ', nmax-n0+1, ' time steps, domain = ',ip,'x',jp,'x',kp
#else
    if (rank==0) print *,'MAIN: running reference LES code for ', nmax-n0+1, ' time steps, subdomain = ',ip,'x',jp,'x',kp, 'total domain = ',ipmax,'x',jpmax,'x',kp
#endif
!#endif
#endif
! --main loop
#ifdef TIMINGS
!    nmax=201
    call system_clock(timestamp(8), clock_rate)
#endif
#ifdef MPI
#ifdef NESTED_LES
inNest = inNestedGrid()
#endif
#endif
    do n = n0,nmax
        time = float(n-n0)*dt
#if defined( MPI ) && defined( NESTED_LES)
        if (inNest) then
            if (mod(n,int(dt_orig/dt_nest))==1) then ! so n % 2 == 1, and n0=1, so 1,3,5, ... 2,4,6,...
                syncTicks = 0
            else
                syncTicks = 1
            end if
        else
            syncTicks = 0
        end if
#endif
! -------calculate turbulent flow--------c
#ifdef TIMINGS
        print *, 'run_LES_reference: time step = ',n
        call system_clock(timestamp(0), clock_rate)
#endif
        call velnw(p,ro,dxs,u,dt,f,dys,v,g,dzs,w,h) !WV: no MPI
#ifdef TIMINGS
        call system_clock(timestamp(1), clock_rate)
#endif
        call bondv1(u,z2,dzn,v,w,n,n0,dt,dxs) !WV: via halos + gatheraaa/bbb. Sideflow etc should be OK as in outer domain ???
#if defined( MPI ) && defined( NESTED_LES)
if (n>n_nest0) then
#endif
#ifdef TIMINGS
        call system_clock(timestamp(2), clock_rate)
#endif
#ifdef WV_NEW_VELFG
      call velfg(dx1,dy1,dzn,f, &
      g,dzs,h,u,v,w)
#else
        call velfg(dx1,dy1,dzn,f,g,h,u,v,w, &
        dfu1,dfv1,dfw1,vn,dzs, &
#ifndef WV_NEW_LES
        diu1,diu2,diu3,diu4,diu5,diu6,diu7,diu8,diu9, &
#endif
        cov1,cov2,cov3,cov4,cov5,cov6,cov7,cov8,cov9, &
#ifndef WV_NEW_LES2
        nou1,nou2,nou3,nou4,nou5,nou6,nou7,nou8,nou9, &
#endif
        uspd,vspd) !WV: calls vel2 which uses halos
#endif
#ifdef TIMINGS
        call system_clock(timestamp(3), clock_rate)
#endif
#if IFBF == 1
#ifdef WV_NEW_FEEDBF
        call feedbf(u,v,w,f,g,h,usum,vsum,wsum,dzn,z2,zbm,alpha,beta,dt) ! WV: no MPI
#else
        call feedbf(usum,u,bmask1,vsum,v,cmask1,wsum,w,dmask1,alpha, &
                    dt,beta,fx,fy,fz,f,g,h,n) ! WV: no MPI
#endif
#endif
#ifdef TIMINGS
        call system_clock(timestamp(4), clock_rate)
#endif
#ifdef WV_NEW_LES
        call les(u,v,w,f,g,h,sm,dx1,dy1,dzn,dzs,dxs,dys)
#else
        call les(delx1,dx1,dy1,dzn,diu1,diu2,diu3,diu4,diu5,diu6, &
                 diu7,diu8,diu9,sm,f,g,h,u,v,uspd,vspd,dxs,dys,n) ! WV: calls boundsm which uses halos
#endif
#ifdef TIMINGS
        call system_clock(timestamp(5), clock_rate)
#endif
        call adam(n,nmax,data21,fold,gold,hold,&
        !fghold,&
        f,g,h) ! WV: no MPI
#ifdef TIMINGS
        call system_clock(timestamp(6), clock_rate)
#endif
#ifdef WV_NEW
        call press(u,v,w,p,rhs,f,g,h,dx1,dy1,dzn,dxs,dys,dzs,dt,n,nmax&
#if !defined( NO_IO)  && !defined( MPI )
        ,usum,vsum,wsum,data20 &
#endif
        )
#else
        call press(rhs,u,dx1,v,dy1,w,dzn,f,g,h,dt,cn1,cn2l,p,cn2s, &
                   cn3l,cn3s,cn4l,cn4s,n, nmax,data20,usum,vsum,wsum) !WV getGlobalSumOf and exchangeRealHalos (in boundp)
#endif
#ifdef TIMINGS
        call system_clock(timestamp(7), clock_rate)
        do i=1, 7
            print '("Time for state ",i2," = ",f6.3," s")',i, &
                  (timestamp(i)-timestamp(i-1))/ real(clock_rate)
        end do
#endif
!#endif
! -------data output ---------------------c
! WV: This is clearly broken, as the dimensions for u/v/w are 150x150x90
#ifdef TIMSERIS_FIXED
        call timseris(n,dt,u,v,w)
#endif
#ifdef I_ANIME
    !print *, n,rank, 'NO ANIME!'
!      if (i_anime .eq. 1) then
        call anime(n,n0,n1,&
#ifdef OLD_CODE
                    nmax,dxl,dx1,dyl,dy1,z2,amask1,zbm,&
#endif
                    u,w,v,&
#ifndef TWINNED_BUFFER
                    p &
#else
                    p(0,:,:,:) &
#endif
                    ) !WV: I put the sync condition in this code
!      end if
#endif
#ifdef I_IFDATA_OUT
      ! WV: sorry, not supported at the moment
!      if (i_ifdata_out .eq. 1) then
        call ifdata_out(n,n0,n1,nmax,time,u,w,v, &
#ifndef TWINNED_BUFFER
                     p, &
#else
                     p(0,:,:,:), &
#endif
        usum,vsum,wsum,f,g,h,fold,gold,hold) !WV: TODO: put the sync condition in this code
!      end if
#endif
#ifdef I_AVEFLOW
!      if (i_aveflow .eq. 1) then
#ifdef WV_NEW
        call aveflow(n,n1,aveu,avev,avew,avep,avel,aveuu,avevv,aveww, &
                     avesm,avesmsm,uwfx, &
                     u,v,w, &
#ifndef TWINNED_BUFFER
                     p &
#else
                     p(0,:,:,:) &
#endif
                     ,sm,nmax)
#else
        call aveflow(n,n1,aveu,avev,avew,avep,avel,aveuu,avevv,aveww, &
                     avesm,avesmsm,uwfx,avesu,avesv,avesw,avesuu,avesvv, &
                     avesww,u,v,w,p,sm,nmax,uwfxs,data10,time,data11,data13,data14)! ,amask1)  !WV: TODO: put the sync condition in this code
#endif
!      end if
#endif
#ifdef MPI
#ifdef NESTED_LES
    end if ! n>n_nest0
#endif
#endif
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
