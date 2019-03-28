module module_press
#ifdef USE_NETCDF_OUTPUT
    use module_LES_write_netcdf
#endif
    use module_bondFG ! add_module_decls() line 156
    use module_boundp ! add_module_decls() line 156
    implicit none
contains

#ifdef WV_NEW
subroutine press(u,v,w,p,rhs,f,g,h,dx1,dy1,dzn,dxs,dys,dzs,dt,n,nmax &
#if !defined( NO_IO)  && !defined( MPI )
    ,usum,vsum,wsum,data20 &
#endif
)
#else
subroutine press(rhs,u,dx1,v,dy1,w,dzn,f,g,h,dt,cn1,cn2l,p,cn2s,cn3l,cn3s,cn4l,cn4s, &
                 n,nmax,data20,usum,vsum,wsum)
#endif
#ifdef WV_NEW
    use params_common_sn
    implicit none
#else
    use common_sn ! create_new_include_statements() line 102
#endif
#ifndef WV_NEW
    real(kind=4), dimension(ip,jp,kp) , intent(In) :: cn1
    real(kind=4), dimension(ip) , intent(In) :: cn2l
    real(kind=4), dimension(ip) , intent(In) :: cn2s
    real(kind=4), dimension(jp) , intent(In) :: cn3l
    real(kind=4), dimension(jp) , intent(In) :: cn3s
    real(kind=4), dimension(kp) , intent(In) :: cn4l
    real(kind=4), dimension(kp) , intent(In) :: cn4s
#else
    real(kind=4), dimension(0:ip) , intent(In) :: dxs
    real(kind=4), dimension(0:jp) , intent(In) :: dys
    real(kind=4), dimension(-1:kp+2) , intent(In) :: dzs
    real(kind=4) :: cn1,cn2l,cn2s,cn3l,cn3s,cn4l,cn4s,dz1,dz2
#endif
#if (!defined( NO_IO)  && !defined( MPI )) || !defined( WV_NEW )
    character(len=70), intent(In) :: data20
    real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(In) :: usum
    real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(In) :: vsum
    real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(In) :: wsum
#endif
    real(kind=4), intent(In) :: dt
    real(kind=4), dimension(-1:ip+1) , intent(In) :: dx1
    real(kind=4), dimension(0:jp+1) , intent(In) :: dy1
    real(kind=4), dimension(-1:kp+2) , intent(In) :: dzn
    real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(InOut) :: f
    real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(InOut) :: g
    real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(InOut) :: h
    integer, intent(In) :: n
    integer, intent(In) :: nmax

#ifndef TWINNED_BUFFER
    real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+1) , intent(InOut) :: p
#else
    real(kind=4), dimension(0:1,0:ip+2,0:jp+2,0:kp+1) :: p
#endif
    real(kind=4), dimension(0:ip+1,0:jp+1,0:kp+1) , intent(Out) :: rhs
    real(kind=4), dimension(0:ip+1,-1:jp+1,0:kp+1) , intent(In) :: u
    real(kind=4), dimension(0:ip+1,-1:jp+1,0:kp+1) , intent(In) :: v
    real(kind=4), dimension(0:ip+1,-1:jp+1,-1:kp+1) , intent(In) :: w

    integer :: nn
    integer :: i,j,k,l,nrd
    real(kind=4) :: rhsav, pav, area, pco, sor, reltmp
    !
    ! --only used mac method
    real, parameter  :: pjuge = 0.0001
    integer, parameter  :: nmaxp = 50 ! WV was 50
    real, parameter  :: omega = 1.

#ifdef CALC_BOUNDS
#if !defined( INLINE_BOUND_CALCS ) || defined( MPI )
    call bondfg(f,g,h)
#else
! --inflow condition
      do k=1,kp
      do j=1,jp
        f( 0,j,k)=f(1  ,j,k)
   end do
      end do
! --sideflow condition
      do k=1,kp
      do i=1,ip
        g(i, 0,k)=g(i,jp  ,k)
   end do
      end do
! --ground and top condition
      do j=1,jp
      do i=1,ip
        h(i,j, 0)=0.0
        h(i,j,kp)=0.0
   end do
      end do
#endif
#endif
    do k = 1,kp
        do j = 1,jp
            do i = 1,ip
                rhs(i,j,k) = (-u(i-1,j,k)+u(i,j,k))/dx1(i) +(-v(i,j-1,k)+ &
                             v(i,j,k))/dy1(j) +(-w(i,j,k-1)+w(i,j,k))/dzn(k)
! --stretch
                rhs(i,j,k) = (f(i,j,k)-f(i-1,j,k))/dx1(i) +(g(i,j,k)- &
                              g(i,j-1,k))/dy1(j) +(h(i,j,k)-h(i,j,k-1))/dzn(k) &
                              +rhs(i,j,k)/dt
            end do
        end do
    end do
!

!    rhs=0

    rhsav = 0.0
    area = 0.0
    do k = 1,kp
        do j = 1,jp
            do i = 1,ip
                rhsav = rhsav+dx1(i)*dy1(j)*dzn(k)*rhs(i,j,k)
                area = area +dx1(i)*dy1(j)*dzn(k)
            end do
        end do
    end do

#ifdef MPI
    call getGlobalSumOf(rhsav)
    call getGlobalSumOf(area)
#endif

#if GR_DEBUG
    print*, 'GR: rhsav ', rhsav, ' area ', area
#endif
    rhsav = rhsav/area
    do k = 1,kp
        do j = 1,jp
            do i = 1,ip
                rhs(i,j,k) = rhs(i,j,k)-rhsav
            end do
        end do
    end do

! --SOR
    do l = 1,nmaxp
        sor = 0.0
        do nrd = 0,1
            do k = 1,kp
#ifndef WV_OPENCL
#ifdef WV_NEW
                dz1 = dzs(k-1)
                dz2 = dzs(k)
                cn4s = 2./(dz1*(dz1+dz2))
                cn4l = 2./(dz2*(dz1+dz2))
#endif
#endif
                do j = 1,jp
#ifndef WV_OPENCL
#ifdef WV_NEW
                    cn3s = 2./(dys(j-1)*(dys(j-1)+dys(j)))
                    cn3l = 2./(dys(j)*(dys(j-1)+dys(j)))
#endif
#endif
#ifndef TWINNED_BUFFER
                    do i = 1+mod(k+j+nrd,2),ip,2
#else
                    do i=1,ip
#endif
#ifdef WV_NEW
#ifdef WV_OPENCL
                dz1 = dzs(k-1)
                dz2 = dzs(k)
                cn4s = 2./(dz1*(dz1+dz2))
                cn4l = 2./(dz2*(dz1+dz2))
                    cn3s = 2./(dys(j-1)*(dys(j-1)+dys(j)))
                    cn3l = 2./(dys(j)*(dys(j-1)+dys(j)))
#endif
                        cn2s = 2./(dxs(i-1)*(dxs(i-1)+dxs(i)))
                        cn2l = 2./(dxs(i)*(dxs(i-1)+dxs(i)))

                        cn1 = 1./ (2./(dxs(i-1)*dxs(i))  + 2./(dys(j-1)*dys(j)) + 2./(dz1*dz2))
!                        cn1 = 1./cn1
#ifndef TWINNED_BUFFER
                        reltmp = omega*(cn1 *(cn2l*p(i+1,j,k) + &
                                 cn2s*p(i-1,j,k) +cn3l*p(i,j+1,k) + &
                                 cn3s*p(i,j-1,k) +cn4l*p(i,j,k+1) + &
                                 cn4s*p(i,j,k-1) -rhs(i,j,k))-p(i,j,k))

#else
                      if (nrd==0) then
                        ! buffer 0
                        reltmp = omega*(cn1 *(cn2l*p(0,i+1,j,k) + &
                                 cn2s*p(0,i-1,j,k) +cn3l*p(0,i,j+1,k) + &
                                 cn3s*p(0,i,j-1,k) +cn4l*p(0,i,j,k+1) + &
                                 cn4s*p(0,i,j,k-1) -rhs(i,j,k))-p(0,i,j,k))
                        p(1,i,j,k) = p(0,i,j,k) +reltmp
                      else
                          ! buffer 1
                        reltmp = omega*(cn1 *(cn2l*p(1,i+1,j,k) + &
                                 cn2s*p(1,i-1,j,k) +cn3l*p(1,i,j+1,k) + &
                                 cn3s*p(1,i,j-1,k) +cn4l*p(1,i,j,k+1) + &
                                 cn4s*p(1,i,j,k-1) -rhs(i,j,k))-p(1,i,j,k))
                        p(0,i,j,k) = p(1,i,j,k) +reltmp
                      end if
#endif

#else
                        reltmp = omega*(cn1(i,j,k)      &
                                *(cn2l(i)*p(i+1,j,k)    &
                                 +cn2s(i)*p(i-1,j,k)    &
                                 +cn3l(j)*p(i,j+1,k)    &
                                 +cn3s(j)*p(i,j-1,k)    &
                                 +cn4l(k)*p(i,j,k+1)    &
                                 +cn4s(k)*p(i,j,k-1)    &
                                 -rhs(i,j,k))-p(i,j,k))
#endif
#ifndef TWINNED_BUFFER
!
                        p(i,j,k) = p(i,j,k) +reltmp
#endif
#ifndef NO_GLOBAL_SOR
                        sor = sor+reltmp*reltmp
#endif
                    end do
                end do
            end do
#if defined( TWINNED_BUFFER ) && defined(INLINE_BOUND_CALCS) && !defined(MPI)
      ! --computational boundary(neumann condition)
          do k=0,kp+1
          do j=0,jp+1
            p(0,   0,j,k) = p(0,1 ,j,k)
            p(0,ip+1,j,k) = p(0,ip,j,k)
          end do
          end do
          do k=0,kp+1
          do i=0,ip+1
            p(0,i,   0,k) = p(0,i,jp,k)
            p(0,i,jp+1,k) = p(0,i, 1,k)
          end do
          end do
#else
            call boundp1(p)
#endif
        end do  ! nrd
#if defined( TWINNED_BUFFER ) && defined(INLINE_BOUND_CALCS) && !defined(MPI)
! --computational boundary(neumann condition)
      do j=0,jp+1
      do i=0,ip+1
        p(0,i,j,   0) = p(0,i,j,1)
        p(0,i,j,kp+1) = p(0,i,j,kp)
      end do
      end do
#else
      call boundp2(p)
#endif
#ifndef NO_IO
#ifdef VERBOSE
! --check
        if ((mod(n-1,10) == 0).and.(mod(l,20) == 0)) then
!        if ((mod(n-1,10) == 0)) then
            print *, 'time step, iteration step, conv =',n,l,sor
        end if
#endif
#endif

#ifndef NO_GLOBAL_SOR

#ifdef MPI
        call getGlobalSumOf(sor)
#endif
        if (sor < pjuge) goto 510
! NO_GLOBAL_SOR
#endif
    end do ! l
#ifndef NO_GLOBAL_SOR
    510 continue
#endif

!print *,rank,'SOR iterations:',l
    pav = 0.0
    pco = 0.0
    do k = 1,kp
        do j = 1,jp
            do i = 1,ip
#ifdef TWINNED_BUFFER
                pav = pav+p(0,i,j,k)*dx1(i)*dy1(j)*dzn(k)
#else
                pav = pav+p(i,j,k)*dx1(i)*dy1(j)*dzn(k)
#endif
                pco = pco+dx1(i)*dy1(j)*dzn(k)
            end do
        end do
    end do

#ifdef MPI
    call getGlobalSumOf(pav)
    call getGlobalSumOf(pco)
#endif
#if GR_DEBUG
    print*, 'GR: pav ', pav, ' pco ', pco
#endif

    pav = pav/pco
    do k = 1,kp
        do j = 1,jp
            do i = 1,ip
#ifdef TWINNED_BUFFER
                p(0,i,j,k) = p(0,i,j,k)-pav
#else
                p(i,j,k) = p(i,j,k)-pav
#endif
            end do
        end do
    end do

#ifdef WV_DEBUG
    print *, "F95: P_SUM_ADJ=",sum(p)
#endif
#if defined( TWINNED_BUFFER ) && defined(INLINE_BOUND_CALCS) && !defined(MPI)
      ! --computational boundary(neumann condition)
          do k=0,kp+1
          do j=0,jp+1
            p(0,   0,j,k) = p(0,1 ,j,k)
            p(0,ip+1,j,k) = p(0,ip,j,k)
          end do
          end do
          do k=0,kp+1
          do i=0,ip+1
            p(0,i,   0,k) = p(0,i,jp,k)
            p(0,i,jp+1,k) = p(0,i, 1,k)
          end do
          end do
#else
            call boundp1(p)
#endif
#ifdef WV_DEBUG
    print *, "F95: P_SUM_1=",sum(p)
#endif

#if defined( TWINNED_BUFFER ) && defined(INLINE_BOUND_CALCS) && !defined(MPI)
! --computational boundary(neumann condition)
      do j=0,jp+1
      do i=0,ip+1
        p(0,i,j,   0) = p(0,i,j,1)
        p(0,i,j,kp+1) = p(0,i,j,kp)
      end do
      end do
#else
    call boundp2(p)
#endif
#ifdef WV_DEBUG
    print *, "F95: P_SUM_BOUND=",sum(p)
#endif

#if !defined( NO_IO ) && defined( VERBOSE )

#ifdef MPI
    if (rank == mpi_size / 2 + procPerRow / 2 - 1 ) then
#endif
    if (mod(n-1,20) == 0) then
        write(6,*) '=mac= time step, iteration step, conv =',n,l,sor
    end if
! --check
    if (mod(n-1,20) == 0) then
        do k = 1,kp,10
            write(6,*) 'Inflow=',k,'u=',u(0,jp/2,k),v(0,jp/2,k) ,w(0,jp/2,k)
        end do
        do k = 1,kp,10
            write(6,*) 'Urbanflow=',k,'u=',u(ip/2,jp/2,k),v(ip/2,jp/2,k) ,w(ip/2,jp/2,k)
        end do
!
        cflu = 0.
        cflv = 0.
        cflw = 0.
        do k = 1,kp
            do j = 1,jp
                do i = 1,ip
                    cflu = amax1(cflu,abs(u(i,j,k)*dt/dx1(i)))
                    cflv = amax1(cflv,abs(v(i,j,k)*dt/dy1(j)))
                    cflw = amax1(cflw,abs(w(i,j,k)*dt/dzn(k)))
                end do
            end do
        end do
    end if

    if (mod(n-1,20) == 0) then
        write(6,*) 'Check_CFL,u*dt/dx,v*dt/dy,w*dt/dz=',cflu,cflv,cflw
    end if
#ifdef MPI
    end if
#endif
#endif


#ifndef NO_IO
    if(mod(n,1000) == 0 .or. n == nmax) then
        nn = n/1000
        print *, 'timestep: ',nn,' pressure at centre: ',&
#ifdef TWINNED_BUFFER
        p(0,ip/2,jp/2,kp/2), &
#else
        p(ip/2,jp/2,kp/2), &
#endif
                'vel at centre: ', &
                u(ip/2,jp/2,kp/2),v(ip/2,jp/2,kp/2),w(ip/2,jp/2,kp/2)
#ifdef USE_NETCDF_OUTPUT
        call write_to_netcdf_file(p,u,v,w,usum,vsum,wsum,nn)
#endif

#ifndef MPI
        open(unit=20,file=data20,form='unformatted',status='unknown')
        write(20) (((u(i,j,k),i=1,ip),j=1,jp),k=1,kp)
        write(20) (((v(i,j,k),i=1,ip),j=1,jp),k=1,kp)
        write(20) (((w(i,j,k),i=1,ip),j=1,jp),k=1,kp)
#ifdef TWINNED_BUFFER
        write(20) (((p(0,i,j,k),i=1,ip),j=1,jp),k=1,kp)
#else
        write(20) (((p(i,j,k),i=1,ip),j=1,jp),k=1,kp)
#endif
        write(20) (((usum(i,j,k),i=1,ip),j=1,jp),k=1,kp)
        write(20) (((vsum(i,j,k),i=1,ip),j=1,jp),k=1,kp)
        write(20) (((wsum(i,j,k),i=1,ip),j=1,jp),k=1,kp)
        close(unit=20)
#endif
    end if
#endif

end subroutine press

end module module_press
