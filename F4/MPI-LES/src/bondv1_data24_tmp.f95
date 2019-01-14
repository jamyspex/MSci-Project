module module_bondv1_data24
use communication_helper_mpi
 contains 
subroutine bondv1_data24(jm,u,z2,dzn,v,w,km,n,im,dt,dxs)
    use common_sn ! create_new_include_statements() line 102
    implicit none
    real(kind=4), intent(In) :: dt
    real(kind=4), dimension(0:ip) , intent(In) :: dxs
    real(kind=4), dimension(-1:kp+2) , intent(In) :: dzn
    integer, intent(In) :: im, jm, km, n
    real(kind=4), dimension(0:ip+1,-1:jp+1,0:kp+1) , intent(InOut) :: u
    real(kind=4), dimension(0:ip+1,-1:jp+1,0:kp+1) , intent(InOut) :: v
    real(kind=4), dimension(0:ip+1,-1:jp+1,-1:kp+1) , intent(InOut) :: w
    real(kind=4), dimension(kp+2) , intent(In) :: z2
    real(kind=4) :: u_val
    integer :: i, j, k
    real(kind=4) :: aaa, bbb, uout
#ifdef MPI
    integer :: a
!mpi_out
    real(kind=4), dimension(1,-1:1201,0:kp+1)  :: ub
    real(kind=4), dimension(1,-1:1201,0:kp+1)  :: vb
    real(kind=4), dimension(1,-1:1201,-1:kp+1)  :: wb
    real(kind=4), dimension(1,-1:jpmax+1,0:kp+1)  :: ua
    real(kind=4), dimension(1,-1:jpmax+1,0:kp+1)  :: va
    real(kind=4), dimension(1,-1:jpmax+1,-1:kp+1)  :: wa
#endif
!
!
! -------------------inflow-------------------
!
!      Setup for initial wind profile
!
       if (isMaster()) then
       write(filename,'("/hdd2/yoshida/b33394/MPI-LES/driver/driver_1_MPI-LES/3_src/data24_out",i6.6, ".dat")') n+48000
      open(unit=40,file=filename,form='unformatted',status='old',access='direct',recl=4*1200)
      irec=1
        do k=1,km
        read(40,rec=irec) (ub(1,j,k),j=1,1200)
      irec=irec+1
        end do
        do k=1,km
        read(40,rec=irec) (wb(1,j,k),j=1,1200)
      irec=irec+1
        end do
        do k=1,km
        read(40,rec=irec) (vb(1,j,k),j=1,1200)
      irec=irec+1
        end do
        close(40)
      end if
      do k=1,km
      do j=1,jpmax
       ua(1,j,k)=ub(1,400+j,k)
       va(1,j,k)=vb(1,400+j,k)
       wa(1,j,k)=wb(1,400+j,k)
      end do
      end do
    if (isTopRow(procPerRow)) then
  call distributebondu(ua, ip, jp, kp, ipmax, jpmax, procPerRow)
  call distributebondv(va, ip, jp, kp, ipmax, jpmax, procPerRow)
  call distributebondw(wa, ip, jp, kp, ipmax, jpmax, procPerRow)
      do k=1,km
      do j=1,jm
      do i=0,1
       u(i,j,k)=ua(1,j,k)
       v(i,j,k)=va(1,j,k)
       w(i,j,k)=wa(1,j,k)
      end do
      end do
      end do
   end if
!#ifdef MPI
!    if (isTopRow(procPerRow)) then
!#endif
!        do i = 0,1
!            do k = 1,78 ! kp = 90 so OK
!                do j = 1,jm
!                    u_val = 5.*((z2(k)+0.5*dzn(k))/600.)**0.2
!                    !print *, u_val
!                    u(i,j,k) = u_val
!                    v(i,j,k) = 0.0
!                    w(i,j,k) = 0.0
!                end do
!            end do
!        end do
!        do i = 0,1
!            do k = 79,km
!                do j = 1,jm
!                    u(i,j,k) = u(i,j,77)
!                    v(i,j,k) = 0.0
!                    w(i,j,k) = 0.0
!                end do
!            end do
!        end do
!#ifdef MPI
!    end if
!#endif
#if ICAL == 0
    !if(ical == 0 .and. n == 1) then
    if(n == 1) then
#ifdef MPI
        ! GR: Actually make this distributed rather than this awful mess
        do a=1, procPerCol
            do k = 1,km
                do j = 1,jm
                    do i = 2, im
                        u(i,j,k) = u(1,j,k)
                        v(i,j,k) = v(1,j,k)
                        w(i,j,k) = w(1,j,k)
                    end do
                end do
            end do
            call exchangeRealHalos(u, procPerRow, neighbours, 2, 1, 3, 0)
            call exchangeRealHalos(v, procPerRow, neighbours, 2, 1, 3, 0)
            call exchangeRealHalos(w, procPerRow, neighbours, 2, 1, 3, 0)
        end do
#endif
        do k = 1,km
            do j = 1,jm
                do i = 2, im
                    u(i,j,k) = u(1,j,k)
                    v(i,j,k) = v(1,j,k)
                    w(i,j,k) = w(1,j,k)
                end do
            end do
        end do
    endif
#endif
#ifdef WV_DEBUG
    print *, 'F95: BONDV1_INIT_UVW: UVWSUM: ',n,sum(u)+sum(v)+sum(w)
#endif
! ------------- outflow condition ------------
!      advective condition
!
    aaa = 0.0
    bbb = 0.0
    do k = 1,km
        do j = 1,jm
            aaa = amax1(aaa,u(im,j,k))
            bbb = amin1(bbb,u(im,j,k))
        end do
    end do
#ifdef MPI
    call getGlobalMaxOf(aaa)
    call getGlobalMinOf(bbb)
#endif
#if GR_DEBUG
    print*, 'GR: aaa ', aaa, ' bbb ', bbb
#endif
    uout = (aaa+bbb)/2.
#ifdef WV_DEBUG
    print *, 'F95: UOUT: ',uout
#endif
    do k = 1,km
        do j = 1,jm
            u(im,j,k) = u(im,j,k)-dt*uout *(u(im,j,k)-u(im-1,j,k))/dxs(im)
        end do
    end do
    do k = 1,km
        do j = 1,jm
            v(im+1,j,k) = v(im+1,j,k)-dt*uout *(v(im+1,j,k)-v(im,j,k))/dxs(im)
        end do
    end do
    do k = 1,km
        do j = 1,jm
            w(im+1,j,k) = w(im+1,j,k)-dt*uout *(w(im+1,j,k)-w(im,j,k))/dxs(im)
        end do
    end do
#if !defined(MPI) || (PROC_PER_ROW==1)
! --side flow condition; periodic
    do k = 0,km+1
        do i = 0,im+1
            u(i,   0,k) = u(i,jm  ,k)
            u(i,jm+1,k) = u(i,   1,k)
        end do
    end do
    do k = 0,km+1
        do i = 0,im+1
            v(i,   0,k) = v(i,jm  ,k)
            v(i,jm+1,k) = v(i,   1,k)
        end do
    end do
    do k = 0,km
        do i = 0,im+1
            w(i,   0,k) = w(i,jm  ,k)
            w(i,jm+1,k) = w(i,   1,k)
        end do
    end do
#else
    ! call assumes column (jp) index from 1, not -1 hence values are +2 from original code
    call sideflowRightLeft(u, procPerRow, jp+2, 2, 0, 0, 0, 0)
    call sideflowLeftRight(u, procPerRow, 3, jp+3, 0, 0, 0, 0)
    call sideflowRightLeft(v, procPerRow, jp+2, 2, 0, 0, 0, 0)
    call sideflowLeftRight(v, procPerRow, 3, jp+3, 0, 0, 0, 0)
    call sideflowRightLeft(w, procPerRow, jp+2, 2, 0, 0, 1, 0)
    call sideflowLeftRight(w, procPerRow, 3, jp+3, 0, 0, 1, 0)
#endif
! =================================
#ifdef MPI
! --halo exchanges
    call exchangeRealHalos(u, procPerRow, neighbours, 2, 1, 1, 1)
    call exchangeRealHalos(v, procPerRow, neighbours, 2, 1, 1, 1)
    call exchangeRealHalos(w, procPerRow, neighbours, 2, 1, 1, 1)
#endif
! -------top and underground condition
    do j = 0,jm+1
        do i = 0,im+1
            u(i,j,   0) = -u(i,j, 1)
            u(i,j,km+1) = u(i,j,km)
        end do
    end do
    do j = 0,jm+1
        do i = 0,im+1
            v(i,j,   0) = -v(i,j, 1)
            v(i,j,km+1) = v(i,j,km)
        end do
    end do
    do j = -1,jm+1 ! 2 !WV: I think this is wrong: j = jm+2 is not allocated!
        do i = 0,im+1
            w(i,j, 0) = 0.0
            w(i,j,km) = 0.0
        end do
    end do
#ifdef WV_DEBUG
    print *,'F95 UVWSUM after bondv1:',sum(u)+sum(v)+sum(w)
#endif
end subroutine bondv1_data24
end module module_bondv1_data24
