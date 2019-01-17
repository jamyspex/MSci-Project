module module_bondv1
#ifdef MPI
    use communication_helper_real
#ifdef NESTED_LES
    use nesting_support
#endif
#endif

contains

subroutine bondv1(u,z2,dzn,v,w,n,n0,dt,dxs)
#ifdef WV_NEW
    use params_common_sn
#else
    use common_sn ! create_new_include_statements() line 102
#endif
    implicit none
    real(kind=4), intent(In) :: dt
    real(kind=4), dimension(0:ip) , intent(In) :: dxs
    real(kind=4), dimension(-1:kp+2) , intent(In) :: dzn
    integer, intent(In) :: n, n0
    real(kind=4), dimension(0:ip+1,-1:jp+1,0:kp+1) , intent(InOut) :: u
    real(kind=4), dimension(0:ip+1,-1:jp+1,0:kp+1) , intent(InOut) :: v
    real(kind=4), dimension(0:ip+1,-1:jp+1,-1:kp+1) , intent(InOut) :: w
    real(kind=4), dimension(0:kp+2) , intent(In) :: z2
    real(kind=4) :: u_val
    integer :: i, j, k
    real(kind=4) :: aaa, bbb, uout, gaaa, gbbb
!    integer, intent(In) :: ical

#ifdef MPI
    integer :: a
#endif
!
!
! -------------------inflow-------------------
!
!      Setup for initial wind profile
!
#ifdef MPI
    if (isTopRow(procPerRow)) then
#endif
        do i = 0,1
            do k = 1,78 ! kp = 90 so OK
                do j = 1,jp
                    u_val = 5.*((z2(k)+0.5*dzn(k))/600.)**0.2
                    u(i,j,k) = u_val
                    !print *, u_val
!                    u(i,j,k) = 5.0
                    v(i,j,k) = 0.0
                    w(i,j,k) = 0.0
                end do
            end do
        end do

        do i = 0,1
            do k = 79,kp
                do j = 1,jp
                    u(i,j,k) = u(i,j,77)
                    v(i,j,k) = 0.0
                    w(i,j,k) = 0.0
                end do
            end do
        end do
#ifdef MPI
    end if
#endif

!#if ICAL == 0
!    if(ical == 0.and.n == 1) then

#ifdef NESTED_LES
#ifdef MPI
    if(syncTicks == 0 .and. n == n_nest0) then
#else
    if(n == n0) then
#endif
#else
    if(n == n0) then
#endif

#ifdef MPI
        ! GR: Actually make this distributed rather than this awful mess
        do a=1, procPerCol
            do k = 1,kp
                do j = 1,jp
                    do i = 2, ip
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
        do k = 1,kp
            do j = 1,jp
                do i = 2, ip
                    u(i,j,k) = u(1,j,k)
                    v(i,j,k) = v(1,j,k)
                    w(i,j,k) = w(1,j,k)
                end do
            end do
        end do

    endif

!#endif

#ifdef WV_DEBUG
    print *, 'F95: BONDV1_INIT_UVW: UVWSUM: ',n,sum(u)+sum(v)+sum(w)
#endif

! ------------- outflow condition ------------
!      advective condition
!

    aaa = 0.0
    do k = 1,kp
        do j = 1,jp
            aaa = amax1(aaa,u(ip,j,k))
        end do
    end do
#ifdef MPI
!WV what this call does: if the process is in the bottom row then gaaa is the max of all processes in that row. Otherwise it is 0
!WV so all processes compute aaa, but only the bottom row does the gather. Very strange.
! print *, rank,'sync:',syncTicks,'bondv1 BGA'
    gaaa = 0.0
    call gatheraaa(gaaa, aaa, procPerRow)
! print *, rank,'sync:',syncTicks,'bondv1 AGA'
#else
    gaaa = aaa
#endif
    bbb = 1e38 ! aaa FIXME
    do k = 1,kp
        do j = 1,jp
            bbb = amin1(bbb,u(ip,j,k))
        end do
    end do
#ifdef MPI
! print *, rank,'sync:',syncTicks,'bondv1 BGB'
    gbbb = gaaa
    call gatherbbb(gbbb, bbb, procPerRow)
! print *, rank,'sync:',syncTicks,'bondv1 AGB'
#else
    gbbb=bbb
#endif

#if GR_DEBUG
    print*, 'GR: gaaa ', gaaa, ' gbbb ', gbbb
#endif

    uout = (gaaa+gbbb)/2.
#ifdef WV_DEBUG
    print *, 'F95: UOUT: ',uout
#endif
#ifdef MPI
    if (isBottomRow(procPerRow)) then
#endif
      do k = 1,kp
          do j = 1,jp
              u(ip,j,k) = u(ip,j,k)-dt*uout *(u(ip,j,k)-u(ip-1,j,k))/dxs(ip)
          end do
      end do
   
      do k = 1,kp
          do j = 1,jp
              v(ip+1,j,k) = v(ip+1,j,k)-dt*uout *(v(ip+1,j,k)-v(ip,j,k))/dxs(ip)
          end do
      end do
   
      do k = 1,kp
          do j = 1,jp
              w(ip+1,j,k) = w(ip+1,j,k)-dt*uout *(w(ip+1,j,k)-w(ip,j,k))/dxs(ip)
          end do
      end do
#ifdef MPI
    end if
#endif
#if !defined(MPI) || (PROC_PER_ROW==1)
! --side flow condition; periodic
    do k = 0,kp+1
        do i = 0,ip+1
            u(i,   0,k) = u(i,jp  ,k)
            u(i,jp+1,k) = u(i,   1,k)
        end do
    end do

    do k = 0,kp+1
        do i = 0,ip+1
            v(i,   0,k) = v(i,jp  ,k)
            v(i,jp+1,k) = v(i,   1,k)
        end do
    end do

    do k = 0,kp
        do i = 0,ip+1
            w(i,   0,k) = w(i,jp  ,k)
            w(i,jp+1,k) = w(i,   1,k)
        end do
    end do
#else
    ! call assumes column (jp) index from 1, not -1 hence values are +2 from original code
#ifdef MPI
#ifdef NESTED_LES
    if (syncTicks == 0) then
#endif
!print *, rank,'sync:',syncTicks,'bondv1 BSF'
    call sideflowRightLeft(u, procPerRow, jp+2, 2, 0, 0, 0, 0)
    call sideflowLeftRight(u, procPerRow, 3, jp+3, 0, 0, 0, 0)
    call sideflowRightLeft(v, procPerRow, jp+2, 2, 0, 0, 0, 0)
    call sideflowLeftRight(v, procPerRow, 3, jp+3, 0, 0, 0, 0)
    call sideflowRightLeft(w, procPerRow, jp+2, 2, 0, 0, 1, 0)
    call sideflowLeftRight(w, procPerRow, 3, jp+3, 0, 0, 1, 0)
!print *, rank,'sync:',syncTicks,'bondv1 ASF'
#ifdef NESTED_LES
    end if
#endif
#endif
#endif

! =================================
#ifdef MPI

#ifdef NESTED_LES
   if (syncTicks == 0  .and. n > n_nest0) then
#endif

!    print *, 'n:', n,'r:',rank,'sync:',syncTicks,'bondv1 BHX2'
! --halo exchanges
    call exchangeRealHalos(u, procPerRow, neighbours, 2, 1, 1, 1)
    call exchangeRealHalos(v, procPerRow, neighbours, 2, 1, 1, 1)
    call exchangeRealHalos(w, procPerRow, neighbours, 2, 1, 1, 1)
#ifdef NESTED_LES
            end if
#endif

#endif

! -------top and underground condition
    do j = 0,jp+1
        do i = 0,ip+1
            u(i,j,   0) = -u(i,j, 1)
            u(i,j,kp+1) = u(i,j,kp)
        end do
    end do

    do j = 0,jp+1
        do i = 0,ip+1
            v(i,j,   0) = -v(i,j, 1)
            v(i,j,kp+1) = v(i,j,kp)
        end do
    end do

    do j = -1,jp+1 ! 2 !WV: I think this is wrong: j = jp+2 is not allocated!
        do i = 0,ip+1
            w(i,j, 0) = 0.0
            w(i,j,kp) = 0.0
        end do
    end do

#ifdef WV_DEBUG
    print *,'F95 UVWSUM after bondv1:',sum(u)+sum(v)+sum(w)
#endif
end subroutine bondv1

end module module_bondv1
