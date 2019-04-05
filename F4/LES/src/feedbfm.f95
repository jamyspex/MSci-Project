module module_feedbfm
#ifdef MPI
    use communication_helper_real
#endif
contains
#ifdef WV_NEW_FEEDBF
subroutine feedbfm(zbm)
#else
subroutine feedbfm(amask1,bmask1,cmask1,dmask1,zbm,z2,dzn)
#endif
#ifdef WV_NEW
    use params_common_sn
#else
    use common_sn ! create_new_include_statements() line 102
#endif
    implicit none
#ifndef WV_NEW_FEEDBF
    real(kind=4), dimension(0:ip+1,0:jp+1,0:kp+1) , intent(Out) :: amask1
    real(kind=4), dimension(-1:ip+1,0:jp+1,0:kp+1) , intent(Out) :: bmask1
    real(kind=4), dimension(0:ip+1,-1:jp+1,0:kp+1) , intent(Out) :: cmask1
    real(kind=4), dimension(0:ip+1,0:jp+1,0:kp+1) , intent(Out) :: dmask1
    real(kind=4), dimension(-1:kp+2) , intent(In) :: dzn
    real(kind=4), dimension(0:kp+2) , intent(In) :: z2

#endif
    real(kind=4), dimension(-1:ipmax+1,-1:jpmax+1) , intent(InOut) :: zbm
    integer :: i, j, k
    real(kind=4), dimension(-1:ipmax+1,-1:jpmax+1) :: dsm,dem

#ifndef WV_NEW_FEEDBF
!
!    print *, 'Urban model'
! -------Urban model----------
    do k = 1,kp
        do j = 1,jp
            do i = 1,ip
                amask1(i,j,k) = 1.
                bmask1(i,j,k) = 0.
                cmask1(i,j,k) = 0.
                dmask1(i,j,k) = 0.
            end do
        end do
    end do
#endif
! WV: better set the whole array to 0.
    zbm=0.

#ifdef VERBOSE
#ifdef MPI
    if (isMaster()) then
#endif
        print*, 'GIS file getting read into zbm'
#ifdef MPI
    end if
#endif
#endif
        !      print *, 'open GIS/Tokyo_20mgrid.txt'
        ! WV: the problem with this is that this input file expects the grid to be 150 x 150, because otherwise zbm segfaults!

!--
#ifdef MPI
      if (isMaster()) then
#endif
        open(70,file=datafile,form='formatted',status='unknown')
           do j=1,jpmax
              do i=1,ipmax
                 read(70,*) zbm(i,j)
              end do
           end do
        close(70)
#ifdef MPI
      end if !for imaster
      call distributeZBM(zbm, ip, jp, ipmax, jpmax, procPerRow)
#endif

#ifndef WV_NEW_FEEDBF
! print *, 'assign amask'
      do j = 1,jp
          do i = 1,ip
              do k = 1,kp
                  if(zbm(i,j) > z2(k)+0.5*dzn(k)) then
                      amask1(i,j,k) = 0.0
                  end if
              end do
          end do
      end do

! -----------------------------------------------------------------------
!print *, 'assign bcd masks'
    do k = 1,kp
        do j = 1,jp
            do i = 1,ip
                if(amask1(i,j,k) == 0.0) then
                    bmask1(i,j,k) = 1.0
                    cmask1(i,j,k) = 1.0
                    dmask1(i,j,k) = 1.0
                end if
            end do
        end do
    end do
#ifdef MPI
    call exchangeRealHalos(amask1, procPerRow, neighbours, 1, 1, 1, 1)
    call exchangeRealHalos(bmask1, procPerRow, neighbours, 1, 1, 2, 1)
    call exchangeRealHalos(cmask1, procPerRow, neighbours, 2, 1, 1, 1)
    call exchangeRealHalos(dmask1, procPerRow, neighbours, 1, 1, 1, 1)
#endif

#endif
!
end subroutine feedbfm

end module module_feedbfm
