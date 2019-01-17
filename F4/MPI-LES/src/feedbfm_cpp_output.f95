module module_feedbfm
 contains
subroutine feedbfm(amask1,bmask1,cmask1,dmask1,zbm,z2,dzn)
    use params_common_sn
    implicit none
    real(kind=4), dimension(0:ip+1,0:jp+1,0:kp+1) , intent(Out) :: amask1
    real(kind=4), dimension(-1:ip+1,0:jp+1,0:kp+1) , intent(Out) :: bmask1
    real(kind=4), dimension(0:ip+1,-1:jp+1,0:kp+1) , intent(Out) :: cmask1
    real(kind=4), dimension(0:ip+1,0:jp+1,0:kp+1) , intent(Out) :: dmask1
    real(kind=4), dimension(-1:kp+2) , intent(In) :: dzn
    real(kind=4), dimension(0:kp+2) , intent(In) :: z2
    real(kind=4), dimension(-1:ipmax+1,-1:jpmax+1) , intent(InOut) :: zbm
    integer :: i, j, k
    real(kind=4), dimension(-1:ipmax+1,-1:jpmax+1) :: dsm,dem
!
! print *, 'Urban model'
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
! WV: better set the whole array to 0.
    zbm=0.
        ! print *, 'open GIS/Tokyo_20mgrid.txt'
        ! WV: the problem with this is that this input file expects the grid to be 150 x 150, because otherwise zbm segfaults!
!--
        open(70,file=datafile,form='formatted',status='unknown')
           do j=1,jpmax
              do i=1,ipmax
                 read(70,*) zbm(i,j)
              end do
           end do
        close(70)
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
!
end subroutine feedbfm
end module module_feedbfm
