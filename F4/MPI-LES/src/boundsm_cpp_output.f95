module module_boundsm
 contains
subroutine boundsm(sm)
    use params_common_sn
    implicit none
    real(kind=4), dimension(-1:ip+1,-1:jp+1,0:kp+1) , intent(InOut) :: sm
    integer :: i, j, k
!
! =================================
        do k = 0,kp+1
            do j = -1,jp+1
                    sm( 0,j,k) = sm(1 ,j,k) ! GR: Why not sm(-1,,) = sm(0,,)?
                    sm(ip+1,j,k) = sm(ip,j,k)
            end do
        end do
! --side flow condition
        do k = 0,kp+1
            do i = 0,ip+1
                    sm(i,jp+1,k) = sm(i,jp ,k)
                    sm(i,0,k) = sm(i,1 ,k) ! GR: Why not sm(,-1,) = sm(,0,)?
            end do
        end do
! --underground condition
    do j = -1,jp+1
        do i = 0,ip+1
            sm(i,j, 0) = -sm(i,j, 1)
            sm(i,j,kp+1) = sm(i,j,kp)
        end do
    end do
end subroutine boundsm
end module module_boundsm
