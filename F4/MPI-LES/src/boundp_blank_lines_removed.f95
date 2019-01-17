module module_boundp
    use params_common_sn
implicit none
 contains
subroutine boundp2(p)
    use params_common_sn
    real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+1) , intent(InOut) :: p
    integer :: i, j
    do j = 0,jp+1
        do i = 0,ip+1
            p(i,j, 0) = p(i,j,1)
            p(i,j,kp+1) = p(i,j,kp)
        end do
    end do
end subroutine boundp2
subroutine boundp1(p)
    use params_common_sn
    real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+1) , intent(InOut) :: p
    integer :: i, j, k
        do k = 0,kp+1
            do j = 0,jp+1
                    p( 0,j,k) = p(1 ,j,k)
                    p(ip+1,j,k) = p(ip,j,k)
            end do
        end do
    do k = 0,kp+1
        do i = 0,ip+1
            p(i, 0,k) = p(i,jp,k) 
            p(i,jp+1,k) = p(i, 1,k) 
        end do
    end do
end subroutine boundp1
end module module_boundp
