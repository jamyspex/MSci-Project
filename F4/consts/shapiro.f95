subroutine shapiro(j,k,wet,etan,eps,eta)
      integer(4), parameter :: nx = 500 
      integer(4), parameter :: ny = 500 
      integer, intent(InOut) :: j
      integer, intent(InOut) :: k
      integer, dimension(0:501,0:501), intent(In) :: wet
      real, dimension(0:501,0:501), intent(In) :: etan
      real, intent(In) :: eps
      real, dimension(0:501,0:501), intent(Out) :: eta
      real :: term1
      real :: term2
      real :: term3
    do j = 1, 500, 1
        do k = 1, 500, 1
            if (wet(j,k)==1) then
                term1 = (1.0-0.25*eps*(wet(j,k+1)+wet(j,k-1)+wet(j+1,k)+wet(j-1,k)))*etan(j,k)
                term2 = 0.25*eps*(wet(j,k+1)*etan(j,k+1)+wet(j,k-1)*etan(j,k-1))
                term3 = 0.25*eps*(wet(j+1,k)*etan(j+1,k)+wet(j-1,k)*etan(j-1,k))
                eta(j,k) = term1+term2+term3
            else
                eta(j,k) = etan(j,k)
            end if
        end do
    end do
end subroutine shapiro
