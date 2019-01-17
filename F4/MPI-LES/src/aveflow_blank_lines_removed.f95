module module_aveflow
    implicit none
 contains
subroutine aveflow(n,n1,aveu,avev,avew,avep,&
aveuu,avevv,aveww,avesm,avesmsm, &
      uwfx,u,v,w,p,sm,nmax)
    use params_common_sn 
    real(kind=4), dimension(ip,jp,kp) , intent(Out) :: avep
    real(kind=4), dimension(ip,jp,kp) , intent(Out) :: avesm
    real(kind=4), dimension(ip,jp,kp) , intent(Out) :: avesmsm
    real(kind=4), dimension(ip,jp,0:kp) , intent(Out) :: aveu
    real(kind=4), dimension(ip,jp,kp) , intent(Out) :: aveuu
    real(kind=4), dimension(ip,jp,0:kp) , intent(Out) :: avev
    real(kind=4), dimension(ip,jp,kp) , intent(Out) :: avevv
    real(kind=4), dimension(ip+1,jp,0:kp+2) , intent(Out) :: avew
    real(kind=4), dimension(ip,jp,kp) , intent(Out) :: aveww
    real(kind=4), dimension(0:ip+1,-1:jp+1,0:kp+1) , intent(In) :: u
    real(kind=4), dimension(0:ip+1,-1:jp+1,0:kp+1) , intent(In) :: v
    real(kind=4), dimension(0:ip+1,-1:jp+1,-1:kp+1) , intent(In) :: w
    real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+1) , intent(In) :: p
    real(kind=4), dimension(-1:ip+1,-1:jp+1,0:kp+1) , intent(In) :: sm
    real(kind=4), dimension(ip,jp,kp) , intent(Out) :: uwfx
    integer :: irec
    character(len=70) :: filename
      integer :: k
      integer :: j
      integer :: i
    integer, intent(In) :: n
    integer, intent(In) :: n1
    integer, intent(In) :: nmax
    real(kind=4), dimension(0:ipmax+1,0:jpmax+1,0:kp+1) :: amask1a
    integer :: ibuffer, jbuffer 
    real(kind=4),allocatable :: aveua(:,:,:)
    real(kind=4),allocatable :: aveva(:,:,:)
    real(kind=4),allocatable :: avewa(:,:,:)
    real(kind=4),allocatable :: aveuua(:,:,:)
    real(kind=4),allocatable :: avevva(:,:,:)
    real(kind=4),allocatable :: avewwa(:,:,:)
    real(kind=4),allocatable :: uwfxa(:,:,:)
    real(kind=4),allocatable :: avepa(:,:,:)
    if(n == n1) then
        do k = 1,kp
            do j = 1,jp
                do i = 1,ip
                    avev(i,j,k) = 0.0
                    avep(i,j,k) = 0.0
                    aveuu(i,j,k) = 0.0
                    avevv(i,j,k) = 0.0
                    aveww(i,j,k) = 0.0
                    avesm(i,j,k) = 0.0
                    avesmsm(i,j,k) = 0.0
                    uwfx(i,j,k) = 0.0
                end do
            end do
        end do
        do k = 0,kp
            do j = 1,jp
                do i = 1,ip
                    aveu(i,j,k) = 0.0
                end do
            end do
        end do
        do k = 0,kp
            do j = 1,jp
                do i = 1,ip+1
                    avew(i,j,k) = 0.0
                end do
            end do
        end do
    end if
    if(n >= n1) then
        do k = 1,kp
            do j = 1,jp
                do i = 1,ip
                    avev(i,j,k) = avev(i,j,k)+v(i,j,k)
                    avep(i,j,k) = avep(i,j,k)+p(i,j,k)
                    aveuu(i,j,k) = aveuu(i,j,k)+u(i,j,k)**2
                    avevv(i,j,k) = avevv(i,j,k)+v(i,j,k)**2
                    aveww(i,j,k) = aveww(i,j,k)+w(i,j,k)**2
                    avesm(i,j,k) = avesm(i,j,k)+sm(i,j,k)
                    avesmsm(i,j,k) = avesmsm(i,j,k)+sm(i,j,k)**2
                    uwfx(i,j,k) = uwfx(i,j,k)+0.5*(u(i,j,k-1)+u(i,j,k)) * &
                                  0.5*(w(i,j,k-1)+w(i+1,j,k-1))
                end do
            end do
        end do
        do k = 0,kp
            do j = 1,jp
                do i = 1,ip
                    aveu(i,j,k) = aveu(i,j,k)+u(i,j,k)
                end do
            end do
        end do
        do k = 0,kp
            do j = 1,jp
                do i = 1,ip+1
                    avew(i,j,k) = avew(i,j,k)+w(i,j,k)
                end do
            end do
        end do
  endif
  if(n == nmax) then
      do k = 1,kp
          do j = 1,jp
              do i = 1,ip
                  avev(i,j,k) = avev(i,j,k)/real(nmax-n1+1)
                  avep(i,j,k) = avep(i,j,k)/real(nmax-n1+1)
                  aveuu(i,j,k) = aveuu(i,j,k)/real(nmax-n1+1)
                  avevv(i,j,k) = avevv(i,j,k)/real(nmax-n1+1)
                  aveww(i,j,k) = aveww(i,j,k)/real(nmax-n1+1)
                  avesm(i,j,k) = avesm(i,j,k)/real(nmax-n1+1)
                  avesmsm(i,j,k) = avesmsm(i,j,k)/real(nmax-n1+1)
              end do
            end do
        end do
        do k = 0,kp
            do j = 1,jp
                do i = 1,ip
                    aveu(i,j,k) = aveu(i,j,k)/real(nmax-n1+1)
                end do
            end do
        end do
        do k = 0,kp
            do j = 1,jp
                do i = 1,ip+1
                    avew(i,j,k) = avew(i,j,k)/real(nmax-n1+1)
                end do
            end do
        end do
        do k = 1,kp
            do j = 1,jp
                do i = 1,ip
                    uwfx(i,j,k) = uwfx(i,j,k)/real(nmax-n1+1) - &
                                  0.5*(aveu(i,j,k-1)+aveu(i,j,k)) * &
                                  0.5*(avew(i,j, k-1)+avew(i+1,j,k-1))
                end do
            end do
        end do
        do k = 1,kp
            do j = 1,jp
                do i = 1,ip
                    aveuu(i,j,k) = sqrt(abs(aveuu(i,j,k)-aveu(i,j,k)**2))
                    avevv(i,j,k) = sqrt(abs(avevv(i,j,k)-avev(i,j,k)**2))
                    aveww(i,j,k) = sqrt(abs(aveww(i,j,k)-avew(i,j,k)**2))
                end do
            end do
        end do
    endif 
end subroutine aveflow
end module module_aveflow
