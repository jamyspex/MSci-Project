module module_set
 contains
      subroutine set(data10,data11,data20,data21,data22,data23,data24,data25,data26,data27,data30, &
      data31,ical,nif,n0,n1,nmax,dt,ro,vn,alpha,beta,data12,data13,data14,data15)
      implicit none
integer, parameter :: kp=80
    integer, parameter :: ip = 300
    integer, parameter :: jp = 300
    integer, parameter :: ipmax = ip
    integer, parameter :: jpmax = jp
    character(300) :: datafile = '../GIS/Kyoto_1km2_4m_with_buffer.txt'
    real, parameter :: dxgrid = 4.
    real, parameter :: dygrid = 4.
    real, parameter :: cs0 = 0.14
    integer, parameter :: i_anime=1
    integer, parameter :: avetime=2
    integer, parameter :: km_sl=80
    integer, parameter :: i_aveflow=0
    integer, parameter :: i_ifdata_out=0
    real, parameter :: dt_orig = 0.05 
            real(kind=4), intent(Out) :: alpha
        real(kind=4), intent(Out) :: beta
        character(len=70), intent(InOut) :: data10
        character(len=70), intent(InOut) :: data11
        character(len=70), intent(InOut) :: data12
        character(len=70), intent(InOut) :: data13
        character(len=70), intent(InOut) :: data14
        character(len=70), intent(InOut) :: data15
        character(len=70), intent(InOut) :: data20
        character(len=70), intent(InOut) :: data21
        character(len=70), intent(InOut) :: data22
        character(len=70), intent(InOut) :: data23
        character(len=70), intent(InOut) :: data24
        character(len=70), intent(InOut) :: data25
        character(len=70), intent(InOut) :: data26
        character(len=70), intent(InOut) :: data27
        character(len=70), intent(InOut) :: data30
        character(len=70), intent(InOut) :: data31
        real(kind=4), intent(Out) :: dt
        integer, intent(Out) :: ical
        integer, intent(Out) :: n0
        integer, intent(Out) :: nif
        integer, intent(Out) :: n1
        integer, intent(Out) :: nmax
        real(kind=4), intent(Out) :: ro
        real(kind=4), intent(Out) :: vn
      data10 = '../ave_data/data10'
      data11 = '../ave_data/data11'
      data12 = '../data/data12'
      data13 = '../data/data13'
      data14 = '../data/data14'
      data15 = '../data/data15'
      data20 = '../data/data20'
      data21 = '../data/data21'
      data22 = '../data/data22'
      data23 = '../data/data23'
      data24 = '../data/data24'
      data25 = '../data/data25'
      data26 = '../data/data26'
      data27 = '../data/data27'
      data30 = '../data/data30'
      data31 = '../data/data31'
      ical = 0
      nif = 8000
      n0 = 1
      n1 = 1
        nmax = 8000
        dt = dt_orig 
      ro = 1.1763
      vn = 15.83*10.**(-6.)
      alpha = -10.
      beta = -1.
      if(((-beta-(beta*beta-2.*alpha)**(0.5))/alpha) < dt) then
      write(6,*) 'CHECK parameter alpha beta of IBM method'
      write(6,*) 'IBM parameter, dt=' ,((-beta-(beta*beta-2.*alpha)**(0.5))/alpha),dt
      stop
      endif
      return
      end subroutine set
end module module_set
