module module_set
#ifdef MPI
#ifdef NESTED_LES
      use nesting_support
#endif
#endif
 contains 
      subroutine set(data10,data11,data20,data21,data22,data23,data24,data25,data26,data27,data30, &
      data31,ical,nif,n0,n1,nmax,dt,ro,vn,alpha,beta,data12,data13,data14,data15)
#ifdef WV_NEW
    use params_common_sn
    implicit none
#else
    use common_sn ! create_new_include_statements() line 102
#endif
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
!        integer, intent(Out) :: ianime
        integer, intent(Out) :: ical
!        integer, intent(Out) :: ifbf
        integer, intent(Out) :: n0
        integer, intent(Out) :: nif
        integer, intent(Out) :: n1
        integer, intent(Out) :: nmax
        real(kind=4), intent(Out) :: ro
        real(kind=4), intent(Out) :: vn
!
!
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
!      data50:tim series of wind data
!      data51:tim series od concentration data
! --flow region
!      im = ip
!      jm = jp
!      km = kp
!#if IFBF == 1
!! --if
!      ifbf = 1
!#else
!! --if
!      ifbf = 0
!#endif
!#if IANIME == 1
!      ianime = 1
!#else
!      ianime = 0
!#endif
! -- call indata
!      ical = 0; initial start
!           = 1; continuous computation
      ical = 0
      nif = 8000
      n0 = 1
      n1 = 1
! --setnmax
#ifndef WV_TIMESTEPS
        nmax = 8000
#else
        nmax = WV_TIMESTEPS
#endif
#ifdef NESTED_LES
#ifdef MPI
        if (inNestedGrid()) then
            nmax = nmax*(dt_orig/dt_nest) ! 40
!        else
!            nmax = nmax !+1 ! 20
        end if
#else
    nmax = nmax*(dt_orig/dt_nest) ! 40
#endif
#endif
! --time step
! WV: NESTING: we need to set dt based on the subgrid coordinates in params_common_sn
#ifndef NESTED_LES
        dt = dt_orig ! seconds
#else
#ifdef MPI
        if (inNestedGrid()) then
!            print *, 'Process ',rank, ' is in nested grid'
            dt = dt_nest
        else
!            print *, 'Process ',rank, ' is in orig grid'
            dt = dt_orig
        end if
#else
        dt = dt_nest
#endif
#endif
! --physical property set
      ro = 1.1763
      vn = 15.83*10.**(-6.)
! --IBM parameter set (Feedback force by Goldstein)
      alpha = -10.
      beta = -1.
! -----check IBM parameter----
      if(((-beta-(beta*beta-2.*alpha)**(0.5))/alpha) < dt) then
      write(6,*) 'CHECK parameter alpha beta of IBM method'
      write(6,*) 'IBM parameter, dt=' ,((-beta-(beta*beta-2.*alpha)**(0.5))/alpha),dt
      stop
      endif
! =======================================
      return
      end subroutine set
end module module_set
